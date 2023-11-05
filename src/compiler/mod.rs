//! This module holds the compiler, which takes Poligon files and compiles them into executables.
//! 
//! The compiler has 3 steps:
//! 1. PLIR code generation
//! 2. LLVM code generation
//! 3. Linking and compilation
//! 
//! The [`Compiler`] struct can do all 3, but each module
//! holds methods to activate each part independently.
//! 
//! # PLIR
//! PLIR, the Poligon Language Intermediate Representation, 
//! simplifies the main language and adds extra restrictions
//! in order to make it simpler to compiler to LLVM.
//! 
//! PLIR can be generated from AST via the [`plir_codegen::plir_codegen`] function, 
//! or the [`PLIRCodegen`] struct.
//! 
//! # LLVM
//! LLVM is a low-level backend for languages and is the backing for `clang`.
//! LLVM holds many tools to compile LLVM into executables.
//! 
//! LLVM can be codegenerated with the [`LLVMCodegen`] struct.
//! 
//! # Linking & compilation
//! The full compilation process is completed by the [`Compiler`] struct.
//! The final step enables functions to be imported across Poligon executables
//! (NOTE: currently, only the standard library is supported).
//! 
//! See the [`Compiler`] struct for usage.

pub mod plir_codegen;
pub mod plir;
mod llvm;
mod internals;
pub mod llvm_codegen;
mod dsds;
mod d_types;

use std::ffi::OsStr;
use std::fmt::Display;
use std::fs;
use std::io::Error as IoErr;
use std::path::{Path, PathBuf};

use inkwell::OptimizationLevel;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::{PassManagerBuilder, PassManager};
use once_cell::sync::Lazy;
pub use plir_codegen::{PLIRCodegen, PLIRErr, PLIRResult};
pub use llvm_codegen::{LLVMCodegen, LLVMErr, LLVMResult};

use crate::err::{FullGonErr, GonErr};
use crate::lexer;
use crate::parser;

use d_types::DeclaredTypes;


macro_rules! to_str {
    ($e:expr) => { $e.to_str().expect("Expected UTF-8 str") }
}
use to_str;

/// Errors that can occur during the full compilation process.
#[derive(Debug)]
pub enum CompileErr {
    #[allow(missing_docs)]
    IoErr(IoErr),
    #[allow(missing_docs)]
    Computed(String),
    #[allow(missing_docs)]
    LLVMErr(LLVMErr)
}

macro_rules! compile_err_impl_from {
    ($e:ident: $t:ty) => {
        impl From<$t> for CompileErr {
            fn from(value: $t) -> CompileErr {
                Self::$e(value)
            }
        }
    };
}

compile_err_impl_from! { IoErr: IoErr }
compile_err_impl_from! { LLVMErr: LLVMErr }

fn cast_e<T, E: GonErr>(r: Result<T, FullGonErr<E>>, code: &str) -> CompileResult<T> {
    r.map_err(|e| CompileErr::Computed(e.full_msg(code)))
}

/// Parses code from a `.d.plir.gon` file into a [`DeclaredTypes`] struct.
fn parse_d_types_from_str(code: &str) -> CompileResult<DeclaredTypes> {
    let stream = cast_e(lexer::tokenize(code), code)?;
    cast_e(parser::parse(&stream), code)
}

impl Display for CompileErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileErr::IoErr(e) => write!(f, "{}", e.at_unknown().short_msg()),
            CompileErr::Computed(e) => write!(f, "{e}"),
            CompileErr::LLVMErr(e) => write!(f, "{}", e.at_unknown().short_msg()),
        }
    }
}
impl std::error::Error for CompileErr {
    fn cause(&self) -> Option<&dyn std::error::Error> {
        match self {
            CompileErr::IoErr(e) => Some(e),
            CompileErr::Computed(_) => None,
            CompileErr::LLVMErr(e) => Some(e),
        }
    }
}

/// A [`Result`] type for operations during the full compilation process.
pub type CompileResult<T> = Result<T, CompileErr>;

// TODO: 
// as it stands, the Compiler does not support importing types & values well.
// any module can access the data of the module loaded before it.
// it also reexports every module, which can lead to duplication if the module
// was ever used again.
// a more reasonable idea is to keep a store HashMap<String, DeclaredTypes>
// and store what modules a module is dependent on in DeclaredTypes.

/// A compiler, which keeps track of multiple files being compiled.
/// 
/// To construct a compiler struct, one can use:
/// - [`Compiler::new`]: Initializes the compiler struct with the standard library linked
/// - [`Compiler::no_std`]: Initializes the compiler struct without the standard library linked
///     - This is useful for constructing builtin functions that should not rely on the standard library.
/// 
/// Files can be loaded into the compiler. When loaded, the compiler links the code into the
/// module it is building.
/// - [`Compiler::load_gon_file`]: Load a Poligon file.
/// - [`Compiler::load_gon_str`]: Load a string which holds Poligon code.
/// - [`Compiler::load_bc`]: Load both a declarations PLIR file (`.d.plir.gon`) and an LLVM bitcode file (`.bc`).
/// 
/// Finally, the module created can be extracted using [`Compiler::write_to_disk`] or ran with
/// [`Compiler::jit_run`].
/// 
/// # Usage
/// ```no_run
/// use std::path::Path;
/// use inkwell::context::Context;
/// use poligon_lang::compiler::Compiler;
/// 
/// let path: &Path = "script.gon".as_ref();
/// 
/// let ctx = Context::create();
/// // loads compiler with std
/// let mut compiler = Compiler::new(&ctx, "script.gon").unwrap();
/// compiler.load_gon_file(path).unwrap();
/// 
/// compiler.write_to_disk().unwrap();
/// ```
pub struct Compiler<'ctx> {
    module: Module<'ctx>,
    declared_types: DeclaredTypes,

    ctx: &'ctx Context,
    llvm_codegen: LLVMCodegen<'ctx>,
}

static STD_FILES: Lazy<Vec<(PathBuf, PathBuf)>> = Lazy::new(|| {
    let std_path = "std";
    
    fs::read_dir(std_path).unwrap()
        .filter_map(|me| {
            let path = me.ok()?.path();

            (path.extension() == Some("bc".as_ref()))
                .then_some(path)
        })
        .map(|bc| (bc.with_extension("d.plir.gon"), bc))
        .collect()
});

impl<'ctx> Compiler<'ctx> {
    /// Create a new compiler.
    /// 
    /// This also loads the Poligon std library.
    pub fn new(ctx: &'ctx Context, in_path: impl AsRef<Path>) -> CompileResult<Self> {
        let mut compiler = Self::no_std(ctx, in_path);

        for (dfile, bc_file) in &*STD_FILES {
            compiler.load_bc(dfile, bc_file)?;
        }

        Ok(compiler)
    }

    /// Creates a new compiler without the Poligon std library.
    pub fn no_std(ctx: &'ctx Context, in_path: impl AsRef<Path>) -> Self {
        let in_path = in_path.as_ref().to_owned();
        let filename = to_str!(in_path.file_name().unwrap());

        Self {
            declared_types: Default::default(),
            ctx,
            llvm_codegen: LLVMCodegen::new(ctx),
            module: ctx.create_module(filename)
        }
    }

    fn update_values(&mut self, dtypes: DeclaredTypes, new_module: Module<'ctx>) -> CompileResult<()> {
        self.declared_types += dtypes;

        if let Some(f) = self.module.get_function("main") {
            unsafe { f.delete() };
        }

        self.module.link_in_module(new_module)
            .and_then(|_| self.module.verify())
            .map_err(LLVMErr::LLVMErr)
            .map_err(Into::into)
    }

    /// Sets the module's filename (including extension)
    pub fn set_module_name(&mut self, filename: &str) {
        self.llvm_codegen.set_filename(filename)
    }

    /// Load a Poligon file into the compiler.
    /// 
    /// In loading the file, type data is preserved in the compiler and the LLVM module
    /// that holds this Poligon file is linked to the compiler's module.
    pub fn load_gon_file(&mut self, p: impl AsRef<Path>) -> CompileResult<()> {
        let path = p.as_ref();
        let filename = path.file_name().and_then(OsStr::to_str);
        let code = fs::read_to_string(path)?;

        self.load_gon_str(&code, filename)
    }

    /// Load a string holding Poligon code into the compiler. The filename of the Poligon code
    /// should be included if present.
    /// 
    /// In loading the file, type data is preserved in the compiler and the LLVM module
    /// that holds this Poligon file is linked to the compiler's module.
    pub fn load_gon_str(&mut self, code: &str, filename: Option<&str>) -> CompileResult<()> {
        let (plir, dtypes) = self.generate_plir(code)?;
        self.set_module_name(filename.unwrap_or("eval"));
        self.load_plir(&plir, dtypes)
    }

    /// Generates PLIR from Poligon code.
    /// 
    /// This function allows Poligon code to reference any declared classes and functions in the compiler.
    /// To load Poligon code into the compiler directly, use [`Compiler::load_gon_file`] or [`Compiler::load_gon_str`].
    pub fn generate_plir(&mut self, code: &str) -> CompileResult<(plir::Program, DeclaredTypes)> {
        use crate::ast;

        let lexed = cast_e(lexer::tokenize(code), code)?;
        let ast: ast::Program = cast_e(parser::parse(&lexed), code)?;

        let mut cg = PLIRCodegen::new();
        cg.add_external_types(self.declared_types.clone());
        cast_e(cg.consume_program(ast), code)?;
        
        let dt = cg.exports();
        let plir = cast_e(cg.unwrap(), code)?;
        
        Ok((plir, dt))
    }

    /// Loads a PLIR tree and the type data of the tree into the compiler.
    /// 
    /// This updates the compiler's LLVM module to include a compiled version of the PLIR code.
    /// To load Poligon code into the compiler directly, use [`Compiler::load_gon_file`] or [`Compiler::load_gon_str`].
    pub fn load_plir(&mut self, plir: &plir::Program, dtypes: DeclaredTypes) -> CompileResult<()> {
        self.llvm_codegen.compile(plir)?;

        let module = self.llvm_codegen.pop_module();

        self.update_values(dtypes, module)?;
        self.llvm_codegen.load_declared_types(&self.declared_types)?;

        Ok(())
    }

    /// Loads bitcode and type data into the compiler.
    /// 
    /// In loading these files, type data and the bitcode are linked in the compiler.
    pub fn load_bc(&mut self, dfile: impl AsRef<Path>, bc_file: impl AsRef<Path>) -> CompileResult<()> {
        let code = fs::read_to_string(dfile)?;
        
        let dtypes = parse_d_types_from_str(&code)?;
        let module = Module::parse_bitcode_from_path(bc_file, self.ctx)
            .map_err(LLVMErr::LLVMErr)?;
    
        self.llvm_codegen.load_declared_types(&dtypes)?;
        self.update_values(dtypes, module)
    }

    /// Exports the module data created by this compiler.
    pub fn export_module(self) -> (Module<'ctx>, DeclaredTypes) {
        (self.module, self.declared_types)
    }
    /// Creates an exporter for the module data created by this compiler.
    pub fn into_exporter(self) -> Exporter<'ctx> {
        let (m, dtypes) = self.export_module();
        Exporter::new(m, dtypes)
    }
}

/// This struct converts a module in memory into some file equivalent
/// (whether that would be text representation or an executable).
/// 
/// To create a usable module, see [`Exporter::export`].
pub struct Exporter<'ctx> {
    module: Module<'ctx>,
    exported_types: DeclaredTypes
}
impl<'ctx> Exporter<'ctx> {
    /// Creates a new instance of this struct.
    pub fn new(module: Module<'ctx>, exported_types: DeclaredTypes) -> Self {
        Self { module, exported_types }
    }

    /// Gets the module's filename (including file extension)
    pub fn get_module_name(&self) -> &str {
        self.module.get_name().to_str().unwrap()
    }

    /// Gets the module's file stem (which is the name excluding extension)
    pub fn get_module_stem(&self) -> String {
        AsRef::<Path>::as_ref(self.get_module_name())
            .with_extension("")
            .to_string_lossy()
            .into_owned()
    }

    /// Optimizes module using the provided optimization level.
    pub fn optimize_module(&self, opt: OptimizationLevel) {
        let pm_builder = PassManagerBuilder::create();
        pm_builder.set_optimization_level(opt);

        let pm = PassManager::create(());
        pm_builder.populate_module_pass_manager(&pm);
        if opt != OptimizationLevel::None {
            pm.add_function_inlining_pass();
        }

        pm.run_on(&self.module);
    }

    /// Writes an .ll containing the module's LLVM bytecode.
    pub fn write_ll(&self, dest: impl AsRef<Path>) -> CompileResult<()> {
        llvm_codegen::module_to_ll(&self.module, dest)
            .map_err(Into::into)
    }

    /// Writes a .d.plir.gon file containing the types exported by this module.
    pub fn write_d(&self, dest: impl AsRef<Path>) -> CompileResult<()> {
        self.exported_types.to_file(dest).map_err(Into::into)
    }

    /// Writes a .bc file containing the module's LLVM bitcode.
    pub fn write_bc(&self, dest: impl AsRef<Path>) {
        llvm_codegen::module_to_bc(&self.module, dest)
    }

    /// Writes both the .d.plir.gon and .bc files in the provided directory.
    /// 
    /// Both are necessary for this module to be imported from other Poligon modules.
    pub fn write(&self, dir: impl AsRef<Path>) -> CompileResult<()> {
        let dir = dir.as_ref();
        fs::create_dir_all(dir)?;

        let mod_stem = self.get_module_stem();
        let bc_path = dir.join(format!("{mod_stem}.bc"));
        let  d_path = dir.join(format!("{mod_stem}.d.plir.gon"));

        self.write_d(d_path)?;
        self.write_bc(bc_path);
        Ok(())
    }

    /// Executes the current module JIT, and returns the resulting value.
    /// 
    /// # Safety
    /// This holds the same safety restraints as [`LLVMCodegen::jit_run`].
    pub unsafe fn jit_run(&self) -> CompileResult<std::process::ExitCode> {
        self.jit_run_raw().map(|t| std::process::ExitCode::from(t as u8))
    }

    /// Executes the current module JIT, and returns the resulting value.
    /// 
    /// # Safety
    /// This holds the same safety restraints as [`LLVMCodegen::jit_run`].
    unsafe fn jit_run_raw(&self) -> CompileResult<std::ffi::c_int> {
        let main = self.module.get_function("main").expect("Expected main function");

        llvm_codegen::module_jit_run(&self.module, main)
            .map_err(Into::into)
    }
}