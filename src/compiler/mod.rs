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
pub(self) mod internals;
pub mod llvm_codegen;
mod d_parser;
mod dsds;

use std::ffi::OsStr;
use std::fmt::Display;
use std::fs;
use std::io::Error as IoErr;
use std::path::{Path, PathBuf};

use inkwell::OptimizationLevel;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::{PassManagerBuilder, PassManager};
pub use plir_codegen::{PLIRCodegen, PLIRErr, PLIRResult};
pub use llvm_codegen::{LLVMCodegen, LLVMErr, LLVMResult};

use crate::err::{FullGonErr, GonErr};
use crate::lexer;
use crate::parser;

use self::d_parser::DParser;
use self::plir_codegen::DeclaredTypes;

use lazy_static::lazy_static;

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

/// Indicates the files where [`Compiler::write_to_disk`] should save its results to.
pub enum GonSaveTo<'p> {
    /// Save the files to the same position in the file system as the provided path, 
    /// using the same name (but different extension) as the provided path.
    SameLoc(&'p Path),

    /// Save to the provided file paths.
    DiffLoc {
        /// This path is where PLIR types should be saved.
        plir: &'p Path,

        /// This path is where LLVM .bc should be saved.
        llvm: &'p Path
    }
}

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
/// use poligon_lang::compiler::{Compiler, GonSaveTo};
/// 
/// let path: &Path = "script.gon".as_ref();
/// 
/// let ctx = Context::create();
/// // loads compiler with std
/// let mut compiler = Compiler::new(&ctx, "script.gon").unwrap();
/// compiler.load_gon_file(path).unwrap();
/// 
/// compiler.write_to_disk(GonSaveTo::DiffLoc {
///     plir: "script.d.plir.gon".as_ref(), // this saves type information for script
///     llvm: "script.bc".as_ref(), // this is bitcode which can be executed by `lli` or `llc`
/// }).unwrap();
/// ```
pub struct Compiler<'ctx> {
    declared_types: DeclaredTypes,
    ctx: &'ctx Context,
    llvm_codegen: LLVMCodegen<'ctx>,
    module: Module<'ctx>,
    opt: OptimizationLevel
}

lazy_static! {
    static ref STD_PATH: &'static Path = "std".as_ref();
    static ref STD_FILES: Vec<(PathBuf, PathBuf)> = {
        fs::read_dir(*STD_PATH).unwrap()
            .filter_map(|me| {
                let path = me.ok()?.path();

                (path.extension() == Some("bc".as_ref()))
                    .then_some(path)
            })
            .map(|bc| (bc.with_extension("d.plir.gon"), bc))
            .collect()
    };
}
impl<'ctx> Compiler<'ctx> {
    /// Create a new compiler. 
    /// 
    /// This includes the Poligon std library.
    pub fn new(ctx: &'ctx Context, filename: &str) -> CompileResult<Self> {
        let mut compiler = Self::no_std(ctx, filename);

        for (dfile, bc_file) in &*STD_FILES {
            compiler.load_bc(dfile, bc_file)?;
        }

        Ok(compiler)
    }

    /// Creates a new compiler without the Poligon std library.
    pub fn no_std(ctx: &'ctx Context, filename: &str) -> Self {
        Self {
            declared_types: Default::default(),
            ctx,
            llvm_codegen: LLVMCodegen::new(ctx),
            module: ctx.create_module(filename),
            opt: Default::default()
        }
    }

    fn get_declared_types_from_d(&mut self, code: &str) -> CompileResult<DeclaredTypes> {
        let lexed = cast_e(lexer::tokenize(code), code)?;
        let parser = DParser::new(lexed, self.declared_types.clone());
        let dt = cast_e(parser.unwrap_dtypes(), code)?;
        Ok(dt)
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

    /// Sets the module's filename
    pub fn set_filename(&mut self, filename: &str) {
        self.llvm_codegen.set_filename(filename)
    }

    /// Sets the compiler's optimization for modules
    pub fn set_optimization(&mut self, opt: OptimizationLevel) {
        self.opt = opt;
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
        self.set_filename(filename.unwrap_or("eval"));
        self.load_plir(&plir, dtypes)
    }

    /// Generates PLIR from Poligon code.
    /// 
    /// This function allows Poligon code to reference any declared classes and functions in the compiler.
    /// To load Poligon code into the compiler directly, use [`Compiler::load_gon_file`] or [`Compiler::load_gon_str`].
    pub fn generate_plir(&mut self, code: &str) -> CompileResult<(plir::Program, DeclaredTypes)> {
        let lexed = cast_e(lexer::tokenize(code), code)?;
        let ast   = cast_e(parser::parse(lexed), code)?;

        let mut cg = PLIRCodegen::new_with_declared_types(self.declared_types.clone());
        cast_e(cg.consume_program(ast), code)?;
        
        let dt = cg.declared_types();
        let plir = cast_e(cg.unwrap(), code)?;
        
        Ok((plir, dt))
    }

    /// Loads a PLIR tree and the type data of the tree into the compiler.
    /// To load Poligon code into the compiler directly, use [`Compiler::load_gon_file`] or [`Compiler::load_gon_str`].
    /// 
    /// This updates the compiler's LLVM module to include a compiled version of the PLIR code.
    /// 
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
        
        let dtypes = self.get_declared_types_from_d(&code)?;
        let module = Module::parse_bitcode_from_path(bc_file, self.ctx)
            .map_err(LLVMErr::LLVMErr)?;
    
        self.llvm_codegen.load_declared_types(&dtypes)?;
        self.update_values(dtypes, module)
    }

    fn optimize_module(&self) {
        let pm_builder = PassManagerBuilder::create();
        pm_builder.set_optimization_level(self.opt);

        let pm = PassManager::create(());
        pm_builder.populate_module_pass_manager(&pm);
        if self.opt != OptimizationLevel::None {
            pm.add_function_inlining_pass();
        }
        pm.run_on(&self.module);
    }

    /// Writes the type data and module to disk.
    pub fn write_to_disk(&self, write_to: GonSaveTo) -> CompileResult<()> {
        self.optimize_module();
        match write_to {
            GonSaveTo::SameLoc(path) => {
                let mut llvm_path = path.to_owned();
                llvm_path.set_extension("bc");

                let plir_path = llvm_path.with_extension("d.plir.gon");

                self.declared_types.to_file(plir_path)?;
                llvm_codegen::module_to_bc(&self.module, llvm_path);
            },
            GonSaveTo::DiffLoc { plir, llvm } => {
                self.declared_types.to_file(plir)?;
                llvm_codegen::module_to_bc(&self.module, llvm);
            },
        };

        Ok(())
    }

    /// Writes the module as LLVM bytecode to disk.
    pub fn to_ll(&self, dest: impl AsRef<Path>) -> CompileResult<()> {
        self.optimize_module();
        llvm_codegen::module_to_ll(&self.module, dest)
            .map_err(Into::into)
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
        self.optimize_module();
        let main = self.module.get_function("main").expect("Expected main function");

        llvm_codegen::module_jit_run(&self.module, main)
            .map_err(Into::into)
    }
}