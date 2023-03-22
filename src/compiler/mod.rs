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

use std::fmt::Display;
use std::fs;
use std::io::Error as IoErr;
use std::path::{Path, PathBuf};

use inkwell::context::Context;
use inkwell::module::Module;
pub use plir_codegen::{PLIRCodegen, PLIRErr, PLIRResult};
pub use llvm_codegen::{LLVMCodegen, LLVMErr, LLVMResult};

use crate::err::{FullGonErr, GonErr};
use crate::lexer;
use crate::parser::{self, Parser};

use self::plir_codegen::DeclaredTypes;

use lazy_static::lazy_static;

/// Errors that can occur during the full compilation process.
#[derive(Debug)]
pub enum CompileErr<'ctx> {
    #[allow(missing_docs)]
    IoErr(IoErr),
    #[allow(missing_docs)]
    Computed(String),
    #[allow(missing_docs)]
    LLVMErr(LLVMErr<'ctx>)
}

macro_rules! compile_err_impl_from {
    ($e:ident: $t:ty) => {
        impl<'ctx> From<$t> for CompileErr<'ctx> {
            fn from(value: $t) -> CompileErr<'ctx> {
                Self::$e(value)
            }
        }
    };
    ($e:ident: $t:ident<$l:lifetime>) => {
        impl<$l> From<$t<$l>> for CompileErr<$l> {
            fn from(value: $t<$l>) -> CompileErr<$l> {
                Self::$e(value)
            }
        }
    };
}

compile_err_impl_from! { IoErr: IoErr }
compile_err_impl_from! { LLVMErr: LLVMErr<'ctx> }

fn cast_e<'ctx, T, E: GonErr>(r: Result<T, FullGonErr<E>>, code: &str) -> CompileResult<'ctx, T> {
    r.map_err(|e| CompileErr::Computed(e.full_msg(code)))
}

impl<'ctx> Display for CompileErr<'ctx> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileErr::IoErr(e) => write!(f, "{}", e.at_unknown().short_msg()),
            CompileErr::Computed(e) => write!(f, "{e}"),
            CompileErr::LLVMErr(e) => write!(f, "{}", e.at_unknown().short_msg()),
        }
    }
}
/// A [`Result`] type for operations during the full compilation process.
pub type CompileResult<'ctx, T> = Result<T, CompileErr<'ctx>>;

/// Indicates the files where [`Compiler::write_files`] should save its results to.
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
/// compiler.load_gon(path).unwrap();
/// 
/// compiler.write_files(GonSaveTo::DiffLoc {
///     plir: "script.d.plir.gon".as_ref(), // this saves type information for script
///     llvm: "script.bc".as_ref(), // this is bitcode which can be executed by `lli` or `llc`
/// }).unwrap();
/// ```
pub struct Compiler<'ctx> {
    declared_types: DeclaredTypes,
    llvm_codegen: LLVMCodegen<'ctx>,
    module: Module<'ctx>
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
    pub fn new(ctx: &'ctx Context, filename: &str) -> CompileResult<'ctx, Self> {
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
            llvm_codegen: LLVMCodegen::new(ctx),
            module: ctx.create_module(filename)
        }
    }

    fn get_plir_from_gon(&mut self, code: &str) -> CompileResult<'ctx, (plir::Program, DeclaredTypes)> {
        let lexed = cast_e(lexer::tokenize(code), code)?;
        let ast   = cast_e(parser::parse(lexed), code)?;

        let mut cg = PLIRCodegen::new_with_declared_types(self.declared_types.clone());
        cast_e(cg.consume_program(ast), code)?;
        
        let dt = cg.declared_types();
        let plir = cast_e(cg.unwrap(), code)?;
        
        Ok((plir, dt))
    }

    fn get_declared_types_from_d(&mut self, code: &str) -> CompileResult<'ctx, DeclaredTypes> {
        let lexed = cast_e(lexer::tokenize(code), code)?;
        let parser = Parser::new(lexed, false);
        let ast = cast_e(parser.unwrap_d_program(), code)?;

        let mut cg = PLIRCodegen::new_with_declared_types(self.declared_types.clone());
        cast_e(cg.consume_program(ast), code)?;
        
        let dt = cg.declared_types();
        Ok(dt)
    }

    fn update_values(&mut self, dtypes: DeclaredTypes, new_module: Module<'ctx>) -> CompileResult<'ctx, ()> {
        self.declared_types += dtypes;

        if let Some(f) = self.module.get_function("main") {
            unsafe { f.delete() };
        }

        self.module.link_in_module(new_module)
            .map_err(LLVMErr::LLVMErr)
            .map_err(Into::into)
    }

    /// Load a Poligon file.
    /// 
    /// In loading the file, type data is preserved in the compiler and the LLVM module
    /// that holds this Poligon file is linked to the compiler's module.
    pub fn load_gon(&mut self, p: impl AsRef<Path>) -> CompileResult<'ctx, plir::Program> {
        self.load_gon_and_save_plir(p, None::<&Path>)
    }

    /// Load a Poligon file and optionally saves PLIR to a specified reference.
    /// 
    /// In loading the file, type data is preserved in the compiler and the LLVM module
    /// that holds this Poligon file is linked to the compiler's module.
    pub fn load_gon_and_save_plir(&mut self, p: impl AsRef<Path>, msave: Option<impl AsRef<Path>>) -> CompileResult<'ctx, plir::Program> {
        use fs::File;
        use std::io::prelude::*;

        let path = p.as_ref();
        let filename = path.to_string_lossy();
        let code = fs::read_to_string(path)?;
        let (plir, dtypes) = self.get_plir_from_gon(&code)?;

        if let Some(save) = msave {
            let mut f = File::create(save.as_ref())?;
            f.write_all(plir.to_string().as_bytes())?;
        }
        self.llvm_codegen.set_filename(&filename);
        self.llvm_codegen.compile(&plir)?;

        let module = self.llvm_codegen.pop_module();
        self.update_values(dtypes, module)?;
        self.llvm_codegen.load_declared_types(&self.declared_types)?;

        Ok(plir)
    }

    /// Loads bitcode and type data.
    /// 
    /// In loading these files, type data and the bitcode are linked in the compiler.
    pub fn load_bc(&mut self, dfile: impl AsRef<Path>, bc_file: impl AsRef<Path>) -> CompileResult<'ctx, ()> {
        let code = fs::read_to_string(dfile)?;
        
        let dtypes = self.get_declared_types_from_d(&code)?;
        let module = Module::parse_bitcode_from_path(bc_file, self.llvm_codegen.ctx)
            .map_err(LLVMErr::LLVMErr)?;
    
        self.llvm_codegen.load_declared_types(&dtypes)?;
        self.update_values(dtypes, module)
    }

    /// Writes the type data and module to disk.
    pub fn write_files(&self, write_to: GonSaveTo) -> CompileResult<'ctx, ()> {
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
    pub fn to_ll(&self, dest: impl AsRef<Path>) -> CompileResult<'ctx, ()> {
        llvm_codegen::module_to_ll(&self.module, dest)
            .map_err(Into::into)
    }

    /// Executes the current module JIT, and returns the resulting value.
    /// 
    /// # Safety
    /// This holds the same safety restraints as [`LLVMCodegen::jit_run`].
    /// 
    /// Any calls to this function should ensure that the value returned in Poligon
    /// would align to the provided type in Rust.
    pub unsafe fn jit_run<T>(&self) -> CompileResult<'ctx, T> {
        let main = self.module.get_function("main").expect("Expected main function");

        llvm_codegen::module_jit_run(&self.module, main)
            .map_err(Into::into)
    }
}