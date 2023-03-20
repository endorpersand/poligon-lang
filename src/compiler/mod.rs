//! The compiler, which takes an AST tree and reads it into LLVM code 
//! which can be compiled into an executable.
//! 
//! The compiler has two steps: converting into PLIR, then compiling into LLVM
//! 
//! # PLIR
//! PLIR, the intermediate language, simplifies the main language in order to make
//! it simpler to compiler to LLVM.
//! 
//! PLIR can be generated from AST via the [`codegen::codegen`] function,
//! or using the [`codegen::CodeGenerator`] struct.
//! 
//! # LLVM
//! The PLIR is then compiled to LLVM via the [`Compiler`] struct.

pub mod plir_codegen;
pub mod plir;
mod llvm;
pub(self) mod internals;
pub mod llvm_codegen;

use std::{fs};
use std::io::Error as IoErr;
use std::path::Path;

use inkwell::context::Context;
use inkwell::module::Module;
pub use plir_codegen::{PLIRCodegen, PLIRErr, PLIRResult};
pub use llvm_codegen::{LLVMCodegen, LLVMErr, LLVMResult};

use crate::err::{FullGonErr, GonErr};
use crate::lexer::{LexErr, self};
use crate::parser::{self, ParseErr};

use self::plir_codegen::DeclaredTypes;

use lazy_static::lazy_static;

/// Errors that can occur during the full compilation process.
#[derive(Debug)]
pub enum CompileErr<'ctx> {
    #[allow(missing_docs)]
    LexErr(FullGonErr<LexErr>),
    #[allow(missing_docs)]
    ParseErr(FullGonErr<ParseErr>),
    #[allow(missing_docs)]
    IoErr(IoErr),
    #[allow(missing_docs)]
    PLIRErr(FullGonErr<PLIRErr>),
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
compile_err_impl_from! { LexErr: FullGonErr<LexErr> }
compile_err_impl_from! { ParseErr: FullGonErr<ParseErr> }
compile_err_impl_from! { IoErr: IoErr }
compile_err_impl_from! { PLIRErr: FullGonErr<PLIRErr> }
compile_err_impl_from! { LLVMErr: LLVMErr<'ctx> }

/// A [`Result`] type for operations during the full compilation process.
pub type CompileResult<'ctx, T> = Result<T, CompileErr<'ctx>>;

impl<'ctx> GonErr for CompileErr<'ctx> {
    fn err_name(&self) -> &'static str {
        match self {
            CompileErr::LexErr(e)   => e.err.err_name(),
            CompileErr::ParseErr(e) => e.err.err_name(),
            CompileErr::IoErr(_)    => "io error",
            CompileErr::PLIRErr(e)  => e.err.err_name(),
            CompileErr::LLVMErr(e)  => e.err_name(),
        }
    }

    fn message(&self) -> String {
        match self {
            CompileErr::LexErr(e)   => e.short_msg(),
            CompileErr::ParseErr(e) => e.short_msg(),
            CompileErr::IoErr(e)    => e.to_string(),
            CompileErr::PLIRErr(e)  => e.short_msg(),
            CompileErr::LLVMErr(e)  => e.message(),
        }
    }
}

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
pub struct Compiler<'ctx> {
    declared_types: DeclaredTypes,
    llvm_codegen: LLVMCodegen<'ctx>,
    module: Module<'ctx>
}

lazy_static! {
    static ref STD_PATH: &'static Path = "std".as_ref();
}
impl<'ctx> Compiler<'ctx> {
    /// Create a new compiler. 
    /// 
    /// This includes the Poligon std library.
    pub fn new(ctx: &'ctx Context, filename: &str) -> CompileResult<'ctx, Self> {
        let mut compiler = Self::no_std(ctx, filename);

        let paths: Vec<_> = fs::read_dir(*STD_PATH)?
            .flat_map(|me| me.map(|e| {
                let path = e.path();

                (path.extension() == Some("bc".as_ref()))
                    .then_some(path)
            }).transpose())
            .collect::<Result<_, _>>()?;

        for bc_file in paths {
            let dfile = bc_file.with_extension("d.plir.gon");
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

    fn get_plir_from_str(&mut self, code: &str) -> CompileResult<'ctx, (plir::Program, DeclaredTypes)> {
        let lexed = lexer::tokenize(code)?;
        let ast   = parser::parse(lexed)?;

        let mut cg = PLIRCodegen::new_with_declared_types(self.declared_types.clone());
        cg.consume_program(ast)?;
        
        let dt = cg.declared_types();
        let plir = cg.unwrap()?;
        
        Ok((plir, dt))
    }

    fn update_values(&mut self, dtypes: DeclaredTypes, new_module: Module<'ctx>) -> CompileResult<'ctx, ()> {
        self.declared_types += dtypes;
        self.module.link_in_module(new_module)
            .map_err(LLVMErr::LLVMErr)
            .map_err(Into::into)
    }

    /// Load a Poligon file.
    /// 
    /// In loading the file, type data is preserved in the compiler and the LLVM module
    /// that holds this Poligon file is linked to the module being created.
    pub fn load_gon(&mut self, p: impl AsRef<Path>) -> CompileResult<'ctx, plir::Program> {
        let path = p.as_ref();
        let filename = path.to_string_lossy();
        let code = fs::read_to_string(path)?;
        let (plir, dtypes) = self.get_plir_from_str(&code)?;

        self.llvm_codegen.set_filename(&filename);
        self.llvm_codegen.compile(&plir)?;

        let module = self.llvm_codegen.pop_module();
        self.update_values(dtypes, module)?;

        Ok(plir)
    }

    /// Loads bitcode and type data.
    /// 
    /// In loading these files, type data and the bitcode are linked in the compiler.
    pub fn load_bc(&mut self, dfile: impl AsRef<Path>, bc_file: impl AsRef<Path>) -> CompileResult<'ctx, ()> {
        let code = fs::read_to_string(dfile)?;
        let (_, dtypes) = self.get_plir_from_str(&code)?;

        let module = Module::parse_bitcode_from_path(bc_file, self.llvm_codegen.ctx)
            .map_err(LLVMErr::LLVMErr)?;
        
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