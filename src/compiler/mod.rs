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

pub use plir_codegen::{PLIRCodegen, PLIRErr, PLIRResult};
pub use llvm_codegen::{LLVMCodegen, LLVMErr, LLVMResult};