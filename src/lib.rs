#![warn(missing_docs)]

//! Tools to run code with the Poligon language.
//! 
//! # Parsing
//! 
//! Parsing of a string to an arbitrary syntax tree (AST) is done 
//! with the [`lexer`] and [`parser`] modules.
//! 
//! These modules provide:
//! - [`Lexer`][`lexer::Lexer`]: A struct that processes strings (or files) into sequences of tokens.
//! - [`Parser`][`parser::Parser`]: A struct that processes sequences of lexer tokens into an AST.
//! - [`ast`]: The components of the AST.
//! 
//! # Compiling
//! 
//! The AST is compiled into LLVM, and is converted to an executable via LLVM's processes.
//! See the [`compiler`] module for more info.
//! 
//! This module provides:
//! - [`PLIRCodegen`][`compiler::plir_codegen::PLIRCodegen`]: The first code generation step, which reduces a Poligon syntax tree into an intermediate PLIR syntax tree.
//! - [`LLVMCodegen`][`compiler::llvm_codegen::LLVMCodegen`]: The second code generation step, which reduces a PLIR syntax tree into LLVM bitcode.
//! into a reduced syntax tree ([`plir`]).
//! - [`Compiler`][`compiler::Compiler`]: A struct that combines the code generation steps to compile files and makes executables out of them
//! - [`plir`]: The components of the intermediate PLIR syntax tree.
//! 
//! [`plir`]: compiler::plir

// useful crate items
use err::GonErr;

// public API
pub mod lexer;
pub mod parser;
pub mod ast;

pub mod compiler;
pub mod err;

#[cfg(test)]
mod test_utils;
mod display;
pub mod span;