#![warn(missing_docs)]

//! Tools to run code with the Poligon language.
//! 
//! This project is split across the [`interpreter`] and [`compiler`] modules.
//! Both perform the same function (convert an arbitrary syntax tree into executable code), 
//! but using different approaches.
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
//! # Interpreting
//! 
//! The AST is run directly, and Poligon's runtime is processed through Rust's.
//! See the [`interpreter`] module for more info.
//! 
//! This module provides:
//! - [`Interpreter`]: A struct which completes the full executing process starting 
//! from string processing.
//! - [`interpreter::Repl`]: A REPL, run using the interpreter
//! - [`runtime`][`interpreter::runtime`]: The runtime where the AST is executed
//! 
//! # Compiling
//! 
//! The AST is compiled into LLVM, and is converted to an executable via LLVM's processes.
//! See the [`compiler`] module for more info.
//! 
//! This module provides:
//! - [`CodeGenerator`][`compiler::codegen::CodeGenerator`]: A struct that processes a syntax tree ([`ast`])
//! into a reduced syntax tree ([`plir`]).
//! - [`Compiler`][`compiler::Compiler`]: A struct that converts a PLIR tree into LLVM code.
//! - [`plir`]: The reduced PLIR tree
//! 
//! [`plir`]: compiler::plir

// useful crate items
use err::{GonErr, FullGonErr};

// public API
pub use interpreter::Interpreter;
pub mod lexer;
pub mod parser;
pub mod ast;

pub mod interpreter;
pub mod compiler;
pub mod err;

#[cfg(test)]
mod test_utils;
mod display;