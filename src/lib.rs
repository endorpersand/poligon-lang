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
//! - [`Lexer`]: A struct that processes strings (or files) into sequences of tokens.
//! - [`Parser`]: A struct that processes sequences of lexer tokens into an AST.
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
//! - [`interpreter::runtime`]: The runtime where the AST is executed
//! 
//! # Compiling
//! 
//! The AST is compiled into LLVM, and is converted to an executable via LLVM's processes.
//! See the [`compiler`] module for more info.
//! TODO!: compiler info
//! 
//! [`Parser`]: parser::Parser

// useful crate items
use lexer::Lexer;
use err::{GonErr, FullGonErr};

// public API
pub use interpreter::Interpreter;
pub mod lexer;
pub mod parser;
pub mod ast;

pub mod interpreter;
pub mod compiler;
pub mod err;