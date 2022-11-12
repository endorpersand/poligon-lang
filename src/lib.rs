//! Tools to run code with the Poligon language.
//! 
//! The command-line tool runs code through the [`Interpreter`] or [`Repl`] structs.
//! This crates provides other utilities beyond those:
//! - [`Lexer`]: The processing of strings (or text from files) into sequences of tokens.
//! - [`Parser`]: The processing of sequences of tokens into parse trees.
//! - [the runtime]: The execution of parse trees
//! 
//! [`Parser`]: parser::Parser
//! [the runtime]: TraverseRt
// useful crate items
use lexer::Lexer;
use err::{GonErr, FullGonErr};

// public API
pub use interpreter::Interpreter;
pub mod lexer;
pub mod parser;
pub(crate) mod tree;

pub mod interpreter;
pub mod compiler;
pub mod err;