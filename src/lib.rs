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
use runtime::{BlockContext, TraverseRt};
use err::{GonErr, FullGonErr};

// public API
pub use repl::Repl;
pub use interpreter::Interpreter;
pub mod lexer;
pub mod parser;
pub mod semantic;
pub mod runtime;
pub mod interpreter;
pub mod err;
pub(crate) mod tree;
mod repl;
mod util;