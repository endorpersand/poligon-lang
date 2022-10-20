//! Tools to run code with the Poligon language.
//! 
//! The command-line tool runs code through the [Interpreter] or [Repl] structs.
//! This provides other utilities beyond that:
//! - [`Lexer`]
//! - [`Parser`]
//! - [the runtime]
//! 
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
pub mod runtime;
pub mod interpreter;
pub mod err;
mod repl;
mod util;