//! Converts text into a running program via interpreting.
//! 
//! This module provides:
//! - [`Interpreter`]: A struct which does the full interpreting from string to execution.
//! - [`Repl`]: A struct which performs read-eval-print loop evaluation in the command line 
//! via the interpreter's runtime.
//! - [`semantic`], [`runtime`]: Modules which execute an AST

use std::{io, fs};
use std::path::Path;

use crate::{lexer, parser, FullGonErr, ast};
use runtime::Value;
use runtime::{BlockContext, TraverseRt};
pub use repl::Repl;

pub mod semantic;
pub mod runtime;
mod repl;

/// The struct that performs the interpretation of strings to executable code.
/// 
/// This struct creates an AST out of a provided string, and directly runs it 
/// through a traversal sequence and runtime processed through Rust's runtime.
/// 
/// As such, this struct may be more limited than the [compiler][crate::compiler] form.
/// 
/// TODO!: usage example
pub struct Interpreter {
    source: String
}

type InterpretResult<T> = Result<T, String>;

impl Interpreter {
    /// Create an interpreter from a string
    pub fn from_string(s: &str) -> Self {
        Self {
            source: String::from(s)
        }
    }

    /// Read the text from a file and create an interpreter out of it if successfully read
    pub fn from_file(fp: impl AsRef<Path>) -> io::Result<Self> {
        fs::read_to_string(fp).map(|source| Self { source })
    }

    /// Lex the source string.
    pub fn lex(&self) -> InterpretResult<Vec<lexer::token::FullToken>> {
        lexer::tokenize(&self.source)
            .map_err(|err| err.full_msg(&self.source))
    }

    /// Parse the source string.
    pub fn parse(&self) -> InterpretResult<ast::Program> {
        let lexed = self.lex()?;

        parser::parse(lexed)
            .map_err(|err| err.full_msg(&self.source))
    }

    /// Execute the source string.
    pub fn run(&self) -> InterpretResult<Value> {
        let parsed = self.parse()?;
        
        parsed.run()
            .map_err(|err| FullGonErr::from(err).full_msg(&self.source))
    }
}