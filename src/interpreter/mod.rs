//! The full process that converts text into a running program.
//! 
//! The [Interpreter] struct does the full processing from string to execution.
//! 
//! TODO!: example

use std::fmt::Display;
use std::{io, fs};
use std::path::Path;

use crate::{FullGonErr, tree};
use crate::runtime::value::Value;
use crate::{lexer, parser};

/// The struct that performs the full processing from string to execution.
pub struct Interpreter {
    source: String
}
pub struct InterpretErr(String);

pub type InterpretResult<T> = Result<T, InterpretErr>;

impl Interpreter {
    pub fn from_string(s: &str) -> Self {
        Self {
            source: s.to_string()
        }
    }
    pub fn from_file(fp: impl AsRef<Path>) -> io::Result<Self> {
        let source = fs::read_to_string(fp)?;
        Ok(Self {
            source
        })
    }

    pub fn lex(&self) -> InterpretResult<Vec<lexer::token::Token>> {
        lexer::tokenize(&self.source)
            .map_err(|err| err.full_msg(&self.source))
            .map_err(InterpretErr)
    }

    pub fn parse(&self) -> InterpretResult<tree::Program> {
        let lexed = self.lex()?;

        parser::parse(lexed)
            .map_err(|err| FullGonErr::from(err).full_msg(&self.source))
            .map_err(InterpretErr)
    }

    pub fn run(&self) -> InterpretResult<Value> {
        let parsed = self.parse()?;
        
        parsed.run()
            .map_err(|err| FullGonErr::from(err).full_msg(&self.source))
            .map_err(InterpretErr)
    }
}

impl Display for InterpretErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}