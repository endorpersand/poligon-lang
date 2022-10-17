use std::fmt::Display;
use std::{fs, io};
use std::path::Path;
use err::FullGonErr;
pub use lexer::{tokenize as lex, Lexer};
pub use parser::{parse, parse_repl};
use program::value::Value;
pub use program::{BlockContext, TraverseRt};
mod lexer;
mod parser;
mod program;
mod util;
pub mod err;

pub struct Interpreter {
    source: String
}
pub struct InterpretErr(String);

type InterpretResult<T> = Result<T, InterpretErr>;

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

    pub fn parse(&self) -> InterpretResult<program::tree::Program> {
        let lexed = self.lex()?;

        parser::parse(lexed)
            .map_err(|err| FullGonErr::from(err).full_msg(&self.source))
            .map_err(InterpretErr)
    }

    pub fn run(&self) -> InterpretResult<Value> {
        let parsed = self.parse()?;
        let mut ctx = BlockContext::new();

        parsed.traverse_rt(&mut ctx)
            .map_err(|err| FullGonErr::from(err).full_msg(&self.source))
            .map_err(InterpretErr)
    }
}

impl Display for InterpretErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

pub trait Printable {
    fn repr(&self) -> String;
    fn str(&self) -> String;
}


#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
