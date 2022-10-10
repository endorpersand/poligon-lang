use std::{fs, io};
use std::path::Path;
pub use lexer::{tokenize as lex, Lexer};
pub use parser::{parse, parse_repl};
pub use program::{BlockContext, TraverseRt};
mod lexer;
mod parser;
mod program;
mod util;
pub mod err;

pub fn lex_from_file(fp: impl AsRef<Path>) -> io::Result<Vec<lexer::token::Token>> {
    let source = fs::read_to_string(fp)?;
    let tokens = lexer::tokenize(&source).unwrap();

    Ok(tokens)
}

pub fn parse_from_file(fp: impl AsRef<Path>) -> io::Result<program::tree::Program> {
    let p = parser::parse(lex_from_file(fp).unwrap()).unwrap();

    Ok(p)
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
