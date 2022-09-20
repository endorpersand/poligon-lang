use std::{fs, io};
use std::path::Path;
mod lexer;

pub fn lex_from_file(fp: impl AsRef<Path>) -> io::Result<Vec<lexer::token::Token>> {
    let source = fs::read_to_string(fp)?;
    let tokens = lexer::tokenize(&source).unwrap();

    Ok(tokens)
}

pub fn parse_from_file(fp: impl AsRef<Path>) -> io::Result<()> {
    todo!()
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
