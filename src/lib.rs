use std::{fs, io};
use std::path::Path;
mod lexer;

pub fn parse_from_file(fp: impl AsRef<Path>) -> io::Result<()> {
    let source = fs::read_to_string(fp)?;

    let tokens = lexer::tokenize(&source);
    Ok(())
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
