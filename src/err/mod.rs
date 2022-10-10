use crate::lexer::LexErr;
use crate::parser::ParseErr;
use crate::program::RuntimeErr;

pub trait GonErr {
    fn message(&self) -> String;
}

/// Enclose quotes around a character
/// 
/// For most characters, it will appear as: `'a', 'b', '"', '@'`, etc.
/// 
/// For ', it appears as `"'"`.
fn wrapq(c: char) -> String {
    if c == '\'' { format!("\"{:?}\"", c) } else { format!("'{:?}'", c) }
}

impl GonErr for LexErr {
    fn message(&self) -> String {
        match self {
            LexErr::UnknownChar(c)      => format!("invalid character {:?}", wrapq(*c)),
            LexErr::UnclosedQuote       => format!("quote was never terminated"),
            LexErr::ExpectedChar(c)     => format!("expected character {:?}", wrapq(*c)),
            LexErr::EmptyChar           => format!("char literal cannot be empty"),
            LexErr::UnknownOp(_op)      => format!("unexpected operator"),
            LexErr::MismatchedDelimiter => format!("mismatched delimiter"),
            LexErr::UnclosedDelimiter   => format!("delimiter was never terminated"),
            LexErr::UnclosedComment     => format!("comment was never terminated"),
        }
    }
}

impl GonErr for ParseErr {
    fn message(&self) -> String {
        todo!()
    }
}

impl GonErr for RuntimeErr {
    fn message(&self) -> String {
        todo!()
    }
}