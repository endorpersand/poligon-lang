use crate::lexer::token::Token;
pub mod tree;

struct Parser {
    tokens: Vec<Token>
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens }
    }

    fn parse(self) -> tree::Program {
        todo!()
    }
}