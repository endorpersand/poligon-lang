use crate::lexer::token::{Token, token};

#[derive(Debug, PartialEq)]
pub enum Unary {
    Plus,
    Minus,
    LogNot,
    BitNot,
    Spread
}

#[derive(Debug, PartialEq)]
pub enum Binary {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitOr,
    BitAnd,
    BitXor,
    LogAnd,
    LogOr
}

#[derive(Debug, PartialEq)]
pub enum Cmp {
    Lt, Gt, Le, Ge, Eq, Ne
}

impl From<Token> for Unary {
    fn from(t: Token) -> Self {
        match t {
            token![+] => Unary::Plus,
            token![-] => Unary::Minus,
            token![!] => Unary::LogNot,
            token![~] => Unary::BitNot,
            token![..] => Unary::Spread,
            _ => panic!("Token cannot be converted into a unary operator")
        }
    }
}
impl From<Token> for Binary {
    fn from(t: Token) -> Self {
        match t {
            token![+] => Binary::Add,
            token![-] => Binary::Sub,
            token![*] => Binary::Mul,
            token![/] => Binary::Div,
            token![%] => Binary::Mod,
            token![|] => Binary::BitOr,
            token![&] => Binary::BitAnd,
            token![^] => Binary::BitXor,
            token![&&] => Binary::LogAnd,
            token![||] => Binary::LogOr,
            _ => panic!("Token cannot be converted into a binary operator")
        }
    }
}
impl From<Token> for Cmp {
    fn from(t: Token) -> Self {
        match t {
            token![<] => Cmp::Lt,
            token![<=] => Cmp::Le,
            token![>] => Cmp::Gt,
            token![>=] => Cmp::Ge,
            token![==] => Cmp::Eq,
            token![!=] => Cmp::Ne,
            _ => panic!("Token cannot be converted into a comparison operator")
        }
    }
}