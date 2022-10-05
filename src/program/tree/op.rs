use crate::lexer::token::{Token, token};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Unary {
    Plus,
    Minus,
    LogNot,
    BitNot,
    Spread
}

#[derive(Debug, PartialEq, Clone, Copy)]
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

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Cmp {
    Lt, Gt, Le, Ge, Eq, Ne
}

impl Cmp {
    pub fn cmp<E>(&self, l: E, r: E) -> bool
        where E: PartialOrd + PartialEq
    {
        match self {
            Cmp::Lt => l < r,
            Cmp::Gt => l > r,
            Cmp::Le => l >= r,
            Cmp::Ge => l >= r,
            Cmp::Eq => l == r,
            Cmp::Ne => l != r,
        }
    }

    pub fn is_ord_cmp(&self) -> bool {
        matches!(self, Cmp::Lt | Cmp::Gt | Cmp::Le | Cmp::Ge)
    }
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