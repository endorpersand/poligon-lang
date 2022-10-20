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

#[derive(Debug)]
pub struct TokenOpCastErr(&'static str);

impl TryFrom<Token> for Unary {
    type Error = TokenOpCastErr;
    
    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            token![+]  => Ok(Unary::Plus),
            token![-]  => Ok(Unary::Minus),
            token![!]  => Ok(Unary::LogNot),
            token![~]  => Ok(Unary::BitNot),
            token![..] => Ok(Unary::Spread),
            _ => Err(TokenOpCastErr("Token cannot be converted into a unary operator"))
        }
        
    }
}
impl TryFrom<Token> for Binary {
    type Error = TokenOpCastErr;
    
    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            token![+]  => Ok(Binary::Add),
            token![-]  => Ok(Binary::Sub),
            token![*]  => Ok(Binary::Mul),
            token![/]  => Ok(Binary::Div),
            token![%]  => Ok(Binary::Mod),
            token![|]  => Ok(Binary::BitOr),
            token![&]  => Ok(Binary::BitAnd),
            token![^]  => Ok(Binary::BitXor),
            token![&&] => Ok(Binary::LogAnd),
            token![||] => Ok(Binary::LogOr),
            _ => Err(TokenOpCastErr("Token cannot be converted into a binary operator"))
        }
    }
}
impl TryFrom<Token> for Cmp {
    type Error = TokenOpCastErr;
    
    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            token![<]  => Ok(Cmp::Lt),
            token![<=] => Ok(Cmp::Le),
            token![>]  => Ok(Cmp::Gt),
            token![>=] => Ok(Cmp::Ge),
            token![==] => Ok(Cmp::Eq),
            token![!=] => Ok(Cmp::Ne),
            _ => Err(TokenOpCastErr("Token cannot be converted into a comparison operator"))
        }
    }
}