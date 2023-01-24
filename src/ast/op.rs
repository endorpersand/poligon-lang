//! The operator AST nodes.
//! 
//! This differs from [operator tokens][crate::lexer::token] 
//! because these nodes have established meanings within the Poligon language
//! and can be used to apply operations in runtimes.

use std::fmt::Display;

use crate::lexer::token::{Token, token};

/// A unary operator AST node.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Unary {
    /// Unary plus (`+x`)
    Plus,

    /// Unary minus (`-x`)
    Minus,

    /// Logical not (`!x`)
    LogNot,

    /// Bitwise not (`~x`)
    BitNot
}

/// A binary operator AST node.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Binary {
    /// Binary plus (`x + y`)
    Add,

    /// Binary subtract (`x - y`)
    Sub,

    /// Multiplication (`x * y`) 
    Mul,

    /// Division (`x / y`)
    Div,

    /// Modulo (`x % y`)
    Mod,

    /// Shift left (`x << y`)
    Shl,

    /// Shift right (`x >> y`)
    Shr,

    /// Bitwise or (`x | y`)
    BitOr,

    /// Bitwise and (`x & y`)
    BitAnd,

    /// Bitwise xor (`x ^ y`)
    BitXor,

    /// Logical and (`x && y`)
    LogAnd,

    /// Logical or (`x || y`)
    LogOr
}

/// A comparison operator AST node.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Cmp {
    /// Less than (`<`)
    Lt, 
    
    /// Greater than (`>`)
    Gt, 
    
    /// Less than or equal (`<=`)
    Le, 
    
    /// Greater than or equal (`>=`)
    Ge, 
    
    /// Equal (`==`)
    Eq, 
    
    /// Not equal (`!=`)
    Ne
}

impl Cmp {
    /// Apply the operator to a partially ordered type.
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

    /// Test if comparison operator is an order comparison (`true`) 
    /// or an equality comparison (`false`).
    pub fn is_ord_cmp(&self) -> bool {
        matches!(self, Cmp::Lt | Cmp::Gt | Cmp::Le | Cmp::Ge)
    }
}

/// Casting a token to an operator node failed.
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
            token![<<] => Ok(Binary::Shl),
            token![>>] => Ok(Binary::Shr),
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

impl Display for Unary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Unary::Plus   => "+",
            Unary::Minus  => "-",
            Unary::LogNot => "!",
            Unary::BitNot => "~",
        })
    }
}

impl Display for Binary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Binary::Add => "+",
            Binary::Sub => "-",
            Binary::Mul => "*",
            Binary::Div => "/",
            Binary::Mod => "%",
            Binary::Shl => "<<",
            Binary::Shr => ">>",
            Binary::BitOr => "|",
            Binary::BitAnd => "&",
            Binary::BitXor => "^",
            Binary::LogAnd => "&&",
            Binary::LogOr => "||",
        })
    }
}

impl Display for Cmp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Cmp::Lt => "<",
            Cmp::Gt => ">",
            Cmp::Le => "<=",
            Cmp::Ge => ">=",
            Cmp::Eq => "==",
            Cmp::Ne => "!=",
        })
    }
}