use std::rc::Rc;

use crate::tree::{ReasgType, MutType, op};

#[derive(Debug, PartialEq)]
pub struct Program(pub Block);

#[derive(Debug, PartialEq)]
pub struct Block(Type, pub Vec<Stmt>);

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Decl(Decl),
    Return(Option<Expr>),
    Break,
    Continue,
    FunDecl(FunDecl),
    Expr(Expr)
}

#[derive(Debug, PartialEq)]
pub struct Decl {
    pub rt: ReasgType,
    pub ident: String, // No pattern matching
    pub ty: Type, // Explicit type
    pub val: Expr
}
#[derive(Debug, PartialEq, Eq)]
pub struct Param {
    pub rt: ReasgType,
    pub mt: MutType,
    pub ident: String,
    pub ty: Type // Explicit type
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Type(pub String, pub Vec<Type>);

#[derive(Debug, PartialEq)]
pub struct FunDecl {
    pub ident: String,
    pub params: Vec<Param>,
    pub ret: Type, // Explicit type
    pub block: Rc<Block>
}

#[derive(Debug, PartialEq)]
pub struct Expr {
    ty: Type, // Explicit type
    expr: ExprType
}

#[derive(Debug, PartialEq)]
pub enum ExprType {
    Ident(String), // a variable
    Block(Block), // a block
    Literal(Literal), // int, float, char, str literal
    ListLiteral(Vec<Expr>), // [1, 2, 3, 4]
    SetLiteral(Vec<Expr>), // set {1, 2, 3, 4}
    DictLiteral(Vec<(Expr, Expr)>), // dict {1: 1, 2: 2, 3: 3, 4: 4}
    
    Assign(AsgUnit, Box<Expr>),
    Path(Path),
    UnaryOps {
        ops: Vec<(op::Unary, Type)>, // Type provides the type after applying the operator
        expr: Box<Expr>
    },
    BinaryOp {
        op: op::Binary,
        left: Box<Expr>,
        right: Box<Expr>
    },
    Range {
        left: Box<Expr>,
        right: Box<Expr>,
        step: Option<Box<Expr>>
    },
    If {
        conditionals: Vec<(Expr, Block)>,
        last: Option<Block>
    },
    While {
        condition: Box<Expr>,
        block: Block
    },
    For {
        ident: String,
        iterator: Box<Expr>,
        block: Block
    },
    Call {
        funct: Box<Expr>,
        params: Vec<Expr>
    },
    Index(Index),
    Spread(Option<Box<Expr>>)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Int(isize),
    Float(f64),
    Char(char),
    Str(String),
    Bool(bool)
}

impl Literal {
    pub fn from_numeric(s: &str) -> Option<Self> {
        s.parse::<isize>()
            .map(Literal::Int)
            .ok()
            .or_else(|| s.parse::<f64>()
                .map(Literal::Float)
                .ok()
            )
        
    }
}

#[derive(Debug, PartialEq)]
pub struct Path {
    pub obj: Box<Expr>,

    // the attribute, whether or not it's static, and the type of the subexpression
    // a.b.c.d vs a::b::c::d
    pub attrs: Vec<(String, bool, Type)>
}

#[derive(Debug, PartialEq)]
pub struct Index {
    pub expr: Box<Expr>,
    pub index: Box<Expr>
}

#[derive(Debug, PartialEq)]
pub enum AsgUnit {
    Ident(String),
    Path(Path),
    Index(Index),
}