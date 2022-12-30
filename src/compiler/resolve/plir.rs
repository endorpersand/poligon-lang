pub mod display;
mod types;

use crate::tree::{op, self};
pub use types::*;
pub(crate) use types::ty;

#[derive(Debug, PartialEq)]
pub struct Program(pub Vec<Stmt>);

#[derive(Debug, PartialEq)]
pub struct Block(pub Type, pub Vec<Stmt>);

impl Default for Block {
    fn default() -> Self {
        Self(ty!(Type::S_VOID), vec![Stmt::Exit(None)])
    }
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Decl(Decl),
    Return(Option<Expr>),
    Break,
    Continue,
    Exit(Option<Expr>),
    FunDecl(FunDecl),
    Expr(Expr)
}

#[derive(Debug, PartialEq)]
pub struct Decl {
    pub rt: tree::ReasgType,
    pub mt: tree::MutType, // No pattern matching
    pub ident: String, // No pattern matching
    pub ty: Type, // Explicit type
    pub val: Expr
}
#[derive(Debug, PartialEq, Eq)]
pub struct Param {
    pub rt: tree::ReasgType,
    pub mt: tree::MutType,
    pub ident: String,
    pub ty: Type // Explicit type
}

#[derive(Debug, PartialEq, Eq)]
pub struct FunSignature {
    pub ident: String,
    pub params: Vec<Param>,
    pub ret: Type, // Explicit type
}
#[derive(Debug, PartialEq)]
pub struct FunDecl {
    pub sig: FunSignature,
    pub block: Block
}

#[derive(Debug, PartialEq)]
pub struct Expr {
    pub ty: Type, // Explicit type
    pub expr: ExprType
}

impl Expr {
    pub fn new(ty: Type, expr: ExprType) -> Self {
        Expr { ty, expr }
    }
}
#[derive(Debug, PartialEq)]
pub enum ExprType {
    Ident(String), // a variable
    Block(Block), // a block
    Literal(tree::Literal), // int, float, char, str literal
    ListLiteral(Vec<Expr>), // [1, 2, 3, 4]
    SetLiteral(Vec<Expr>), // set {1, 2, 3, 4}
    DictLiteral(Vec<(Expr, Expr)>), // dict {1: 1, 2: 2, 3: 3, 4: 4}
    
    Assign(AsgUnit, Box<Expr>),
    Path(Path),
    UnaryOps {
        // Type provides the value's type after applying the operator of subexpression
        ops: Vec<(op::Unary, Type)>,
        expr: Box<Expr>
    },
    BinaryOp {
        op: op::Binary,
        left: Box<Expr>,
        right: Box<Expr>
    },
    Comparison {
        left: Box<Expr>,
        rights: Vec<(op::Cmp, Expr)>
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
    Spread(Option<Box<Expr>>),
    Split(String, Split)
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

impl Stmt {
    pub fn ends_with_block(&self) -> bool {
        matches!(self, 
            | Stmt::FunDecl(_)
            | Stmt::Expr(Expr { expr: ExprType::Block(_), .. })
            | Stmt::Expr(Expr { expr: ExprType::If { .. }, .. })
            | Stmt::Expr(Expr { expr: ExprType::While { .. }, .. })
            | Stmt::Expr(Expr { expr: ExprType::For { .. }, .. })
        )
    }
}

// TODO: can this be combined with Index?
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Split {
    Left(usize),
    Middle(usize, usize),
    Right(usize)
}

#[derive(Debug, PartialEq)]
pub enum AsgUnit {
    Ident(String),
    Path(Path),
    Index(Index),
}