use std::ops::Range;

use crate::tree::{op, self};

use super::{PLIRResult, PLIRErr};

#[derive(Debug, PartialEq)]
pub struct Program(pub Vec<Stmt>);

#[derive(Debug, PartialEq)]
pub struct Block(pub Type, pub Vec<Stmt>);

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Decl(Decl),
    Return(Option<Expr>),
    Break,
    Continue,
    FunDecl(FunDecl),
    Expr(Expr),
    // HACK?
    VerifyPresence(Expr, Split, bool)
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Prim(String),
    Generic(String, Vec<Type>),
    Tuple(Vec<Type>)
}
impl Type {
    pub fn int() -> Self {
        Type::Prim(String::from("int"))
    }
    pub fn float() -> Self {
        Type::Prim(String::from("float"))
    }
    pub fn bool() -> Self {
        Type::Prim(String::from("bool"))
    }
    pub fn char() -> Self {
        Type::Prim(String::from("char"))
    }
    pub fn str() -> Self {
        Type::Prim(String::from("string"))
    }
    pub fn void() -> Self {
        Type::Prim(String::from("void"))
    }
    pub fn unk() -> Self {
        Type::Prim(String::from("unk"))
    }

    pub fn list(t: Type) -> Self {
        Type::Generic(String::from("list"), vec![t])
    }
}
impl From<tree::Type> for Type {
    fn from(ty: tree::Type) -> Self {
        let tree::Type(ident, params) = ty;
        
        if params.is_empty() {
            Type::Prim(ident)
        } else {
            let p = params.into_iter()
                .map(Type::from)
                .collect();
            Type::Generic(ident, p)
        }
    }
}
impl Type {
    // technically index but whatever
    pub fn split(&self, sp: Split) -> PLIRResult<Type> {
        match self {
            Type::Prim(_) => Err(PLIRErr::CannotSplitType),
            Type::Generic(ident, params) => {
                if ident == "list" {
                    if let Some(param) = params.first() {
                        match sp {
                            Split::Left(_)
                            | Split::Right(_) => Ok(param.clone()),
                            Split::Middle(_) => Ok(self.clone()),
                        }
                    } else {
                        panic!("list cannot be defined without parameters")
                    }
                } else {
                    todo!()
                }
            },
            Type::Tuple(tpl) => match sp {
                Split::Left(idx) => tpl.get(idx).cloned()
                    .ok_or_else(|| PLIRErr::InvalidSplit(self.clone(), sp)),
                Split::Middle(Range { start, end }) => {
                    let vec: Vec<_> = tpl.get((start as usize)..(tpl.len() - end as usize))
                        .ok_or_else(|| PLIRErr::InvalidSplit(self.clone(), sp))?
                        .into_iter()
                        .cloned()
                        .collect();
                    
                    Ok(Type::Tuple(vec))
                },
                Split::Right(idx) => tpl.get(tpl.len() - idx).cloned()
                    .ok_or_else(|| PLIRErr::InvalidSplit(self.clone(), sp)),
            },
        }
    }
}
#[derive(Debug, PartialEq)]
pub struct FunDecl {
    pub ident: String,
    pub params: Vec<Param>,
    pub ret: Type, // Explicit type
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

// TODO: can this be combined with Index?
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Split {
    Left(usize),
    Middle(Range<isize>),
    Right(usize)
}

#[derive(Debug, PartialEq)]
pub enum AsgUnit {
    Ident(String),
    Path(Path),
    Index(Index),
}