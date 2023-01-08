mod display;
mod types;

use crate::ast::{op, self};
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
    ExternFunDecl(FunSignature),
    Expr(Expr)
}

#[derive(Debug, PartialEq)]
pub struct Decl {
    pub rt: ast::ReasgType,
    pub mt: ast::MutType, // No pattern matching
    pub ident: String, // No pattern matching
    pub ty: Type, // Explicit type
    pub val: Expr
}
#[derive(Debug, PartialEq, Eq)]
pub struct Param {
    pub rt: ast::ReasgType,
    pub mt: ast::MutType,
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
    /// Variable access.
    Ident(String),

    /// A block of statements.
    Block(Block),

    /// An int, float, char, or string literal.
    Literal(ast::Literal),

    /// A list literal (e.g. `[1, 2, 3, 4]`).
    ListLiteral(Vec<Expr>),

    /// A set literal (e.g. `set {1, 2, 3, 4}`).
    SetLiteral(Vec<Expr>),

    /// A dict literal (e.g. `dict {1: "a", 2: "b", 3: "c", 4: "d"}`).
    DictLiteral(Vec<(Expr, Expr)>),

    /// An assignment operation.
    Assign(AsgUnit, Box<Expr>),

    /// A path (e.g. `obj.prop.prop.prop`).
    Path(Path),

    /// A chain of unary operations (e.g. `+-+-~!+e`).
    UnaryOps {
        /// The operators applied. These are in display order 
        /// (i.e. they are applied to the expression from right to left).
        /// 
        /// Unlike [`ast::Expr::UnaryOps`], this includes a [`Type`] 
        /// which indicates the type of the expression after the unary operator is applied.
        ops: Vec<(op::Unary, Type)>,
        /// Expression to apply the unary operations to.
        expr: Box<Expr>
    },

    /// A binary operation (e.g. `a + b`).
    BinaryOp {
        /// Operator to apply.
        op: op::Binary,
        /// The left expression.
        left: Box<Expr>,
        /// The right expression.
        right: Box<Expr>
    },

    /// A comparison operation (e.g. `a < b < c < d`).
    /// 
    /// Compound comparison operations are broken down by `&&`.
    /// For example, `a < b < c < d` breaks down into `a < b && b < c && c < d`.
    Comparison {
        /// The left expression
        left: Box<Expr>,
        /// A list of comparison operators and a right expressions to apply.
        rights: Vec<(op::Cmp, Expr)>
    },

    /// A range (e.g. `1..10` or `1..10 step 1`).
    Range {
        /// The left expression
        left: Box<Expr>,
        /// The right expression
        right: Box<Expr>,
        /// The expression for the step if it exists
        step: Option<Box<Expr>>
    },

    /// An if expression or if-else expression. (e.g. `if cond {}`, `if cond {} else {}`, `if cond1 {} else if cond2 {} else {}`).
    If {
        /// The condition and block connected to each `if` of the chain
        conditionals: Vec<(Expr, Block)>,
        /// The final bare `else` block (if it exists)
        last: Option<Block>
    },

    /// A `while` loop.
    While {
        /// The condition to check before each iteration.
        condition: Box<Expr>,
        /// The block to run in each iteration.
        block: Block
    },

    /// A `for` loop.
    For {
        /// Variable to bind elements of the iterator to.
        ident: String,
        /// The iterator.
        iterator: Box<Expr>,
        /// The block to run in each iteration.
        block: Block
    },

    /// A function call.
    Call {
        /// The function to call.
        funct: Box<Expr>,
        /// The parameters to the function call.
        params: Vec<Expr>
    },
    /// An index operation (e.g. `lst[0]`).
    Index(Index),
    /// A spread operation (e.g. `..`, `..lst`).
    Spread(Option<Box<Expr>>),
    /// Similar to Index but optimized for literal indexing.
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