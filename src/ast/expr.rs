use crate::span::{Span, Spanned};

use super::{Ident, Block, Type, AsgPat, Path, StaticPath, op};

define_enum! {
    /// An expression.
    #[derive(Debug, PartialEq, Eq, Clone)]
    pub enum Expr {
        /// Variable access.
        Ident,
    
        /// A block of statements.
        /// 
        /// See [`Block`] for examples.
        Block,
    
        /// An int, float, char, or string literal.
        /// 
        /// See [`Literal`] for examples.
        Literal,
    
        /// A list literal (e.g. `[1, 2, 3, 4]`).
        ListLiteral,
    
        /// A set literal (e.g. `set {1, 2, 3, 4}`).
        SetLiteral,
        
        /// A dict literal (e.g. `dict {1: "a", 2: "b", 3: "c", 4: "d"}`).
        DictLiteral,
    
        /// A class initializer (e.g. `Animal {age: 1, size: 2}`).
        ClassLiteral,
        
        /// An assignment operation.
        /// 
        /// # Examples
        /// ```text
        /// a = 1;
        /// b[0] = 3;
        /// [a, b, c] = [1, 2, 3];
        /// ```
        Assign,
    
        /// A path.
        /// 
        /// See [`Path`] for examples.
        Path,
    
        /// A static path.
        /// 
        /// This does a static access on a type (e.g. `Type::attr`).
        StaticPath,
        
        /// A chain of unary operations (e.g. `+-+-~!+e`).
        UnaryOps,
    
        /// A binary operation (e.g. `a + b`).
        BinaryOp,
    
        /// A comparison operation (e.g. `a < b < c < d`).
        /// 
        /// Compound comparison operations are broken down by `&&`.
        /// For example, `a < b < c < d` breaks down into `a < b && b < c && c < d`.
        Comparison,
    
        /// A range (e.g. `1..10` or `1..10 step 1`).
        Range,
    
        /// An if expression or if-else expression. (e.g. `if cond {}`, `if cond {} else {}`, `if cond1 {} else if cond2 {} else {}`).
        If,

        /// A `while` loop.
        While,
    
        /// A `for` loop.
        For,
    
        /// A function call.
        Call,

        /// An index operation.
        /// 
        /// See [`Index`] for examples.
        Index,
        /// A spread operation (e.g. `..`, `..lst`).
        Spread,
    
        /// Dereferencing intrinsic pointers.
        /// 
        /// See [`IDeref`] for examples.
        IDeref
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Literal {
    pub kind: LitKind,
    pub span: Span
}
impl Spanned for Literal {
    fn span(&self) -> &Span {
        &self.span
    }
}

/// A primitive literal.
/// 
/// # Examples
/// ```text
/// 14    // int
/// 14.4  // float
/// 'x'   // char
/// "abc" // string
/// true  // bool
/// ```
#[derive(Debug, Clone)]
pub enum LitKind {
    #[allow(missing_docs)] Int(isize),
    #[allow(missing_docs)] Float(f64),
    #[allow(missing_docs)] Char(char),
    #[allow(missing_docs)] Str(String),
    #[allow(missing_docs)] Bool(bool)
}

impl LitKind {
    /// Create a literal from a string representing a numeric value.
    pub fn from_numeric(s: &str) -> Option<Self> {
        s.parse::<isize>().ok().map(LitKind::Int)
            .or_else(|| s.parse::<f64>().ok().map(LitKind::Float))
    }
}

impl PartialEq for LitKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(l0), Self::Int(r0))     => l0 == r0,
            // since this is an AST, we want the EXACT values of floats to be the same
            // hence, we can compare the bits
            (Self::Float(l0), Self::Float(r0)) => l0.to_bits() == r0.to_bits(),
            (Self::Char(l0),  Self::Char(r0))  => l0 == r0,
            (Self::Str(l0),   Self::Str(r0))   => l0 == r0,
            (Self::Bool(l0),  Self::Bool(r0))  => l0 == r0,
            _ => false,
        }
    }
}
impl Eq for LitKind {}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ListLiteral {
    pub values: Vec<Expr>,
    pub span: Span
}
impl Spanned for ListLiteral {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SetLiteral {
    pub values: Vec<Expr>,
    pub span: Span
}
impl Spanned for SetLiteral {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct DictLiteral {
    pub entries: Vec<(Expr, Expr)>,
    pub span: Span
}
impl Spanned for DictLiteral {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ClassLiteral {
    pub ty: Type,
    pub entries: Vec<(Ident, Expr)>,
    pub span: Span
}
impl Spanned for ClassLiteral {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Assign {
    pub target: AsgPat,
    pub value: Box<Expr>,
    pub span: Span
}
impl Spanned for Assign {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct UnaryOps {
    /// The operators applied. These are in display order 
    /// (i.e. they are applied to the expression from right to left).
    pub ops: Vec<op::Unary>,
    /// Expression to apply the unary operations to.
    pub expr: Box<Expr>,
    pub span: Span
}
impl Spanned for UnaryOps {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BinaryOp {
    /// Operator to apply.
    pub op: op::Binary,
    /// The left expression.
    pub left: Box<Expr>,
    /// The right expression.
    pub right: Box<Expr>,
    pub span: Span
}
impl Spanned for BinaryOp {
    fn span(&self) -> &Span {
        &self.span
    }
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Comparison {
    /// The left expression
    pub left: Box<Expr>,
    /// A list of comparison operators and a right expressions to apply.
    pub rights: Vec<(op::Cmp, Expr)>,
    
    pub span: Span
}
impl Spanned for Comparison {
    fn span(&self) -> &Span {
        &self.span
    }
}

/// Value indexing.
/// 
/// # Syntax
/// ```text
/// index = expr "[" expr "]";
/// ```
/// 
/// # Examples
/// ```text
/// lst[0]
/// dct["hello"]
/// ```
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Index {
    /// The expression to index
    pub expr: Box<Expr>,
    /// The index
    pub index: Box<Expr>,
    pub span: Span
}
impl Spanned for Index {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Spread {
    pub expr: Option<Box<Expr>>,
    pub span: Span
}
impl Spanned for Spread {
    fn span(&self) -> &Span {
        &self.span
    }
}

/// A range (e.g. `1..10` or `1..10 step 1`).
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Range {
    /// The left expression
    pub left: Box<Expr>,
    /// The right expression
    pub right: Box<Expr>,
    /// The expression for the step if it exists
    pub step: Option<Box<Expr>>,

    pub span: Span
}
impl Spanned for Range {
    fn span(&self) -> &Span {
        &self.span
    }
}

/// An if expression or if-else expression. (e.g. `if cond {}`, `if cond {} else {}`, `if cond1 {} else if cond2 {} else {}`).
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct If {
    /// The condition and block connected to each `if` of the chain
    pub conditionals: Vec<(Expr, Block)>,
    /// The final bare `else` block (if it exists)
    pub last: Option<Block>,
    pub span: Span
}
impl Spanned for If {
    fn span(&self) -> &Span {
        &self.span
    }
}

/// A `while` loop.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct While {
    /// The condition to check before each iteration.
    pub condition: Box<Expr>,
    /// The block to run in each iteration.
    pub block: Block,
    pub span: Span
}
impl Spanned for While {
    fn span(&self) -> &Span {
        &self.span
    }
}

/// A `for` loop.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct For {
    /// Variable to bind elements of the iterator to.
    pub ident: Ident,
    /// The iterator.
    pub iterator: Box<Expr>,
    /// The block to run in each iteration.
    pub block: Block,
    pub span: Span
}
impl Spanned for For {
    fn span(&self) -> &Span {
        &self.span
    }
}

/// A function call.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Call {
    /// The function to call.
    pub funct: Box<Expr>,
    /// The parameters to the function call.
    pub args: Vec<Expr>,

    pub span: Span
}
impl Spanned for Call {
    fn span(&self) -> &Span {
        &self.span
    }
}

/// Dereferencing of an intrinsic pointer.
/// 
/// # Example
/// ```text
/// *ptr
/// ```
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IDeref {
    pub reference: Box<Expr>,
    pub span: Span
}
impl Spanned for IDeref {
    fn span(&self) -> &Span {
        &self.span
    }
}