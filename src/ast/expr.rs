use crate::span::{Span, Spanned};

use super::{Ident, Block, Type, AsgPat, Path, StaticPath, op};

define_enum! {
    /// An expression.
    /// 
    /// Precedence:
    /// 1.  Unit expressions (ident, block, parenthesized expressions, literals, if statements, loops)
    /// 2.  Calling, indexing, paths
    /// 3.  Deref
    /// 4.  Other unary operators (+, -, !, ~)
    /// 5.  Mult., division, mod (*, /, %)
    /// 6.  Add, sub (+, -)
    /// 7.  Shifts (<<, >>)
    /// 8.  Bitwise and (&)
    /// 9.  Bitwise xor (^)
    /// 10. Bitwise or (|)
    /// 11. Range (a..b)
    /// 12. Spread (..a)
    /// 13. Comparisons (<, >, <=, >=, ==, !=)
    /// 14. Logical and (&&)
    /// 15. Logical or (||)
    /// 16. Assignment
    #[derive(Debug, PartialEq, Eq, Clone)]
    pub enum Expr {
        /// Variable access.
        /// 
        /// See [`Ident`] for examples.
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
        /// See [`Assign`] for examples.
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
        /// For more details, see [`Comparison`].
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

/// A primitive literal with a defined span.
/// 
/// # Examples
/// ```text
/// 14    // int
/// 14.4  // float
/// 'x'   // char
/// "abc" // string
/// true  // bool
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Literal {
    /// The data held by this literal
    pub kind: LitKind,
    /// The span of characters encompassed by this expression
    pub span: Span
}
impl Spanned for Literal {
    fn span(&self) -> Span {
        self.span
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

/// A list literal (e.g. `[1, 2, 3, 4]`).
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ListLiteral {
    /// The values held in this list literal
    pub values: Vec<Expr>,
    /// The span of characters encompassed by this expression
    pub span: Span
}
impl Spanned for ListLiteral {
    fn span(&self) -> Span {
        self.span
    }
}

/// A set literal (e.g. `set {1, 2, 3, 4}`).
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SetLiteral {
    /// The values held in this set literal
    pub values: Vec<Expr>,
    /// The span of characters encompassed by this expression
    pub span: Span
}
impl Spanned for SetLiteral {
    fn span(&self) -> Span {
        self.span
    }
}

/// A dict literal (e.g. `dict {1: "a", 2: "b", 3: "c", 4: "d"}`).
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct DictLiteral {
    /// The entries held in this dict literal
    pub entries: Vec<(Expr, Expr)>,
    /// The span of characters encompassed by this expression
    pub span: Span
}
impl Spanned for DictLiteral {
    fn span(&self) -> Span {
        self.span
    }
}

/// A class initializer (e.g. `Animal {age: 1, size: 2}`).
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ClassLiteral {
    /// The type to instantiate by this class instantiation
    pub ty: Type,
    /// The declaration of the fields in this class instantiation
    pub entries: Vec<(Ident, Expr)>,
    /// The span of characters encompassed by this expression
    pub span: Span
}
impl Spanned for ClassLiteral {
    fn span(&self) -> Span {
        self.span
    }
}

/// An assignment operation.
/// 
/// # Examples
/// ```text
/// a = 1;
/// b[0] = 3;
/// [a, b, c] = [1, 2, 3];
/// ```
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Assign {
    /// The target pattern to assign
    pub target: AsgPat,
    /// The expression to assign to the target
    pub value: Box<Expr>,
    /// The span of characters encompassed by this expression
    pub span: Span
}
impl Spanned for Assign {
    fn span(&self) -> Span {
        self.span
    }
}

/// A chain of unary operations (e.g. `+-+-~!+e`).
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct UnaryOps {
    /// The operators applied. These are in display order 
    /// (i.e. they are applied to the expression from right to left).
    pub ops: Vec<op::Unary>,
    /// Expression to apply the unary operations to.
    pub expr: Box<Expr>,
    /// The span of characters encompassed by this expression
    pub span: Span
}
impl Spanned for UnaryOps {
    fn span(&self) -> Span {
        self.span
    }
}

/// A binary operation (e.g. `a + b`).
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BinaryOp {
    /// Operator to apply.
    pub op: op::Binary,
    /// The left expression.
    pub left: Box<Expr>,
    /// The right expression.
    pub right: Box<Expr>,
    /// The span of characters encompassed by this expression
    pub span: Span
}
impl Spanned for BinaryOp {
    fn span(&self) -> Span {
        self.span
    }
}

/// A comparison operation (e.g. `a < b < c < d`).
/// 
/// Compound comparison operations are broken down by `&&`.
/// For example, `a < b < c < d` breaks down into `a < b && b < c && c < d`.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Comparison {
    /// The left expression
    pub left: Box<Expr>,
    /// A list of comparison operators and a right expressions to apply.
    pub rights: Vec<(op::Cmp, Expr)>,
    /// The span of characters encompassed by this expression
    pub span: Span
}
impl Spanned for Comparison {
    fn span(&self) -> Span {
        self.span
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
    /// The span of characters encompassed by this expression
    pub span: Span
}
impl Spanned for Index {
    fn span(&self) -> Span {
        self.span
    }
}

/// A spread operation (e.g. `..`, `..lst`).
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Spread {
    /// The expression spread by this spread operation, if it exists.
    pub expr: Option<Box<Expr>>,
    /// The span of characters encompassed by this expression
    pub span: Span
}
impl Spanned for Spread {
    fn span(&self) -> Span {
        self.span
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
    /// The span of characters encompassed by this expression
    pub span: Span
}
impl Spanned for Range {
    fn span(&self) -> Span {
        self.span
    }
}

/// An if expression or if-else expression. (e.g. `if cond {}`, `if cond {} else {}`, `if cond1 {} else if cond2 {} else {}`).
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct If {
    /// The condition and block connected to each `if` of the chain
    pub conditionals: Vec<(Expr, Block)>,
    /// The final bare `else` block (if it exists)
    pub last: Option<Block>,
    /// The span of characters encompassed by this expression
    pub span: Span
}
impl Spanned for If {
    fn span(&self) -> Span {
        self.span
    }
}

/// A `while` loop.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct While {
    /// The condition to check before each iteration.
    pub condition: Box<Expr>,
    /// The block to run in each iteration.
    pub block: Block,
    /// The span of characters encompassed by this expression
    pub span: Span
}
impl Spanned for While {
    fn span(&self) -> Span {
        self.span
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
    /// The span of characters encompassed by this expression
    pub span: Span
}
impl Spanned for For {
    fn span(&self) -> Span {
        self.span
    }
}

/// A function call.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Call {
    /// The function to call.
    pub funct: Box<Expr>,
    /// The generic (type) arguments to the function call.
    pub generic_args: Vec<Type>,
    /// The argument to the function call.
    pub args: Vec<Expr>,
    /// The span of characters encompassed by this expression
    pub span: Span
}
impl Spanned for Call {
    fn span(&self) -> Span {
        self.span
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
    /// The expression being dereferenced
    pub reference: Box<Expr>,
    /// The span of characters encompassed by this expression
    pub span: Span
}
impl Spanned for IDeref {
    fn span(&self) -> Span {
        self.span
    }
}