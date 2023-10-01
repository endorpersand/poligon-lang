//! This module holds an abstract syntax tree (AST) generated through 
//! the [parser][`crate::parser`] module.
//! 
//! A full program is held in the [`Program`] struct.
//! 
//! # Further notes
//! 
//! The AST only holds the semantic expressions of the program. 
//! It does *not* hold computed values.
//! 
//! An AST *can* be built manually by using these structs, but that is very painful.
//! Instead, [`crate::lexer`] and [`crate::parser`] should be used to create one from a string.

macro_rules! define_enum {
    ($(#[$ta:meta])* $vis:vis enum $n:ident {$($(#[$a:meta])* $e:ident),*}) => {
        $(#[$ta])*
        $vis enum $n {
            $(
                $(#[$a])*
                $e($e)
            ),*
        }

        impl $crate::span::Spanned for $n {
            fn span(&self) -> &$crate::span::Span {
                match self {
                    $(Self::$e(t) => t.span()),*
                }
            }
        }
    }
}

use crate::err::{FullGonErr, GonErr};
use crate::span::{Span, Spanned};
pub use types::*;
pub use located::*;
pub use stmt::*;
pub use fun::*;

pub mod op;
mod types;
mod located;
mod stmt;
mod fun;

/// A complete program.
/// 
/// # Syntax
/// ```text
/// program = (stmt ";")* ;
/// ```
/// 
/// # Example
/// ```text
///  let a = 1;
///  let b = 2;
///  print(a + b);
///  for i in [1, 2, 3] {
///      print(i);
///  }
/// ```
#[derive(Debug, PartialEq, Eq)]
pub struct Program {
    pub stmts: Vec<Stmt>,
    pub span: Span
}
impl Spanned for Program {
    fn span(&self) -> &Span {
        &self.span
    }
}

/// An enclosed scope with a list of statements.
/// 
/// # Syntax
/// ```text
/// block = "{" (stmt ";")* "}" ;
/// ```
/// 
/// # Example
/// ```text
/// {
///     let a = 1;
///     let b = 2;
///     a + b;
/// }
/// ```
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub span: Span
}
impl Spanned for Block {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Ident {
    pub ident: String,
    pub span: Span
}
impl Spanned for Ident {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct StrLiteral {
    pub literal: String,
    pub span: Span
}
impl Spanned for StrLiteral {
    fn span(&self) -> &Span {
        &self.span
    }
}

/// Reassignment types for variables, parameters, etc.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Default)]
pub enum ReasgType {
    /// `let`. This variable can be reassigned later.
    #[default]
    Let, 
    /// `const`. This variable cannot be reassigned later. It is always the same value it was originally assigned.
    Const 
}

/// Mutability types for variables, parameters, etc.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Default)]
pub enum MutType {
    /// `mut`. This variable can be mutated and changed (e.g. list mutation).
    Mut,
    /// Ø. This variable cannot be mutated and changed.
    #[default]
    Immut
}

/// An expression.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    /// Variable access.
    Ident(Ident),

    /// A block of statements.
    /// 
    /// See [`Block`] for examples.
    Block(Block),

    /// An int, float, char, or string literal.
    /// 
    /// See [`Literal`] for examples.
    Literal(Literal),

    /// A list literal (e.g. `[1, 2, 3, 4]`).
    ListLiteral {
        values: Vec<Expr>,
        span: Span
    },

    /// A set literal (e.g. `set {1, 2, 3, 4}`).
    SetLiteral {
        values: Vec<Expr>,
        span: Span
    },
    
    /// A dict literal (e.g. `dict {1: "a", 2: "b", 3: "c", 4: "d"}`).
    DictLiteral {
        entries: Vec<(Expr, Expr)>,
        span: Span
    },

    /// A class initializer (e.g. `Animal {age: 1, size: 2}`).
    ClassLiteral {
        ty: Type,
        entries: Vec<(Ident, Expr)>,
        span: Span
    },
    
    /// An assignment operation.
    /// 
    /// # Examples
    /// ```text
    /// a = 1;
    /// b[0] = 3;
    /// [a, b, c] = [1, 2, 3];
    /// ```
    Assign {
        target: AsgPat,
        value: Box<Expr>,
        span: Span
    },

    /// A path.
    /// 
    /// See [`Path`] for examples.
    Path(Path),

    /// A static path.
    /// 
    /// This does a static access on a type (e.g. `Type::attr`).
    StaticPath(StaticPath),
    
    /// A chain of unary operations (e.g. `+-+-~!+e`).
    UnaryOps {
        /// The operators applied. These are in display order 
        /// (i.e. they are applied to the expression from right to left).
        ops: Vec<op::Unary>,
        /// Expression to apply the unary operations to.
        expr: Box<Expr>,
        span: Span
    },

    /// A binary operation (e.g. `a + b`).
    BinaryOp {
        /// Operator to apply.
        op: op::Binary,
        /// The left expression.
        left: Box<Expr>,
        /// The right expression.
        right: Box<Expr>,
        span: Span
    },

    /// A comparison operation (e.g. `a < b < c < d`).
    /// 
    /// Compound comparison operations are broken down by `&&`.
    /// For example, `a < b < c < d` breaks down into `a < b && b < c && c < d`.
    Comparison {
        /// The left expression
        left: Box<Expr>,
        /// A list of comparison operators and a right expressions to apply.
        rights: Vec<(op::Cmp, Expr)>,
        
        span: Span
    },

    /// A range (e.g. `1..10` or `1..10 step 1`).
    Range {
        /// The left expression
        left: Box<Expr>,
        /// The right expression
        right: Box<Expr>,
        /// The expression for the step if it exists
        step: Option<Box<Expr>>,

        span: Span
    },

    /// An if expression or if-else expression. (e.g. `if cond {}`, `if cond {} else {}`, `if cond1 {} else if cond2 {} else {}`).
    If {
        /// The condition and block connected to each `if` of the chain
        conditionals: Vec<(Expr, Block)>,
        /// The final bare `else` block (if it exists)
        last: Option<Block>,
        span: Span
    },

    /// A `while` loop.
    While {
        /// The condition to check before each iteration.
        condition: Box<Expr>,
        /// The block to run in each iteration.
        block: Block,
        span: Span
    },

    /// A `for` loop.
    For {
        /// Variable to bind elements of the iterator to.
        ident: Ident,
        /// The iterator.
        iterator: Box<Expr>,
        /// The block to run in each iteration.
        block: Block,
        span: Span
    },

    /// A function call.
    Call {
        /// The function to call.
        funct: Box<Expr>,
        /// The parameters to the function call.
        args: Vec<Expr>,

        span: Span
    },
    /// An index operation.
    /// 
    /// See [`Index`] for examples.
    Index(Index),
    /// A spread operation (e.g. `..`, `..lst`).
    Spread {
        expr: Option<Box<Expr>>,
        span: Span
    },

    /// Dereferencing intrinsic pointers.
    /// 
    /// See [`IDeref`] for examples.
    Deref(IDeref)
}
impl Spanned for Expr {
    fn span(&self) -> &Span {
        match self {
            | Expr::ListLiteral { span, .. }
            | Expr::SetLiteral { span, .. }
            | Expr::DictLiteral { span, .. }
            | Expr::ClassLiteral { span, .. }
            | Expr::Assign { span, .. }
            | Expr::UnaryOps { span, .. }
            | Expr::BinaryOp { span, .. }
            | Expr::Comparison { span, .. }
            | Expr::Range { span, .. }
            | Expr::If { span, .. }
            | Expr::While { span, .. }
            | Expr::For { span, .. }
            | Expr::Call { span, .. }
            | Expr::Spread { span, .. }
            => span,
            
            Expr::Ident(e) => e.span(),
            Expr::Block(e) => e.span(),
            Expr::Literal(e) => e.span(),
            Expr::Path(e) => e.span(),
            Expr::StaticPath(e) => e.span(),
            Expr::Index(e) => e.span(),
            Expr::Deref(e) => e.span(),
        }
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

/// A path, which accesses attributes from an expression.
/// 
/// # Syntax
/// ```text
/// path = expr ("." ident)+;
/// ```
/// 
/// # Examples
/// ```text
/// a.b
/// a.b.c.d.e
/// ```
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Path {
    /// The expression to access an attribute of
    pub obj: Box<Expr>,

    /// The chain of attributes
    pub attrs: Vec<Ident>,

    pub span: Span
}
impl Spanned for Path {
    fn span(&self) -> &Span {
        &self.span
    }
}

/// A path, which accesses attributes from an expression.
/// 
/// # Syntax
/// ```text
/// path = expr ("." ident)+;
/// ```
/// 
/// # Examples
/// ```text
/// a.b
/// a.b.c.d.e
/// ```
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StaticPath {
    /// The type to access an attribute of
    pub ty: Type,

    /// The attribute to access
    pub attr: Ident,

    pub span: Span
}
impl Spanned for StaticPath {
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

/// A unit to assign to.
/// 
/// See [`Expr::Assign`].
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AsgUnit {
    #[allow(missing_docs)] Ident(Ident),
    #[allow(missing_docs)] Path(Path),
    #[allow(missing_docs)] Index(Index),
    #[allow(missing_docs)] Deref(IDeref),
}
impl Spanned for AsgUnit {
    fn span(&self) -> &Span {
        match self {
            AsgUnit::Ident(e) => e.span(),
            AsgUnit::Path(e)  => e.span(),
            AsgUnit::Index(e) => e.span(),
            AsgUnit::Deref(e) => e.span(),
        }
    }
}

/// A unit to declare to.
/// This is a variable's identifier and mutability.
/// 
/// See [`Decl`].
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct DeclUnit {
    pub ident: Ident, 
    pub mt: MutType,
    pub span: Span
}
impl Spanned for DeclUnit {
    fn span(&self) -> &Span {
        &self.span
    }
}

/// A pattern.
/// 
/// A pattern is a syntactic structure which 
/// simulates the structures of the language and can be unpacked.
/// 
/// This is used in [declarations][`Decl`] and [assignments][`Expr::Assign`], 
/// and can be unpacked to perform the needed declaration or assignment.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Pat<T> {
    /// An indivisible unit. This can be directly assigned to.
    // This should be used as LocatedPat<T>, in which case, the unit has a provided range.
    Unit(T),

    /// Spread (possibly with a pattern to assign to).
    /// 
    /// This collects the remainder of the current pattern 
    /// and assigns it to its parameter (if present).
    Spread {
        inner: Option<Box<Self>>,
        span: Span
    },

    /// A list of patterns.
    /// 
    /// The values of the RHS are aligned by index.
    List {
        values: Vec<Self>,
        span: Span
    }
}
impl<T: Spanned> Spanned for Pat<T> {
    fn span(&self) -> &Span {
        match self {
            Pat::Unit(t) => t.span(),
            | Pat::Spread { span, .. }
            | Pat::List { span, .. }
            => span,
        }
    }
}

/// An assignment [pattern][`Pat`] (used for [assignments][`Expr::Assign`]).
pub type AsgPat = Pat<AsgUnit>;
/// A declaration [pattern][`Pat`] (used for [declarations][`Decl`]).
pub type DeclPat = Pat<DeclUnit>;

/// An error with converting an expression to a pattern.
#[derive(Debug, PartialEq, Eq)]
pub enum PatErr {
    /// This expression cannot be used as a unit for the pattern.
    InvalidAssignTarget,

    /// More than one spread appeared.
    CannotSpreadMultiple,
}
type FullPatErr = FullGonErr<PatErr>;

impl GonErr for PatErr {
    fn err_name(&self) -> &'static str {
        "syntax error"
    }
}

impl std::fmt::Display for PatErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PatErr::InvalidAssignTarget  => write!(f, "invalid assign target"),
            PatErr::CannotSpreadMultiple => write!(f, "cannot use spread pattern more than once"),
        }
    }
}
impl std::error::Error for PatErr {}

impl TryFrom<Expr> for AsgUnit {
    type Error = FullPatErr;
    
    fn try_from(value: Expr) -> Result<Self, Self::Error> {
        match value {
            Expr::Ident(ident) => Ok(AsgUnit::Ident(ident)),
            Expr::Path(attrs)  => Ok(AsgUnit::Path(attrs)),
            Expr::Index(idx)   => Ok(AsgUnit::Index(idx)),
            Expr::Deref(deref) => Ok(AsgUnit::Deref(deref)),
            e => Err(PatErr::InvalidAssignTarget.at_range(e.span().clone()))
        }
    }
}

impl<T> TryFrom<Expr> for Pat<T>
    where T: TryFrom<Expr, Error = FullPatErr>
{
    type Error = FullPatErr;

    /// Patterns can be created if the unit type of the pattern can 
    /// fallibly be parsed from an expression.
    fn try_from(value: Expr) -> Result<Self, Self::Error> {
        match value {
            Expr::Spread { expr, span } => {
                let inner = match expr {
                    Some(e) => {
                        let pat = Self::try_from(*e)?;
                        Some(Box::new(pat))
                    },
                    None => None
                };

                Ok(Pat::Spread { inner, span })
            },
            Expr::ListLiteral { values, span } => {
                let pats: Vec<Self> = values.into_iter()
                    .map(TryFrom::try_from)
                    .collect::<Result<_, _>>()?;

                // check spread count is <2
                let mut it = pats.iter()
                    .filter(|pat| matches!(pat, Pat::Spread { .. }));
                
                it.next(); // skip 

                if it.next().is_some() {
                    Err(PatErr::CannotSpreadMultiple.at_range(span))
                } else {
                    Ok(Pat::List { values: pats, span })
                }
            }
            expr => {
                let unit = T::try_from(expr)?;
                Ok(Pat::Unit(unit))
            }
        }
    }
}