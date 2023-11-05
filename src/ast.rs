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
            fn span(&self) -> $crate::span::Span {
                match self {
                    $(Self::$e(t) => t.span()),*
                }
            }
        }

        $(
            impl From<$e> for $n {
                fn from(value: $e) -> $n {
                    Self::$e(value)
                }
            }
        )*
    }
}

use crate::err::{FullGonErr, GonErr};
use crate::span::{Span, Spanned};
pub use types::*;
pub use located::*;
pub use stmt::*;
pub use fun::*;
pub use expr::*;
pub use parsing::AstParseErr;

pub mod op;
mod types;
mod located;
mod stmt;
mod fun;
mod expr;
mod parsing;

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
    /// The statements which this program holds
    pub stmts: Vec<Stmt>,
    /// The span of characters which this program ranges
    pub span: Span
}
impl Spanned for Program {
    fn span(&self) -> Span {
        self.span
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
    /// The statements which this block holds
    pub stmts: Vec<Stmt>,
    /// The span of characters which this block ranges
    pub span: Span
}
impl Spanned for Block {
    fn span(&self) -> Span {
        self.span
    }
}

/// An identifier.
/// 
/// Examples include `foo`, `bar`,` abc123`,`_12abc`.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Ident {
    /// The identifier
    pub ident: String,
    /// The span of characters which this identiifier ranges
    pub span: Span
}
impl Spanned for Ident {
    fn span(&self) -> Span {
        self.span
    }
}

/// A string literal.
/// 
/// Examples include `"Hello, World!"`.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct StrLiteral {
    /// The string stored in this literal
    pub literal: String,
    /// The span of characters which this literal ranges
    pub span: Span
}
impl Spanned for StrLiteral {
    fn span(&self) -> Span {
        self.span
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

    /// The span of characters this path ranges
    pub span: Span
}
impl Spanned for Path {
    fn span(&self) -> Span {
        self.span
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

    /// The span of characters this path ranges
    pub span: Span
}
impl Spanned for StaticPath {
    fn span(&self) -> Span {
        self.span
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
    fn span(&self) -> Span {
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
    /// The identifier of the variable being declared
    pub ident: Ident,
    /// The mutability type of the variable being declared
    pub mt: MutType,
    /// The span of characters this declaration unit ranges
    pub span: Span
}
impl Spanned for DeclUnit {
    fn span(&self) -> Span {
        self.span
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
    Unit(T),

    /// Spread (possibly with a pattern to assign to).
    /// 
    /// This collects the remainder of the current pattern 
    /// and assigns it to its parameter (if present).
    Spread {
        /// The pattern inside of the spread (if it exists)
        inner: Option<Box<Self>>,
        /// The span of characters which this spread pattern contains
        span: Span
    },

    /// A list of patterns.
    /// 
    /// The values of the RHS are aligned by index.
    List {
        /// The list of patterns inside of this list
        values: Vec<Self>,
        /// The span of characters which this list pattern contains
        span: Span
    }
}
impl<T: Spanned> Spanned for Pat<T> {
    fn span(&self) -> Span {
        match self {
            Pat::Unit(t) => t.span(),
            | Pat::Spread { span, .. }
            | Pat::List { span, .. }
            => *span,
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
            Expr::Ident(ident)  => Ok(AsgUnit::Ident(ident)),
            Expr::Path(attrs)   => Ok(AsgUnit::Path(attrs)),
            Expr::Index(idx)    => Ok(AsgUnit::Index(idx)),
            Expr::IDeref(deref) => Ok(AsgUnit::Deref(deref)),
            e => Err(PatErr::InvalidAssignTarget.at_range(e.span()))
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
            Expr::Spread(Spread { expr, span }) => {
                let inner = match expr {
                    Some(e) => {
                        let pat = Self::try_from(*e)?;
                        Some(Box::new(pat))
                    },
                    None => None
                };

                Ok(Pat::Spread { inner, span })
            },
            Expr::ListLiteral(ListLiteral { values, span }) => {
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