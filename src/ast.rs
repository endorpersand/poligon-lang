//! The components of the AST generated through the [parser][`crate::parser`] module.
//! These structs are used to construct an AST within Rust.
//! 
//! A full program is described as [`Program`] struct.
//! 
//! # Further notes
//! 
//! The AST only holds the expressions. It does *not* hold computed values 
//! (this is done in [runtime][crate::interpreter::runtime]).
//! 
//! Typically, an AST should not be defined manually because it is painful. 
//! Instead, [`crate::lexer`] and [`crate::parser`] should be used to create one from a string.

use std::rc::Rc;

use crate::err::{CursorRange, FullGonErr, GonErr};

pub use self::types::*;

pub mod op;
mod display;
mod types;

/// AST node with a known location.
#[derive(PartialEq, Eq)]
pub struct Located<T>(pub T, pub CursorRange);

impl<T: std::fmt::Debug> std::fmt::Debug for Located<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Located(node, loc) = self;

        write!(f, "[{:?} ..= {:?}]", loc.start(), loc.end())?;
        write!(f, "{node:?}")
    }
}
impl<T> std::ops::Deref for Located<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<T> std::ops::DerefMut for Located<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

pub(crate) type LocatedBox<T> = Box<Located<T>>;

impl<T: PartialEq> PartialEq<T> for Located<T> {
    fn eq(&self, other: &T) -> bool {
        &self.0 == other
    }
}

impl<T> Located<T> {
    /// Create a new located node.
    pub fn new(t: T, loc: CursorRange) -> Self {
        Self(t, loc)
    }

    /// Create a new boxed located node.
    pub fn boxed(t: T, loc: CursorRange) -> LocatedBox<T> {
        Box::new(Located(t, loc))
    }

    /// Map a Located with a given type to a Located of another type.
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Located<U> {
        let Located(node, loc) = self;
        Located(f(node), loc)
    }

    /// Gets this located node's range.
    pub fn range(&self) -> CursorRange {
        self.1.clone()
    }

}

impl<T> Located<Option<T>> {
    /// Transpose a located Option into an Option of a located node.
    pub fn transpose_option(self) -> Option<Located<T>> {
        let Located(value, range) = self;
        value.map(|v| Located(v, range))
    }
}

impl<T, E> Located<Result<T, E>> {
    /// Transpose a located Result into a Result of a located node.
    pub fn transpose_result(self) -> Result<Located<T>, E> {
        let Located(value, range) = self;
        value.map(|v| Located(v, range))
    }
}

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
#[derive(Debug, PartialEq)]
pub struct Program(pub Vec<Stmt>);

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
#[derive(Debug, PartialEq)]
pub struct Block(pub Vec<Stmt>);

/// A statement.
#[derive(Debug, PartialEq)]
pub enum Stmt {
    /// A variable declaration with a value initializer.
    /// 
    /// See [`Decl`] for examples.
    Decl(Decl),
    
    /// A return statement that signals to exit the function body.
    /// 
    /// This return statement can return nothing (`void`), 
    /// or return a value from a given expression.
    /// 
    /// # Examples
    /// ```text
    /// return; // no expr
    /// return 2; // with expr
    /// ```
    Return(Option<Located<Expr>>),
    
    /// `break`
    Break,

    /// `continue`
    Continue,
    
    /// A function declaration with a defined body.
    /// 
    /// See [`FunDecl`] for examples.
    FunDecl(FunDecl),

    /// A function declaration without a specified body
    /// 
    /// In the compiler, this is used to call functions from libc.
    /// 
    /// # Example
    /// ```text
    /// extern fun puts(s: string) -> int;
    /// ```
    ExternFunDecl(FunSignature),

    /// An expression.
    Expr(Located<Expr>),

    /// A struct declaration.
    ClassDecl(Class)
}

impl Stmt {
    /// Test if this statement ends with a block.
    pub fn ends_with_block(&self) -> bool {
        matches!(self, 
            | Stmt::FunDecl(_)
            | Stmt::ClassDecl(_)
            | Stmt::Expr(Located(Expr::Block(_), _))
            | Stmt::Expr(Located(Expr::If { .. }, _))
            | Stmt::Expr(Located(Expr::While { .. }, _))
            | Stmt::Expr(Located(Expr::For { .. }, _))
        )
    }
}

/// A variable declaration.
/// 
/// This struct also requires a value initializer.
/// 
/// # Syntax
/// ```text
/// decl = ("let" | "const") decl_pat (: ty)? = expr;
/// ```
/// 
/// # Examples
/// ```text
/// // basic declarations:
/// let a = 1;
/// const b = 2;
/// let mut c = 3;
/// const mut d = 4;
/// let e: int = 5;
/// 
/// // with patterns:
/// let [mut x, y, z] = [1, 2, 3];
/// ```
#[derive(Debug, PartialEq)]
pub struct Decl {
    /// Whether the variable can be reassigned later
    pub rt: ReasgType,

    /// The pattern to declare to
    pub pat: DeclPat,

    /// The type of the declaration (inferred if not present)
    pub ty: Option<Type>,

    /// The value to declare the variable to
    pub val: Located<Expr>
}

/// A function parameter.
/// 
/// This struct is similar to [`Decl`], 
/// but omits the value initializer and does not accept patterns.
/// 
/// # Syntax
/// ```text
/// param = ("let" | "const")? ident (: ty)?;
/// ```
/// 
/// # Example
/// ```text
/// fun funny(
///     a: int, 
///     const b: float, 
///     mut c: string, 
///     const mut d: list<string>
/// ) {}
/// ```
#[derive(Debug, PartialEq, Eq)]
pub struct Param {
    /// Whether the parameter variable can be reassigned later
    pub rt: ReasgType,

    /// Whether the parameter variable can be mutated
    pub mt: MutType,

    /// The parameter variable
    pub ident: String,

    /// The type of the parameter variable (inferred if not present)
    pub ty: Option<Type>
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

/// A function header / signature.
/// 
/// If a return type is not provided, it is assumed to be `void`.
/// 
/// # Syntax
/// ```text
/// sig = "fun" ident "(" (param,)* ")" (-> ty)?;
/// ```
/// 
/// # Examples
/// ```text
/// fun abc(a: int);
/// fun def(a: int) -> string;
/// ```
#[derive(Debug, PartialEq, Eq)]
pub struct FunSignature {
    /// The function's identifier
    pub ident: String,
    /// The function's parameters
    pub params: Vec<Param>,
    /// The function's return type (or `void` if unspecified)
    pub ret: Option<Type>,
}

/// A complete function declaration with a function body.
/// 
/// # Syntax
/// ```text
/// fun_decl = "fun" ident "(" (param,)* ")" (-> ty)? block;
/// ```
/// 
/// # Example
/// ```text
/// fun double(n: int) -> int {
///     n * 2;
/// }
/// ```
#[derive(Debug, PartialEq)]
pub struct FunDecl {
    /// The function's signature
    pub sig: FunSignature,
    /// The function's body
    pub block: Rc<Block>
}

/// An expression.
#[derive(Debug, PartialEq)]
pub enum Expr {
    /// Variable access.
    Ident(String),

    /// A block of statements.
    /// 
    /// See [`Block`] for examples.
    Block(Block),

    /// An int, float, char, or string literal.
    /// 
    /// See [`Literal`] for examples.
    Literal(Literal),

    /// A list literal (e.g. `[1, 2, 3, 4]`).
    ListLiteral(Vec<Located<Expr>>),

    /// A set literal (e.g. `set {1, 2, 3, 4}`).
    SetLiteral(Vec<Located<Expr>>),
    
    /// A dict literal (e.g. `dict {1: "a", 2: "b", 3: "c", 4: "d"}`).
    DictLiteral(Vec<(Located<Expr>, Located<Expr>)>),

    /// A class initializer (e.g. `Animal {age: 1, size: 2}`).
    ClassLiteral(Type, Vec<(String, Located<Expr>)>),
    
    /// An assignment operation.
    /// 
    /// # Examples
    /// ```text
    /// a = 1;
    /// b[0] = 3;
    /// [a, b, c] = [1, 2, 3];
    /// ```
    Assign(AsgPat, LocatedBox<Expr>),

    /// A path.
    /// 
    /// See [`Path`] for examples.
    Path(Path),

    /// A static path.
    /// 
    /// This does a static access on a type (e.g. `Type::attr`).
    StaticPath(Type, String),
    /// A chain of unary operations (e.g. `+-+-~!+e`).
    UnaryOps {
        /// The operators applied. These are in display order 
        /// (i.e. they are applied to the expression from right to left).
        ops: Vec<op::Unary>,
        /// Expression to apply the unary operations to.
        expr: LocatedBox<Expr>
    },

    /// A binary operation (e.g. `a + b`).
    BinaryOp {
        /// Operator to apply.
        op: op::Binary,
        /// The left expression.
        left: LocatedBox<Expr>,
        /// The right expression.
        right: LocatedBox<Expr>
    },

    /// A comparison operation (e.g. `a < b < c < d`).
    /// 
    /// Compound comparison operations are broken down by `&&`.
    /// For example, `a < b < c < d` breaks down into `a < b && b < c && c < d`.
    Comparison {
        /// The left expression
        left: LocatedBox<Expr>,
        /// A list of comparison operators and a right expressions to apply.
        rights: Vec<(op::Cmp, Located<Expr>)>
    },

    /// A range (e.g. `1..10` or `1..10 step 1`).
    Range {
        /// The left expression
        left: LocatedBox<Expr>,
        /// The right expression
        right: LocatedBox<Expr>,
        /// The expression for the step if it exists
        step: Option<LocatedBox<Expr>>
    },

    /// An if expression or if-else expression. (e.g. `if cond {}`, `if cond {} else {}`, `if cond1 {} else if cond2 {} else {}`).
    If {
        /// The condition and block connected to each `if` of the chain
        conditionals: Vec<(Located<Expr>, Block)>,
        /// The final bare `else` block (if it exists)
        last: Option<Block>
    },

    /// A `while` loop.
    While {
        /// The condition to check before each iteration.
        condition: LocatedBox<Expr>,
        /// The block to run in each iteration.
        block: Block
    },

    /// A `for` loop.
    For {
        /// Variable to bind elements of the iterator to.
        ident: String,
        /// The iterator.
        iterator: LocatedBox<Expr>,
        /// The block to run in each iteration.
        block: Block
    },

    /// A function call.
    Call {
        /// The function to call.
        funct: LocatedBox<Expr>,
        /// The parameters to the function call.
        params: Vec<Located<Expr>>
    },
    /// An index operation.
    /// 
    /// See [`Index`] for examples.
    Index(Index),
    /// A spread operation (e.g. `..`, `..lst`).
    Spread(Option<LocatedBox<Expr>>)
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
#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    #[allow(missing_docs)] Int(isize),
    #[allow(missing_docs)] Float(f64),
    #[allow(missing_docs)] Char(char),
    #[allow(missing_docs)] Str(String),
    #[allow(missing_docs)] Bool(bool)
}

impl Literal {
    /// Create a literal from a string representing a numeric value.
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
#[derive(Debug, PartialEq)]
pub struct Path {
    /// The expression to access an attribute of
    pub obj: LocatedBox<Expr>,

    /// The chain of attributes
    pub attrs: Vec<String>
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
#[derive(Debug, PartialEq)]
pub struct Index {
    /// The expression to index
    pub expr: LocatedBox<Expr>,
    /// The index
    pub index: LocatedBox<Expr>
}

/// A unit to assign to.
/// 
/// See [`Expr::Assign`].
#[derive(Debug, PartialEq)]
pub enum AsgUnit {
    #[allow(missing_docs)] Ident(String),
    #[allow(missing_docs)] Path(Path),
    #[allow(missing_docs)] Index(Index),
}

/// A unit to declare to.
/// This is a variable's identifier and mutability.
/// 
/// See [`Decl`].
#[derive(Debug, PartialEq, Eq)]
pub struct DeclUnit(pub String, pub MutType);

/// A pattern.
/// 
/// A pattern is a syntactic structure which 
/// simulates the structures of the language and can be unpacked.
/// 
/// This is used in [declarations][`Decl`] and [assignments][`Expr::Assign`], 
/// and can be unpacked to perform the needed declaration or assignment.
#[derive(Debug, PartialEq)]
pub enum Pat<T> {
    /// An indivisible unit. This can be directly assigned to.
    // This should be used as LocatedPat<T>, in which case, the unit has a provided range.
    Unit(T),

    /// Spread (possibly with a pattern to assign to).
    /// 
    /// This collects the remainder of the current pattern 
    /// and assigns it to its parameter (if present).
    Spread(Option<LocatedBox<Self>>),

    /// A list of patterns.
    /// 
    /// The values of the RHS are aligned by index.
    List(Vec<Located<Self>>)
}

/// A pattern with a known location.
pub type LocatedPat<T> = Located<Pat<T>>;
/// An assignment [pattern][`Pat`] (used for [assignments][`Expr::Assign`]).
pub type AsgPat = LocatedPat<AsgUnit>;
/// A declaration [pattern][`Pat`] (used for [declarations][`Decl`]).
pub type DeclPat = LocatedPat<DeclUnit>;

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

    fn message(&self) -> String {
        match self {
            PatErr::InvalidAssignTarget => String::from("invalid assign target"),
            PatErr::CannotSpreadMultiple => String::from("cannot use spread pattern more than once"),
        }
    }
}

impl TryFrom<Located<Expr>> for Located<AsgUnit> {
    type Error = FullPatErr;
    
    fn try_from(value: Located<Expr>) -> Result<Self, Self::Error> {
        let Located(expr, range) = value;
        match expr {
            Expr::Ident(ident) => Ok(Located(AsgUnit::Ident(ident), range)),
            Expr::Path(attrs)  => Ok(Located(AsgUnit::Path(attrs), range)),
            Expr::Index(idx)   => Ok(Located(AsgUnit::Index(idx), range)),
            _ => Err(PatErr::InvalidAssignTarget.at_range(range))
        }
    }
}

impl<T> TryFrom<Located<Expr>> for LocatedPat<T> 
    where Located<T>: TryFrom<Located<Expr>, Error = FullPatErr>
{
    type Error = FullPatErr;

    /// Patterns can be created if the unit type of the pattern can 
    /// fallibly be parsed from an expression.
    fn try_from(value: Located<Expr>) -> Result<Self, Self::Error> {
        #[inline]
        fn ok_located<T, E>(t: T, range: CursorRange) -> Result<Located<T>, E> {
            Ok(Located::new(t, range))
        }

        let Located(expr, range) = value;

        match expr {
            Expr::Spread(me) => match me {
                Some(e) => {
                    let pat = Self::try_from(*e)?;
                    let inner = Some(Box::new(pat));
                    
                    ok_located(Pat::Spread(inner), range)
                },
                None => ok_located(Pat::Spread(None), range),
            }
            Expr::ListLiteral(lst) => {
                let vec: Vec<Self> = lst.into_iter()
                    .map(TryFrom::try_from)
                    .collect::<Result<_, _>>()?;

                // check spread count is <2
                let mut it = vec.iter()
                    .filter(|pat| matches!(&***pat, Pat::Spread(_)));
                
                it.next(); // skip 

                if it.next().is_some() {
                    Err(PatErr::CannotSpreadMultiple.at_range(range))
                } else {
                    ok_located(Pat::List(vec), range)
                }
            }
            e => {
                let value = Located::new(e, range);
                let Located(unit, range) = Located::<T>::try_from(value)?;

                ok_located(Pat::Unit(unit), range)
            }
        }
    }
}