//! The components of the PLIR (Poligon Intermediate Representation) tree, 
//! which is derived from the [AST].
//! 
//! The AST is compiled to the PLIR AST via the [`codegen`][`crate::compiler::codegen`] module,
//! which is then compiled to LLVM via the [`compiler`][`crate::compiler`] module.
//! 
//! Many of the structs here are similar (or identical) to those in [AST], 
//! usually with extra type checking.
//! 
//! [AST]: crate::ast

mod display;
mod types;

use std::borrow::Cow;

use crate::ast::{op, self};
pub use types::*;
pub(crate) use types::ty;

/// A complete program.
/// 
/// This struct corresponds to [`ast::Program`].
#[derive(Debug, PartialEq)]
pub struct Program(pub Vec<HoistedStmt>, pub Vec<ProcStmt>);

/// An enclosed scope with a list of statements.
/// 
/// This struct corresponds to [`ast::Block`], 
/// with an extra Type parameter to indicate the block's return type.
/// 
/// # Example
/// ```text
/// {
///     let a: int = <int>1;
///     let b: int = <int>2;
///     exit a + b;
/// }
/// ```
#[derive(Debug, PartialEq)]
pub struct Block(pub Type, pub Vec<ProcStmt>);

impl Default for Block {
    fn default() -> Self {
        Self(ty!(Type::S_VOID), vec![ProcStmt::Exit(None)])
    }
}

mod stmt {
    use super::{FunDecl, FunSignature, Class, Decl, Expr, ExprType};

    /// A statement that is always available (within PLIR).
    /// 
    /// This is one of the enums that corresponds to [`ast::Stmt`][`crate::ast::Stmt`]. 
    /// See also [`ProcStmt`].
    /// 
    /// Hoisted statements are special in that they can be partially resolved. 
    /// For example, all function signatures are resolved before function bodies.
    /// This quality of hoisted statements mean that a list of hoisted statements 
    /// is not necessarily evaluated in order.
    #[derive(Debug, PartialEq)]
    pub enum HoistedStmt {

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

        /// A struct declaration.
        ClassDecl(Class)
    }

    /// A statement that happens as a part of a procedure (e.g. a block).
    /// 
    /// This is one of the enums that corresponds to [`ast::Stmt`][`crate::ast::Stmt`].
    /// See also [`HoistedStmt`].
    /// 
    /// A list of procedural statements can simply be evaluated in order.
    #[derive(Debug, PartialEq)]
    pub enum ProcStmt {
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
        /// return <int>2; // with expr
        /// ```
        Return(Option<Expr>),

        /// `break`
        Break,

        /// `continue`
        Continue,

        /// A statement that signals to exit the current block.
        /// 
        /// This statement can exit the block with nothing (`void`), 
        /// or with a value from a given expression.
        Exit(Option<Expr>),

        /// An expression.
        Expr(Expr)
    }

    impl HoistedStmt {
        /// Test if this statement ends with a block.
        /// 
        /// This function corresponds to [`ast::Stmt::ends_with_block`].
        pub fn ends_with_block(&self) -> bool {
            use HoistedStmt::*;

            matches!(self,
                | FunDecl(_)
                | ClassDecl(_)
            )
        }
    }

    impl ProcStmt {
        /// Test if this statement ends with a block.
        /// 
        /// This function corresponds to [`ast::Stmt::ends_with_block`].
        pub fn ends_with_block(&self) -> bool {
            matches!(self,
                | ProcStmt::Expr(Expr { expr: ExprType::Block(_), .. })
                | ProcStmt::Expr(Expr { expr: ExprType::If { .. }, .. })
                | ProcStmt::Expr(Expr { expr: ExprType::While { .. }, .. })
                | ProcStmt::Expr(Expr { expr: ExprType::For { .. }, .. })
            )
        }
    }

    pub(super) trait EndsWithBlock {
        fn ends_with_block(&self) -> bool;
    }
    impl EndsWithBlock for ProcStmt {
        fn ends_with_block(&self) -> bool { self.ends_with_block() }
    }
    impl EndsWithBlock for HoistedStmt {
        fn ends_with_block(&self) -> bool { self.ends_with_block() }
    }

    impl From<Decl> for ProcStmt {
        fn from(value: Decl) -> Self {
            ProcStmt::Decl(value)
        }
    }
    impl From<Expr> for ProcStmt {
        fn from(value: Expr) -> Self {
            ProcStmt::Expr(value)
        }
    }
    impl From<FunDecl> for HoistedStmt {
        fn from(value: FunDecl) -> Self {
            HoistedStmt::FunDecl(value)
        }
    }
    impl From<FunSignature> for HoistedStmt {
        fn from(value: FunSignature) -> Self {
            HoistedStmt::ExternFunDecl(value)
        }
    }
    impl From<Class> for HoistedStmt {
        fn from(value: Class) -> Self {
            HoistedStmt::ClassDecl(value)
        }
    }
}

use stmt::EndsWithBlock;
pub use stmt::*;

/// A variable declaration.
/// 
/// This struct also requires a value initializer.
/// 
/// This struct corresponds to [`ast::Decl`],
/// with a required Type parameter that explicitly declares the variable's type.
/// 
/// Additionally, pattern matching is not supported with PLIR's version of Decl.
/// 
/// # Examples
/// ```text
/// let a: int = <int>1;
/// const b: int = <int>2;
/// let mut c: int = <int>3;
/// const mut d: int = <int>4;
/// let e: int = <int>5;
/// ```
#[derive(Debug, PartialEq)]
pub struct Decl {
    /// Whether the variable can be reassigned later
    pub rt: ast::ReasgType,

    /// Whether the variable can be mutated
    pub mt: ast::MutType,

    /// The variable to declare to
    pub ident: String,

    /// The type of the variable
    pub ty: Type,

    /// The value to declare the variable to
    pub val: Expr
}

/// A function parameter.
/// 
/// This struct corresponds to [`ast::Param`],
/// with a required Type parameter that explicitly declares the variable's type.
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
    pub rt: ast::ReasgType,

    /// Whether the parameter variable can be mutated
    pub mt: ast::MutType,

    /// The parameter variable
    pub ident: String,

    /// The type of the parameter variable
    pub ty: Type
}

/// The function header / signature.
/// 
/// This struct corresponds to [`ast::FunSignature`],
/// with the return type being explicitly evaluated.
/// 
/// # Examples
/// ```text
/// fun abc(a: int) -> void;
/// fun def(a: int) -> string;
/// ```
#[derive(Debug, PartialEq, Eq)]
pub struct FunSignature {
        /// The function's identifier
    pub ident: String,
        /// The function's parameters
    pub params: Vec<Param>,
        /// The function's return type
    pub ret: Type
}

/// A complete function declaration with a function body.
/// 
/// This struct corresponds to [`ast::FunDecl`].
/// 
/// # Example
/// ```text
/// fun double(n: int) -> int {
///     return <int>(<int>n * <int>2);
/// }
/// ```
#[derive(Debug, PartialEq)]
pub struct FunDecl {
    /// The function's signature
    pub sig: FunSignature,
    /// The function's body
    pub block: Block
}

/// A typed expression.
/// 
/// This does not correspond exactly to [`ast::Expr`]. 
/// Instead, [`ExprType`] corresponds to [`ast::Expr`] 
/// and this struct provides the expression's type information.
#[derive(Debug, PartialEq)]
pub struct Expr {
    /// Value type of the expression
    pub ty: Type,

    /// The specific expression
    pub expr: ExprType
}

impl Expr {
    /// Create a new expression.
    pub fn new(ty: Type, expr: ExprType) -> Self {
        Expr { ty, expr }
    }

    /// Create a new boxed expression.
    pub fn boxed(ty: Type, expr: ExprType) -> Box<Self> {
        Box::new(Self::new(ty, expr))
    }

    fn invalid() -> Self {
        Self::new(ty!("never"), ExprType::Spread(None))
    }
}

/// An expression.
/// 
/// This corresponds to [`ast::Expr`], however cannot be used in the PLIR AST directly.
/// 
/// See [`plir::Expr`][`Expr`].
#[derive(Debug, PartialEq)]
pub enum ExprType {
    /// Variable access.
    Ident(String),

    /// A block of statements.
    /// 
    /// See [`Block`] for examples.
    Block(Block),

    /// An int, float, char, or string literal.
    /// 
    /// See [`ast::Literal`] for examples.
    Literal(ast::Literal),

    /// A list literal (e.g. `[1, 2, 3, 4]`).
    ListLiteral(Vec<Expr>),

    /// A set literal (e.g. `set {1, 2, 3, 4}`).
    SetLiteral(Vec<Expr>),

    /// A dict literal (e.g. `dict {1: "a", 2: "b", 3: "c", 4: "d"}`).
    DictLiteral(Vec<(Expr, Expr)>),

    /// A class initializer (e.g. `Animal {age: 1, size: 2}`).
    ClassLiteral(Type, Vec<Expr>),

    /// An assignment operation.
    /// 
    /// Unlike [`ast::Expr::Assign`], this cannot be a pattern.
    /// 
    /// # Examples
    /// ```text
    /// a = <int>1;
    /// b[0] = <int>3;
    /// ```
    Assign(AsgUnit, Box<Expr>),

    /// A path.
    /// 
    /// See [`Path`] for examples.
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
    /// An index operation.
    /// 
    /// See [`Index`] for examples.
    Index(Index),
    /// A spread operation (e.g. `..`, `..lst`).
    Spread(Option<Box<Expr>>),
    /// Similar to Index but optimized for literal indexing.
    Split(String, Split),
    /// Change the type of this expression to a new type.
    /// 
    /// This enables int to float casts and char to string casts.
    Cast(Box<Expr>)
}

/// A path.
/// 
/// This struct corresponds to [`ast::Path`],
/// with an additional type parameter in the attributes to
/// indicate the type of the access.
#[derive(Debug, PartialEq)]
pub enum Path {
    /// A static path (Type::attr)
    /// 
    /// This includes the type expression being accessed, 
    /// the attribute being accessed,
    /// and the type of the access
    Static(Type, String, Type),
    
    /// A chain of struct accesses (obj.0.1.2.3.4)
    /// 
    /// This includes the expression being accessed, 
    /// and a vector holding the chain of attributes accessed (and the type of the access)
    Struct(Box<Expr>, Vec<(usize, Type)>),

    /// An method access (a.b where b is a method on type A)
    /// 
    /// This includes the expression being accessed,
    /// the method on that expression,
    /// and the type of the method
    Method(Box<Expr>, String, FunType)
}
impl Path {
    pub(crate) fn ty(&self) -> Cow<Type> {
        match self {
            Path::Static(_, _, ty) => Cow::Borrowed(ty),
            Path::Struct(e, attrs) => Cow::Borrowed(attrs.last().map(|(_, t)| t).unwrap_or(&e.ty)),
            Path::Method(_, _, ty) => Cow::Owned(Type::Fun(ty.clone())),
        }
    }

    pub(crate) fn add_struct_seg(&mut self, seg: (usize, Type)) -> Result<(), ()> {
        match self {
            Path::Static(_, _, _) => {
                let placeholder = Path::Struct(Box::new(Expr::invalid()), vec![seg]);
                
                let old_path = std::mem::replace(self, placeholder);
                let new_obj = Box::new(old_path.into());

                let Path::Struct(obj_ref, _) = self else {
                    unreachable!("initialized placeholder is Path::struct")
                };
                *obj_ref = new_obj;
                Ok(())
            },
            Path::Struct(_, segs) => {
                segs.push(seg);
                Ok(())
            },
            Path::Method(_, _, _) => Err(()),
        }
    }
}

impl From<Path> for Expr {
    fn from(value: Path) -> Self {
        fn can_flatten(this: &Path) -> bool {
            match this {
                Path::Struct(_, attrs) => attrs.is_empty(),
                Path::Static(_, _, _) => false,
                Path::Method(_, _, _) => false
            }
        }

        let mut e = Expr::new(value.ty().into_owned(), ExprType::Path(value));
        
        loop {
            match e {
                Expr { expr: ExprType::Path(p), ..} if can_flatten(&p) => {
                    e = match p {
                        Path::Struct(e, _) => *e,
                        Path::Static(_, _, _) => unreachable!(),
                        Path::Method(_, _, _) => unreachable!(),
                    }
                },
                e => break e
            }
        }
    }
}

/// Value indexing.
/// 
/// This struct corresponds to [`ast::Index`].
/// # Examples
/// ```text
/// (<list<unk>> lst)[<int>0]
/// (<dict<string, unk>> dct)[<string>"hello"]
/// ```
#[derive(Debug, PartialEq)]
pub struct Index {
    /// The expression to index
    pub expr: Box<Expr>,
    /// The index
    pub index: Box<Expr>
}

/// A literal index used for splitting patterns.
// TODO: can this be combined with Index?
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Split {
    /// Index from the left (first is 0)
    Left(usize),
    /// Index from the middle, starting from .0 (inclusive) and ending at .1 (inclusive)
    Middle(usize, usize),
    /// Index from the right (last is 0)
    Right(usize)
}

/// A unit to assign to.
/// 
/// This corresponds to [`ast::AsgUnit`].
#[derive(Debug, PartialEq)]
pub enum AsgUnit {
    #[allow(missing_docs)] Ident(String),
    #[allow(missing_docs)] Path(Path),
    #[allow(missing_docs)] Index(Index),
}