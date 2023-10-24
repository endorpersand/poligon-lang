use crate::span::{Span, Spanned};

use super::{FunDecl, Expr, Class, StaticPath, StrLiteral, ReasgType, DeclPat, Type, ExternFunDecl, Ident, MethodDecl};

define_enum! {
    /// A statement.
    #[derive(Debug, PartialEq, Eq, Clone)]
    pub enum Stmt {
        /// A variable declaration with a value initializer.
        Decl, 
        /// A return statement that signals to exit the function body.
        Return,
        /// `break`
        Break,
        /// `continue`
        Continue,
        /// `throw` statements (a very primitive version)
        Throw,
        /// A function declaration with a defined body.
        FunDecl, 
        /// A function declaration without a specified body.
        ExternFunDecl, 
        /// An expression.
        Expr, 
        /// A struct declaration.
        Class, 
        /// An import declaration.
        Import,
        /// `import intrinsic`. Enables intrinsic functionality.
        ImportIntrinsic,
        /// A global declaration. This is part of intrinsic functionality.
        IGlobal, 
        /// An intrinsic `fit class` declaration. This is part of intrinsic functionality.
        FitClassDecl
    }
}
impl Stmt {
    /// Test if this statement ends with a block.
    pub fn ends_with_block(&self) -> bool {
        matches!(self, 
            | Stmt::FunDecl(_)
            | Stmt::Class(_)
            | Stmt::FitClassDecl(_)
            | Stmt::Expr(Expr::Block(_))
            | Stmt::Expr(Expr::If { .. })
            | Stmt::Expr(Expr::While { .. })
            | Stmt::Expr(Expr::For { .. })
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
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Decl {
    /// Whether the variable can be reassigned later
    pub rt: ReasgType,

    /// The pattern to declare to
    pub pat: DeclPat,

    /// The type of the declaration (inferred if not present)
    pub ty: Option<Type>,

    /// The value to declare the variable to
    pub val: Expr,

    /// The span of characters ranged by the entire statement
    pub span: Span
}
impl Spanned for Decl {
    fn span(&self) -> Span {
        self.span
    }
}

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
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Return {
    /// The expression to return, if it exists
    pub expr: Option<Expr>,
    /// The span of characters ranged by the entire statement
    pub span: Span
}
impl Spanned for Return {
    fn span(&self) -> Span {
        self.span
    }
}
/// `break`
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Break {
    /// The span of characters ranged by the entire statement
    pub span: Span
}
impl Spanned for Break {
    fn span(&self) -> Span {
        self.span
    }
}

/// `continue`
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Continue {
    /// The span of characters ranged by the entire statement
    pub span: Span
}
impl Spanned for Continue {
    fn span(&self) -> Span {
        self.span
    }
}

/// `throw` statements (a very primitive version)
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Throw {
    /// The error message to display by this `throw` statement
    pub message: StrLiteral,
    /// The span of characters ranged by the entire statement
    pub span: Span
}
impl Spanned for Throw {
    fn span(&self) -> Span {
        self.span
    }
}

/// An import declaration.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Import {
    /// The path to import by this import declaration
    pub path: StaticPath,
    /// The span of characters ranged by the entire statement
    pub span: Span
}
impl Spanned for Import {
    fn span(&self) -> Span {
        self.span
    }
}

/// `import intrinsic`. Enables intrinsic functionality.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ImportIntrinsic {
    /// The span of characters ranged by the entire statement
    pub span: Span
}
impl Spanned for ImportIntrinsic {
    fn span(&self) -> Span {
        self.span
    }
}
/// A global declaration. This is part of intrinsic functionality.
/// 
/// This is used to create a global pointer that points to some string of characters.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IGlobal {
    /// The identifier used to refer to the pointer
    pub ident: Ident,
    /// The string held by the global
    pub value: StrLiteral,
    /// The span of characters ranged by the entire statement
    pub span: Span
}
impl Spanned for IGlobal {
    fn span(&self) -> Span {
        self.span
    }
}
/// An intrinsic `fit class` declaration. This is part of intrinsic functionality.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FitClassDecl {
    /// The type that methods are being added to in this `fit class` declaration.
    pub ty: Type,
    /// The methods added to in this `fit class` declaration.
    pub methods: Vec<MethodDecl>,
    /// The span of characters ranged by the entire statement
    pub span: Span
}
impl Spanned for FitClassDecl {
    fn span(&self) -> Span {
        self.span
    }
}