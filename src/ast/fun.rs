use crate::span::{Span, Spanned};

use super::{Block, Ident, Type, ReasgType, MutType};


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
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Param {
    /// Whether the parameter variable can be reassigned later
    pub rt: ReasgType,

    /// Whether the parameter variable can be mutated
    pub mt: MutType,

    /// The parameter variable
    pub ident: Ident,

    /// The type of the parameter variable (inferred if not present)
    pub ty: Option<Type>,

    /// The span of characters this parameter ranges over.
    pub span: Span
}
impl Spanned for Param {
    fn span(&self) -> Span {
        self.span
    }
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
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunSignature {
    /// The function's identifier
    pub ident: Ident,
    /// Generic parameters
    pub generics: Vec<Ident>,
    /// The function's parameters
    pub params: Vec<Param>,
    /// Whether the function is varargs
    pub varargs: bool,
    /// The function's return type (or `void` if unspecified)
    pub ret: Option<Type>,
    /// The span of characters this function signature ranges over.
    pub span: Span
}
impl Spanned for FunSignature {
    fn span(&self) -> Span {
        self.span
    }
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
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunDecl {
    /// The function's signature
    pub sig: FunSignature,
    /// The function's body
    pub block: Block,
/// The span of characters this function declaration ranges over.
    pub span: Span
}
impl Spanned for FunDecl {
    fn span(&self) -> Span {
        self.span
    }
}

/// A function declaration without a specified body
/// 
/// In the compiler, this is used to call functions that are
/// declared internally or in a separate file or module.
/// 
/// # Example
/// ```text
/// extern fun puts(s: string) -> int;
/// ```
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ExternFunDecl {
    /// The signature of the external function declaration.
    pub sig: FunSignature,
    /// The span of characters this external function declaration ranges over.
    pub span: Span
}
impl Spanned for ExternFunDecl {
    fn span(&self) -> Span {
        self.span
    }
}