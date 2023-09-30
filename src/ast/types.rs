//! These components are the components of the AST involved with types 
//! (type expressions, classes, shapes).

use std::rc::Rc;

use crate::span::{Span, Spanned};

use super::{MutType, ReasgType, Block, Param, Ident};

/// A type expression.
/// 
/// # Examples
/// ```text
/// string
/// list<string>
/// map<string, list<int>>
/// ```
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Type {
    pub ident: Ident,
    pub params: Vec<Type>,
    pub span: Span
}
impl Spanned for Type {
    fn span(&self) -> &Span {
        &self.span
    }
}

/// A class declaration.
/// 
/// # Example
/// ```text
/// class Animal {
///     // fields
///     age: int;
///     size: int;
/// 
///     // methods
///     fn self.roar() { /* ... */ }
///     fn self.grow() { /* ... */ }
/// }
/// ```
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Class {
    /// Name of the class
    pub ident: Ident,
    /// Generic parameters of the class
    pub generic_params: Vec<Ident>,
    /// A vec of fields declared in this class
    pub fields: Vec<FieldDecl>,
    /// A vec of methods declared in this class
    pub methods: Vec<MethodDecl>,
    pub span: Span
}
impl Spanned for Class {
    fn span(&self) -> &Span {
        &self.span
    }
}

/// A field declaration.
/// 
/// This is similar to [`Decl`][`super::Decl`], with some differences. Namely:
/// - The declaration's type has to be specified.
/// - A initializing value cannot be specified.
/// - The keyword `let` in reassignable fields is optional.
/// - A field declaration only accepts normal identifiers, not patterns.
/// 
/// # Examples
/// ```text
/// age: int,
/// let size: int,
/// const cost: double,
/// mut colors: list<int>,
/// const mut content: list<str>
/// ```
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FieldDecl {
    /// Whether the field can be reassigned later
    pub rt: ReasgType,
    
    /// Whether the field can be mutated
    pub mt: MutType,

    /// The field's name
    pub ident: Ident,

    /// The type of the declaration (inferred if not present)
    pub ty: Type,

    pub span: Span
}
impl Spanned for FieldDecl {
    fn span(&self) -> &Span {
        &self.span
    }
}

/// A method signature.
/// 
/// This is similar to [`FunSignature`][`super::FunSignature`] with some notable differences. Namely:
/// - Syntactically, method identifiers are more than just an identifier.
///     - For instance methods, the identifier is of the form `self.ident`
///     - For static methods, the identifier is of the form `Self::ident`
/// 
/// # Examples
/// ```text
/// fun self.meow();
/// fun Self::abbr();
/// 
/// // If unused, the referent can be omitted.
/// fun .half();
/// fun ::count();
/// ```
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct MethodSignature {
    /// This method's referent (typically `self` or `Self`)
    pub referent: Option<Ident>,
    /// Whether this method is static
    pub is_static: bool,
    /// The function's name
    pub name: Ident,
    /// The function's generic parameters
    pub generic_params: Vec<Ident>,
    /// The function's parameters
    pub params: Vec<Param>,
    /// The function's return type (or `void` if unspecified)
    pub ret: Option<Type>,

    pub span: Span
}
impl Spanned for MethodSignature {
    fn span(&self) -> &Span {
        &self.span
    }
}

/// A method declaration.
/// 
/// This is similar to [`FunDecl`][`super::FunDecl`], 
/// except using a [`MethodSignature`] instead of a [`FunSignature`][`super::FunSignature`].
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct MethodDecl {
    /// The method's signature
    pub sig: MethodSignature,
    /// The method's body
    pub block: Rc<Block>,

    pub span: Span
}
impl Spanned for MethodDecl {
    fn span(&self) -> &Span {
        &self.span
    }
}
