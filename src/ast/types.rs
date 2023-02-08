//! These components are the components of the AST involved with types 
//! (type expressions, classes, shapes).

use std::rc::Rc;

use super::{MutType, ReasgType, Block, Param};

/// A type expression.
/// 
/// # Examples
/// ```text
/// string
/// list<string>
/// map<string, list<int>>
/// ```
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Type(pub String, pub Vec<Type>);

#[derive(Debug, PartialEq)]
pub struct Class {
    /// Name of the struct
    pub ident: String,
    /// Struct's fields
    pub fields: Vec<FieldDecl>,
    /// Struct's methods
    pub methods: Vec<MethodDecl>
}

#[derive(Debug, PartialEq)]
pub struct FieldDecl {
    /// Whether the field can be reassigned later
    pub rt: ReasgType,
    
    /// Whether the field can be mutated
    pub mt: MutType,

    /// The field's name
    pub ident: String,

    /// The type of the declaration (inferred if not present)
    pub ty: Type
}

#[derive(Debug, PartialEq)]
pub struct MethodSignature {
    /// This method's self, Self
    pub this: Option<String>,
    /// Whether this method is static
    pub is_static: bool,
    /// The function's identifier
    pub ident: String,
    /// The function's parameters
    pub params: Vec<Param>,
    /// The function's return type (or `void` if unspecified)
    pub ret: Option<Type>,
}

#[derive(Debug, PartialEq)]
pub struct MethodDecl {
    /// The function's signature
    pub sig: MethodSignature,
    /// The function's body
    pub block: Rc<Block>
}