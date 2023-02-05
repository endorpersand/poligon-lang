//! These components are the components of the AST involved with types 
//! (type expressions, classes, shapes).

use super::{MutType, ReasgType, FunDecl};

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
pub struct MethodDecl {
    /// Whether this method is static
    pub is_static: bool,
    /// The actual function
    pub decl: FunDecl
}