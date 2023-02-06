use std::collections::HashMap;

use crate::ast;
use crate::compiler::codegen::OpErr;

use super::Split;

/// A type expression.
/// 
/// This corresponds to [`ast::Type`].
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Type {
    /// A type without type parameters (e.g. `string`, `int`).
    Prim(String),
    /// A type with type parameters (e.g. `list<string>`, `dict<string, int>`).
    Generic(String, Vec<Type>),
    /// A tuple of types (e.g. `[int, int, int]`).
    Tuple(Vec<Type>),
    /// A function (e.g. `() -> int`, `str -> int`).
    Fun(FunType)
}

/// A function type expression.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct FunType(pub Vec<Type>, pub Box<Type>);

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) enum TypeRef<'a> {
    Prim(&'a str),
    Generic(&'a str, &'a [Type]),
    Tuple(&'a [Type]),
    Fun(&'a [Type], &'a Type)
}
impl TypeRef<'_> {
    #[allow(unused)]
    pub(crate) fn to_owned(self) -> Type {
        match self {
            TypeRef::Prim(ident) => Type::Prim(String::from(ident)),
            TypeRef::Generic(ident, params) => Type::Generic(String::from(ident), Vec::from(params)),
            TypeRef::Tuple(tys) => Type::Tuple(Vec::from(tys)),
            TypeRef::Fun(params, ret) => Type::fun_type(Vec::from(params), ret.clone()),
        }
    }
}

enum TypeRezError {
    NoBranches,
    MultipleBranches
}
impl Type {
    pub(crate) const S_INT:   &'static str = "int";
    pub(crate) const S_FLOAT: &'static str = "float";
    pub(crate) const S_BOOL:  &'static str = "bool";
    pub(crate) const S_CHAR:  &'static str = "char";
    pub(crate) const S_STR:   &'static str = "string";
    pub(crate) const S_VOID:  &'static str = "void";
    pub(crate) const S_LIST:  &'static str = "list";
    pub(crate) const S_SET:   &'static str = "set";
    pub(crate) const S_DICT:  &'static str = "dict";
    pub(crate) const S_RANGE: &'static str = "range";
    pub(crate) const S_NEVER: &'static str = "never";
    pub(crate) const S_UNK:   &'static str = "unk";

    pub(crate) fn as_ref(&self) -> TypeRef {
        match self {
            Type::Prim(ident) => TypeRef::Prim(ident),
            Type::Generic(ident, params) => TypeRef::Generic(ident, params),
            Type::Tuple(params) => TypeRef::Tuple(params),
            Type::Fun(ft) => ft.as_ref()
        }
    }

    pub(crate) fn fun_type(params: Vec<Type>, ret: Type) -> Self {
        Type::Fun(FunType(params, Box::new(ret)))
    }

    /// Test if this type is `never`.
    #[inline]
    fn is_never(&self) -> bool {
        matches!(self.as_ref(), TypeRef::Prim(Type::S_NEVER))
    }
    
    /// Test if this type is a numeric type (`int`, `float`).
    pub fn is_numeric(&self) -> bool {
        matches!(self.as_ref(), TypeRef::Prim(Type::S_INT) | TypeRef::Prim(Type::S_FLOAT))
    }

    /// Given some types, merge them into what type it could be.
    fn resolve_type<'a>(into_it: impl IntoIterator<Item=&'a Type>) -> Result<Type, TypeRezError> {
        let mut it = into_it.into_iter()
            .filter(|ty| !ty.is_never());
    
        match it.next() {
            Some(head) => if it.all(|u| head == u) {
                Ok(head.clone())
            } else {
                Err(TypeRezError::MultipleBranches)
            },
            None => Err(TypeRezError::NoBranches),
        }
    }

    /// Resolve a block's type, given the types of the values exited from the block.
    pub fn resolve_branches<'a>(into_it: impl IntoIterator<Item=&'a Type>) -> Option<Type> {
        match Type::resolve_type(into_it) {
            Ok(ty) => Some(ty),
            Err(TypeRezError::NoBranches) => Some(ty!(Type::S_NEVER)),
            Err(TypeRezError::MultipleBranches) => None,
        }
    }
    /// Resolve a collection literal's type, given the types of the elements of the collection.
    pub fn resolve_collection_ty<'a>(into_it: impl IntoIterator<Item=&'a Type>) -> Option<Type> {
        Type::resolve_type(into_it).ok()
    }

    /// Compute the type that would result by splitting this type with the given [`Split`].
    pub fn split(&self, sp: Split) -> Result<Type, OpErr> {
        match self.as_ref() {
            TypeRef::Prim(Type::S_STR) => match sp {
                Split::Left(_)
                | Split::Right(_) 
                => Ok(ty!(Type::S_CHAR)),
                Split::Middle(_, _) => Ok(self.clone()),
            },

            TypeRef::Generic(Type::S_LIST, params) => {
                let Some(param) = params.first() else {
                    unreachable!("list cannot be defined without parameters")
                };

                match sp {
                    Split::Left(_)
                    | Split::Right(_) => Ok(param.clone()),
                    Split::Middle(_, _) => Ok(self.clone()),
                }
            },

            TypeRef::Tuple(tpl) => match sp {
                Split::Left(idx) => tpl.get(idx).cloned()
                    .ok_or_else(|| OpErr::InvalidSplit(self.clone(), sp)),
                Split::Middle(start, end) => {
                    let vec = tpl.get(start..(tpl.len() - end))
                        .ok_or_else(|| OpErr::InvalidSplit(self.clone(), sp))?
                        .to_vec();
                    
                    Ok(Type::Tuple(vec))
                },
                Split::Right(idx) => tpl.get(tpl.len() - idx).cloned()
                    .ok_or_else(|| OpErr::InvalidSplit(self.clone(), sp)),
            },

            _ => Err(OpErr::CannotIndex(self.clone()))
        }
    }
}

impl<'a> PartialEq<TypeRef<'a>> for Type {
    fn eq(&self, &other: &TypeRef) -> bool {
        self.as_ref() == other
    }
}
impl<'a> PartialEq<Type> for TypeRef<'a> {
    fn eq(&self, other: &Type) -> bool { other.eq(self) }
}
impl<'a> PartialEq<TypeRef<'a>> for &'a Type {
    fn eq(&self, other: &TypeRef) -> bool { (*self).eq(other) }
}
impl<'a> PartialEq<&'a Type> for TypeRef<'a> {
    fn eq(&self, other: &&Type) -> bool { self.eq(*other) }
}

impl FunType {
    pub(crate) fn as_ref(&self) -> TypeRef {
        TypeRef::Fun(&self.0, &self.1)
    }

    pub fn pop_front(&mut self) {
        self.0.remove(0);
    }
}

/// Utility macro to make PLIR type expressions easier to read.
macro_rules! ty {
    ($e:expr) => {
        $crate::compiler::plir::Type::Prim(String::from($e))
    };

    ($e:expr, [$($p:expr),+]) => {
        $crate::compiler::plir::Type::Generic(String::from($e), vec![$($p),+])
    };

    ([$($p:expr),+]) => {
        $crate::compiler::plir::Type::Tuple(vec![$($p),+])
    };

    ([$($p:expr),*] -> $r:expr) => {
        $crate::compiler::plir::Type::Fun(vec![$($p),*], Box::new($r))
    };
}
pub(crate) use ty;

impl From<ast::Type> for Type {
    fn from(ty: ast::Type) -> Self {
        let ast::Type(ident, params) = ty;
        
        if params.is_empty() {
            Type::Prim(ident)
        } else {
            let p = params.into_iter()
                .map(Type::from)
                .collect();
            Type::Generic(ident, p)
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Class {
    /// Name of the struct
    pub ident: String,
    /// Struct's fields
    pub fields: HashMap<String, (usize, FieldDecl)>
}

#[derive(Debug, PartialEq)]
pub struct FieldDecl {
    /// Whether the field can be reassigned later
    pub rt: ast::ReasgType,
    
    /// Whether the field can be mutated
    pub mt: ast::MutType,

    /// The type of the declaration (inferred if not present)
    pub ty: Type
}