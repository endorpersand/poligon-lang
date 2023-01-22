use crate::ast::{op, self};
use crate::err::GonErr;

use super::{Split, Expr, ExprType};

/// An operation between types failed.
#[derive(Debug)]
pub enum OpErr {
    /// The unary operator cannot be applied to this type.
    CannotUnary(op::Unary, Type),
    /// The binary operator cannot be applied between these two types.
    CannotBinary(op::Binary, Type, Type),
    /// These two types can't be compared using the given operation.
    CannotCmp(op::Cmp, Type, Type),
    /// Cannot index this type.
    CannotIndex(Type),
    /// Cannot index this type using the other type.
    CannotIndexWith(Type, Type),
    /// Type is a tuple, and cannot be indexed by a non-literal.
    TupleIndexNonLiteral(Type),
    /// Type is a tuple, and the index provided was out of bounds.
    TupleIndexOOB(Type, isize),
    /// Type cannot be split properly using this split.
    InvalidSplit(Type, Split)
}

impl GonErr for OpErr {
    fn err_name(&self) -> &'static str {
        "type error"
    }

    fn message(&self) -> String {
        match self {
            Self::CannotUnary(op, t1) => format!("cannot apply '{op}' to {t1}"),
            Self::CannotBinary(op, t1, t2) => format!("cannot apply '{op}' to {t1} and {t2}"),
            Self::CannotCmp(op, t1, t2) => format!("cannot compare '{op}' between {t1} and {t2}"),
            Self::CannotIndex(t1) => format!("cannot index {t1}"),
            Self::CannotIndexWith(t1, t2) => format!("cannot index {t1} with {t2}"),
            Self::TupleIndexNonLiteral(t) => format!("cannot index type '{t}' with a non-literal"),
            Self::TupleIndexOOB(t, i) => format!("index out of bounds: {t}[{i}]"),
            Self::InvalidSplit(t, s) => format!("cannot index: {t}~[{}]", match s {
                Split::Left(l) => format!("{l}"),
                Split::Middle(l, r) => format!("{l}..-{r}"),
                Split::Right(r) => format!("{r}")
            }),
        }
    }
}
/// A type expression.
/// 
/// This corresponds to [`ast::Type`].
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    /// A type without type parameters (e.g. `string`, `int`).
    Prim(String),
    /// A type with type parameters (e.g. `list<string>`, `dict<string, int>`).
    Generic(String, Vec<Type>),
    /// A tuple of types (e.g. `[int, int, int]`).
    Tuple(Vec<Type>),
    /// A function (e.g. `() -> int`, `str -> int`).
    Fun(Vec<Type>, Box<Type>)
}

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
            TypeRef::Fun(params, ret) => Type::Fun(Vec::from(params), Box::new(ret.clone())),
        }
    }
}

enum TypeRezError {
    NoBranches,
    MultipleBranches
}
impl Type {
    pub(crate) const S_INT: &'static str   = "int";
    pub(crate) const S_FLOAT: &'static str = "float";
    pub(crate) const S_BOOL: &'static str  = "bool";
    pub(crate) const S_CHAR: &'static str  = "char";
    pub(crate) const S_STR: &'static str   = "string";
    pub(crate) const S_VOID: &'static str  = "void";
    pub(crate) const S_LIST: &'static str  = "list";
    pub(crate) const S_SET: &'static str   = "set";
    pub(crate) const S_DICT: &'static str  = "dict";
    pub(crate) const S_RANGE: &'static str = "range";
    pub(crate) const S_NEVER: &'static str = "never";
    pub(crate) const S_UNK: &'static str   = "unk";

    pub(crate) fn as_ref(&self) -> TypeRef {
        match self {
            Type::Prim(ident) => TypeRef::Prim(ident),
            Type::Generic(ident, params) => TypeRef::Generic(ident, params),
            Type::Tuple(params) => TypeRef::Tuple(params),
            Type::Fun(params, ret) => TypeRef::Fun(params, ret),
        }
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

    /// Test if this type is `int`.
    #[inline]
    fn is_int(&self) -> bool {
        matches!(self.as_ref(), TypeRef::Prim(Type::S_INT))
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

    /// Compute the type that would result by indexing a value of this type with a given expression.
    pub fn resolve_index_type(&self, idx: &Expr) -> Result<Type, OpErr> {
        let idx_ty = &idx.ty;

        match self.as_ref() {
            TypeRef::Prim(Type::S_STR) => match idx_ty.is_int() {
                true  => Ok(ty!(Type::S_CHAR)),
                false => Err(OpErr::CannotIndexWith(self.clone(), idx_ty.clone())),
            }
            TypeRef::Generic(Type::S_LIST, params) => match idx_ty.is_int() {
                true  => Ok(params[0].clone()),
                false => Err(OpErr::CannotIndexWith(self.clone(), idx_ty.clone())),
            }
            TypeRef::Generic(Type::S_DICT, params) => {
                let (key, val) = (&params[0], &params[1]);
                match key == idx_ty {
                    true  => Ok(val.clone()),
                    false => Err(OpErr::CannotIndexWith(self.clone(), idx_ty.clone())),
                }
            }
            TypeRef::Tuple(tys) => match idx.expr {
                ExprType::Literal(ast::Literal::Int(idx_lit)) => {
                    usize::try_from(idx_lit)
                        .ok()
                        .and_then(|idx| tys.get(idx))
                        .ok_or_else(|| OpErr::TupleIndexOOB(self.clone(), idx_lit))
                        .map(Clone::clone)
                },
                _ => match idx_ty.is_int() {
                    true  => Err(OpErr::TupleIndexNonLiteral(self.clone())),
                    false => Err(OpErr::CannotIndexWith(self.clone(), idx_ty.clone()))
                }
            }
            _ => Err(OpErr::CannotIndex(self.clone()))
        }
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
