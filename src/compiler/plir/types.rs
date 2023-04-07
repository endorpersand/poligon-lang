use std::borrow::Cow;

use indexmap::IndexMap;
use lazy_static::lazy_static;

use crate::ast;
use crate::compiler::plir_codegen::OpErr;

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
pub struct FunType {
    /// The parameter types of this function type
    pub params: Vec<Type>, 

    /// The return type
    pub ret: Box<Type>,

    /// Whether this type has varargs at the end of its parameters
    pub varargs: bool
}

impl FunType {
    /// Constructs a new function type.
    pub fn new(params: Vec<Type>, ret: Type, varargs: bool) -> Self {
        FunType {params, ret: Box::new(ret), varargs }
    }

    /// Constructs a function signature (with parameter names lost) out of a given function type.
    pub fn extern_fun_sig(&self, ident: super::FunIdent) -> super::FunSignature {
        let params = self.params.iter()
            .enumerate()
            .map(|(i, t)| super::Param {
                rt: Default::default(),
                mt: Default::default(),
                ident: format!("arg{i}"),
                ty: t.clone(),
            })
            .collect();

        super::FunSignature {
            private: false,
            ident,
            params,
            varargs: self.varargs,
            ret: (*self.ret).clone(),
        }
    }

    pub(crate) fn as_ref(&self) -> TypeRef {
        TypeRef::Fun(&self.params, &self.ret, self.varargs)
    }

    /// Removes the first parameter of the function.
    /// 
    /// This is useful for removing the referent in method types.
    pub fn pop_front(&mut self) {
        self.params.remove(0);
    }

}

impl TryFrom<Type> for FunType {
    type Error = crate::compiler::plir_codegen::PLIRErr;

    fn try_from(value: Type) -> Result<Self, Self::Error> {
        use crate::compiler::plir_codegen::PLIRErr;

        match value {
            Type::Fun(f) => Ok(f),
            t => Err(PLIRErr::CannotCall(t))
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) enum TypeRef<'a> {
    Prim(&'a str),
    Generic(&'a str, &'a [Type]),
    Tuple(&'a [Type]),
    Fun(&'a [Type], &'a Type, bool)
}
impl TypeRef<'_> {
    #[allow(unused)]
    pub(crate) fn to_owned(self) -> Type {
        match self {
            TypeRef::Prim(ident) => Type::Prim(String::from(ident)),
            TypeRef::Generic(ident, params) => Type::Generic(String::from(ident), Vec::from(params)),
            TypeRef::Tuple(tys) => Type::Tuple(Vec::from(tys)),
            TypeRef::Fun(params, ret, var_args) => Type::fun_type(Vec::from(params), ret.clone(), var_args),
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

    pub(crate) fn fun_type(params: impl IntoIterator<Item=Type>, ret: Type, var_args: bool) -> Self {
        Type::Fun(FunType::new(params.into_iter().collect(), ret, var_args))
    }

    /// Test if this type is `never`.
    #[inline]
    pub fn is_never(&self) -> bool {
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

    /// Return a short form of this type's identifier.
    pub fn short_ident(&self) -> &str {
        match self {
            Type::Prim(n) => n,
            Type::Generic(n, _) => n,
            Type::Tuple(_) => "#tuple",
            Type::Fun(_) => "#fun",
        }
    }

    /// Return this type's full identifier.
    pub fn ident(&self) -> Cow<str> {
        #[inline]
        fn param_tuple(params: &[Type]) -> String {
            params.iter()
                .map(Type::ident)
                .collect::<Vec<_>>()
                .join(", ")
        }

        match self.as_ref() {
            TypeRef::Prim(ident) => Cow::from(ident),
            TypeRef::Generic(ident, params) => {
                Cow::from(format!("{ident}<{}>", param_tuple(params)))
            },
            TypeRef::Tuple(params) => {
                Cow::from(format!("#tuple<{}>", param_tuple(params)))
            },
            TypeRef::Fun(params, ret, varargs) => {
                let va_filler = match (params.is_empty(), varargs) {
                    (true, true) => "..",
                    (false, true) => ", ..",
                    (_, false) => ""
                };

                Cow::from(format!("#fun<({}{}) -> {}>", param_tuple(params), va_filler, ret.ident()))
            },
        }
    }

    /// Gets the generic parameters of this type.
    /// 
    /// This function does not allocate or clone anything.
    pub fn generic_args(&self) -> Cow<[Type]> {
        match self {
            Type::Prim(_)       => Cow::from(vec![]),
            Type::Generic(_, p) => Cow::from(p),
            Type::Tuple(_)      => Cow::from(vec![]),
            Type::Fun(_)        => Cow::from(vec![]),
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

/// Wrapper to test if a type is numeric.
#[derive(PartialEq, Eq)]
pub(crate) struct NumType<'a>(pub &'a Type);
lazy_static! {
    static ref NUM_TYPE_ORDER: IndexMap<Type, (bool /* float? */, usize /* size */)> = {
        let mut m = IndexMap::new();

        m.insert(ty!("#byte"),       (false, 8));
        m.insert(ty!(Type::S_INT),   (false, 64));
        m.insert(ty!(Type::S_FLOAT), (true,  64));
        
        m
    };
}
impl<'a> NumType<'a> {
    pub fn encoding(&self) -> Option<(bool /* float? */, usize /* size */)> {
        NUM_TYPE_ORDER.get(self.0).copied()
    }

    pub fn is_numeric(&self) -> bool {
        NUM_TYPE_ORDER.contains_key(self.0)
    }
    pub fn is_integer(&self) -> bool {
        matches!(self.encoding(), Some((false, _)))
    }
    pub fn is_floating(&self) -> bool {
        matches!(self.encoding(), Some((true, _)))
    }

    pub fn order() -> Vec<&'a Type> {
        NUM_TYPE_ORDER.keys().collect()
    }
    pub fn int_order() -> Vec<&'a Type> {
        NUM_TYPE_ORDER.iter()
            .filter_map(|(t, (is_float, _))| (!is_float).then_some(t))
            .collect()
    }
    pub fn float_order() -> Vec<&'a Type> {
        NUM_TYPE_ORDER.iter()
            .filter_map(|(t, (is_float, _))| is_float.then_some(t))
            .collect()
    }
}
impl<'a> PartialOrd for NumType<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Option::zip(self.encoding(), other.encoding())
            .map(|(a, b)| a.cmp(&b))
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

    (($($p:expr),*) -> $r:expr) => {
        $crate::compiler::plir::Type::Fun(
            $crate::compiler::plir::FunType {
                params: vec![$($p),*], 
                ret: Box::new($r),
                varargs: false
            }
        )
    };
}
pub(crate) use ty;

/// A class declaration.
/// 
/// This corresponds to [`ast::Class`].
/// However, unlike `ast::Class`, 
/// this struct does not contain the class's methods.
/// 
/// Those methods are redefined as functions in PLIR codegen.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Class {
    /// The associated [`Type`] of this class
    pub ty: Type,
    /// A mapping of identifiers to fields in the class
    pub fields: IndexMap<String, Field>
}

/// A field declaration.
///
/// This corresponds to [`ast::FieldDecl`].
/// Unlike `ast::FieldDecl`, the field's name is omitted (and has been moved to [`Class`]).
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Field {
    /// Whether the field can be reassigned later
    pub rt: ast::ReasgType,
    
    /// Whether the field can be mutated
    pub mt: ast::MutType,

    /// The type of the declaration (inferred if not present)
    pub ty: Type
}