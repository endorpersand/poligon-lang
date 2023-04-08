use std::borrow::{Cow, Borrow};

use indexmap::IndexMap;
use lazy_static::lazy_static;

use crate::ast;
use crate::compiler::plir_codegen::{OpErr, PLIRErr};

use super::Split;

/// Trait holding any type which can be used as a primitive for [Type].
pub trait TypeUnit: Borrow<Self::Ref> {
    /// Reference type for parametrized [TypeRef]
    type Ref: ?Sized + ToOwned<Owned=Self> + PartialEq;
}
impl TypeUnit for String {
    type Ref = str;
}

/// A type expression.
/// 
/// This corresponds to [`ast::Type`].
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Type<U: TypeUnit> {
    /// A type without type parameters (e.g. `string`, `int`).
    Prim(U),
    /// A type with type parameters (e.g. `list<string>`, `dict<string, int>`).
    Generic(String, Vec<Type<U>>),
    /// A tuple of types (e.g. `[int, int, int]`).
    Tuple(Vec<Type<U>>),
    /// A function (e.g. `() -> int`, `str -> int`).
    Fun(FunType<U>)
}

/// A function type expression.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct FunType<U: TypeUnit = String> {
    /// The parameter types of this function type
    pub params: Vec<Type<U>>, 

    /// The return type
    pub ret: Box<Type<U>>,

    /// Whether this type has varargs at the end of its parameters
    pub varargs: bool
}


/// Unit type which includes the possibility of an unknown type variable.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum MTypeUnit {
    /// Primitive type which is known
    Known(String),
    /// Unknown type variable
    Unk(usize)
}
impl TypeUnit for MTypeUnit {
    type Ref = MTypeUnit;
}
/// A type expression which can include an unknown type variable.
pub type MaybeType = Type<MTypeUnit>;
/// A known type unit.
pub type KTypeUnit = String;
/// A type expression whose type is fully known
pub type KnownType = Type<KTypeUnit>;

impl<U: TypeUnit> FunType<U> {
    /// Constructs a new function type.
    pub fn new(params: Vec<Type<U>>, ret: Type<U>, varargs: bool) -> Self {
        FunType {params, ret: Box::new(ret), varargs }
    }

    /// Removes the first parameter of the function.
    /// 
    /// This is useful for removing the referent in method types.
    pub fn pop_front(&mut self) {
        self.params.remove(0);
    }

    pub(crate) fn as_ref(&self) -> TypeRef<U> {
        TypeRef::Fun(&self.params, &self.ret, self.varargs)
    }
}
impl FunType {
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
}

impl TryFrom<KnownType> for FunType {
    type Error = PLIRErr;

    fn try_from(value: KnownType) -> Result<Self, Self::Error> {
        match value {
            Type::Fun(f) => Ok(f),
            t => Err(PLIRErr::CannotCall(t))
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum TypeRef<'a, U: TypeUnit = String> {
    Prim(&'a U::Ref),
    Generic(&'a str, &'a [Type<U>]),
    Tuple(&'a [Type<U>]),
    Fun(&'a [Type<U>], &'a Type<U>, bool)
}
impl<U: TypeUnit + Clone> TypeRef<'_, U> {
    #[allow(unused)]
    pub(crate) fn to_owned(self) -> Type<U> {
        match self {
            TypeRef::Prim(ident) => Type::Prim(ident.to_owned()),
            TypeRef::Generic(ident, params) => Type::Generic(ident.to_owned(), Vec::from(params)),
            TypeRef::Tuple(tys) => Type::Tuple(Vec::from(tys)),
            TypeRef::Fun(params, ret, var_args) => Type::fun_type(Vec::from(params), ret.clone(), var_args),
        }
    }
}
impl<U: TypeUnit> Clone for TypeRef<'_, U> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<U: TypeUnit> Copy for TypeRef<'_, U> {}

enum TypeRezError {
    NoBranches,
    MultipleBranches
}
impl<U: TypeUnit> Type<U> {
    pub(crate) fn fun_type(params: impl IntoIterator<Item=Self>, ret: Self, var_args: bool) -> Self {
        Type::Fun(FunType::new(params.into_iter().collect(), ret, var_args))
    }

    /// Gets the generic parameters of this type.
    /// 
    /// This function does not allocate or clone anything.
    pub fn generic_args(&self) -> Cow<[Self]> 
        where Self: Clone
    {
        match self {
            Type::Prim(_)       => Cow::from(vec![]),
            Type::Generic(_, p) => Cow::from(p),
            Type::Tuple(_)      => Cow::from(vec![]),
            Type::Fun(_)        => Cow::from(vec![]),
        }
    }

    pub(crate) fn as_ref(&self) -> TypeRef<<U::Ref as ToOwned>::Owned> {
        match self {
            Type::Prim(prim) => TypeRef::Prim(prim.borrow()),
            Type::Generic(ident, params) => TypeRef::Generic(ident.borrow(), params),
            Type::Tuple(params) => TypeRef::Tuple(params),
            Type::Fun(ft) => ft.as_ref()
        }
    }
}
impl KnownType {
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

    /// Test if this type is `never`.
    #[inline]
    pub fn is_never(&self) -> bool {
        matches!(self.as_ref(), TypeRef::Prim(Type::S_NEVER))
    }
    
    /// Test if this type is a numeric type (e.g., `int`, `float`).
    pub fn is_numeric(&self) -> bool {
        NumType::new(self).is_some()
    }
    /// Test if this type is an int type (e.g., `int`, `float`).
    pub fn is_int_like(&self) -> bool {
        match NumType::new(self) {
            Some(t) => t.is_int_like(),
            None => false,
        }
    }
    /// Test if this type is a float type (e.g., `int`, `float`).
    pub fn is_float_like(&self) -> bool {
        match NumType::new(self) {
            Some(t) => t.is_float_like(),
            None => false,
        }
    }

    /// Given some types, merge them into what type it could be.
    fn resolve_type<'a>(into_it: impl IntoIterator<Item=&'a KnownType>) -> Result<KnownType, TypeRezError> {
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
    pub fn resolve_branches<'a>(into_it: impl IntoIterator<Item=&'a KnownType>) -> Option<KnownType> {
        match Type::resolve_type(into_it) {
            Ok(ty) => Some(ty),
            Err(TypeRezError::NoBranches) => Some(ty!(Type::S_NEVER)),
            Err(TypeRezError::MultipleBranches) => None,
        }
    }
    /// Resolve a collection literal's type, given the types of the elements of the collection.
    pub fn resolve_collection_ty<'a>(into_it: impl IntoIterator<Item=&'a KnownType>) -> Option<KnownType> {
        Type::resolve_type(into_it).ok()
    }

    /// Compute the type that would result by splitting this type with the given [`Split`].
    pub fn split(&self, sp: Split) -> Result<KnownType, OpErr> {
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
        fn param_tuple(params: &[KnownType]) -> String {
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
}

impl<'a, U: TypeUnit + PartialEq> PartialEq<TypeRef<'a, U>> for Type<U> {
    fn eq(&self, &other: &TypeRef<U>) -> bool {
        self.as_ref() == other
    }
}
impl<'a, U: TypeUnit + PartialEq> PartialEq<Type<U>> for TypeRef<'a, U> {
    fn eq(&self, other: &Type<U>) -> bool { other.eq(self) }
}
impl<'a, U: TypeUnit + PartialEq> PartialEq<TypeRef<'a, U>> for &'a Type<U> {
    fn eq(&self, other: &TypeRef<U>) -> bool { (*self).eq(other) }
}
impl<'a, U: TypeUnit + PartialEq> PartialEq<&'a Type<U>> for TypeRef<'a, U> {
    fn eq(&self, other: &&Type<U>) -> bool { self.eq(*other) }
}

/// Helper type which generalizes numerics into categories.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub(crate) struct NumType {
    pub floating: bool,
    pub bit_len: usize
}
lazy_static! {
    static ref NUM_TYPE_ORDER: IndexMap<KnownType, NumType> = {
        let mut m = IndexMap::new();

        m.insert(ty!("#byte"),       NumType { floating: false, bit_len: 8  });
        m.insert(ty!(Type::S_INT),   NumType { floating: false, bit_len: 64 });
        m.insert(ty!(Type::S_FLOAT), NumType { floating: true,  bit_len: 64 });
        
        m
    };
}
impl NumType {
    pub fn new(ty: &KnownType) -> Option<Self> {
        NUM_TYPE_ORDER.get(ty).copied()
    }

    pub fn is_int_like(&self) -> bool {
        !self.floating
    }
    pub fn is_float_like(&self) -> bool {
        self.floating
    }

    pub fn order<'a>() -> Vec<&'a KnownType> {
        NUM_TYPE_ORDER.keys().collect()
    }
    pub fn int_order<'a>() -> Vec<&'a KnownType> {
        NUM_TYPE_ORDER.iter()
            .filter_map(|(pt, nt)| nt.is_int_like().then_some(pt))
            .collect()
    }
    pub fn float_order<'a>() -> Vec<&'a KnownType> {
        NUM_TYPE_ORDER.iter()
            .filter_map(|(t, nt)| nt.is_float_like().then_some(t))
            .collect()
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
    pub ty: KnownType,
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
    pub ty: KnownType
}