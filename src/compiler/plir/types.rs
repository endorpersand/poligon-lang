use std::borrow::Cow;

use indexmap::IndexMap;
use lazy_static::lazy_static;

use crate::ast;
use crate::compiler::plir_codegen::{OpErr, PLIRErr};

use super::{Split, own_cow};

/// An owned type value.
/// 
/// The reference version (which this value is based on) is located at [`TypeRef`].
pub type Type = TypeRef<'static>;
/// An owned function type value.
/// 
/// The reference version (which this value is based on) is located at [`FunTypeRef`].
pub type FunType = FunTypeRef<'static>;
/// A type expression.
/// 
/// This corresponds to [`ast::Type`].
/// 
/// This includes owned type values and type references.
/// The pure owned type version of this enum is located at [`Type`].
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum TypeRef<'t> {
    /// An unresolved monotype (these are of the form `?1`, `?2`)
    Unk(usize),
    /// A type variable
    TypeVar(Cow<'t, str>, Cow<'t, str>),
    /// A concrete type without type parameters (e.g. `string`, `int`).
    Prim(Cow<'t, str>),
    /// A type with type parameters (e.g. `list<string>`, `dict<string, int>`).
    Generic(Cow<'t, str>, Cow<'t, [Type]>, ()),
    /// A tuple of types (e.g. `[int, int, int]`).
    Tuple(Cow<'t, [Type]>, ()),
    /// A function (e.g. `() -> int`, `str -> int`).
    Fun(FunTypeRef<'t>)
}

/// A function type expression.
/// 
/// This includes owned function type values and type references.
/// The pure owned function type version of this enum is located at [`FunType`].
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct FunTypeRef<'t> {
    /// The parameter types of this function type
    pub params: Cow<'t, [Type]>, 

    /// The return type
    pub ret: Cow<'t, Box<Type>>,

    /// Whether this type has varargs at the end of its parameters
    pub varargs: bool
}

fn downgrade<T: ToOwned + ?Sized>(p: &T) -> Cow<T> {
    Cow::Borrowed(p)
}
fn upgrade<T: ToOwned + ?Sized>(p: &T) -> Cow<'static, T> {
    Cow::Owned(p.to_owned())
}

impl TypeRef<'_> {
    pub(crate) fn downgrade(&self) -> TypeRef {
        match self {
            TypeRef::Unk(idx) => TypeRef::Unk(*idx),
            TypeRef::TypeVar(ty, var) => TypeRef::TypeVar(downgrade(ty), downgrade(var)),
            TypeRef::Prim(id) => TypeRef::Prim(downgrade(id)),
            TypeRef::Generic(id, params, _) => TypeRef::Generic(downgrade(id), downgrade(params), ()),
            TypeRef::Tuple(params, _) => TypeRef::Tuple(downgrade(params), ()),
            TypeRef::Fun(f) => TypeRef::Fun(f.downgrade()),
        }
    }
    pub(crate) fn upgrade(&self) -> Type {
        match self {
            TypeRef::Unk(idx) => TypeRef::Unk(*idx),
            TypeRef::TypeVar(ty, var) => TypeRef::TypeVar(upgrade(ty), upgrade(var)),
            TypeRef::Prim(id) => TypeRef::Prim(upgrade(id)),
            TypeRef::Generic(id, params, _) => TypeRef::Generic(upgrade(id), upgrade(params), ()),
            TypeRef::Tuple(params, _) => TypeRef::Tuple(upgrade(params), ()),
            TypeRef::Fun(f) => TypeRef::Fun(f.upgrade()),
        }
    }

    /// Gets the generic parameters of this type.
    /// 
    /// This function does not allocate or clone anything.
    pub fn generic_args(&self) -> &[Type] {
        match self {
            TypeRef::Generic(_, p, ()) => p,
            _ => &[]
        }
    }

    /// Test if this type is `never`.
    #[inline]
    pub fn is_never(&self) -> bool {
        matches!(self.downgrade(), TypeRef::Prim(Cow::Borrowed(Type::S_NEVER)))
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

    /// This function fallibly maps Type units to other Types, via a mutable reference.
    /// 
    /// This will walk through the Type to replace units (f.e. Unk, TypeVar, Prim) 
    /// using the provided replacer function. 
    /// This function does not walk through the type returned by the replacer and
    /// therefore will not replace any units of the type returned by the replacer.
    /// 
    /// If this function raises an error, the Type will be in a state of being partially mapped.
    fn try_map_mut<E>(&mut self, f: &mut impl FnMut(TypeRef) -> Result<Type, E>) -> Result<(), E> {
        match self {
            unit @ (TypeRef::Unk(_) | TypeRef::TypeVar(_, _) | TypeRef::Prim(_)) => {
                *unit = f(unit.downgrade())?;
                
                Ok(())
            },
            TypeRef::Generic(_, ref mut params, _) | TypeRef::Tuple(ref mut params, _) => {
                for par in own_cow(params) {
                    par.try_map_mut(f)?;
                }

                Ok(())
            },
            TypeRef::Fun(FunTypeRef { ref mut params, ref mut ret, varargs: _ }) => {
                for par in own_cow(params) {
                    par.try_map_mut(f)?;
                }

                own_cow(ret).try_map_mut(f)
            },
        }
    }

    /// This function fallibly maps Type units in a given Type to other Types.
    /// 
    /// This will walk through the Type to replace units (f.e. Unk, TypeVar, Prim) 
    /// using the provided replacer function. 
    /// This function does not walk through the type returned by the replacer and
    /// therefore will not replace any units of the type returned by the replacer.
    pub(crate) fn try_map<E>(mut self, mut f: impl FnMut(TypeRef) -> Result<Type, E>) -> Result<Self, E> {
        self.try_map_mut(&mut f)?;
        Ok(self)
    }

    /// This function fallibly maps Type units in a given Type to other Types.
    /// 
    /// This will walk through the Type to replace units (f.e. Unk, TypeVar, Prim) 
    /// using the provided replacer function. 
    /// This function does not walk through the type returned by the replacer and
    /// therefore will not replace any units of the type returned by the replacer.
    pub(crate) fn map(self, mut f: impl FnMut(TypeRef) -> Type) -> Self {
        self.try_map(|t| Ok::<_, std::convert::Infallible>(f(t))).unwrap()
    }

    /// This function substitutes Type units in a given Type using a substitution hash map.
    /// 
    /// This will walk through the Type to replace units (f.e. Unk, TypeVar, Prim) 
    /// using the provided hash map.
    /// This function does not walk through the type returned by the map and
    /// therefore will not replace any units of the type returned by the map.
    pub(crate) fn substitute(self, m: &std::collections::HashMap<TypeRef, TypeRef>) -> Self {
        self.map(|t| m.get(&t).unwrap_or(&t).upgrade())
    }
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

    pub(crate) fn new_type_var(ty: impl Into<Cow<'static, str>>, var: impl Into<Cow<'static, str>>) -> Self {
        Type::TypeVar(ty.into(), var.into())
    }
    pub(crate) fn new_prim(id: impl Into<Cow<'static, str>>) -> Self {
        Type::Prim(id.into())
    }
    pub(crate) fn new_generic(id: impl Into<Cow<'static, str>>, params: impl IntoIterator<Item=Self>) -> Self {
        let params: Vec<_> = params.into_iter().collect();

        if params.is_empty() {
            Type::new_prim(id)
        } else {
            Type::Generic(id.into(), Cow::from(params), ())
        }
    }
    pub(crate) fn new_tuple(params: impl IntoIterator<Item=Self>) -> Self {
        Type::Tuple(params.into_iter().collect(), ())
    }
    pub(crate) fn new_fun(params: impl IntoIterator<Item=Self>, ret: Self, var_args: bool) -> Self {
        Type::Fun(FunType::new(params.into_iter().collect(), ret, var_args))
    }
}

impl FunTypeRef<'_> {
    pub(crate) fn downgrade(&self) -> FunTypeRef {
        let FunTypeRef { params, ret, varargs } = self;
        FunTypeRef {
            params: downgrade(params),
            ret: downgrade(ret),
            varargs: *varargs
        }
    }

    pub(crate) fn upgrade(&self) -> FunType {
        let FunTypeRef { params, ret, varargs } = self;
        FunTypeRef {
            params: upgrade(params),
            ret: upgrade(ret),
            varargs: *varargs,
        }
    }

    /// Removes the first parameter of the function.
    /// 
    /// This is useful for removing the referent in method types.
    pub fn pop_front(&mut self) {
        match &mut self.params {
            Cow::Borrowed(p) => *p = &p[1..],
            Cow::Owned(p) => { p.remove(0); },
        }
    }

    /// Constructs a function signature (with parameter names lost) out of a given function type.
    pub fn extern_fun_sig(&self, ident: super::FunIdent) -> super::FunSignature {
        let params = self.params.iter()
            .enumerate()
            .map(|(i, t)| super::Param {
                rt: Default::default(),
                mt: Default::default(),
                ident: format!("arg{i}"),
                ty: t.upgrade(),
            })
            .collect();

        super::FunSignature {
            private: false,
            ident,
            params,
            varargs: self.varargs,
            ret: TypeRef::upgrade(&self.ret),
        }
    }
}

impl FunType {
    /// Constructs a new function type.
    pub fn new(params: Vec<Type>, ret: Type, varargs: bool) -> Self {
        FunType {
            params: params.into(), 
            ret: Cow::Owned(Box::new(ret)), 
            varargs 
        }
    }
}

impl<'a> TryFrom<TypeRef<'a>> for FunTypeRef<'a> {
    type Error = PLIRErr;

    fn try_from(value: TypeRef<'a>) -> Result<Self, Self::Error> {
        match value {
            TypeRef::Fun(f) => Ok(f),
            t => Err(PLIRErr::CannotCall(t.upgrade()))
        }
    }
}
impl<'a> From<FunTypeRef<'a>> for TypeRef<'a> {
    fn from(value: FunTypeRef<'a>) -> Self {
        Self::Fun(value)
    }
}

impl<'a> PartialEq<TypeRef<'a>> for FunTypeRef<'a> {
    fn eq(&self, other: &TypeRef<'a>) -> bool {
        TypeRef::from(self.downgrade()).eq(other)
    }
}
impl<'a> PartialEq<FunTypeRef<'a>> for TypeRef<'a> {
    fn eq(&self, other: &FunTypeRef<'a>) -> bool {
        self.eq(&TypeRef::from(other.downgrade()))
    }
}

enum TypeRezError {
    NoBranches,
    MultipleBranches
}

impl Type {
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
        match self.downgrade() {
            TypeRef::Prim(Cow::Borrowed(Type::S_STR)) => match sp {
                Split::Left(_)
                | Split::Right(_) 
                => Ok(ty!(Type::S_CHAR)),
                Split::Middle(_, _) => Ok(self.clone()),
            },

            TypeRef::Generic(Cow::Borrowed(Type::S_LIST), params, ()) => {
                let Some(param) = params.first() else {
                    unreachable!("list cannot be defined without parameters")
                };

                match sp {
                    Split::Left(_)
                    | Split::Right(_) => Ok(param.clone()),
                    Split::Middle(_, _) => Ok(self.clone()),
                }
            },

            TypeRef::Tuple(tpl, ()) => match sp {
                Split::Left(idx) => tpl.get(idx).cloned()
                    .ok_or_else(|| OpErr::InvalidSplit(self.clone(), sp)),
                Split::Middle(start, end) => {
                    let vec = tpl.get(start..(tpl.len() - end))
                        .ok_or_else(|| OpErr::InvalidSplit(self.clone(), sp))?
                        .to_vec();
                    
                    Ok(Type::new_tuple(vec))
                },
                Split::Right(idx) => tpl.get(tpl.len() - idx).cloned()
                    .ok_or_else(|| OpErr::InvalidSplit(self.clone(), sp)),
            },

            _ => Err(OpErr::CannotIndex(self.clone()))
        }
    }

    /// Return this type's string name in LLVM bytecode.
    /// 
    /// This differs from the Display type to make it look cleaner in bytecode.
    pub fn llvm_ident(&self) -> Cow<str> {
        #[inline]
        fn param_tuple(params: &[Type]) -> String {
            params.iter()
                .map(Type::llvm_ident)
                .collect::<Vec<_>>()
                .join(", ")
        }

        match self.downgrade() {
            TypeRef::Unk(idx) => Cow::from(format!("#unk{idx}")),
            TypeRef::TypeVar(ty, var) => Cow::from(format!("#{ty}::{var}")),
            TypeRef::Prim(ident) => ident,
            TypeRef::Generic(ident, params, ()) => {
                Cow::from(format!("{ident}<{}>", param_tuple(&params)))
            },
            TypeRef::Tuple(params, ()) => {
                Cow::from(format!("#tuple<{}>", param_tuple(&params)))
            },
            TypeRef::Fun(FunTypeRef { params, ret, varargs }) => {
                let va_filler = match (params.is_empty(), varargs) {
                    (true, true) => "..",
                    (false, true) => ", ..",
                    (_, false) => ""
                };

                Cow::from(format!("#fun<({}{}) -> {}>", param_tuple(&params), va_filler, ret.llvm_ident()))
            },
        }
    }
}

/// Helper type which generalizes numerics into categories.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub(crate) struct NumType {
    pub floating: bool,
    pub bit_len: usize
}
lazy_static! {
    static ref NUM_TYPE_ORDER: IndexMap<Type, NumType> = {
        let mut m = IndexMap::new();

        m.insert(ty!("#byte"),       NumType { floating: false, bit_len: 8  });
        m.insert(ty!(Type::S_INT),   NumType { floating: false, bit_len: 64 });
        m.insert(ty!(Type::S_FLOAT), NumType { floating: true,  bit_len: 64 });
        
        m
    };
}
impl NumType {
    pub fn new(ty: &TypeRef) -> Option<Self> {
        NUM_TYPE_ORDER.get(ty).copied()
    }

    pub fn is_int_like(&self) -> bool {
        !self.floating
    }
    pub fn is_float_like(&self) -> bool {
        self.floating
    }

    pub fn order() -> Vec<&'static Type> {
        NUM_TYPE_ORDER.keys().collect()
    }
    pub fn int_order() -> Vec<&'static Type> {
        NUM_TYPE_ORDER.iter()
            .filter_map(|(pt, nt)| nt.is_int_like().then_some(pt))
            .collect()
    }
    pub fn float_order() -> Vec<&'static Type> {
        NUM_TYPE_ORDER.iter()
            .filter_map(|(t, nt)| nt.is_float_like().then_some(t))
            .collect()
    }
}

/// Utility macro to make PLIR type expressions easier to read.
macro_rules! ty {
    ($e:expr) => {
        $crate::compiler::plir::Type::Prim(std::borrow::Cow::from($e))
    };

    ($e:expr, [$($p:expr),+]) => {
        $crate::compiler::plir::Type::Generic(
            std::borrow::Cow::from($e), 
            std::borrow::Cow::from(vec![$($p),+]), 
            ()
        )
    };

    ([$($p:expr),+]) => {
        $crate::compiler::plir::Type::Tuple(
            std::borrow::Cow::from(vec![$($p),+]), 
            ()
        )
    };

    (($($p:expr),*) -> $r:expr) => {
        $crate::compiler::plir::Type::Fun(
            $crate::compiler::plir::FunType {
                params: std::borrow::Cow::from(vec![$($p),*]), 
                ret: std::borrow::Cow::Owned(Box::new($r)),
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