use std::borrow::Cow;
use std::collections::HashMap;

use crate::compiler::plir::{self, Type, FunIdent};


#[derive(PartialEq, Eq, Hash, Debug)]
pub(super) struct SigKey<'k> {
    ident: Cow<'k, str>,
    params: Cow<'k, [plir::Type]>
}

impl<'k> SigKey<'k> {
    fn new(ident: impl Into<Cow<'k, str>>, params: impl Into<Cow<'k, [plir::Type]>>) -> Self {
        Self { ident: ident.into(), params: params.into() }
    }
}

#[derive(Debug)]
pub(super) enum TypeStructure {
    Primitive,
    Class(plir::Class)
}

/// Value type that holds the data of a type.
/// 
/// This allows the PLIR codegen to replace methods with functions
/// and access type fields.
#[derive(Debug)]
pub(super) struct TypeData {
    ty_shape: Type,
    structure: TypeStructure,
    methods: TDExtMap
}
type TDExtMap = ExtMap<SigKey<'static>, FunIdent>;
#[derive(Debug)]
pub(super) enum ExtMap<K, V> {
    One(HashMap<K, V>),
    Many(HashMap<Box<[Type]>, HashMap<K, V>>)
}
impl<K, V> ExtMap<K, V> {
    fn one() -> Self {
        ExtMap::One(HashMap::new())
    }
    fn many() -> Self {
        ExtMap::Many(HashMap::new())
    }
}

impl <K: Eq + std::hash::Hash, V> ExtMap<K, V> {
    pub(super) fn iter(&self) -> ExtMapIter<K, V> {
        match self {
            ExtMap::One(m) => ExtMapIter::One(Some(m)),
            ExtMap::Many(mm) => ExtMapIter::Many(mm.iter()),
        }
    }
    pub(super) fn get_appl_maps<'a>(&'a self, k: &'a [Type]) -> MetMatchIter<K, V> {
        MetMatchIter(self.iter(), k)
    }
    pub(super) fn get_map_mut(&mut self, k: Box<[Type]>) -> &mut HashMap<K, V> {
        match self {
            ExtMap::One(m) => m,
            ExtMap::Many(mm) => mm.entry(k).or_default(),
        }
    }
}
pub(super) enum ExtMapIter<'a, K, V> {
    One(Option<&'a HashMap<K, V>>),
    Many(std::collections::hash_map::Iter<'a, Box<[Type]>, HashMap<K, V>>)
}
impl<'a, K, V> Iterator for ExtMapIter<'a, K, V> {
    type Item = (&'a [Type], &'a HashMap<K, V>);

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            ExtMapIter::One(it) => it.take().map(|v| (&[][..], v)),
            ExtMapIter::Many(it) => it.next().map(|(k, v)| (&k[..], v)),
        }
    }
}

pub(super) struct MetMatchIter<'a, K, V>(ExtMapIter<'a, K, V>, &'a [Type]);
fn type_align(target: &[Type], real: &[Type]) -> bool {
    use std::array::from_ref;

    if target.len() == real.len() {
        std::iter::zip(target, real)
            .all(|pair| {
                match pair {
                    (Type::Unk(u1), Type::Unk(u2)) => u1 == u2,
                    (Type::Unk(_), _) => todo!(),
                    (_, Type::Unk(_)) => todo!(),
                    (Type::Prim(p1), Type::Prim(p2)) => p1 == p2,
                    (Type::Generic(i1, p1, _), Type::Generic(i2, p2, _)) => i1 == i2 && type_align(p1, p2),
                    (Type::Tuple(p1, _), Type::Tuple(p2, _)) => type_align(p1, p2),
                    (Type::Fun(f1), Type::Fun(f2)) => {
                        type_align(&f1.params, &f2.params) 
                        && type_align(from_ref(&**f1.ret), from_ref(&**f2.ret))
                        && f1.varargs == f2.varargs
                    },
                    _ => false,
                }
            })
    } else {
        false
    }
}
impl<'a, K, V> Iterator for MetMatchIter<'a, K, V> {
    type Item = &'a HashMap<K, V>;

    fn next(&mut self) -> Option<Self::Item> {
        for (k, v) in self.0.by_ref() {
            if type_align(k, self.1) {
                return Some(v);
            }
        }

        None
    }
}
impl TypeData {
    fn method_skeleton(ty: &Type) -> TDExtMap {
        match ty {
            Type::Unk(_) => panic!("cannot resolve type shape of unknown"),
            Type::Prim(_) => ExtMap::one(),
            Type::Generic(_, _, _) => ExtMap::many(),
            Type::Tuple(_, _) => ExtMap::one(),
            Type::Fun(_) => ExtMap::one(),
        }
    }
    /// Create a primitive type (a type whose fields are defined in LLVM instead of Poligon)
    pub fn primitive(ty: Type) -> Self {
        let methods = Self::method_skeleton(&ty);

        Self {
            ty_shape: ty,
            structure: TypeStructure::Primitive,
            methods
        }
    }
    /// Create a structural type (a type whose fields are defined in Poligon)
    pub fn structural(cls: plir::Class) -> Self {
        let methods = Self::method_skeleton(&cls.ty);
        
        Self {
            ty_shape: cls.ty.clone(),
            structure: TypeStructure::Class(cls),
            methods
        }
    }
}