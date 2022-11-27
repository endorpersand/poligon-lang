use crate::tree::{op, self};

use super::{Split, Expr, ExprType};

#[derive(Debug)]
pub enum OpErr {
    CannotUnary(op::Unary, Type),
    CannotBinary(op::Binary, Type, Type),
    CannotCmp(op::Cmp, Type, Type),
    CannotIndex(Type),
    CannotIndexWith(Type, Type),
    
    TupleIndexNonLiteral(Type),
    TupleIndexOOB(Type, isize),
    InvalidSplit(Type, Split)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Prim(String),
    Generic(String, Vec<Type>),
    Tuple(Vec<Type>),
    Fun(Vec<Type>, Box<Type>)
}
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum TypeRef<'a> {
    Prim(&'a str),
    Generic(&'a str, &'a [Type]),
    Tuple(&'a [Type]),
    Fun(&'a [Type], &'a Type)
}
impl TypeRef<'_> {
    fn cloned(self) -> Type {
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

    fn referenced(&self) -> TypeRef {
        match self {
            Type::Prim(ident) => TypeRef::Prim(ident),
            Type::Generic(ident, params) => TypeRef::Generic(ident, params),
            Type::Tuple(params) => TypeRef::Tuple(params),
            Type::Fun(params, ret) => TypeRef::Fun(params, ret),
        }
    }

    pub fn is_never(&self) -> bool {
        matches!(self.referenced(), TypeRef::Prim(Type::S_NEVER))
    }
    pub fn is_numeric(&self) -> bool {
        matches!(self.referenced(), TypeRef::Prim(Type::S_INT) | TypeRef::Prim(Type::S_FLOAT))
    }
    #[inline]
    fn is_int(&self) -> bool {
        matches!(self.referenced(), TypeRef::Prim(Type::S_INT))
    }

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
    pub fn resolve_branches<'a>(into_it: impl IntoIterator<Item=&'a Type>) -> Option<Type> {
        match Type::resolve_type(into_it) {
            Ok(ty) => Some(ty),
            Err(TypeRezError::NoBranches) => Some(ty!(Type::S_NEVER)),
            Err(TypeRezError::MultipleBranches) => None,
        }
    }
    pub fn resolve_collection_ty<'a>(into_it: impl IntoIterator<Item=&'a Type>) -> Option<Type> {
        Type::resolve_type(into_it).ok()
    }

    // technically index but whatever
    pub fn split(&self, sp: Split) -> Result<Type, OpErr> {
        match self.referenced() {
            TypeRef::Prim(Type::S_STR) => match sp {
                Split::Left(_)
                | Split::Right(_) 
                => Ok(ty!(Type::S_CHAR)),
                Split::Middle(_, _) => Ok(self.clone()),
            },

            TypeRef::Generic(Type::S_LIST, params) => {
                if let Some(param) = params.first() {
                    match sp {
                        Split::Left(_)
                        | Split::Right(_) => Ok(param.clone()),
                        Split::Middle(_, _) => Ok(self.clone()),
                    }
                } else {
                    unreachable!("list cannot be defined without parameters")
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

    pub fn resolve_unary_type(op: &op::Unary, ty: &Type) -> Result<Type, OpErr> {
        match op {
            op::Unary::Plus   => ty.is_numeric().then(|| ty.clone()),
            op::Unary::Minus  => ty.is_numeric().then(|| ty.clone()),
            op::Unary::LogNot => Some(ty!(Type::S_BOOL)),
            op::Unary::BitNot => ty.is_int().then(|| ty.clone()),
        }.ok_or_else(|| OpErr::CannotUnary(*op, ty.clone()))
    }

    pub fn resolve_binary_type(op: &op::Binary, left: &Type, right: &Type) -> Result<Type, OpErr> {
        #[inline]
        fn bin_op(ty: TypeRef, left: &Type, right: &Type) -> Option<Type> {
            (left.referenced() == ty && right.referenced() == ty).then(|| ty.cloned())
        }

        macro_rules! numeric_op_else {
            ($left:expr, $right:expr, $(($a:pat, $b:pat) => $bl:expr),+) => {
                match (left.referenced(), right.referenced()) {
                    (TypeRef::Prim(Type::S_FLOAT), TypeRef::Prim(Type::S_FLOAT)) => Ok(ty!(Type::S_FLOAT)),
                    (TypeRef::Prim(Type::S_INT), TypeRef::Prim(Type::S_FLOAT))   => Ok(ty!(Type::S_FLOAT)),
                    (TypeRef::Prim(Type::S_FLOAT), TypeRef::Prim(Type::S_INT))   => Ok(ty!(Type::S_FLOAT)),
                    (TypeRef::Prim(Type::S_INT), TypeRef::Prim(Type::S_INT))     => Ok(ty!(Type::S_INT)),
                    $(($a, $b) => $bl),+
                }
            }
        }

        match op {
            op::Binary::Add 
            | op::Binary::Sub
            | op::Binary::Mul
            | op::Binary::Div
            | op::Binary::Mod
            => numeric_op_else!(
                left, right,
                (_, _) => Err(OpErr::CannotBinary(*op, left.clone(), right.clone()))
            ),

            op::Binary::Shl
            | op::Binary::Shr
            | op::Binary::BitAnd
            | op::Binary::BitOr
            | op::Binary::BitXor
            => bin_op(TypeRef::Prim(Type::S_INT), left, right)
                .ok_or_else(|| OpErr::CannotBinary(*op, left.clone(), right.clone())),

            // TODO: &&, || typing
            op::Binary::LogAnd => Ok(ty!(Type::S_BOOL)),
            op::Binary::LogOr  => Ok(ty!(Type::S_BOOL)),
        }
    }

    // pub fn resolve_cmp_type(_: &op::Cmp, left: Type, right: Type) -> Option<Type> {
    //     let comparable = (left.is_numeric() && right.is_numeric()) || (left == right);

    //     comparable.then(|| Type::bool())
    // }
    pub fn resolve_index_type(expr: &Type, idx: &Expr) -> Result<Type, OpErr> {
        let idx_ty = &idx.ty;

        match expr.referenced() {
            TypeRef::Prim(Type::S_STR) => match idx_ty.is_int() {
                true  => Ok(ty!(Type::S_CHAR)),
                false => Err(OpErr::CannotIndexWith(expr.clone(), idx_ty.clone())),
            }
            TypeRef::Generic(Type::S_LIST, params) => match idx_ty.is_int() {
                true  => Ok(params[0].clone()),
                false => Err(OpErr::CannotIndexWith(expr.clone(), idx_ty.clone())),
            }
            TypeRef::Generic(Type::S_DICT, params) => {
                let (key, val) = (&params[0], &params[1]);
                match key == idx_ty {
                    true  => Ok(val.clone()),
                    false => Err(OpErr::CannotIndexWith(expr.clone(), idx_ty.clone())),
                }
            }
            TypeRef::Tuple(tys) => match idx.expr {
                ExprType::Literal(tree::Literal::Int(idx_lit)) => {
                    usize::try_from(idx_lit)
                        .ok()
                        .and_then(|idx| tys.get(idx))
                        .ok_or_else(|| OpErr::TupleIndexOOB(expr.clone(), idx_lit))
                        .map(Clone::clone)
                },
                _ => match idx_ty.is_int() {
                    true  => Err(OpErr::TupleIndexNonLiteral(expr.clone())),
                    false => Err(OpErr::CannotIndexWith(expr.clone(), idx_ty.clone()))
                }
            }
            _ => Err(OpErr::CannotIndex(expr.clone()))
        }
    }
}

macro_rules! ty {
    ($e:expr) => {
        $crate::compiler::resolve::plir::Type::Prim(String::from($e))
    };

    ($e:expr, [$($p:expr),+]) => {
        $crate::compiler::resolve::plir::Type::Generic(String::from($e), vec![$($p),+])
    };

    ([$($p:expr),+]) => {
        $crate::compiler::resolve::plir::Type::Tuple(vec![$($p),+])
    };
}
pub(crate) use ty;

impl From<tree::Type> for Type {
    fn from(ty: tree::Type) -> Self {
        let tree::Type(ident, params) = ty;
        
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
