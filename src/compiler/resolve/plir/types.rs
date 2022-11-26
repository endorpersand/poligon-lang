use crate::compiler::resolve::plir::ExprType;
use crate::compiler::resolve::{PLIRResult, PLIRErr};
use crate::tree::{op, self};

use super::{Split, Expr};


#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Prim(String),
    Generic(String, Vec<Type>),
    Tuple(Vec<Type>)
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

    pub fn is_never(&self) -> bool {
        match self {
            Type::Prim(ty) => ty == Type::S_NEVER,
            _ => false,
        }
    }
    pub fn is_numeric(&self) -> bool {
        match self {
            Type::Prim(ty) => ty == Type::S_INT || ty == Type::S_FLOAT,
            _ => false
        }
    }
    #[inline]
    fn is_int(&self) -> bool {
        matches!(self, Type::Prim(ty) if ty == Type::S_INT)
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
    pub fn split(&self, sp: Split) -> PLIRResult<Type> {
        match self {
            Type::Prim(_) => Err(PLIRErr::CannotSplitType),
            Type::Generic(ident, params) => {
                if ident == Type::S_LIST {
                    if let Some(param) = params.first() {
                        match sp {
                            Split::Left(_)
                            | Split::Right(_) => Ok(param.clone()),
                            Split::Middle(_, _) => Ok(self.clone()),
                        }
                    } else {
                        panic!("list cannot be defined without parameters")
                    }
                } else {
                    todo!()
                }
            },
            Type::Tuple(tpl) => match sp {
                Split::Left(idx) => tpl.get(idx).cloned()
                    .ok_or_else(|| PLIRErr::InvalidSplit(self.clone(), sp)),
                Split::Middle(start, end) => {
                    let vec = tpl.get(start..(tpl.len() - end))
                        .ok_or_else(|| PLIRErr::InvalidSplit(self.clone(), sp))?
                        .to_vec();
                    
                    Ok(Type::Tuple(vec))
                },
                Split::Right(idx) => tpl.get(tpl.len() - idx).cloned()
                    .ok_or_else(|| PLIRErr::InvalidSplit(self.clone(), sp)),
            },
        }
    }

    pub fn resolve_unary_type(op: &op::Unary, ty: &Type) -> Option<Type> {
        match op {
            op::Unary::Plus   => ty.is_numeric().then(|| ty.clone()),
            op::Unary::Minus  => ty.is_numeric().then(|| ty.clone()),
            op::Unary::LogNot => Some(ty!(Type::S_BOOL)),
            op::Unary::BitNot => ty.is_int().then(|| ty.clone()),
        }
    }
    pub fn resolve_binary_type(op: &op::Binary, left: &Type, right: &Type) -> Option<Type> {
        todo!()
    }
    // pub fn resolve_cmp_type(_: &op::Cmp, left: Type, right: Type) -> Option<Type> {
    //     let comparable = (left.is_numeric() && right.is_numeric()) || (left == right);

    //     comparable.then(|| Type::bool())
    // }
    pub fn resolve_index_type(expr: &Type, idx: &Expr) -> Option<Type> {
        let idx_ty = &idx.ty;

        match expr {
            Type::Prim(expr_ty) => match expr_ty.as_ref() {
                Type::S_STR => idx_ty.is_int().then(|| ty!(Type::S_CHAR)),
                _ => None
            },
            Type::Generic(expr_ty, param_tys) => match expr_ty.as_ref() {
                Type::S_LIST => idx_ty.is_int().then(|| param_tys[0].clone()),
                Type::S_DICT => (idx_ty == &param_tys[0]).then(|| param_tys[1].clone()),
                _ => None
            },
            Type::Tuple(tys) => match &idx.expr {
                ExprType::Literal(literal) => match *literal {
                    tree::Literal::Int(idx_literal) => usize::try_from(idx_literal)
                        .ok()
                        .and_then(|idx| tys.get(idx))
                        .cloned(), // TODO: properly error handle
                    _ => None
                },
                _ => None
            },
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
