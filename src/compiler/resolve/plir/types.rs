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
    pub fn int() -> Self {
        Type::Prim(String::from("int"))
    }
    pub fn float() -> Self {
        Type::Prim(String::from("float"))
    }
    pub fn bool() -> Self {
        Type::Prim(String::from("bool"))
    }
    pub fn char() -> Self {
        Type::Prim(String::from("char"))
    }
    pub fn str() -> Self {
        Type::Prim(String::from("string"))
    }
    pub fn void() -> Self {
        Type::Prim(String::from("void"))
    }
    pub fn unk() -> Self {
        Type::Prim(String::from("unk"))
    }
    pub fn never() -> Self {
        Type::Prim(String::from("never"))
    }

    pub fn list(t: Type) -> Self {
        Type::Generic(String::from("list"), vec![t])
    }
    pub fn generic(ident: &str, params: Vec<Type>) -> Self {
        Type::Generic(String::from(ident), params)
    }

    pub fn is_never(&self) -> bool {
        match self {
            Type::Prim(ty) => ty == "never",
            _ => false,
        }
    }
    pub fn is_numeric(&self) -> bool {
        match self {
            Type::Prim(ty) => ty == "int" || ty == "float",
            _ => false
        }
    }
    #[inline]
    fn is_int(&self) -> bool {
        matches!(self, Type::Prim(ty) if ty == "int")
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
            Err(TypeRezError::NoBranches) => Some(Type::never()),
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
                if ident == "list" {
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
            op::Unary::LogNot => Some(Type::bool()),
            op::Unary::BitNot => match ty {
                Type::Prim(tyname) if tyname == "int" => Some(ty.clone()),
                _ => None
            },
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
                "string" => idx_ty.is_int().then(|| Type::char()),
                _ => None
            },
            Type::Generic(expr_ty, param_tys) => match expr_ty.as_ref() {
                "list" => idx_ty.is_int().then(|| param_tys[0].clone()),
                "dict" => (idx_ty == &param_tys[0]).then(|| param_tys[1].clone()),
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
