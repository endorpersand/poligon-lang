use crate::ast::{op, Literal};
use crate::compiler::plir::*;

use super::PLIRResult;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum CastType {
    All, Decl
}
/// Try to cast the expression to the given type, erroring if the cast fails.
pub fn apply_cast(e: Expr, ty: &Type) -> Result<Expr, Expr> {
    apply_special_cast(e, ty, CastType::All)
}

fn accept_cast(left: TypeRef, right: TypeRef, ct: CastType) -> bool {
    use CastType::*;
    use TypeRef::*;

    match (left, right) {
        (Prim(Type::S_INT),  Prim(Type::S_FLOAT)) => matches!(ct, All | Decl),
        (Prim(Type::S_CHAR), Prim(Type::S_STR))   => matches!(ct, All | Decl),
        (_, Prim(Type::S_BOOL)) => matches!(ct, All),
        (_, Prim(Type::S_VOID)) => matches!(ct, All | Decl),
        _ => false
    }
}

pub fn apply_special_cast(e: Expr, ty: &Type, ct: CastType) -> Result<Expr, Expr> {
    let left = e.ty.as_ref();
    let right = ty.as_ref();

    if left == right { return Ok(e); }

    match accept_cast(left, right, ct) {
        true => Ok(Expr {
            ty: ty.clone(), expr: ExprType::Cast(Box::new(e))
        }),
        false => Err(e),
    }
}

fn uncast(e: Expr) -> Expr {
    if let Expr {expr: ExprType::Cast(inner), ..} = e {
        *inner
    } else {
        e
    }
}

/// Try the function on each of the function until an Ok result is obtained.
fn chain<T>(e: T, tys: &[Type], mut f: impl FnMut(T, &Type) -> Result<T, T>) -> Result<T, T> {
    let mut uncast = e;
    for ty in tys {
        match f(uncast, ty) {
            Ok(t) => return Ok(t),
            Err(e) => uncast = e,
        }
    }

    Err(uncast)
}

fn apply_cast2((left, right): (Expr, Expr), t: &Type) -> Result<(Expr, Expr), (Expr, Expr)> {
    match apply_cast(left, t) {
        Ok(l) => match apply_cast(right, t) {
            Ok(r) => Ok((l, r)),
            Err(r) => Err((uncast(l), uncast(r))),
        }
        Err(l) => Err((uncast(l), right)),
    }
}

trait ResultFlatten {
    type Inner;
    fn flatten(self) -> Self::Inner;
}

impl<T> ResultFlatten for Result<T, T> {
    type Inner = T;

    fn flatten(self) -> Self::Inner {
        match self {
            Ok(t) => t,
            Err(t) => t,
        }
    }
}

pub fn apply_unary(e: Expr, op: op::Unary) -> PLIRResult<Expr> {
    // Check for any valid casts that can be applied here:
    let cast = match op {
        op::Unary::Plus => {
            chain(e, &[ty!(Type::S_INT), ty!(Type::S_FLOAT)], apply_cast)
                .map_err(|e| OpErr::CannotUnary(op, e.ty))?
        },
        op::Unary::Minus => {
            chain(e, &[ty!(Type::S_INT), ty!(Type::S_FLOAT)], apply_cast)
                .map_err(|e| OpErr::CannotUnary(op, e.ty))?
        },
        op::Unary::LogNot => {
            apply_cast(e, &ty!(Type::S_BOOL))
                .map_err(|e| OpErr::CannotUnary(op, e.ty))?
        },
        op::Unary::BitNot => e,
    };

    // Type check and compute resulting expr type:
    let ty = {
        let ty = cast.ty.clone();

        match (op, ty.as_ref()) {
            (op::Unary::Plus, TypeRef::Prim(Type::S_INT) | TypeRef::Prim(Type::S_FLOAT)) => ty,
            (op::Unary::Minus, TypeRef::Prim(Type::S_INT) | TypeRef::Prim(Type::S_FLOAT)) => ty,
            (op::Unary::LogNot, TypeRef::Prim(Type::S_BOOL)) => ty,
            (op::Unary::BitNot, TypeRef::Prim(Type::S_INT)) => ty,
            _ => Err(OpErr::CannotUnary(op, ty))?
        }
    };

    // Construct expression:
    let expr = match cast.expr {
        ExprType::UnaryOps { mut ops, expr } => {
            ops.insert(0, (op, cast.ty)); // This bothers me immensely.
            ExprType::UnaryOps { ops, expr }
        }
        e => ExprType::UnaryOps { 
            ops: vec![(op, ty.clone())], 
            expr: Box::new(Expr { ty: cast.ty, expr: e }) 
        }
    };
    Ok(Expr { ty, expr })
}

pub fn apply_binary(op: op::Binary, left: Expr, right: Expr) -> PLIRResult<Expr> {
    // Check for any valid casts that can be applied here:
    let (lcast, rcast) = match op {
        op::Binary::Add => {
            let types = &[
                ty!(Type::S_STR),
                // list cast ?
                ty!(Type::S_INT),
                ty!(Type::S_FLOAT)
            ][..];
            chain((left, right), types, apply_cast2)
                .map_err(|(l, r)| OpErr::CannotBinary(op, l.ty, r.ty))?
        },
        op::Binary::Sub => {
            chain((left, right), &[ty!(Type::S_INT), ty!(Type::S_FLOAT)][..], apply_cast2)
                .map_err(|(l, r)| OpErr::CannotBinary(op, l.ty, r.ty))?
        },
        op::Binary::Mul => {
            chain((left, right), &[ty!(Type::S_INT), ty!(Type::S_FLOAT)][..], apply_cast2)
                .map_err(|(l, r)| OpErr::CannotBinary(op, l.ty, r.ty))?
        },
        op::Binary::Div => {
            chain((left, right), &[ty!(Type::S_INT), ty!(Type::S_FLOAT)][..], apply_cast2)
                .map_err(|(l, r)| OpErr::CannotBinary(op, l.ty, r.ty))?
        },
        op::Binary::Mod => {
            chain((left, right), &[ty!(Type::S_INT), ty!(Type::S_FLOAT)][..], apply_cast2)
                .map_err(|(l, r)| OpErr::CannotBinary(op, l.ty, r.ty))?
        },
        op::Binary::Shl    => (left, right),
        op::Binary::Shr    => (left, right),
        op::Binary::BitOr  => (left, right),
        op::Binary::BitAnd => (left, right),
        op::Binary::BitXor => (left, right),
        op::Binary::LogAnd => (left, right),
        op::Binary::LogOr  => (left, right),
    };

    // Type check and compute resulting expr type:
    let ty = {
        let left = lcast.ty.clone();
        let right = &rcast.ty;
        match (op, left.as_ref(), right.as_ref()) {
            // numeric operators:
            (op::Binary::Add, l @ TypeRef::Prim(Type::S_INT | Type::S_FLOAT), r) if l == r => left,
            (op::Binary::Sub, l @ TypeRef::Prim(Type::S_INT | Type::S_FLOAT), r) if l == r => left,
            (op::Binary::Mul, l @ TypeRef::Prim(Type::S_INT | Type::S_FLOAT), r) if l == r => left,
            (op::Binary::Div, l @ TypeRef::Prim(Type::S_INT | Type::S_FLOAT), r) if l == r => left,
            (op::Binary::Mod, l @ TypeRef::Prim(Type::S_INT | Type::S_FLOAT), r) if l == r => left,
            // collections:
            (op::Binary::Add, l @ (TypeRef::Prim(Type::S_STR) | TypeRef::Generic(Type::S_LIST, _)), r) if l == r => left,
            // bitwise operators:
            (op::Binary::Shl, TypeRef::Prim(Type::S_INT), TypeRef::Prim(Type::S_INT)) => left,
            (op::Binary::Shr, TypeRef::Prim(Type::S_INT), TypeRef::Prim(Type::S_INT)) => left,
            (op::Binary::BitOr,  l @ TypeRef::Prim(Type::S_INT | Type::S_BOOL), r) if l == r => left,
            (op::Binary::BitAnd, l @ TypeRef::Prim(Type::S_INT | Type::S_BOOL), r) if l == r => left,
            (op::Binary::BitXor, l @ TypeRef::Prim(Type::S_INT | Type::S_BOOL), r) if l == r => left,
            (op::Binary::LogAnd, l, r) if l == r => left,
            (op::Binary::LogOr, l, r) if l == r => left,
            _ => Err(OpErr::CannotBinary(op, left, right.clone()))?
        }
    };

    // Construct expression:
    Ok(Expr { ty, expr: ExprType::BinaryOp { op, left: Box::new(lcast), right: Box::new(rcast) }})
}

pub fn apply_index(left: Expr, index: Expr) -> PLIRResult<(Type, Index)> {
    // Check for any valid casts that can be applied here:
    let lcast = apply_cast(left, &ty!(Type::S_STR)).flatten();

    let icast = match lcast.ty.as_ref() {
        | TypeRef::Prim(Type::S_STR)
        | TypeRef::Generic(Type::S_LIST, _)
        | TypeRef::Tuple(_)
        => apply_cast(index, &ty!(Type::S_INT)).flatten(),
        
        TypeRef::Generic(Type::S_DICT, [k, _]) => apply_cast(index, k).flatten(),
        
        _ => return Err(OpErr::CannotIndex(lcast.ty).into())
    };

    // Type check and compute resulting expr type:
    let ty = {
        let left = lcast.ty.clone();
        let index = &icast.ty;
        match (left.as_ref(), index.as_ref()) {
            (TypeRef::Prim(Type::S_STR), TypeRef::Prim(Type::S_INT)) => ty!(Type::S_CHAR),
            (TypeRef::Generic(Type::S_LIST, [t]), TypeRef::Prim(Type::S_INT)) => t.clone(),
            (TypeRef::Generic(Type::S_DICT, [k, v]), idx) if idx == k => v.clone(),
            (TypeRef::Tuple(tys), TypeRef::Prim(Type::S_INT)) => {
                let Expr { expr: ExprType::Literal(Literal::Int(lit)), ..} = icast else {
                    return Err(OpErr::TupleIndexNonLiteral(left).into());
                };
                let Ok(idx) = usize::try_from(lit) else {
                    return Err(OpErr::TupleIndexOOB(left, lit).into());
                };

                tys.get(idx).cloned().ok_or_else(|| OpErr::TupleIndexOOB(left, lit))?
            },
            _ => Err(OpErr::CannotIndexWith(left, index.clone()))?
        }
    };

    // Construct expression:
    Ok((ty, Index { expr: Box::new(lcast), index: Box::new(icast) }))
}