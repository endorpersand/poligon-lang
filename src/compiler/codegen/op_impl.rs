use crate::ast::op;
use crate::compiler::plir::*;

use super::PLIRResult;

/// Try to cast the expression to the given type, erroring if the cast fails.
pub fn apply_cast(e: Expr, ty: &Type) -> Result<Expr, Expr> {
    let left = e.ty.as_ref();
    let right = ty.as_ref();

    if left == right { return Ok(e); }
    
    let accepted = matches!((left, right),
        | (TypeRef::Prim(Type::S_INT), TypeRef::Prim(Type::S_FLOAT)) 
        | (TypeRef::Prim(Type::S_CHAR), TypeRef::Prim(Type::S_STR)) 
        | (_, TypeRef::Prim(Type::S_BOOL)) 
        | (_, TypeRef::Prim(Type::S_VOID))
    );
    match accepted {
        true => Ok(Expr {
            ty: ty.clone(), expr: ExprType::Cast(Box::new(e))
        }),
        false => Err(e),
    }
}

/// Try to cast the expression to one of the given types (in the order provided), erroring if all casts fail.
pub fn apply_cast_chain(e: Expr, tys: &[Type]) -> Result<Expr, Expr> {
    let mut uncast = e;
    for ty in tys {
        match apply_cast(uncast, ty) {
            Ok(t) => return Ok(t),
            Err(e) => uncast = e,
        }
    }

    Err(uncast)
}

fn uncast(e: Expr) -> Expr {
    if let Expr {expr: ExprType::Cast(inner), ..} = e {
        *inner
    } else {
        e
    }
}

fn try2<T: Copy>(left: Expr, right: Expr, t: T, mut f: impl FnMut(Expr, T) -> Result<Expr, Expr>) -> Result<(Expr, Expr), (Expr, Expr)> {
    match f(left, t) {
        Ok(l) => match f(right, t) {
            Ok(r) => Ok((l, r)),
            Err(r) => Err((uncast(l), uncast(r))),
        }
        Err(l) => Err((uncast(l), right)),
    }
}

pub fn apply_unary(e: Expr, op: op::Unary) -> PLIRResult<Expr> {
    // Check for any valid casts that can be applied here:
    let cast = match op {
        op::Unary::Plus => {
            apply_cast_chain(e, &[ty!(Type::S_INT), ty!(Type::S_FLOAT)])
                .map_err(|e| OpErr::CannotUnary(op, e.ty))?
        },
        op::Unary::Minus => {
            apply_cast_chain(e, &[ty!(Type::S_INT), ty!(Type::S_FLOAT)])
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
            try2(left, right, types, apply_cast_chain)
                .map_err(|(l, r)| OpErr::CannotBinary(op, l.ty, r.ty))?
        },
        op::Binary::Sub => {
            try2(left, right, &[ty!(Type::S_INT), ty!(Type::S_FLOAT)][..], apply_cast_chain)
                .map_err(|(l, r)| OpErr::CannotBinary(op, l.ty, r.ty))?
        },
        op::Binary::Mul => {
            try2(left, right, &[ty!(Type::S_INT), ty!(Type::S_FLOAT)][..], apply_cast_chain)
                .map_err(|(l, r)| OpErr::CannotBinary(op, l.ty, r.ty))?
        },
        op::Binary::Div => {
            try2(left, right, &[ty!(Type::S_INT), ty!(Type::S_FLOAT)][..], apply_cast_chain)
                .map_err(|(l, r)| OpErr::CannotBinary(op, l.ty, r.ty))?
        },
        op::Binary::Mod => {
            try2(left, right, &[ty!(Type::S_INT), ty!(Type::S_FLOAT)][..], apply_cast_chain)
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