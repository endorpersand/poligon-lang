use std::borrow::Cow;

use crate::ast::{op, LitKind};
use crate::compiler::plir::*;
use crate::err::GonErr;
use crate::span::Span;

use super::PLIRResult;

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
}
impl std::fmt::Display for OpErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CannotUnary(op, t1)      => write!(f, "cannot apply '{op}' to {t1}"),
            Self::CannotBinary(op, t1, t2) => write!(f, "cannot apply '{op}' to {t1} and {t2}"),
            Self::CannotCmp(op, t1, t2)    => write!(f, "cannot compare '{op}' between {t1} and {t2}"),
            Self::CannotIndex(t1)          => write!(f, "cannot index {t1}"),
            Self::CannotIndexWith(t1, t2)  => write!(f, "cannot index {t1} with {t2}"),
            Self::TupleIndexNonLiteral(t)  => write!(f, "cannot index type '{t}' with a non-literal"),
            Self::TupleIndexOOB(t, i)      => write!(f, "index out of bounds: {t}[{i}]"),
            Self::InvalidSplit(t, s) => {
                write!(f, "cannot index: {t}~[")?;
                match s {
                    Split::Left(l) => write!(f, "{l}"),
                    Split::Middle(l, r) => write!(f, "{l}..-{r}"),
                    Split::Right(r) => write!(f, "{r}")
                }?;
                write!(f, "]")
            },
        }
    }
}
impl std::error::Error for OpErr {}

type HomoResult<T> = Result<T, T>;
struct Cast<'a> {
    src: Located<Expr>,
    dest: &'a Type,
    cf: CastFlags
}

impl<'a> Cast<'a> {
    fn can_cast(&self, cg: &mut super::PLIRCodegen) -> PLIRResult<bool> {
        use TypeRef::*;
        use Cow::Borrowed;

        let result = match (self.src.ty.downgrade(), self.dest.downgrade()) {
            (l, r) if l == r => true,
            (Prim(Borrowed(Type::S_NEVER)), _) => true,
            (_, Prim(Borrowed(Type::S_STR))) if self.cf.allows(CastFlags::Stringify) => {
                // Load src class
                let src_key = cg.get_class_key(self.src.as_ref().map(|e| &e.ty))?;
                // Check if it has to_string method (with correct signature)

                cg.get_method(&src_key, "to_string")?.is_some_and(|(_, met_ty)| {
                    matches!(met_ty, Fun(FunTypeRef {
                        params: Cow::Borrowed([p1]), 
                        ret, 
                        varargs: false
                    }) if p1 == &self.src.ty && &**ret == self.dest)
                })
            },
            (_, Prim(Borrowed(Type::S_BOOL))) => self.cf.allows(CastFlags::Truth),
            (_, Prim(Borrowed(Type::S_VOID))) => self.cf.allows(CastFlags::Void),
            (_, _) if self.src.ty.is_numeric() && self.dest.is_numeric() => {
                let l = NumType::new(&self.src.ty).unwrap();
                let r = NumType::new(self.dest).unwrap();

                (l < r && self.cf.allows(CastFlags::NumWiden))
                || (l > r && self.cf.allows(CastFlags::NumNarrow))
            }
            _ => false
        };

        Ok(result)
    }

    fn apply_cast(self, cg: &mut super::PLIRCodegen) -> PLIRResult<HomoResult<Located<Expr>>> {
        fn basic_cast(src: Located<Expr>, dest: &Type) -> Located<Expr> {
            src.map(|e| Expr {
                ty: dest.clone(), expr: ExprType::Cast(Box::new(e))
            })
        }

        let result = if self.can_cast(cg)? {
            // all casts are basic casts except str casts
            let result = match (self.src.ty.downgrade(), self.dest.downgrade()) {
                (l, r) if l == r => self.src,
                (_, TypeRef::Prim(s)) if s == Type::S_STR => {
                    let Located(src, src_range) = self.src;
                    
                    let src_key = cg.get_class_key(Located::new(&src.ty, src_range))?;
                    let (to_str, to_str_ty) = cg.get_method_or_err(&src_key, "to_string", src_range)?;
                    let to_str_expr = to_str.into_expr(to_str_ty);

                    Located::new(Expr::call(
                        Located::new(to_str_expr, src_range), 
                        vec![src]
                    )?, src_range)
                },
                _ => basic_cast(self.src, self.dest)
            };

            Ok(result)
        } else {
            Err(self.src)
        };

        Ok(result)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct CastFlags(u8);

#[allow(non_upper_case_globals)]
impl CastFlags {
    /// A cast which casts numeric types to be cast into wider types losslessly (e.g. int -> float)
    pub const NumWiden:  CastFlags = CastFlags(1 << 0);
    /// A cast which casts numeric types to be cast into narrower types lossily (e.g. float -> int)
    pub const NumNarrow: CastFlags = CastFlags(1 << 1);
    /// A cast which casts any type to bool
    pub const Truth:     CastFlags = CastFlags(1 << 2);
    /// A cast which casts any stringifiable type to string
    pub const Stringify: CastFlags = CastFlags(1 << 3);
    /// A cast which casts any type to void
    pub const Void:      CastFlags = CastFlags(1 << 4);
    
    /// Accept all implicit casts. 
    /// These casts are lossless and therefore will not cause problems if implicitly occurring.
    pub const Implicit:  CastFlags = CastFlags(CastFlags::NumWiden.0);
    /// Casts that can occur in slots where a type is known (e.g. declarations, function declarations, call expressions).
    /// Like Implicit, but allows Stringify.
    pub const Decl:      CastFlags = CastFlags(CastFlags::NumWiden.0 | CastFlags::Stringify.0);

    pub fn allows(self, sub: CastFlags) -> bool {
        self.0 & sub.0 == sub.0
    }
}
impl std::ops::BitOr for CastFlags {
    type Output = CastFlags;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

trait ResultIntoInner {
    type Inner;
    fn into_inner(self) -> Self::Inner;
}

impl<T> ResultIntoInner for HomoResult<T> {
    type Inner = T;

    fn into_inner(self) -> Self::Inner {
        match self {
            Ok(t) => t,
            Err(t) => t,
        }
    }
}

impl super::PLIRCodegen {
    pub(super) fn apply_cast(&mut self, src: Located<Expr>, dest: &Type, cf: CastFlags) -> PLIRResult<HomoResult<Located<Expr>>> {
        Cast { src, dest, cf }.apply_cast(self)
    }
    fn apply_cast2(&mut self, (left, right): (Located<Expr>, Located<Expr>), dest: &Type, cf: CastFlags) -> PLIRResult<HomoResult<(Located<Expr>, Located<Expr>)>> {
        let cast1 = Cast { src: left, dest, cf };
        let cast2 = Cast { src: right, dest, cf };

        let result = if cast1.can_cast(self)? && cast2.can_cast(self)? {
            Ok(cast1.apply_cast(self)?.ok().zip(cast2.apply_cast(self)?.ok()).unwrap())
        } else {
            Err((cast1.src, cast2.src))
        };

        Ok(result)
    }

    /// Try casting the expression to one of the given types until successful.
    fn cast_chain<'a, I>(&mut self, src: Located<Expr>, tys: I, cf: CastFlags) -> PLIRResult<HomoResult<Located<Expr>>> 
        where I: IntoIterator<Item=&'a Type>
    {
        let mut cast = Cast { src, dest: &ty!(Type::S_NEVER), cf };
        for dest in tys {
            cast.dest = dest;

            if cast.can_cast(self)? {
                return cast.apply_cast(self);
            }
        }
        
        Ok(Err(cast.src))
    }

    /// Try casting the expression to one of the given types until successful.
    fn cast_chain2<'a, I>(&mut self, (left, right): (Located<Expr>, Located<Expr>), tys: I, cf: CastFlags) -> PLIRResult<HomoResult<(Located<Expr>, Located<Expr>)>> 
        where I: IntoIterator<Item=&'a Type>
    {
        let mut cast1 = Cast { src: left,  dest: &ty!(Type::S_NEVER), cf };
        let mut cast2 = Cast { src: right, dest: &ty!(Type::S_NEVER), cf };

        for dest in tys {
            cast1.dest = dest;
            cast2.dest = dest;

            if cast1.can_cast(self)? && cast2.can_cast(self)? {
                let dest1 = cast1.apply_cast(self)?.unwrap();
                let dest2 = cast2.apply_cast(self)?.unwrap();

                return Ok(Ok((dest1, dest2)))
            }
        }
        
        Ok(Err((cast1.src, cast2.src)))
    }

    /// Checks if this unary operator exists as a method.
    fn find_unary_method(&mut self, op: op::Unary, left: Located<&Type>) -> PLIRResult<Option<Expr>> {
        let method_name = match op {
            op::Unary::Plus   => "plus",
            op::Unary::Minus  => "minus",
            op::Unary::LogNot => return Ok(None), // ! is implemented in terms of truth
            op::Unary::BitNot => "bitnot",
        };

        let left_key = self.get_class_key(left)?;
        let e = self.get_method(&left_key, method_name)?
            .map(|(fun_id, fun_ty)| fun_id.into_expr(fun_ty));
        
        Ok(e)
    }
    
    pub(super) fn apply_unary(&mut self, e: Located<Expr>, op: op::Unary, unary_range: Span) -> PLIRResult<Located<Expr>> {
        // Check for any valid casts that can be applied here:
        let Located(cast, left_range) = match op {
            op::Unary::Plus => {
                self.cast_chain(e, NumType::order(), CastFlags::Implicit)?.into_inner()
            },
            op::Unary::Minus => {
                self.cast_chain(e, NumType::order(), CastFlags::Implicit)?.into_inner()
            },
            op::Unary::LogNot => {
                self.apply_cast(e, &ty!(Type::S_BOOL), CastFlags::Truth)?.into_inner()
            },
            op::Unary::BitNot => {
                self.cast_chain(e, NumType::int_order(), CastFlags::Implicit)?.into_inner()
            },
        };
    
        // Type check and compute resulting expr type:
        let ty = {
            let ty = cast.ty.clone();
    
            match (op, ty.downgrade()) {
                (op::Unary::Plus,  _) if ty.is_numeric() => ty,
                (op::Unary::Minus, _) if ty.is_numeric() => ty,
                (op::Unary::LogNot, TypeRef::Prim(Cow::Borrowed(Type::S_BOOL))) => ty,
                (op::Unary::BitNot, TypeRef::Prim(Cow::Borrowed(Type::S_BOOL))) => ty,
                (op::Unary::BitNot, _) if ty.is_int_like() => ty,
                (op, _) => {
                    let fun = self.find_unary_method(op, Located::new(&ty, left_range))?
                        .ok_or_else(|| {
                            OpErr::CannotUnary(op, ty).at_range(unary_range)
                        })?;
                    
                    return Expr::call(Located::new(fun, unary_range), vec![cast])
                        .map(|e| Located::new(e, unary_range));
                }
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
        Ok(Located::new(Expr { ty, expr }, unary_range))
    }

    /// Checks if this unary operator exists as a method.
    fn find_binary_method(&mut self, op: op::Binary, left: Located<&Type>, right: &Type) -> PLIRResult<Option<Expr>> {
        let method_name = match op {
            op::Binary::Add => "add",
            op::Binary::Sub => "sub",
            op::Binary::Mul => "mul",
            op::Binary::Div => "div",
            op::Binary::Mod => "mod",
            op::Binary::Shl => "shl",
            op::Binary::Shr => "shr",
            op::Binary::BitOr => "bitor",
            op::Binary::BitAnd => "bitand",
            op::Binary::BitXor => "bitxor",
            op::Binary::LogAnd => return Ok(None),
            op::Binary::LogOr  => return Ok(None),
        };

        let left_key = self.get_class_key(left)?;
        let e = self.get_method(&left_key, &format!("{method_name}_{right}"))?
            .map(|(fun_id, fun_ty)| fun_id.into_expr(fun_ty));
        
        Ok(e)
    }

    pub(super) fn apply_binary(
        &mut self, 
        op: op::Binary, 
        left: Located<Expr>, 
        right: Located<Expr>, 
        expr_range: Span
    ) -> PLIRResult<Expr> {
        use TypeRef::*;
        use Cow::Borrowed;

        // Check for any valid casts that can be applied here:
        let (lcast, rcast) = match op {
            op::Binary::Add => {
                match self.cast_chain2((left, right), NumType::order(), CastFlags::Implicit)? {
                    Ok(exprs) => exprs,
                    Err((l, r)) => {
                        let cf = CastFlags::Implicit | CastFlags::Stringify;
                        match (l.ty.downgrade(), r.ty.downgrade()) {
                            (Prim(Borrowed(Type::S_STR | Type::S_CHAR)), _) => {
                                self.apply_cast2((l, r), &ty!(Type::S_STR), cf)?.into_inner()
                            },
                            (_, Prim(Borrowed(Type::S_STR | Type::S_CHAR))) => {
                                self.apply_cast2((l, r), &ty!(Type::S_STR), cf)?.into_inner()
                            },
                            _ => (l, r)
                        }
                    },
                }
            },
            op::Binary::Sub => {
                self.cast_chain2(
                    (left, right), 
                    NumType::order(), 
                    CastFlags::Implicit
                )?.into_inner()
            },
            op::Binary::Mul => {
                self.cast_chain2(
                    (left, right), 
                    NumType::order(), 
                    CastFlags::Implicit
                )?.into_inner()
            },
            op::Binary::Div => {
                self.cast_chain2(
                    (left, right), 
                    NumType::float_order(), 
                    CastFlags::Implicit
                )?.into_inner()
            },
            op::Binary::Mod => {
                self.cast_chain2(
                    (left, right), 
                    NumType::order(), 
                    CastFlags::Implicit
                )?.into_inner()
            },
            op::Binary::Shl => {
                self.cast_chain2(
                    (left, right), 
                    NumType::int_order(), 
                    CastFlags::Implicit
                )?.into_inner()
            },
            op::Binary::Shr => {
                self.cast_chain2(
                    (left, right), 
                    NumType::int_order(), 
                    CastFlags::Implicit
                )?.into_inner()
            },
            op::Binary::BitOr  => {
                let bool_ = ty!(Type::S_BOOL);
                let mut bittypes = vec![&bool_];
                bittypes.extend(NumType::int_order());

                self.cast_chain2(
                    (left, right), 
                    bittypes, 
                    CastFlags::Implicit
                )?.into_inner()
            },
            op::Binary::BitAnd => {
                let bool_ = ty!(Type::S_BOOL);
                let mut bittypes = vec![&bool_];
                bittypes.extend(NumType::int_order());

                self.cast_chain2(
                    (left, right), 
                    bittypes, 
                    CastFlags::Implicit
                )?.into_inner()
            },
            op::Binary::BitXor => {
                let bool_ = ty!(Type::S_BOOL);
                let mut bittypes = vec![&bool_];
                bittypes.extend(NumType::int_order());

                self.cast_chain2(
                    (left, right), 
                    bittypes, 
                    CastFlags::Implicit
                )?.into_inner()
            },
            op::Binary::LogAnd => (left, right),
            op::Binary::LogOr  => (left, right),
        };
    
        // Type check and compute resulting expr type:
        let ty = {
            let left = lcast.ty.clone();
            let right = &rcast.ty;
            match (op, left.downgrade(), right.downgrade()) {
                // numeric operators:
                (op::Binary::Add, l, r) if left.is_numeric() && l == r => left,
                (op::Binary::Sub, l, r) if left.is_numeric() && l == r => left,
                (op::Binary::Mul, l, r) if left.is_numeric() && l == r => left,
                (op::Binary::Div, l, r) if left.is_float_like() && l == r => left,
                (op::Binary::Mod, l, r) if left.is_numeric() && l == r => left,
                // bitwise operators:
                (op::Binary::Shl, l, r) if left.is_int_like() && l == r => left,
                (op::Binary::Shr, l, r) if left.is_int_like() && l == r => left,
                (op::Binary::BitOr,  Prim(Borrowed(Type::S_BOOL)), Prim(Borrowed(Type::S_BOOL))) => left,
                (op::Binary::BitAnd, Prim(Borrowed(Type::S_BOOL)), Prim(Borrowed(Type::S_BOOL))) => left,
                (op::Binary::BitXor, Prim(Borrowed(Type::S_BOOL)), Prim(Borrowed(Type::S_BOOL))) => left,
                (op::Binary::BitOr,  l, r) if left.is_int_like() && l == r => left,
                (op::Binary::BitAnd, l, r) if left.is_int_like() && l == r => left,
                (op::Binary::BitXor, l, r) if left.is_int_like() && l == r => left,
                // logical operators:
                (op::Binary::LogAnd, l, r) if l == r => left,
                (op::Binary::LogOr, l, r)  if l == r => left,
                (op, _, _) => {
                    let fun = self.find_binary_method(op, Located::new(&left, lcast.1), right)?
                        .ok_or_else(|| {
                            OpErr::CannotBinary(op, left, right.clone())
                                .at_range(expr_range)
                        })?;

                    return Expr::call(Located::new(fun, expr_range), vec![lcast.0, rcast.0]);
                }
            }
        };
    
        // Construct expression:
        Ok(Expr { ty, expr: ExprType::BinaryOp { op, left: Box::new(lcast.0), right: Box::new(rcast.0) }})
    }

    pub(super) fn apply_index(&mut self, left: Located<Expr>, index: Located<Expr>, expr_range: Span) -> PLIRResult<(Type, Index)> {
        use TypeRef::*;
        use Cow::Borrowed;

        // Check for any valid casts that can be applied here:
        let lcast = self.apply_cast(left, &ty!(Type::S_STR), CastFlags::Implicit)?.into_inner();
    
        let icast = match lcast.ty.downgrade() {
            | Prim(Borrowed(Type::S_STR))
            | Generic(Borrowed(Type::S_LIST), _, ())
            | Tuple(_, ())
            => self.apply_cast(index, &ty!(Type::S_INT), CastFlags::Implicit)?.into_inner(),
            
            Generic(Borrowed(Type::S_DICT), Borrowed([k, _]), ()) => {
                self.apply_cast(index, k, CastFlags::Implicit)?.into_inner()
            },
            
            _ => return Err(OpErr::CannotIndex(lcast.0.ty).at_range(lcast.1).into())
        }.0;
    
        // Type check and compute resulting expr type:
        let ty = {
            let left = lcast.ty.clone();
            let index = &icast.ty;
            match (left.downgrade(), index.downgrade()) {
                (Prim(Borrowed(Type::S_STR)), Prim(Borrowed(Type::S_INT))) => ty!(Type::S_CHAR),
                (Generic(Borrowed(Type::S_LIST), Borrowed([t]), ()), Prim(Borrowed(Type::S_INT))) => t.clone(),
                (Generic(Borrowed(Type::S_DICT), Borrowed([k, v]), ()), idx) if &idx == k => v.clone(),
                (Tuple(tys, ()), Prim(Borrowed(Type::S_INT))) => {
                    let Expr { expr: ExprType::Literal(LitKind::Int(lit)), ..} = icast else {
                        let err = OpErr::TupleIndexNonLiteral(left).at_range(expr_range);
                        return Err(err.into());
                    };
                    let Ok(idx) = usize::try_from(lit) else {
                        let err = OpErr::TupleIndexOOB(left, lit).at_range(expr_range);
                        return Err(err.into());
                    };
    
                    tys.get(idx).cloned().ok_or_else(|| OpErr::TupleIndexOOB(left, lit).at_range(expr_range))?
                },
                _ => Err(OpErr::CannotIndexWith(left, index.clone()).at_range(expr_range))?
            }
        };
    
        // Construct expression:
        Ok((ty, Index { expr: Box::new(lcast.0), index: Box::new(icast) }))
    }
}

