use crate::ast::{op, Literal};
use crate::compiler::plir::*;
use crate::err::{GonErr, CursorRange};

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

type HomoResult<T> = Result<T, T>;
struct Cast<'a> {
    src: Located<Expr>,
    dest: &'a Type,
    ct: CastType
}
impl<'a> Cast<'a> {
    fn can_cast(&self, cg: &mut super::PLIRCodegen) -> PLIRResult<bool> {
        use CastType::*;
        use TypeRef::*;

        let result = match (self.src.ty.as_ref(), self.dest.as_ref(), self.ct) {
            (l, r, _) if l == r => true,
            (Prim(Type::S_INT),  Prim(Type::S_FLOAT), Any | Decl | FunDecl | Call) => true,
            (_, Prim(Type::S_STR), Any | Decl | FunDecl | Call) => {
                // Load src class
                let cls = cg.get_class(Located::new(&self.src.ty, self.src.1.clone()))?;
                // Check if it has to_string method (with correct signature)
                if let Some(met_ident) = cls.get_method("to_string") {
                    let met_ident = met_ident.to_string();
                    
                    cg.get_var_type_opt(&met_ident)?
                        .filter(|&t| match t.as_ref() {
                            Fun([p1], ret, false) => p1 == &self.src.ty && ret == self.dest,
                            _ => false
                        })
                        .is_some()
                } else {
                    false
                }
            },
            (_, Prim(Type::S_BOOL), Any) => true,
            (_, Prim(Type::S_VOID), Any | FunDecl) => true,
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
            let result = match (self.src.ty.as_ref(), self.dest.as_ref()) {
                (l, r) if l == r => self.src,
                (_, TypeRef::Prim(s)) if s == Type::S_STR => {
                    let Located(src, src_range) = self.src;
                    let to_string = cg.get_class(Located::new(&src.ty, src_range.clone()))?
                        .get_method("to_string")
                        .unwrap()
                        .to_string();
                    
                    let fn_type = cg.get_var_type(&to_string, src_range.clone())?
                        .clone();
                    
                    Located::new(Expr::call(
                        Located::new(Expr::new(fn_type, ExprType::Ident(to_string)), src_range.clone()), 
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
pub enum CastType {
    Any, Decl, FunDecl, Call
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
    /// Try to cast the expression to the given type, returning the expression if it fails.
    pub(super) fn apply_cast(&mut self, src: Located<Expr>, dest: &Type) -> PLIRResult<HomoResult<Located<Expr>>> {
        self.apply_special_cast(src, dest, CastType::Any)
    }
    pub(super) fn apply_special_cast(&mut self, src: Located<Expr>, dest: &Type, ct: CastType) -> PLIRResult<HomoResult<Located<Expr>>> {
        Cast { src, dest, ct }.apply_cast(self)
    }
    fn apply_cast2(&mut self, (left, right): (Located<Expr>, Located<Expr>), dest: &Type) -> PLIRResult<HomoResult<(Located<Expr>, Located<Expr>)>> {
        let cast1 = Cast { src: left, dest, ct: CastType::Any };
        let cast2 = Cast { src: right, dest, ct: CastType::Any };

        let result = if cast1.can_cast(self)? && cast2.can_cast(self)? {
            Ok(cast1.apply_cast(self)?.ok().zip(cast2.apply_cast(self)?.ok()).unwrap())
        } else {
            Err((cast1.src, cast2.src))
        };

        Ok(result)
    }

    /// Try casting the expression to one of the given types until successful.
    fn cast_chain(&mut self, src: Located<Expr>, tys: &[Type]) -> PLIRResult<HomoResult<Located<Expr>>> {
        let mut cast = Cast { src, dest: &ty!(Type::S_NEVER), ct: CastType::Any };
        for dest in tys {
            cast.dest = dest;

            if cast.can_cast(self)? {
                return cast.apply_cast(self);
            }
        }
        
        Ok(Err(cast.src))
    }

    /// Try casting the expression to one of the given types until successful.
    fn cast_chain2(&mut self, (left, right): (Located<Expr>, Located<Expr>), tys: &[Type]) -> PLIRResult<HomoResult<(Located<Expr>, Located<Expr>)>> {
        let mut cast1 = Cast { src: left,  dest: &ty!(Type::S_NEVER), ct: CastType::Any };
        let mut cast2 = Cast { src: right, dest: &ty!(Type::S_NEVER), ct: CastType::Any };

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

        let ident = self.get_class(left)?
            .get_method(method_name)
            .unwrap()
            .to_string();

        let e = self.get_var_type_opt(&ident)?
            .cloned()
            .map(|t| Expr::new(t, ExprType::Ident(ident)));
        
        Ok(e)
    }
    
    pub(super) fn apply_unary(&mut self, e: Located<Expr>, op: op::Unary, unary_range: CursorRange) -> PLIRResult<Located<Expr>> {
        // Check for any valid casts that can be applied here:
        let Located(cast, left_range) = match op {
            op::Unary::Plus => {
                self.cast_chain(e, &[ty!(Type::S_INT), ty!(Type::S_FLOAT)])?.into_inner()
            },
            op::Unary::Minus => {
                self.cast_chain(e, &[ty!(Type::S_INT), ty!(Type::S_FLOAT)])?.into_inner()
            },
            op::Unary::LogNot => {
                self.apply_cast(e, &ty!(Type::S_BOOL))?.into_inner()
            },
            op::Unary::BitNot => e,
        };
    
        // Type check and compute resulting expr type:
        let ty = {
            let ty = cast.ty.clone();
    
            match (op, ty.as_ref()) {
                (op::Unary::Plus,   TypeRef::Prim(Type::S_INT | Type::S_FLOAT)) => ty,
                (op::Unary::Minus,  TypeRef::Prim(Type::S_INT | Type::S_FLOAT)) => ty,
                (op::Unary::LogNot, TypeRef::Prim(Type::S_BOOL)) => ty,
                (op::Unary::BitNot, TypeRef::Prim(Type::S_INT)) => ty,
                (op, _) => {
                    let fun = self.find_unary_method(op, Located::new(&ty, left_range))?
                        .ok_or_else(|| {
                            OpErr::CannotUnary(op, ty).at_range(unary_range.clone())
                        })?;
                    
                    return Expr::call(Located::new(fun, unary_range.clone()), vec![cast])
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

        let ident = self.get_class(left)?
            .get_method(&format!("{method_name}_{right}"))
            .unwrap()
            .to_string();

        let e = self.get_var_type_opt(&ident)?
            .cloned()
            .map(|t| Expr::new(t, ExprType::Ident(ident)));
        
        Ok(e)
    }

    pub(super) fn apply_binary(
        &mut self, 
        op: op::Binary, 
        left: Located<Expr>, 
        right: Located<Expr>, 
        expr_range: CursorRange
    ) -> PLIRResult<Expr> {
        // Check for any valid casts that can be applied here:
        let (lcast, rcast) = match op {
            op::Binary::Add => {
                let types = &[
                    ty!(Type::S_INT),
                    ty!(Type::S_FLOAT)
                ];
                match self.cast_chain2((left, right), types)? {
                    Ok(exprs) => exprs,
                    Err((l, r)) => match (l.ty.as_ref(), r.ty.as_ref()) {
                        (TypeRef::Prim(Type::S_STR | Type::S_CHAR), _) => {
                            self.apply_cast2((l, r), &ty!(Type::S_STR))?.into_inner()
                        },
                        (_, TypeRef::Prim(Type::S_STR | Type::S_CHAR)) => {
                            self.apply_cast2((l, r), &ty!(Type::S_STR))?.into_inner()
                        },
                        _ => (l, r)
                    },
                }
            },
            op::Binary::Sub => {
                self.cast_chain2((left, right), &[ty!(Type::S_INT), ty!(Type::S_FLOAT)])?.into_inner()
            },
            op::Binary::Mul => {
                self.cast_chain2((left, right), &[ty!(Type::S_INT), ty!(Type::S_FLOAT)])?.into_inner()
            },
            op::Binary::Div => {
                self.apply_cast2((left, right), &ty!(Type::S_FLOAT))?.into_inner()
            },
            op::Binary::Mod => {
                self.cast_chain2((left, right), &[ty!(Type::S_INT), ty!(Type::S_FLOAT)])?.into_inner()
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
                (op::Binary::Div, l @ TypeRef::Prim(Type::S_INT | Type::S_FLOAT), r) if l == r => ty!(Type::S_FLOAT),
                (op::Binary::Mod, l @ TypeRef::Prim(Type::S_INT | Type::S_FLOAT), r) if l == r => left,
                // bitwise operators:
                (op::Binary::Shl, TypeRef::Prim(Type::S_INT), TypeRef::Prim(Type::S_INT)) => left,
                (op::Binary::Shr, TypeRef::Prim(Type::S_INT), TypeRef::Prim(Type::S_INT)) => left,
                (op::Binary::BitOr,  l @ TypeRef::Prim(Type::S_INT | Type::S_BOOL), r) if l == r => left,
                (op::Binary::BitAnd, l @ TypeRef::Prim(Type::S_INT | Type::S_BOOL), r) if l == r => left,
                (op::Binary::BitXor, l @ TypeRef::Prim(Type::S_INT | Type::S_BOOL), r) if l == r => left,
                // logical operators:
                (op::Binary::LogAnd, l, r) if l == r => left,
                (op::Binary::LogOr, l, r) if l == r => left,
                (op, _, _) => {
                    let fun = self.find_binary_method(op, Located::new(&left, lcast.1), right)?
                        .ok_or_else(|| {
                            OpErr::CannotBinary(op, left, right.clone())
                                .at_range(expr_range.clone())
                        })?;

                    return Expr::call(Located::new(fun, expr_range), vec![lcast.0, rcast.0]);
                }
            }
        };
    
        // Construct expression:
        Ok(Expr { ty, expr: ExprType::BinaryOp { op, left: Box::new(lcast.0), right: Box::new(rcast.0) }})
    }

    pub(super) fn apply_index(&mut self, left: Located<Expr>, index: Located<Expr>, expr_range: CursorRange) -> PLIRResult<(Type, Index)> {
        // Check for any valid casts that can be applied here:
        let lcast = self.apply_cast(left, &ty!(Type::S_STR))?.into_inner();
    
        let icast = match lcast.ty.as_ref() {
            | TypeRef::Prim(Type::S_STR)
            | TypeRef::Generic(Type::S_LIST, _)
            | TypeRef::Tuple(_)
            => self.apply_cast(index, &ty!(Type::S_INT))?.into_inner(),
            
            TypeRef::Generic(Type::S_DICT, [k, _]) => self.apply_cast(index, k)?.into_inner(),
            
            _ => return Err(OpErr::CannotIndex(lcast.0.ty).at_range(lcast.1).into())
        }.0;
    
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

