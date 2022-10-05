use self::tree::op;
use self::value::Value;
use self::vars::VarContext;

pub(crate) mod tree;
pub(crate) mod value;
mod vars;

pub struct BlockContext<'ctx> {
    vars: VarContext<'ctx>,
}

impl BlockContext<'_> {
    pub fn new() -> Self {
        Self {
            vars: VarContext::new()
        }
    }

    pub fn child(&mut self) -> BlockContext {
        BlockContext {
            vars: self.vars.child()
        }
    }
}

#[derive(Debug)]
pub enum RuntimeErr {
    CannotCompare(op::Cmp, String, String),
    CannotApplyUnary(op::Unary, String),
    CannotApplyBinary(op::Binary, String, String),
    CannotApplySpread(String, String),
    DivisionByZero,
    ExpectedType(String),
    RangeIsInfinite, // TODO: remove
    CannotIterateOver(String)
}

type RtResult<T> = Result<T, RuntimeErr>;
pub trait TraverseRt {
    fn traverse_rt(&self, ctx: &mut BlockContext) -> RtResult<Value>;
}

impl TraverseRt for tree::Expr {
    fn traverse_rt(&self, ctx: &mut BlockContext) -> RtResult<Value> {
        match self {
            tree::Expr::Ident(_) => todo!(),
            tree::Expr::Block(e) => e.traverse_rt(&mut ctx.child()),
            tree::Expr::Literal(e) => e.traverse_rt(ctx),
            tree::Expr::ListLiteral(exprs) => {
                let vec = exprs.iter()
                    .map(|expr| expr.traverse_rt(ctx))
                    .collect::<Result<_, _>>()?;

                Ok(Value::List(vec))
            },
            tree::Expr::SetLiteral(_) => todo!(),
            tree::Expr::DictLiteral(_) => todo!(),
            tree::Expr::Assignment(_, _) => todo!(),
            tree::Expr::Attr(_) => todo!(),
            tree::Expr::StaticAttr(_) => todo!(),
            tree::Expr::UnaryOps(o) => o.traverse_rt(ctx),
            tree::Expr::BinaryOp(o) => o.traverse_rt(ctx),
            tree::Expr::Comparison { left, right, extra } => {
                let mut cmps = vec![right];
                cmps.extend(extra);

                let mut lval = left.traverse_rt(ctx)?;
                // for cmp a < b < c < d < e,
                // break it up into a < b && b < c && c < d && d < e
                // do each comparison. if any ever returns false, short circuit and return
                for (cmp, rexpr) in cmps {
                    let rval = rexpr.traverse_rt(ctx)?;

                    if lval.apply_cmp(cmp, &rval)? {
                        lval = rval;
                    } else {
                        return Ok(Value::Bool(false));
                    }
                }

                Ok(Value::Bool(true))
            },
            tree::Expr::Range { left, right, step } => {
                // TODO: be lazy
                let (l, r) = (left.traverse_rt(ctx)?, right.traverse_rt(ctx)?);
                let step_value = step.as_ref().map(|e| e.traverse_rt(ctx)).unwrap_or(Ok(Value::Int(1)))?;

                match (&l, &r) {
                    (Value::Int(a), Value::Int(b)) => if let Value::Int(s) = step_value {
                        let values = compute_int_range(*a, *b, s)?
                            .into_iter()
                            .map(|i| Value::Int(i))
                            .collect();

                        Ok(Value::List(values))
                    } else {
                        Err(RuntimeErr::ExpectedType(Value::Int(0).ty()))
                    },
                    // (a, b @ Value::Float(_)) => compute_float_range(a, b, &step_value),
                    // (a @ Value::Float(_), b) => compute_float_range(a, b, &step_value),
                    (Value::Char(a), Value::Char(b)) => if let Value::Int(s) = step_value {
                        let (a, b) = (*a as u32, *b as u32);
                        let values = compute_uint_range(a, b, s)?
                            .into_iter()
                            .map(|i| {
                                char::from_u32(i)
                                    .expect(&format!("u32 '{:x}' could not be parsed as char", i))
                            })
                            .map(Value::Char)
                            .collect();

                        Ok(Value::List(values))
                    } else {
                        Err(RuntimeErr::ExpectedType(Value::Int(0).ty()))
                    },
                    _ => Err(RuntimeErr::CannotApplySpread(l.ty(), r.ty()))
                }
            },
            tree::Expr::If(e) => e.traverse_rt(ctx),
            tree::Expr::While { condition, block } => {
                let mut values = vec![];
                while condition.traverse_rt(ctx)?.truth() {
                    values.push(block.traverse_rt(&mut ctx.child())?);
                }

                Ok(Value::List(values))
            },
            tree::Expr::For { ident, iterator, block } => todo!(),
        }
    }
}

fn compute_int_range(left: isize, right: isize, step: isize) -> RtResult<Vec<isize>>
{
    if step == 0 { return Err(RuntimeErr::RangeIsInfinite); }
    let mut values = vec![];
    let mut n = left;

    if step > 0 {
        while n < right {
            values.push(n);
            n += step;
        }
    } else {
        while n > right {
            values.push(n);
            n += step;
        }
    }
    Ok(values)
}
fn compute_uint_range(left: u32, right: u32, step: isize) -> RtResult<Vec<u32>>
{
    if step == 0 { return Err(RuntimeErr::RangeIsInfinite); }
    let mut values = vec![];

    let positive = step > 0;
    if let Ok(step) = u32::try_from(step.abs()) {
        let mut n = left;
    
        if positive {
            while n < right {
                values.push(n);
                n += step;
            }
        } else {
            while n > right {
                values.push(n);
                n -= step;
            }
        }
    }

    Ok(values)
}

// fn compute_float_range(left: &Value, right: &Value, step: &Value) -> RtResult<Value> {
//     if let (Some(a), Some(b)) = (left.as_float(), right.as_float()) {
//         let s = step.as_float()
//             .ok_or(RuntimeErr::ExpectedType(Value::Float(0.).ty()))?;
        
//         let n_steps_f = (b - a) / s;

//         if n_steps_f.is_finite() && n_steps_f.is_sign_positive() {
//             let n_steps = n_steps_f.floor() as isize;

//             let values = (0..n_steps)
//                 .map(|i| a + (i as f64) * s)
//                 .map(Value::Float)
//                 .collect();

//             Ok(Value::List(values))
//         } else {
//             Err(RuntimeErr::RangeIsInfinite)
//         }
//     } else {
//         Err(RuntimeErr::CannotApplySpread(left.ty(), right.ty()))
//     }
// }

impl TraverseRt for tree::Literal {
    fn traverse_rt(&self, ctx: &mut BlockContext) -> RtResult<Value> {
        Ok(self.clone().into())
    }
}

impl TraverseRt for tree::UnaryOps {
    fn traverse_rt(&self, ctx: &mut BlockContext) -> RtResult<Value> {
        let tree::UnaryOps {ops, expr} = self;

        let mut ops_iter = ops.iter().rev();
        
        // ops should always have at least 1 unary op, so this should always be true
        let mut e = if let Some(op) = ops_iter.next() {
            expr.apply_unary(op, ctx)
        } else {
            // should never happen, but in case it does
            expr.traverse_rt(ctx)
        }?;

        // apply the rest:
        for op in ops_iter {
            e = e.apply_unary(op)?;
        }

        Ok(e)
    }
}

impl TraverseRt for tree::BinaryOp {
    fn traverse_rt(&self, ctx: &mut BlockContext) -> RtResult<Value> {
        let tree::BinaryOp { op, left, right } = self;

        left.apply_binary(op, right, ctx)
    }
}

impl TraverseRt for tree::Program {
    fn traverse_rt(&self, ctx: &mut BlockContext) -> RtResult<Value> {
        todo!()
    }
}

impl TraverseRt for tree::If {
    fn traverse_rt(&self, ctx: &mut BlockContext) -> RtResult<Value> {
        let tree::If { condition, if_true, if_false } = self;

        if condition.traverse_rt(ctx)?.truth() {
            if_true.traverse_rt(&mut ctx.child())
        } else {
            if_false.as_ref()
                .map(|e| e.traverse_rt(ctx))
                .unwrap_or(Ok(Value::Unit))
        }
    }
}

impl TraverseRt for tree::Else {
    fn traverse_rt(&self, ctx: &mut BlockContext) -> RtResult<Value> {
        match self {
            tree::Else::If(e) => e.traverse_rt(ctx),
            tree::Else::Block(e) => e.traverse_rt(&mut ctx.child()),
        }
    }
}
