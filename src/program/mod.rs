use std::rc::Rc;

use self::tree::op;
use self::value::{Value, ValueType};
use self::vars::VarContext;

pub(crate) mod tree;
pub(crate) mod value;
mod vars;

/// Struct that holds all of the state information 
/// of the current scope (variables, functions, etc).
pub struct BlockContext<'ctx> {
    vars: VarContext<'ctx>,
}

impl BlockContext<'_> {
    /// Create a new context.
    pub fn new() -> Self {
        Self {
            vars: VarContext::new()
        }
    }

    /// Create a new scope. 
    /// 
    /// As long as the child scope is in use, this scope cannot be used.
    pub fn child(&mut self) -> BlockContext {
        BlockContext {
            vars: self.vars.child()
        }
    }
}

#[derive(Debug)]
pub enum RuntimeErr {
    CannotCompare(op::Cmp, ValueType, ValueType),
    CannotApplyUnary(op::Unary, ValueType),
    CannotApplyBinary(op::Binary, ValueType, ValueType),
    CannotApplySpread(ValueType, ValueType),
    DivisionByZero,
    ExpectedType(ValueType),
    RangeIsInfinite, // TODO: remove
    CannotIterateOver(ValueType),
    CannotIndex(ValueType),
    CannotIndexWith(ValueType, ValueType),
    IndexOutOfBounds,
    UndefinedVar(String),
    ExpectedNArgs(usize)
}

type RtResult<T> = Result<T, RuntimeErr>;

/// This trait enables the traversal of a program tree.
pub trait TraverseRt {
    /// Apply the effects of this node, and evaluate any of the children nodes to do so.
    fn traverse_rt(&self, ctx: &mut BlockContext) -> RtResult<Value>;
}

impl TraverseRt for tree::Expr {
    fn traverse_rt(&self, ctx: &mut BlockContext) -> RtResult<Value> {
        match self {
            tree::Expr::Ident(ident) => {
                ctx.vars.get(ident)
                    .ok_or(RuntimeErr::UndefinedVar(ident.clone()))
                    .map(|v| v.new_ref())
            },
            tree::Expr::Block(e) => e.traverse_rt(&mut ctx.child()),
            tree::Expr::Literal(e) => e.traverse_rt(ctx),
            tree::Expr::ListLiteral(exprs) => {
                let vec = exprs.iter()
                    .map(|expr| expr.traverse_rt(ctx))
                    .collect::<Result<_, _>>()?;

                Ok(Value::list(vec))
            },
            tree::Expr::SetLiteral(_) => todo!(),
            tree::Expr::DictLiteral(_) => todo!(),
            tree::Expr::Assignment(ident, expr) => {
                let result = expr.traverse_rt(ctx)?;
                
                let val = ctx.vars.set(ident.clone(), result);
                Ok(val.new_ref())
            },
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
                let step_value = step.as_ref()
                    .map(|e| e.traverse_rt(ctx))
                    .unwrap_or(Ok(Value::Int(1)))?;

                // for now, step can only be int:
                let step = match step_value { 
                    Value::Int(s) => s,
                    _ => Err(RuntimeErr::ExpectedType(ValueType::Int))?
                };

                match (&l, &r) {
                    (Value::Int(a), Value::Int(b)) => {
                        let values = compute_int_range(*a, *b, step)?
                            .into_iter()
                            .map(Value::Int)
                            .collect();

                        Ok(Value::list(values))
                    },
                    // (a, b @ Value::Float(_)) => compute_float_range(a, b, &step_value),
                    // (a @ Value::Float(_), b) => compute_float_range(a, b, &step_value),
                    (Value::Char(a), Value::Char(b)) => {
                        let (a, b) = (*a as u32, *b as u32);
                        let values = compute_uint_range(a, b, step)?
                            .into_iter()
                            .map(|i| {
                                char::from_u32(i)
                                    .expect(&format!("u32 '{:x}' could not be parsed as char", i))
                            })
                            .map(Value::Char)
                            .collect();

                        Ok(Value::list(values))
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

                Ok(Value::list(values))
            },
            tree::Expr::For { ident, iterator, block } => {
                let it_val = iterator.traverse_rt(ctx)?;
                let it = it_val.as_iterator()
                    .ok_or(RuntimeErr::CannotIterateOver(it_val.ty()))?;

                let mut result = vec![];
                for val in it {
                    let mut scope = ctx.child();
                    scope.vars.set(ident.clone(), val);

                    let iteration = block.traverse_rt(&mut scope)?;
                    result.push(iteration);
                }

                Ok(Value::list(result))
            },
            tree::Expr::Call { funct, params } => {
                match &**funct {
                    tree::Expr::Ident(s) => {
                        if s == "print" {
                            let exprs: Vec<_> = params.iter()
                                .map(|e| e.traverse_rt(ctx))
                                .collect::<Result<_, _>>()?;
                            
                            let strs = exprs.into_iter()
                                .map(|v| format!("{:?}", v))
                                .collect::<Vec<_>>()
                                .join(" ");

                            println!("{}", strs);
                            
                            Ok(Value::Unit)
                        } else if s == "is" {
                            if let [a, b] = &params[..2] {
                                let eval = match (a.traverse_rt(ctx)?, b.traverse_rt(ctx)?) {
                                    (Value::List(al), Value::List(bl)) => Rc::ptr_eq(&al, &bl),
                                    (av, bv) => av == bv
                                };

                                Ok(Value::Bool(eval))
                            } else {
                                Err(RuntimeErr::ExpectedNArgs(2))
                            }
                        } else {
                            todo!()
                        }
                    },
                    _ => todo!(),
                }
            }
            tree::Expr::Index { expr, index } => {
                let val = expr.traverse_rt(ctx)?;
                let index_val = index.traverse_rt(ctx)?;

                match val {
                    // There is a more efficient method of indexing lists 
                    // than just "conv to iter => get nth item":
                    e @ Value::List(_) => {
                        if let Value::Int(signed_index) = index_val {
                            let mi = usize::try_from(signed_index).ok();
                            
                            if let Value::List(l) = e {
                                mi.and_then(|i| l.borrow().get(i).map(Value::new_ref))
                                    .ok_or(RuntimeErr::IndexOutOfBounds)
                            } else {
                                unreachable!();
                            }
                        } else {
                            Err(RuntimeErr::CannotIndexWith(e.ty(), index_val.ty()))
                        }
                    },

                    // Convert to iter => get nth item
                    e => {
                        if let Some(mut it) = e.as_iterator() {
                            if let Value::Int(signed_index) = index_val {
                                let i = usize::try_from(signed_index).map_err(|_| RuntimeErr::IndexOutOfBounds)?;
                                it.nth(i).ok_or(RuntimeErr::IndexOutOfBounds)
                            } else {
                                Err(RuntimeErr::CannotIndexWith(e.ty(), index_val.ty()))
                            }
                        } else {
                            Err(RuntimeErr::CannotIndex(e.ty()))
                        }
                    }
                }
            },
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
//             .ok_or(RuntimeErr::ExpectedType(ValueType::Float)?;
        
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
    fn traverse_rt(&self, _ctx: &mut BlockContext) -> RtResult<Value> {
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

impl TraverseRt for tree::Program {
    fn traverse_rt(&self, ctx: &mut BlockContext) -> RtResult<Value> {
        let mut stmts = self.iter();
        let maybe_last = stmts.next_back();

        if let Some(last) = maybe_last {
            for stmt in stmts {
                stmt.traverse_rt(ctx)?;
            }
            last.traverse_rt(ctx)
        } else {
            Ok(Value::Unit)
        }
    }
}

impl TraverseRt for tree::Stmt {
    fn traverse_rt(&self, ctx: &mut BlockContext) -> RtResult<Value> {
        match self {
            tree::Stmt::Decl(_) => todo!(),
            tree::Stmt::Return(_) => todo!(),
            tree::Stmt::Break => todo!(),
            tree::Stmt::Continue => todo!(),
            tree::Stmt::FunDecl(_) => todo!(),
            tree::Stmt::Expr(e) => e.traverse_rt(ctx),
        }
    }
}