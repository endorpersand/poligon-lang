//! Takes a parse tree and executes it.
//! 
//! TODO! more doc

use std::rc::Rc;

use crate::GonErr;
use crate::util::RvErr;

use self::tree::op;
use self::value::{Value, ValueType, VArbType, FunType, FunParamType};
use self::vars::VarContext;

pub(crate) mod tree;
pub(crate) mod value;
mod vars;
mod gstd;

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

impl Default for BlockContext<'_> {
    fn default() -> Self {
        Self::new()
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
    CannotSetIndex(ValueType),
    CannotIndexWith(ValueType, ValueType),
    IndexOutOfBounds,
    UndefinedVar(String),
    WrongArity(usize),
    CannotCall,
    CannotReturn,
    CannotBreak,
    CannotContinue,
    NotIterable(ValueType),
    UnpackTooLittle(usize /* expected */, usize /* got */),
    UnpackTooMany(usize /* expected */),
    RvErr(RvErr),

    AlreadyDeclared(String),
    NotDeclared(String)
}
impl GonErr for RuntimeErr {
    fn err_name(&self) -> &'static str {
        "runtime error"
    }

    fn message(&self) -> String {
        // TODO
        format!("{:?}", self)
    }
}
type RtResult<T> = Result<T, RuntimeErr>;

pub enum TermOp<T, E> {
    Err(E),
    Return(T),
    Break,
    Continue
}
impl<T> From<RuntimeErr> for TermOp<T, RuntimeErr> {
    fn from(e: RuntimeErr) -> Self {
        TermOp::Err(e)
    }
}
type RtTraversal<T> = Result<T, TermOp<T, RuntimeErr>>;

/// This trait enables the traversal of a program tree.
pub trait TraverseRt {
    /// Apply the effects of this node, and evaluate any of the children nodes to do so.
    fn traverse_rt(&self, ctx: &mut BlockContext) -> RtTraversal<Value>;
}

impl TraverseRt for tree::Expr {
    fn traverse_rt(&self, ctx: &mut BlockContext) -> RtTraversal<Value> {
        match self {
            tree::Expr::Ident(ident) => {
                ctx.vars.get(ident)
                    .ok_or_else(|| RuntimeErr::UndefinedVar(String::from(ident)))
                    .map(Value::clone)
                    .map_err(TermOp::Err)
            },
            tree::Expr::Block(e) => e.traverse_rt(&mut ctx.child()),
            tree::Expr::Literal(e) => e.traverse_rt(ctx),
            tree::Expr::ListLiteral(exprs) => {
                let vec = exprs.iter()
                    .map(|expr| expr.traverse_rt(ctx))
                    .collect::<Result<_, _>>()?;

                Ok(Value::new_list(vec))
            },
            tree::Expr::SetLiteral(_) => todo!(),
            tree::Expr::DictLiteral(_) => todo!(),
            tree::Expr::Assign(pat, expr) => {
                let result = expr.traverse_rt(ctx)?;
                
                assign_pat(pat, result, ctx)
                    .map_err(TermOp::Err)
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

                        Ok(Value::new_list(values))
                    },
                    // (a, b @ Value::Float(_)) => compute_float_range(a, b, &step_value),
                    // (a @ Value::Float(_), b) => compute_float_range(a, b, &step_value),
                    (Value::Char(a), Value::Char(b)) => {
                        let (a, b) = (*a as u32, *b as u32);
                        let values = compute_uint_range(a, b, step)?
                            .into_iter()
                            .map(|i| {
                                char::from_u32(i)
                                    .unwrap_or_else(|| panic!("u32 '{:x}' could not be parsed as char", i))
                            })
                            .map(Value::Char)
                            .collect();

                        Ok(Value::new_list(values))
                    },
                    _ => Err(RuntimeErr::CannotApplySpread(l.ty(), r.ty()))?
                }
            },
            tree::Expr::If(e) => e.traverse_rt(ctx),
            tree::Expr::While { condition, block } => {
                let mut values = vec![];
                while condition.traverse_rt(ctx)?.truth() {
                    let iteration = match block.traverse_rt(&mut ctx.child()) {
                        Ok(t) => t,
                        Err(TermOp::Break) => break,
                        Err(TermOp::Continue) => continue,
                        e => e?
                    };

                    values.push(iteration);
                }

                Ok(Value::new_list(values))
            },
            tree::Expr::For { ident, iterator, block } => {
                let it_val = iterator.traverse_rt(ctx)?;
                let it = it_val.as_iterator()
                    .ok_or_else(|| RuntimeErr::CannotIterateOver(it_val.ty()))?;

                let mut result = vec![];
                for val in it {
                    let mut scope = ctx.child();
                    scope.vars.declare(ident.clone(), val)?;

                    let iteration = match block.traverse_rt(&mut scope) {
                        Ok(t) => t,
                        Err(TermOp::Break) => break,
                        Err(TermOp::Continue) => continue,
                        e => e?,
                    };
                    result.push(iteration);
                }

                Ok(Value::new_list(result))
            },
            tree::Expr::Call { funct, params } => {
                if let Value::Fun(f) = funct.traverse_rt(ctx)? {
                    f.call(params, &mut ctx.child())
                } else {
                    Err(RuntimeErr::CannotCall)?
                }
            }
            tree::Expr::Index(idx) => {
                let tree::Index {expr, index} = idx;
                let val = expr.traverse_rt(ctx)?;
                let index_val = index.traverse_rt(ctx)?;

                val.get_index(index_val).map_err(TermOp::Err)
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

fn into_err<T>(t: TermOp<T, RuntimeErr>) -> RuntimeErr {
    match t {
        TermOp::Err(e)    => e,
        TermOp::Return(_) => RuntimeErr::CannotReturn,
        TermOp::Break     => RuntimeErr::CannotBreak,
        TermOp::Continue  => RuntimeErr::CannotContinue,
    }
}
fn assign_pat(pat: &tree::AsgPat, rhs: Value, ctx: &mut BlockContext) -> RtResult<Value> {
    let val = match pat {
        tree::AsgPat::Unit(unit) => match unit {
            tree::AsgUnit::Ident(ident) => {
                ctx.vars.set(ident, rhs)?.clone()
            },
            tree::AsgUnit::Path(_, _) => todo!(),
            tree::AsgUnit::Index(idx) => {
                let tree::Index {expr, index} = idx;
                
                let mut val = expr.traverse_rt(ctx).map_err(into_err)?;
                let index_val = index.traverse_rt(ctx).map_err(into_err)?;

                val.set_index(index_val, rhs)?
            },
        },
        tree::AsgPat::List(pats) => {
            let pat_len = pats.len();
            let mut it = rhs.as_iterator()
                .ok_or_else(|| RuntimeErr::NotIterable(rhs.ty()))?;
            
            let values: Vec<_> = it.by_ref().take(pat_len).collect();
            let values_len = values.len();

            if pat_len > values_len {
                Err(RuntimeErr::UnpackTooLittle(pat_len, values_len))?
            } else if it.next().is_some() {
                Err(RuntimeErr::UnpackTooMany(pat_len))?
            };

            std::mem::drop(it); // & give me 20
            // allows rhs to be used again

            for (pat, val) in std::iter::zip(pats, values) {
                assign_pat(pat, val, ctx)?;
            }

            rhs
        },
    };

    Ok(val)
}

impl TraverseRt for tree::Literal {
    fn traverse_rt(&self, _ctx: &mut BlockContext) -> RtTraversal<Value> {
        Ok(self.clone().into())
    }
}

impl TraverseRt for tree::UnaryOps {
    fn traverse_rt(&self, ctx: &mut BlockContext) -> RtTraversal<Value> {
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
    fn traverse_rt(&self, ctx: &mut BlockContext) -> RtTraversal<Value> {
        let tree::BinaryOp { op, left, right } = self;

        left.apply_binary(op, right, ctx)
    }
}

impl TraverseRt for tree::If {
    fn traverse_rt(&self, ctx: &mut BlockContext) -> RtTraversal<Value> {
        for (cond, block) in &self.conditionals {
            if cond.traverse_rt(ctx)?.truth() {
                return block.traverse_rt(&mut ctx.child());
            }
        }

        self.last.as_ref().map_or(
            Ok(Value::Unit),
            |block| block.traverse_rt(&mut ctx.child())
        )
    }
}

impl TraverseRt for tree::Program {
    fn traverse_rt(&self, ctx: &mut BlockContext) -> RtTraversal<Value> {
        let mut stmts = self.0.iter();
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
    fn traverse_rt(&self, ctx: &mut BlockContext) -> RtTraversal<Value> {
        match self {
            tree::Stmt::Decl(dcl) => dcl.traverse_rt(ctx),
            tree::Stmt::Return(t) => {
                let expr = t.traverse_rt(ctx)?;
                
                Err(TermOp::Return(expr))
            },
            tree::Stmt::Break     => Err(TermOp::Break),
            tree::Stmt::Continue  => Err(TermOp::Continue),
            tree::Stmt::FunDecl(dcl) => dcl.traverse_rt(ctx),
            tree::Stmt::Expr(e) => e.traverse_rt(ctx),
        }
    }
}

impl TraverseRt for tree::Decl {
    fn traverse_rt(&self, ctx: &mut BlockContext) -> RtTraversal<Value> {
        let rhs = self.val.traverse_rt(ctx)?;
        
        ctx.vars.declare_full(
            self.ident.clone(), 
            rhs, 
            self.rt, 
            self.mt
        ).map_err(TermOp::Err)
    }
}

impl TraverseRt for tree::FunDecl {
    fn traverse_rt(&self, ctx: &mut BlockContext) -> RtTraversal<Value> {
        let tree::FunDecl { ident, params, ret, block } = self;
        
        let mut param_types = vec![];
        let mut param_names = vec![];

        for p in params {
            let tree::Param { ident, ty, .. } = p;

            param_types.push(
                ty.as_ref()
                    .map_or(VArbType::Unk, VArbType::lookup)
            );
            param_names.push(ident.clone());
        }

        let p = FunParamType::Positional(param_types);
        let r = ret.as_ref()
            .map_or(
                VArbType::Value(ValueType::Unit), 
                VArbType::lookup
            );

        let ty = FunType::new(p, r);

        let val = Value::new_gon_fn(
            Some(ident),
            ty,
            param_names,
            Rc::clone(block)
        );
        
        let rf = ctx.vars.declare(ident.clone(), val)?;
        Ok(rf)
    }
}