//! Takes a parse tree and executes it.
//! 
//! TODO! more doc

use std::rc::Rc;

use super::semantic::{ResolveState, ResolveErr};
use crate::tree;

use crate::tree::op;
pub use self::value::Value;
use self::value::{ValueType, VArbType, FunType, FunParamType};
use self::vars::VarContext;

pub(crate) mod value;
mod vars;
mod gstd;

/// Struct that holds all of the state information 
/// of the current scope (variables, functions, etc).
pub struct BlockContext<'ctx> {
    vars: VarContext<'ctx>,
    rs: Rc<ResolveState>
}

impl BlockContext<'_> {
    /// Create a new context.
    pub fn new() -> Self {
        Self {
            vars: VarContext::new_with_std(),
            rs: Rc::new(ResolveState::new())
        }
    }

    /// Create a new scope. 
    /// 
    /// As long as the child scope is in use, this scope cannot be used.
    pub fn child(&mut self) -> BlockContext {
        BlockContext {
            vars: self.vars.child(),
            rs: Rc::clone(&self.rs)
        }
    }
    pub fn fun_body_scope_at(&mut self, idx: usize) -> BlockContext {
        let mv = self.vars.goto_idx(idx);

        mv.map(|v| BlockContext {
            vars: v.child(),
            rs: Rc::clone(&self.rs)
        }).unwrap()
    }

    pub fn get_var(&self, ident: &str, e: &tree::Expr) -> Option<&Value> {
        self.vars.get_indexed(ident, self.rs.get_steps(e))
    }
    pub fn set_var(&mut self, ident: &str, v: Value, e: &tree::Expr) -> RtResult<Value> {
        self.vars.set_indexed(ident, v, self.rs.get_steps(e))
    }
}

impl Default for BlockContext<'_> {
    fn default() -> Self {
        Self::new()
    }
}

macro_rules! cast {
    ($e:expr) => { Ok($e?) }
}

impl tree::Expr {
    /// Evaluate an expression and then apply the unary operator for it.
    pub fn apply_unary(&self, o: op::Unary, ctx: &mut BlockContext) -> RtTraversal<Value> {
        self.traverse_rt(ctx)
            .and_then(|v| cast! { v.apply_unary(o) })
    }

    /// Evaluate the two arguments to the binary operator and then apply the operator to it.
    /// 
    /// If the operator is `&&` or `||`, the evaluation can be short-circuited.
    pub fn apply_binary(&self, o: op::Binary, right: &Self, ctx: &mut BlockContext) -> RtTraversal<Value> {
        match o {
            // &&, || have special short circuiting that needs to be dealt with
            op::Binary::LogAnd => {
                let left = self.traverse_rt(ctx)?;
                if left.truth() { right.traverse_rt(ctx) } else { Ok(left) }
            },
            op::Binary::LogOr => {
                let left = self.traverse_rt(ctx)?;
                if left.truth() { Ok(left) } else { right.traverse_rt(ctx) }
            },
    
            // fallback to eager value binary
            _ => self.traverse_rt(ctx)
                .and_then(|v| cast! { v.apply_binary(o, right.traverse_rt(ctx)?) } )
        }
    }
}

impl tree::Program {
    pub fn run(self) -> RtResult<Value> {
        self.run_with_ctx(&mut BlockContext::new())
    }

    pub fn run_with_ctx(self, ctx: &mut BlockContext) -> RtResult<Value> {
        // Semantic traversal
        let rs = Rc::get_mut(&mut ctx.rs)
            .expect("Cannot resolve while in traversal");
        
        rs.clear();
        rs.traverse_tree(&self)?;

        // Runtime traversal
        self.traverse_rt(ctx).map_err(into_err)
    }
}

/// Runtime errors
pub mod err {
    use crate::tree::op;

    use super::value::ValueType;
    use super::value::RvErr;
    use super::super::semantic::ResolveErr;
    use crate::GonErr;

    macro_rules! rt_err {
        ($($e:ident),*) => {
            /// Errors that occur during runtime evaluation.
            #[derive(Debug)]
            pub enum RuntimeErr {
                $(
                    #[allow(missing_docs)]
                    $e($e)
                ),*
            }

            $(
                impl From<$e> for RuntimeErr {
                    fn from(e: $e) -> Self {
                        Self::$e(e)
                    }
                }
            )*
            
            impl GonErr for RuntimeErr {
                fn err_name(&self) -> &'static str {
                    match self {
                        $(
                            Self::$e(e) => e.err_name()
                        ),*
                    }
                }
            
                fn message(&self) -> String {
                    match self {
                        $(
                            Self::$e(e) => e.message()
                        ),*
                    }
                }
            }
        }
    }

    rt_err! { TypeErr, ValueErr, NameErr, ResolveErr, RvErr, FeatureErr }

    /// An error caused by type mismatches
    #[derive(Debug)]
    pub enum TypeErr {
        /// These two types can't be compared using the given operation.
        CannotCompare(op::Cmp, ValueType, ValueType),

        /// The unary operator cannot be applied to this type.
        CannotApplyUnary(op::Unary, ValueType),

        /// The binary operator cannot be applied between these two types.
        CannotApplyBinary(op::Binary, ValueType, ValueType),

        /// Cannot compute a range between these two types.
        CannotApplyRange(ValueType, ValueType),

        /// Cannot iterate over this type.
        NotIterable(ValueType),

        /// A specific type was expected here.
        ExpectedType(ValueType),

        /// Cannot index this type.
        CannotIndex(ValueType),

        /// Cannot set index of this type.
        CannotSetIndex(ValueType),

        /// Cannot index this type using the other type.
        CannotIndexWith(ValueType, ValueType),

        /// Cannot call this type.
        CannotCall(ValueType),
    }

    impl GonErr for TypeErr {
        fn err_name(&self) -> &'static str {
            "type error"
        }

        fn message(&self) -> String {
            match self {
                TypeErr::CannotCompare(op, t1, t2) => format!("cannot compare '{op}' between {t1} and {t2}"),
                TypeErr::CannotApplyUnary(op, t1) => format!("cannot apply '{op}' to {t1}"),
                TypeErr::CannotApplyBinary(op, t1, t2) => format!("cannot apply '{op}' to {t1} and {t2}"),
                TypeErr::CannotApplyRange(t1, t2) => format!("cannot create range {t1}..{t2}"),
                TypeErr::NotIterable(t1) => format!("{t1} is not iterable"),
                TypeErr::ExpectedType(t1) => format!("expected {t1}"),
                TypeErr::CannotIndex(t1) => format!("cannot index {t1}"),
                TypeErr::CannotSetIndex(t1) => format!("cannot assign to elements of {t1}"),
                TypeErr::CannotIndexWith(t1, t2) => format!("cannot index {t1} with {t2}"),
                TypeErr::CannotCall(t1) => format!("{t1} is not callable"),
            }
        }
    }

    /// An error caused by invalid value arguments
    #[derive(Debug)]
    pub enum ValueErr {
        /// Integer division was attempted when the second parameter was 0.
        DivisionByZero,

        /// Ranges are computed as lists in the interpreter. Therefore, ranges cannot be infinite.
        RangeIsInfinite,

        /// Index out of bounds.
        IndexOutOfBounds,

        /// Function was called with the wrong number of parameters. It actually expected this number.
        WrongArity(usize),

        /// Unpack (without spread) was attempted, but there needed to be more elements
        UnpackTooLittle(usize /* expected */, usize /* got */),

        /// Unpack (without spread) was attempted, but there needed to be more elements
        UnpackTooLittleS(usize /* expected at least */, usize /* got */),

        /// Unpack was attempted, but there were not enough elements
        UnpackTooMany(usize /* expected */),
    }

    impl GonErr for ValueErr {
        fn err_name(&self) -> &'static str {
            "value error"
        }

        fn message(&self) -> String {
            match self {
                ValueErr::DivisionByZero => String::from("division by zero"),
                ValueErr::RangeIsInfinite => String::from("range produced is infinite length"),
                ValueErr::IndexOutOfBounds => String::from("index out of bounds"),
                ValueErr::WrongArity(n) => format!("expected {n} parameters in function call"),
                ValueErr::UnpackTooLittle(ex, got) => format!("unpack failed, expected {ex} elements, but got {got}"),
                ValueErr::UnpackTooLittleS(exa, got) => format!("unpack failed, expected at least {exa} elements, but got {got}"),
                ValueErr::UnpackTooMany(ex) => format!("unpack failed, only expected {ex} elements"),
            }
        }
    }

    /// An error caused by variable name conflicts
    #[derive(Debug)]
    pub enum NameErr {
        /// Could not find this variable.
        UndefinedVar(String),

        /// Variable was already declared.
        AlreadyDeclared(String),

        /// Variable was not declared.
        NotDeclared(String),
    }


    impl GonErr for NameErr {
        fn err_name(&self) -> &'static str {
            "name error"
        }

        fn message(&self) -> String {
            match self {
                NameErr::UndefinedVar(name) => format!("could not find variable \'{name}\'"),
                NameErr::AlreadyDeclared(name) => format!("cannot redeclare \'{name}\'"),
                NameErr::NotDeclared(name) => format!("undeclared variable \'{name}\'"),
            }
        }
    }

    /// An error caused because the feature is unimplemented
    #[derive(Debug)]
    pub enum FeatureErr {
        /// Feature not completed.
        Incomplete(&'static str),

        /// This feature is only implemented in the compiler.
        CompilerOnly(&'static str)
    }

    impl GonErr for FeatureErr {
        fn err_name(&self) -> &'static str {
            "feature error"
        }

        fn message(&self) -> String {
            match self {
                FeatureErr::Incomplete(s) => format!("not yet implemented - {s}"),
                FeatureErr::CompilerOnly(s) => format!("compiler-only feature - {s}"),
            }
        }
    }
}
use err::*;

/// Fallible evaluation in runtime
pub type RtResult<T> = Result<T, RuntimeErr>;

/// Operations that result in interruption of normal program flow
pub enum TermOp<T, E> {
    /// An error occurred
    Err(E),

    /// A value was returned
    Return(T),

    /// `break` was called
    Break,

    /// `continue` was called
    Continue
}
impl<T, E: Into<RuntimeErr>> From<E> for TermOp<T, RuntimeErr> {
    fn from(e: E) -> Self {
        TermOp::Err(e.into())
    }
}
/// Evaluation in runtime that could interrupt normal program flow
pub type RtTraversal<T> = Result<T, TermOp<T, RuntimeErr>>;

/// This trait enables the traversal of a program tree.
pub trait TraverseRt {
    /// Apply the effects of this node, and evaluate any of the children nodes to do so.
    fn traverse_rt(&self, ctx: &mut BlockContext) -> RtTraversal<Value>;
}

impl TraverseRt for tree::Expr {
    fn traverse_rt(&self, ctx: &mut BlockContext) -> RtTraversal<Value> {
        match self {
            tree::Expr::Ident(ident) => {
                ctx.get_var(ident, self)
                    .ok_or_else(|| NameErr::UndefinedVar(String::from(ident)).into())
                    .map(Value::clone)
                    .map_err(TermOp::Err)
            },
            tree::Expr::Block(e) => e.traverse_rt(&mut ctx.child()),
            tree::Expr::Literal(e) => e.traverse_rt(ctx),
            tree::Expr::ListLiteral(exprs) => {
                let mut vec = vec![];
                
                for e in exprs.iter() {
                    match e {
                        tree::Expr::Spread(inner) => {
                            let inner = inner.as_ref()
                                .ok_or(ResolveErr::CannotSpreadNone)?
                                .traverse_rt(ctx)?;
                            let it = inner.as_iterator()
                                .ok_or_else(|| TypeErr::NotIterable(inner.ty()))?;

                            vec.extend(it);
                        },
                        e => vec.push(e.traverse_rt(ctx)?)
                    }
                }

                Ok(Value::new_list(vec))
            },
            tree::Expr::SetLiteral(_) => Err(FeatureErr::Incomplete("sets"))?,
            tree::Expr::DictLiteral(_) => Err(FeatureErr::Incomplete("dicts"))?,
            tree::Expr::Assign(pat, expr) => {
                let result = expr.traverse_rt(ctx)?;
                
                assign_pat(pat, result, ctx, self)
                    .map_err(TermOp::Err)
            },
            tree::Expr::Path(_) => Err(FeatureErr::Incomplete("general attribute functionality"))?,
            tree::Expr::UnaryOps { ops, expr } => {
                let mut ops_iter = ops.iter().rev();
        
                // ops should always have at least 1 unary op, so this should always be true
                let mut e = if let Some(&op) = ops_iter.next() {
                    expr.apply_unary(op, ctx)
                } else {
                    // should never happen, but in case it does
                    expr.traverse_rt(ctx)
                }?;

                // apply the rest:
                for &op in ops_iter {
                    e = e.apply_unary(op)?;
                }

                Ok(e)
            },
            tree::Expr::BinaryOp { op, left, right } => left.apply_binary(*op, right, ctx),
            tree::Expr::Comparison { left, rights } => {
                let mut lval = left.traverse_rt(ctx)?;
                // for cmp a < b < c < d < e,
                // break it up into a < b && b < c && c < d && d < e
                // do each comparison. if any ever returns false, short circuit and return
                for (cmp, rexpr) in rights {
                    let rval = rexpr.traverse_rt(ctx)?;

                    if lval.apply_cmp(*cmp, &rval)? {
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
                    _ => Err(TypeErr::ExpectedType(ValueType::Int))?
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
                    _ => Err(TypeErr::CannotApplyRange(l.ty(), r.ty()))?
                }
            },
            tree::Expr::If { conditionals, last } => {
                for (cond, block) in conditionals {
                    if cond.traverse_rt(ctx)?.truth() {
                        return block.traverse_rt(&mut ctx.child());
                    }
                }
        
                last.as_ref().map_or(
                    Ok(Value::Unit),
                    |block| block.traverse_rt(&mut ctx.child())
                )
            },
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
                    .ok_or_else(|| TypeErr::NotIterable(it_val.ty()))?;

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
                match &**funct {
                    // HACK: generalize methods
                    e @ tree::Expr::Path(tree::Path { obj, attrs }) => {
                        if let this @ Value::List(_) = obj.traverse_rt(ctx)? {
                            if attrs.len() == 1 {
                                let (property, _) = &attrs[0];
                                let mut call_params = vec![this];
                                call_params.extend(params.iter()
                                    .map(|e| e.traverse_rt(ctx))
                                    .collect::<Result<Vec<_>, _>>()?
                                );

                                match property.as_str() {
                                    "contains" => {
                                        let var = ctx.get_var("in", e).cloned();
                                        if let Some(Value::Fun(f)) = var {
                                            return f.call_computed(call_params, ctx)
                                        }
                                    },
                                    "push" => {
                                        let var = ctx.get_var("@@push", e).cloned();
                                        if let Some(Value::Fun(f)) = var {
                                            return f.call_computed(call_params, ctx)
                                        }
                                    },
                                    "pop" => {
                                        let var = ctx.get_var("@@pop", e).cloned();
                                        if let Some(Value::Fun(f)) = var {
                                            return f.call_computed(call_params, ctx)
                                        }
                                    },
                                    _ => {}
                                }
                            }
                        }
                        
                        Err(FeatureErr::Incomplete("general attribute functionality"))?
                    },
                    funct => match funct.traverse_rt(ctx)? {
                        Value::Fun(f) => f.call(params, ctx),
                        val => Err(TypeErr::CannotCall(val.ty()))?
                    }
                }
            }
            tree::Expr::Index(idx) => {
                let tree::Index {expr, index} = idx;
                let val = expr.traverse_rt(ctx)?;
                let index_val = index.traverse_rt(ctx)?;

                val.get_index(index_val).map_err(TermOp::Err)
            },
            tree::Expr::Spread(_) => Err(ResolveErr::CannotSpread)?,
        }
    }
}

fn compute_int_range(left: isize, right: isize, step: isize) -> RtResult<Vec<isize>>
{
    if step == 0 { Err(ValueErr::RangeIsInfinite)? }
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
    if step == 0 { Err(ValueErr::RangeIsInfinite)? }
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
//         Err(RuntimeErr::CannotApplyRange(left.ty(), right.ty()))
//     }
// }

fn into_err<T>(t: TermOp<T, RuntimeErr>) -> RuntimeErr {
    match t {
        TermOp::Err(e)    => e,
        TermOp::Return(_) => ResolveErr::CannotReturn.into(),
        TermOp::Break     => ResolveErr::CannotBreak.into(),
        TermOp::Continue  => ResolveErr::CannotContinue.into(),
    }
}

impl TraverseRt for tree::Literal {
    fn traverse_rt(&self, _ctx: &mut BlockContext) -> RtTraversal<Value> {
        Ok(self.clone().into())
    }
}

impl TraverseRt for tree::Program {
    fn traverse_rt(&self, ctx: &mut BlockContext) -> RtTraversal<Value> {
        self.0.traverse_rt(ctx)
    }
}

impl TraverseRt for tree::Block {
    fn traverse_rt(&self, ctx: &mut BlockContext) -> RtTraversal<Value> {
        match self.0.split_last() {
            Some((tail, head)) => {
                for stmt in head {
                    stmt.traverse_rt(ctx)?;
                }
                tail.traverse_rt(ctx)
            }
            None => Ok(Value::Unit),
        }
    }
}

impl TraverseRt for tree::Stmt {
    fn traverse_rt(&self, ctx: &mut BlockContext) -> RtTraversal<Value> {
        match self {
            tree::Stmt::Decl(dcl) => dcl.traverse_rt(ctx),
            tree::Stmt::Return(mt) => {
                let expr = mt.as_ref()
                    .map_or(
                        Ok(Value::Unit), 
                        |t| t.traverse_rt(ctx)
                    )?;
                
                Err(TermOp::Return(expr))
            },
            tree::Stmt::Break     => Err(TermOp::Break),
            tree::Stmt::Continue  => Err(TermOp::Continue),
            tree::Stmt::FunDecl(dcl) => dcl.traverse_rt(ctx),
            tree::Stmt::ExternFunDecl(_) => Err(FeatureErr::CompilerOnly("extern function declarations"))?,
            tree::Stmt::Expr(e) => e.traverse_rt(ctx),
        }
    }
}

impl TraverseRt for tree::Decl {
    fn traverse_rt(&self, ctx: &mut BlockContext) -> RtTraversal<Value> {
        let rhs = self.val.traverse_rt(ctx)?;
        
        declare_pat(&self.pat, rhs, ctx, self.rt)
            .map_err(TermOp::Err)
    }
}

impl TraverseRt for tree::FunDecl {
    fn traverse_rt(&self, ctx: &mut BlockContext) -> RtTraversal<Value> {
        let tree::FunDecl { sig: tree::FunSignature { ident, params, ret }, block } = self;
        
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
            Rc::clone(block),
            ctx.vars.idx()
        );
        
        let rf = ctx.vars.declare(ident.clone(), val)?.clone();
        Ok(rf)
    }
}

#[cfg(test)]
mod tests {
    use crate::Interpreter;

    #[test]
    fn lexical_scope_test() -> std::io::Result<()> {
        Interpreter::from_file("_test_files/lexical_scope.gon")?
            .run()
            .unwrap();

        Ok(())
    }
}

impl<T> tree::Pat<T> {
    pub fn unpack<F>(&self, rhs: Value, mut unit_mapper: F) -> RtResult<Value>
        where F: FnMut(&T, Value) -> RtResult<Value>
    {
        self.unpack_mut(rhs, &mut unit_mapper)
    }

    /// Unpack with a mutable reference to a closure.
    /// This is necessary because the mutable reference is passed several times.
    /// 
    /// This function is used to implement `unpack`.
    fn unpack_mut<F>(&self, rhs: Value, unit_mapper: &mut F) -> RtResult<Value>
        where F: FnMut(&T, Value) -> RtResult<Value>
    {
        match self {
            tree::Pat::Unit(unit) => unit_mapper(unit, rhs),
            tree::Pat::List(pats) => {
                let mut it = rhs.as_iterator()
                    .ok_or_else(|| TypeErr::NotIterable(rhs.ty()))?;
    
                let has_spread = pats.iter().any(|p| matches!(p, Self::Spread(_)));
                let mut left_values = vec![];
                let mut right_values = vec![];
                let mut consumed = 0;
    
                for (i, p) in pats.iter().enumerate() {
                    if matches!(p, Self::Spread(_)) {
                        consumed = i;
                        break;
                    } else if let Some(t) = it.next() {
                        left_values.push(t);
                    } else {
                        // we ran out of elements:
    
                        if has_spread {
                            Err(ValueErr::UnpackTooLittleS(pats.len() - 1, i))
                        } else {
                            Err(ValueErr::UnpackTooLittle(pats.len(), i))
                        }?
                    };
                }
    
                if has_spread {
                    // do the reverse pass:
                    for (i, p) in std::iter::zip(consumed.., pats.iter().rev()) {
                        if matches!(p, Self::Spread(_)) {
                            break;
                        } else if let Some(t) = it.next_back() {
                            right_values.push(t);
                        } else {
                            // we ran out of elements:
                            Err(ValueErr::UnpackTooLittleS(pats.len() - 1, i))?
                        };
                    }
                } else {
                    // confirm no extra elements:
                    if it.next().is_some() {
                        Err(ValueErr::UnpackTooMany(pats.len()))?
                    }
                }
                
                let middle_values = Value::new_list(it.collect());
    
                let val_chain = left_values.into_iter()
                    .chain(std::iter::once(middle_values))
                    .chain(right_values);
                
                for (pat, val) in std::iter::zip(pats, val_chain) {
                    pat.unpack_mut(val, unit_mapper)?;
                }
                
                Ok(rhs)
            },
            tree::Pat::Spread(mp) => match mp {
                Some(p) => p.unpack_mut(rhs, unit_mapper),
                None => Ok(rhs), // we can dispose if spread is None, TODO!: what does a no-spread return?
            },
        }
    }
}

fn assign_pat(pat: &tree::AsgPat, rhs: Value, ctx: &mut BlockContext, from: &tree::Expr) -> RtResult<Value> {
    pat.unpack(rhs, |unit, rhs| match unit {
        tree::AsgUnit::Ident(ident) => {
            ctx.set_var(ident, rhs, from)
        },
        tree::AsgUnit::Path(_) => Err(FeatureErr::Incomplete("general attribute functionality"))?,
        tree::AsgUnit::Index(idx) => {
            let tree::Index {expr, index} = idx;
            
            let mut val = expr.traverse_rt(ctx).map_err(into_err)?;
            let index_val = index.traverse_rt(ctx).map_err(into_err)?;
    
            val.set_index(index_val, rhs)
        },
    })
}

fn declare_pat(pat: &tree::DeclPat, rhs: Value, ctx: &mut BlockContext, rt: tree::ReasgType) -> RtResult<Value> {
    pat.unpack(rhs, |tree::DeclUnit(ident, mt), rhs| ctx.vars.declare_full(
        String::from(ident), 
        rhs,
        rt,
        *mt
    ).cloned())
}