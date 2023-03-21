//! Takes a parse tree and executes it.
//! 
//! This module provides:
//! - [`TraverseRt`]: The traversal trait that enables the execution of AST in runtime.
//! - [`RtContext`]: The runtime state.
//! - [`value`]: The representation of values in the runtime.

use std::rc::Rc;

use super::semantic::{ResolveState, ResolveErr};
use crate::ast::{self, Located};

use crate::ast::op;
use crate::err::{GonErr, FullGonErr, CursorRange};
use self::value::*;
use self::vars::VarContext;

pub mod value;
mod vars;
mod gstd;

pub use rtio::IoHook;

/// Struct that holds state and scope in runtime.
/// It can be used to access variables and functions
/// once a static resolution has occurred.
pub struct RtContext<'ctx> {
    vars: VarContext<'ctx>,
    rs: Rc<ResolveState>,
    io: IoHook<'ctx>
}

mod rtio {
    use std::io;
    use std::ptr::NonNull;

    pub struct Io<T>(pub Vec<u8>, pub T);

    impl<T> Io<T> {
        pub fn pure(t: T) -> Self { Self(vec![], t) }
    }
    impl<T> io::Write for Io<T> {
        fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
            self.0.write(buf)
        }

        fn flush(&mut self) -> io::Result<()> {
            self.0.flush()
        }
    }

    /// A hook on IO operations.
    /// 
    /// If present, STDIN and STDOUT are read/written to the provided values
    /// instead of from STDIN and STDOUT.
    #[derive(Default)]
    pub struct IoHook<'ctx> {
        stdin: Option<NonNull<dyn io::Read + 'ctx>>,
        stdout: Option<NonNull<dyn io::Write + 'ctx>>
    }

    impl<'ctx> IoHook<'ctx> {
        /// Create a new read hook. 
        /// Any reads from STDIN are pulled from this value, and
        /// any writes to STDOUT are sent to STDOUT.
        /// 
        /// The IoHook holds a mutable reference to the reader 
        /// until it is dropped.
        pub fn new_r(stdin: &'ctx mut (dyn io::Read + 'ctx)) -> Self { 
            Self {
                stdin: Some(NonNull::from(stdin)),
                ..Default::default()
            }
        }

        /// Create a new write hook.
        /// Any reads from STDIN are pulled from STDIN, and
        /// any writes to STDOUT are sent to this value.
        /// 
        /// The IoHook holds a mutable reference to the writer
        /// until it is dropped.
        pub fn new_w(stdout: &'ctx mut (dyn io::Write + 'ctx)) -> Self { 
            Self {
                stdout: Some(NonNull::from(stdout)),
                ..Default::default()
            }
        }

        /// Create a new read-write hook.
        /// Reads and writes from STDIN and STDOUT are sent through the corresponding values.
        /// 
        /// The IoHook holds mutable references to theh reader and writer
        /// until it is dropped.
        pub fn new_rw(
            stdin: &'ctx mut (dyn io::Read + 'ctx), 
            stdout: &'ctx mut (dyn io::Write + 'ctx)
        ) -> Self {
            Self {
                stdin: Some(NonNull::from(stdin)),
                stdout: Some(NonNull::from(stdout))
            }
        }

        /// Duplicates the IoHook for temporary usage.
        /// 
        /// This differs from [`Clone::clone`] because it
        /// creates a new IoHook which has a shorter lifetime
        /// and maintains exclusivity until the clone is destroyed.
        pub fn clone(&mut self) -> IoHook {
            IoHook {
                ..(*self)
            }
        }
    }

    impl<'ctx> io::Write for IoHook<'ctx> {
        fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
            match self.stdout {
                // SAFETY: IoHook's lifetime is <'ctx>, 
                // so exclusive access is present until then,
                // and the data should still be intact.
                Some(mut w) => unsafe { w.as_mut().write(buf) },
                None => io::stdout().write(buf),
            }
        }

        fn flush(&mut self) -> io::Result<()> {
            match self.stdout {
                // SAFETY: IoHook's lifetime is <'ctx>, 
                // so exclusive access is present until then,
                // and the data should still be intact.
                Some(mut w) => unsafe { w.as_mut().flush() },
                None => io::stdout().flush(),
            }
        }
    }
    impl<'ctx> io::Read for IoHook<'ctx> {
        fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
            match self.stdin {
                // SAFETY: IoHook's lifetime is <'ctx>, 
                // so exclusive access is present until then,
                // and the data should still be intact.
                Some(mut r) => unsafe { r.as_mut().read(buf) },
                None => io::stdin().read(buf),
            }
        }
    }
}

impl<'ctx> RtContext<'ctx> {
    /// Create a new context.
    pub fn new() -> Self {
        Self::new_with_io(Default::default())
    }

    /// Create a new context, using the provided argument as an IO hook.
    /// 
    /// See [`IoHook`] for more details.
    pub fn new_with_io(io: IoHook<'ctx>) -> Self {
        Self {
            vars: VarContext::new_with_std(),
            rs: Rc::new(ResolveState::new()),
            io
        }
    }
    
    /// Get the resolve state held by this context 
    /// (or panicking if it is accessed while traversing)
    pub fn resolve_state(&mut self) -> &mut ResolveState {
        Rc::get_mut(&mut self.rs)
            .expect("Resolve state was accessed while in traversal")
    }

    /// Compute the given tree, modifying the context if necessary.
    pub fn execute<T: TraverseRt>(&mut self, t: &T) -> RtTraversal<Value> {
        t.traverse_rt(self)
    }

    /// Create a new scope. 
    /// 
    /// As long as the child scope is in use, the original cannot be used.
    pub fn child(&mut self) -> RtContext {
        RtContext {
            vars: self.vars.child(),
            rs: Rc::clone(&self.rs),
            io: self.io.clone()
        }
    }

    /// Create a new scope, branching off of a 
    /// scope that is further up the scope list.
    /// 
    /// This is useful for switching to the lexical scope during
    /// a function call.
    pub fn branch_at(&mut self, idx: usize) -> RtContext {
        let mv = self.vars.goto_idx(idx);

        mv.map(|v| RtContext {
            vars: v.child(),
            rs: Rc::clone(&self.rs),
            io: self.io.clone()
        }).unwrap()
    }

    /// Get the variable specified by the identifier parameter,
    /// with the static resolution for the given expression.
    pub fn get_var(&self, ident: &str, e: &ast::Expr) -> Option<&Value> {
        self.vars.get_indexed(ident, self.rs.get_steps(e))
    }

    /// Set the variable specified by the identifier parameter,
    /// with the static resolution for the given expression.
    pub fn set_var(&mut self, ident: &str, v: Value, e: &ast::Expr) -> BasicRtResult<Value> {
        self.vars.set_indexed(ident, v, self.rs.get_steps(e))
    }
}

impl Default for RtContext<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl Located<ast::Expr> {
    /// Evaluate an expression and then apply the unary operator for it.
    pub fn apply_unary(&self, o: op::Unary, ctx: &mut RtContext, range: CursorRange) -> RtTraversal<Value> {
        let e = self.traverse_rt(ctx)?;
        e.apply_unary(o)
            .map_err(|e| e.at_range(range))
            .map_err(Into::into)
    }

    /// Evaluate the two arguments to the binary operator and then apply the operator to it.
    /// 
    /// If the operator is `&&` or `||`, the evaluation can be short-circuited.
    pub fn apply_binary(&self, o: op::Binary, right: &Self, ctx: &mut RtContext, range: CursorRange) -> RtTraversal<Value> {
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
            _ => {
                let left = self.traverse_rt(ctx)?;
                let right = right.traverse_rt(ctx)?;

                left.apply_binary(o, right)
                    .map_err(|e| e.at_range(range))
                    .map_err(Into::into)
            }
        }
    }
}

impl ast::Program {
    /// Executes the program and returns the output.
    /// 
    /// This first resolves the program via the 
    /// [`semantic`][`crate::interpreter::semantic`] module,
    /// then executes the program in runtime with the
    /// [`runtime`][`crate::interpreter::runtime`] module.
    pub fn run(&self) -> RtResult<Value> {
        self.run_with_ctx(&mut RtContext::new())
    }

    /// Executes the program with a predefined variable context.
    /// 
    /// This first resolves the program via the 
    /// [`semantic`][`crate::interpreter::semantic`] module,
    /// then executes the program in runtime with the
    /// [`runtime`][`crate::interpreter::runtime`] module.
    pub fn run_with_ctx(&self, ctx: &mut RtContext) -> RtResult<Value> {
        let rs = ctx.resolve_state();
        rs.clear();
        rs.traverse(self)?;

        ctx.execute(self).map_err(TermOp::flatten)
    }
}

/// Runtime errors.
/// 
/// The most general error is [`RuntimeErr`].
pub mod err {
    use crate::ast::op;

    use super::value::ValueType;
    use super::value::RvErr;
    use super::super::semantic::ResolveErr;
    use crate::GonErr;
    use crate::err::full_gon_cast_impl;

    use std::io::Error as IoErr;

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

                full_gon_cast_impl!($e, RuntimeErr);
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

    rt_err! { TypeErr, ValueErr, NameErr, ResolveErr, RvErr, FeatureErr, IoErr }

    /// An error caused by type mismatches
    #[derive(Debug)]
    pub enum TypeErr {
        /// The unary operator cannot be applied to this type.
        CannotUnary(op::Unary, ValueType),

        /// The binary operator cannot be applied between these two types.
        CannotBinary(op::Binary, ValueType, ValueType),

        /// These two types can't be compared using the given operation.
        CannotCmp(op::Cmp, ValueType, ValueType),
        
        /// Cannot compute a range between these two types.
        CannotRange(ValueType, ValueType),

        /// Cannot iterate over this type.
        NotIterable(ValueType),

        /// Cannot unpack this pattern because the corresponding value is not iterable.
        NotUnpackable(ValueType),

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
                TypeErr::CannotCmp(op, t1, t2) => format!("cannot compare '{op}' between {t1} and {t2}"),
                TypeErr::CannotUnary(op, t1) => format!("cannot apply '{op}' to {t1}"),
                TypeErr::CannotBinary(op, t1, t2) => format!("cannot apply '{op}' to {t1} and {t2}"),
                TypeErr::CannotRange(t1, t2) => format!("cannot create range {t1}..{t2}"),
                TypeErr::NotIterable(t1) => format!("{t1} is not iterable"),
                TypeErr::NotUnpackable(t1) => format!("{t1} cannot be assigned to this pattern"),
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

    impl GonErr for IoErr {
        fn err_name(&self) -> &'static str {
            "io error"
        }

        fn message(&self) -> String {
            self.to_string()
        }
    }
}
use err::*;

type FullRuntimeErr = FullGonErr<RuntimeErr>;
pub(super) type BasicRtResult<T> = Result<T, RuntimeErr>;
/// A [`Result`] type for operations in runtime execution. 
/// 
/// This is for functions that compute during runtime. 
/// For operations whose normal runtime flow may be interrupted
/// (where `return`, `break`, and `continue` have observable effects),
/// see [`RtTraversal`].
pub type RtResult<T> = Result<T, FullRuntimeErr>;

/// Operations that result in interruption of normal program flow.
pub enum TermOp<R, E> {
    /// An error occurred (This is propagated to the program).
    Err(E),

    /// A value was returned (This is propagated to the top of the function block).
    Return(R),

    /// `break` was called (This is propagated to the top of the loop block).
    Break,

    /// `continue` was called (This is propagated to the top of the loop block).
    Continue
}

impl<T, E: Into<FullRuntimeErr>> From<E> for TermOp<T, FullRuntimeErr> {
    fn from(e: E) -> Self {
        TermOp::Err(e.into())
    }
}
impl<T, E: Into<RuntimeErr>> From<E> for TermOp<T, RuntimeErr> {
    fn from(e: E) -> Self {
        TermOp::Err(e.into())
    }
}
/// An evaluation type for operations in runtime that may
/// have their normal runtime flow broken.
/// 
/// If the function's flow is interrupted, [`TermOp`] is returned.
pub type RtTraversal<T> = Result<T, TermOp<T, FullRuntimeErr>>;

/// This trait is implemented for values that can be traversed in runtime.
/// 
/// A traversal can be initiated either with the [`TraverseRt::traverse_rt`] method 
/// or [`RtContext::execute`].
pub trait TraverseRt {
    /// Evaluate this node in runtime, possibly creating side effects.
    fn traverse_rt(&self, ctx: &mut RtContext) -> RtTraversal<Value>;
}

impl TraverseRt for Located<ast::Expr> {
    fn traverse_rt(&self, ctx: &mut RtContext) -> RtTraversal<Value> {
        let Located(expr, range) = self;
        let range = range.clone();

        match expr {
            ast::Expr::Ident(ident) => {
                ctx.get_var(ident, self)
                    .ok_or_else(|| NameErr::UndefinedVar(String::from(ident)).at_range(range).into())
                    .map(Value::clone)
                    .map_err(TermOp::Err)
            },
            ast::Expr::Block(e) => e.traverse_rt(&mut ctx.child()),
            ast::Expr::Literal(e) => e.traverse_rt(ctx),
            ast::Expr::ListLiteral(exprs) => {
                let mut vec = vec![];
                
                for e in exprs.iter() {
                    match &**e {
                        ast::Expr::Spread(inner) => {
                            let inner = inner.as_ref()
                                .ok_or_else(|| {
                                    ResolveErr::CannotSpreadNone.at_range(e.range())
                                })?
                                .traverse_rt(ctx)?;

                            let it = inner.as_iterator()
                                .ok_or_else(|| {
                                    TypeErr::NotIterable(inner.ty()).at_range(e.range())
                                })?;

                            vec.extend(it);
                        },
                        _ => vec.push(e.traverse_rt(ctx)?)
                    }
                }

                Ok(Value::new_list(vec))
            },
            ast::Expr::SetLiteral(_) => Err(FeatureErr::Incomplete("sets").at_range(range))?,
            ast::Expr::DictLiteral(_) => Err(FeatureErr::Incomplete("dicts").at_range(range))?,
            ast::Expr::ClassLiteral(_, _) => Err(FeatureErr::CompilerOnly("classes").at_range(range))?,
            ast::Expr::Assign(pat, expr) => {
                let result = expr.traverse_rt(ctx)?;
                
                assign_pat(pat, result, ctx, self)
                    .map_err(TermOp::Err)
            },
            ast::Expr::Path(_) => Err(FeatureErr::Incomplete("general attribute functionality").at_range(range))?,
            ast::Expr::StaticPath(_) => Err(FeatureErr::Incomplete("general attribute functionality").at_range(range))?,
            ast::Expr::UnaryOps { ops, expr } => {
                let mut ops_iter = ops.iter().rev();
        
                // ops should always have at least 1 unary op, so this should always be true
                let mut e = if let Some(&op) = ops_iter.next() {
                    expr.apply_unary(op, ctx, range)
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
            ast::Expr::BinaryOp { op, left, right } => left.apply_binary(*op, right, ctx, range),
            ast::Expr::Comparison { left, rights } => {
                let mut lval = left.traverse_rt(ctx)?;
                // for cmp a < b < c < d < e,
                // break it up into a < b && b < c && c < d && d < e
                // do each comparison. if any ever returns false, short circuit and return
                for (cmp, rexpr) in rights {
                    let rval = rexpr.traverse_rt(ctx)?;

                    let cmp_result = match lval.apply_cmp(*cmp, &rval) {
                        Ok(t) => t,
                        Err(e) => return Err(e.at_range(range).into()),
                    };
                    if cmp_result {
                        lval = rval;
                    } else {
                        return Ok(Value::Bool(false));
                    }
                }

                Ok(Value::Bool(true))
            },
            ast::Expr::Range { left, right, step: step_expr } => {
                let (l, r) = (left.traverse_rt(ctx)?, right.traverse_rt(ctx)?);
                
                let step = match step_expr {
                    Some(e) => match e.traverse_rt(ctx)? {
                        Value::Int(s) => s,
                        _ => Err(TypeErr::ExpectedType(ValueType::Int).at_range(e.range()))?
                    },
                    None => 1,
                };

                match (&l, &r) {
                    (Value::Int(a), Value::Int(b)) => {
                        let values = compute_int_range(*a, *b, step, range)?
                            .into_iter()
                            .map(Value::Int)
                            .collect();

                        Ok(Value::new_list(values))
                    },
                    // (a, b @ Value::Float(_)) => compute_float_range(a, b, &step_value),
                    // (a @ Value::Float(_), b) => compute_float_range(a, b, &step_value),
                    (Value::Char(a), Value::Char(b)) => {
                        let (a, b) = (*a as u32, *b as u32);
                        let values = compute_uint_range(a, b, step, range)?
                            .into_iter()
                            .map(|i| {
                                char::from_u32(i)
                                    .unwrap_or_else(|| panic!("u32 '{:x}' could not be parsed as char", i))
                            })
                            .map(Value::Char)
                            .collect();

                        Ok(Value::new_list(values))
                    },
                    _ => Err(TypeErr::CannotRange(l.ty(), r.ty()).at_range(range))?
                }
            },
            ast::Expr::If { conditionals, last } => {
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
            ast::Expr::While { condition, block } => {
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
            ast::Expr::For { ident, iterator, block } => {
                let it_val = iterator.traverse_rt(ctx)?;
                let it = it_val.as_iterator()
                    .ok_or_else(|| {
                        TypeErr::NotIterable(it_val.ty()).at_range(iterator.range())
                    })?;

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
            ast::Expr::Call { funct, params } => {
                match &***funct {
                    // HACK: generalize methods
                    e @ ast::Expr::Path(ast::Path { obj, attrs }) => {
                        if let this @ Value::List(_) = obj.traverse_rt(ctx)? {
                            if attrs.len() == 1 {
                                let property = &attrs[0];
                                let mut call_params = vec![this];
                                call_params.extend(params.iter()
                                    .map(|e| e.traverse_rt(ctx))
                                    .collect::<Result<Vec<_>, _>>()?
                                );

                                match property.as_str() {
                                    "contains" => {
                                        let var = ctx.get_var("in", e).cloned();
                                        if let Some(Value::Fun(f)) = var {
                                            return f.call_computed(call_params, ctx, funct.range())
                                        }
                                    },
                                    "push" => {
                                        let var = ctx.get_var("@@push", e).cloned();
                                        if let Some(Value::Fun(f)) = var {
                                            return f.call_computed(call_params, ctx, funct.range())
                                        }
                                    },
                                    "pop" => {
                                        let var = ctx.get_var("@@pop", e).cloned();
                                        if let Some(Value::Fun(f)) = var {
                                            return f.call_computed(call_params, ctx, funct.range())
                                        }
                                    },
                                    _ => {}
                                }
                            }
                        }
                        
                        Err(FeatureErr::Incomplete("general attribute functionality").at_range(funct.range()))?
                    },
                    _ => match funct.traverse_rt(ctx)? {
                        Value::Fun(f) => f.call(params, ctx, funct.range()),
                        val => Err(TypeErr::CannotCall(val.ty()).at_range(funct.range()))?
                    }
                }
            }
            ast::Expr::Index(idx) => {
                let ast::Index {expr, index} = idx;
                let val = expr.traverse_rt(ctx)?;
                let index_val = index.traverse_rt(ctx)?;

                val.get_index(index_val)
                    .map_err(|e| e.at_range(range))
                    .map_err(TermOp::Err)
            },
            ast::Expr::Spread(_) => Err(ResolveErr::CannotSpread.at_range(range))?,
            ast::Expr::Deref(_) => Err(FeatureErr::CompilerOnly("intrinsic dereferencing").at_range(range))?,
        }
    }
}

fn compute_int_range(left: isize, right: isize, step: isize, token_range: CursorRange) -> RtResult<Vec<isize>>
{
    if step == 0 { Err(ValueErr::RangeIsInfinite.at_range(token_range))? }
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
fn compute_uint_range(left: u32, right: u32, step: isize, token_range: CursorRange) -> RtResult<Vec<u32>>
{
    if step == 0 { Err(ValueErr::RangeIsInfinite.at_range(token_range))? }
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

impl<T> TermOp<T, FullRuntimeErr> {
    fn flatten(self) -> FullRuntimeErr {
        match self {
            TermOp::Err(e)    => e,
            TermOp::Return(_) => RuntimeErr::from(ResolveErr::CannotReturn).at_unknown(),
            TermOp::Break     => RuntimeErr::from(ResolveErr::CannotBreak).at_unknown(),
            TermOp::Continue  => RuntimeErr::from(ResolveErr::CannotContinue).at_unknown(),
        }
    }
}

impl TraverseRt for ast::Literal {
    fn traverse_rt(&self, _ctx: &mut RtContext) -> RtTraversal<Value> {
        Ok(self.clone().into())
    }
}

impl TraverseRt for Vec<Located<ast::Stmt>> {
    fn traverse_rt(&self, ctx: &mut RtContext) -> RtTraversal<Value> {
        match self.split_last() {
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
impl TraverseRt for ast::Program {
    fn traverse_rt(&self, ctx: &mut RtContext) -> RtTraversal<Value> {
        self.0.traverse_rt(ctx)
    }
}
impl TraverseRt for ast::Block {
    fn traverse_rt(&self, ctx: &mut RtContext) -> RtTraversal<Value> {
        self.0.traverse_rt(ctx)
    }
}

impl TraverseRt for Located<ast::Stmt> {
    fn traverse_rt(&self, ctx: &mut RtContext) -> RtTraversal<Value> {
        let Located(stmt, range) = self;
        let range = range.clone();

        match stmt {
            ast::Stmt::Decl(dcl) => dcl.traverse_rt(ctx),
            ast::Stmt::Return(mt) => {
                let expr = mt.as_ref()
                    .map_or(
                        Ok(Value::Unit), 
                        |t| t.traverse_rt(ctx)
                    )?;
                
                Err(TermOp::Return(expr))
            },
            ast::Stmt::Break     => Err(TermOp::Break),
            ast::Stmt::Continue  => Err(TermOp::Continue),
            ast::Stmt::FunDecl(dcl) => dcl.traverse_rt(ctx),
            ast::Stmt::ExternFunDecl(_) => Err(FeatureErr::CompilerOnly("extern function declarations").at_range(range))?,
            ast::Stmt::Expr(e) => e.traverse_rt(ctx),
            ast::Stmt::ClassDecl(_) => Err(FeatureErr::CompilerOnly("classes").at_range(range))?,
            ast::Stmt::Import(_) => Err(ResolveErr::CompilerOnly("importing").at_range(range))?,
            ast::Stmt::ImportIntrinsic => Err(ResolveErr::CompilerOnly("intrinsics").at_range(range))?,
            ast::Stmt::IGlobal(_, _) => Err(ResolveErr::CompilerOnly("intrinsics").at_range(range))?,
        }
    }
}

impl TraverseRt for ast::Decl {
    fn traverse_rt(&self, ctx: &mut RtContext) -> RtTraversal<Value> {
        let rhs = self.val.traverse_rt(ctx)?;
        
        declare_pat(&self.pat, rhs, ctx, self.rt)
            .map_err(TermOp::Err)
    }
}

impl TraverseRt for ast::FunDecl {
    fn traverse_rt(&self, ctx: &mut RtContext) -> RtTraversal<Value> {
        let ast::FunDecl { sig: ast::FunSignature { ident, params, varargs, ret }, block } = self;
        
        if *varargs {
            let &pt = block.1.start();
            return Err(FeatureErr::Incomplete("varargs").at(pt))?
        }

        let mut param_types = vec![];
        let mut param_names = vec![];

        for p in params {
            let ast::Param { ident, ty, .. } = p;

            param_types.push(
                ty.as_deref()
                    .map_or(VArbType::Unk, VArbType::lookup)
            );
            param_names.push(ident.clone());
        }

        let p = FunParamType::Positional(param_types);
        let r = ret.as_deref()
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
        
        let rf = ctx.vars.declare(ident.to_string(), val)?.clone();
        Ok(rf)
    }
}

/// Given a pattern and a value, traverse the pattern
/// and apply the mapper function to the individual units.
fn unpack_pat<T>(
    pat: &Located<ast::Pat<T>>, 
    rhs: Value, 
    mut unit_mapper: impl FnMut(Located<&T>, Value) -> RtResult<Value>,
) -> RtResult<Value> {
    return unpack_mut(pat, rhs, &mut unit_mapper);

    /// Unpack with a mutable reference to a closure.
    /// This is necessary because the mutable reference is passed several times.
    fn unpack_mut<T, F>(pat: &Located<ast::Pat<T>>, rhs: Value, unit_mapper: &mut F) -> RtResult<Value>
        where F: FnMut(Located<&T>, Value) -> RtResult<Value>
    {
        let Located(pat, pat_range) = pat;
        let pat_range = pat_range.clone();
        match pat {
            ast::Pat::Unit(unit) => unit_mapper(Located::new(unit, pat_range), rhs),
            ast::Pat::List(pats) => {
                let mut it = rhs.as_iterator()
                    .ok_or_else(|| TypeErr::NotUnpackable(rhs.ty()).at_range(pat_range.clone()))?;

                let has_spread = pats.iter().any(|p| matches!(&**p, ast::Pat::Spread(_)));
                let mut left_values = vec![];
                let mut right_values = vec![];
                let mut consumed = 0;

                for (i, p) in pats.iter().enumerate() {
                    if matches!(&**p, ast::Pat::Spread(_)) {
                        consumed = i;
                        break;
                    } else if let Some(t) = it.next() {
                        left_values.push(t);
                    } else {
                        // we ran out of elements:
                        let err = if has_spread {
                            ValueErr::UnpackTooLittleS(pats.len() - 1, i).at_range(pat_range)
                        } else {
                            ValueErr::UnpackTooLittle(pats.len(), i).at_range(pat_range)
                        };
                        return Err(err.into());
                    };
                }

                if has_spread {
                    // do the reverse pass:
                    for (i, p) in std::iter::zip(consumed.., pats.iter().rev()) {
                        if matches!(&**p, ast::Pat::Spread(_)) {
                            break;
                        } else if let Some(t) = it.next_back() {
                            right_values.push(t);
                        } else {
                            // we ran out of elements:
                            let err = ValueErr::UnpackTooLittleS(pats.len() - 1, i).at_range(pat_range);
                            return Err(err.into());
                        };
                    }
                } else {
                    // confirm no extra elements:
                    if it.next().is_some() {
                        Err(ValueErr::UnpackTooMany(pats.len()).at_range(pat_range))?
                    }
                }
                
                let middle_values = Value::new_list(it.collect());

                let val_chain = left_values.into_iter()
                    .chain(std::iter::once(middle_values))
                    .chain(right_values);
                
                for (pat, val) in std::iter::zip(pats, val_chain) {
                    unpack_mut(pat, val, unit_mapper)?;
                }
                
                Ok(rhs)
            },
            ast::Pat::Spread(mp) => match mp {
                Some(p) => unpack_mut(p, rhs, unit_mapper),
                None => Ok(rhs), // we can dispose if spread is None, TODO!: what does a no-spread return?
            },
        }
    }
}

fn assign_pat(pat: &ast::AsgPat, rhs: Value, ctx: &mut RtContext, from: &ast::Expr) -> RtResult<Value> {
    unpack_pat(pat, rhs, |value, rhs| {
        let Located(unit, range) = value;
        match unit {
            ast::AsgUnit::Ident(ident) => {
                ctx.set_var(ident, rhs, from)
                    .map_err(|e| e.at_range(range))
            },
            ast::AsgUnit::Path(_) => Err(FeatureErr::Incomplete("general attribute functionality").at_range(range))?,
            ast::AsgUnit::Index(idx) => {
                let ast::Index {expr, index} = idx;
                
                let mut val = expr.traverse_rt(ctx).map_err(TermOp::flatten)?;
                let index_val = index.traverse_rt(ctx).map_err(TermOp::flatten)?;
        
                val.set_index(index_val, rhs)
                    .map_err(|e| e.at_range(range))
            },
            ast::AsgUnit::Deref(_) => Err(FeatureErr::CompilerOnly("intrinsic dereferencing").at_range(range))?,
        }
    })
}

fn declare_pat(pat: &ast::DeclPat, rhs: Value, ctx: &mut RtContext, rt: ast::ReasgType) -> RtResult<Value> {
    unpack_pat(pat, rhs, |Located(ast::DeclUnit(ident, mt), range), rhs| {
        match ctx.vars.declare_full(String::from(ident), rhs, rt, *mt) {
            Ok(t) => Ok(t.clone()),
            Err(e) => Err(e.at_range(range)),
        }
    })
}