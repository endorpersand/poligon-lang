use std::io::Write;
use std::rc::Rc;

use crate::interpreter::{RtContext, TraverseRt, ast};
use crate::interpreter::runtime::{RtResult, RtTraversal, TermOp, ValueErr, ResolveErr, rtio};

use super::{VArbType, Value};

/// A function that can be called in Poligon's runtime.
#[derive(PartialEq, Clone, Debug)]
pub struct GonFun {
    pub(super) ident: Option<String>,
    pub(super) ty: FunType,
    pub(super) fun: GInternalFun
}

/// The type of the function. 
/// 
/// This struct includes the parameter types and the return type.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct FunType(pub(super) Box<FunParamType>, pub(super) Box<VArbType>);

/// The types of the parameters of a given function.
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum FunParamType {
    /// There are no spread types in the parameter list
    /// (e.g. `(int, int, int) -> ...`).
    Positional(Vec<VArbType>), // (int, int, int) -> ..
    /// There are spread types in the parameter list
    /// (e.g. `(int, int, ..int[]) -> ...`).
    PosSpread(Vec<VArbType>, VArbType) // (int, int, ..int) -> ..
}

#[derive(PartialEq, Clone, Debug)]
pub(super) enum GInternalFun {
    Rust(fn(Vec<Value>) -> RtResult<rtio::Io<Value>>),
    Poligon(Vec<String>, Rc<ast::Block>, usize /* scope idx */)
}

impl GonFun {
    /// Find the arity of the function.
    /// 
    /// This returns None if there is a spread argument 
    /// (which means that any number of elements are allowed).
    pub fn arity(&self) -> Option<usize> {
        let FunType(params, _) = &self.ty;
        match &**params {
            FunParamType::Positional(p) => Some(p.len()),
            FunParamType::PosSpread(_, _) => None,
        }
    }

    /// Call this function with a list of expressions.
    /// 
    /// In specification, this would only compute the expressions needed for the function.
    /// In implementation, this computes all the expressions before inserting them to the function.
    pub fn call(&self, params: &[ast::Expr], ctx: &mut RtContext) -> RtTraversal<Value> {
        // check if arity matches
        if let Some(arity) = self.arity() {
            if params.len() != arity {
                Err(ValueErr::WrongArity(arity))?;
            }
        }

        // TODO, make lazy
        let pvals: Vec<_> = params.iter()
            .map(|e| e.traverse_rt(ctx))
            .collect::<Result<_, _>>()?;

        self.call_computed(pvals, ctx)
    }

    /// Call this function with a list of computed values.
    pub fn call_computed(&self, pvals: Vec<Value>, ctx: &mut RtContext) -> RtTraversal<Value> {
        // check if arity matches
        if let Some(arity) = self.arity() {
            if pvals.len() != arity {
                Err(ValueErr::WrongArity(arity))?;
            }
        }

        match &self.fun {
            GInternalFun::Rust(f) => {
                let rtio::Io(out, val) = (f)(pvals).map_err(TermOp::Err)?;
                ctx.io.write_all(&out)?;
                Ok(val)
            },
            GInternalFun::Poligon(params, block, idx) => {
                let mut fscope = ctx.branch_at(*idx);
                
                for (ident, v) in std::iter::zip(params, pvals) {
                    fscope.vars.declare(ident.clone(), v)?;
                }
                // traverse through the code.
                // treat return statements as Ok's!
                match block.traverse_rt(&mut fscope) {
                    Ok(t) => Ok(t),
                    Err(TermOp::Return(t)) => Ok(t),
                    Err(TermOp::Err(e))    => Err(e)?,
                    Err(TermOp::Break)     => Err(ResolveErr::CannotBreak)?,
                    Err(TermOp::Continue)  => Err(ResolveErr::CannotContinue)?,
                }
            },
        }
    }
}

impl FunType {
    /// Create a new function type.
    pub fn new(params: FunParamType, ret: VArbType) -> Self {
        Self (
            Box::new(params),
            Box::new(ret)
        )
    }
}

/// A macro that makes the syntax for creating [`FunType`]s simpler.
macro_rules! fun_type {
    (($($e:expr),*) -> $r:expr) => {
        FunType::new(FunParamType::Positional(vec![$($e),*]), $r)
    };
    (($($($e:expr),+,)? ~$f:expr) -> $r:expr) => {
        FunType::new(FunParamType::PosSpread(vec![$($($e),+)?], $f), $r)
    };
}
pub(in crate::interpreter) use fun_type;