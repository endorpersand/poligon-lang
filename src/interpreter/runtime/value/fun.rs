use std::rc::Rc;

use crate::interpreter::{BlockContext, TraverseRt, tree};
use crate::interpreter::runtime::{RtResult, RtTraversal, TermOp, ValueErr, ResolveErr};

use super::{VArbType, Value};

#[derive(PartialEq, Clone, Debug)]
pub struct GonFun {
    pub(super) ident: Option<String>,
    pub(super) ty: FunType,
    pub(super) fun: GInternalFun
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct FunType(pub(super) Box<FunParamType>, pub(super) Box<VArbType>);
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum FunParamType {
    Positional(Vec<VArbType>), // (int, int, int) -> ..
    PosSpread(Vec<VArbType>, VArbType) // (int, int, ..int) -> ..
}

#[derive(PartialEq, Clone, Debug)]
pub(super) enum GInternalFun {
    Rust(fn(Vec<Value>) -> RtResult<Value>),
    Poligon(Vec<String>, Rc<tree::Block>, usize /* scope idx */)
}

impl GonFun {
    pub fn arity(&self) -> Option<usize> {
        let FunType(params, _) = &self.ty;
        match &**params {
            FunParamType::Positional(p) => Some(p.len()),
            FunParamType::PosSpread(_, _) => None,
        }
    }

    pub fn call(&self, params: &[tree::Expr], ctx: &mut BlockContext) -> RtTraversal<Value> {
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

    pub fn call_computed(&self, pvals: Vec<Value>, ctx: &mut BlockContext) -> RtTraversal<Value> {
        // check if arity matches
        if let Some(arity) = self.arity() {
            if pvals.len() != arity {
                Err(ValueErr::WrongArity(arity))?;
            }
        }

        match &self.fun {
            GInternalFun::Rust(f) => (f)(pvals).map_err(TermOp::Err),
            GInternalFun::Poligon(params, block, idx) => {
                let mut fscope = ctx.fun_body_scope_at(*idx);
                
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
    pub fn new(params: FunParamType, ret: VArbType) -> Self {
        Self (
            Box::new(params),
            Box::new(ret)
        )
    }
}

macro_rules! fun_type {
    (($($e:expr),*) -> $r:expr) => {
        FunType::new(FunParamType::Positional(vec![$($e),*]), $r)
    };
    (($($($e:expr),+,)? ~$f:expr) -> $r:expr) => {
        FunType::new(FunParamType::PosSpread(vec![$($($e),+)?], $f), $r)
    };
}
pub(crate) use fun_type;