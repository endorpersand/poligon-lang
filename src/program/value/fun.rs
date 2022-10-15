use std::rc::Rc;

use crate::{BlockContext, TraverseRt};
use crate::program::{RtResult, tree, RuntimeErr};

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
    Poligon(Vec<String>, Rc<tree::Program>)
}

impl GonFun {
    pub fn arity(&self) -> Option<usize> {
        let FunType(params, _) = &self.ty;
        match &**params {
            FunParamType::Positional(p) => Some(p.len()),
            FunParamType::PosSpread(_, _) => None,
        }
    }

    pub fn call(&self, params: &Vec<tree::Expr>, ctx: &mut BlockContext) -> RtResult<Value> {
        // TODO, make lazy
        let pvals = params.iter()
            .map(|e| e.traverse_rt(ctx))
            .collect::<Result<Vec<_>, _>>()?;
    
        // check if arity matches
        if let Some(a) = self.arity() {
            if pvals.len() != a {
                return Err(RuntimeErr::WrongArity(a));
            }
        }

        match &self.fun {
            GInternalFun::Rust(f) => (f)(pvals),
            GInternalFun::Poligon(params, block) => {
                let mut fscope = ctx.child(); // TODO: lexical scope
                
                for (ident, v) in std::iter::zip(params, pvals) {
                    fscope.vars.set_top(ident.clone(), v);
                }
                block.traverse_rt(&mut fscope)
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