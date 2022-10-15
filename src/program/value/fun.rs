use crate::{BlockContext, TraverseRt};
use crate::program::{RtResult, tree, RuntimeErr};

use super::{VArbType, ValueType, Value};

#[derive(PartialEq, Clone, Debug)]
pub struct GonFun {
    pub(super) ident: Option<String>,
    pub(super) ty: FunType,
    pub(super) fun: fn(Vec<Value>) -> RtResult<Value>
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct FunType(pub(super) Box<FunParams>, pub(super) Box<ValueType>);
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum FunParams {
    Positional(Vec<VArbType>), // (int, int, int) -> ..
    PosSpread(Vec<VArbType>, VArbType) // (int, int, ..int) -> ..
}

impl GonFun {
    pub fn arity(&self) -> Option<usize> {
        let FunType(params, _) = &self.ty;
        match &**params {
            FunParams::Positional(p) => Some(p.len()),
            FunParams::PosSpread(_, _) => None,
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

        (self.fun)(pvals)
    }
}