use crate::program::tree::op::UnaryApplicable;

use self::tree::op::{self, CmpApplicable, BinaryApplicable};
use self::value::Value;

pub(crate) mod tree;
pub(crate) mod value;

#[derive(Debug)]
pub enum RuntimeErr {
    CannotCompare(op::Cmp, String, String),
    CannotApplyUnary(op::Unary, String),
    CannotApplyBinary(op::Binary, String, String),
    DivisionByZero
}

type TRReturn = Result<Value, RuntimeErr>;
pub trait TraverseRt {
    fn traverse_rt(&self) -> TRReturn;
}

impl TraverseRt for tree::Expr {
    fn traverse_rt(&self) -> TRReturn {
        match self {
            tree::Expr::Ident(_) => todo!(),
            tree::Expr::Block(e) => e.traverse_rt(),
            tree::Expr::Literal(e) => e.traverse_rt(),
            tree::Expr::ListLiteral(e) => {
                let vec = e.iter()
                    .map(TraverseRt::traverse_rt)
                    .collect::<Result<_, _>>()?;

                Ok(Value::List(vec))
            },
            tree::Expr::SetLiteral(_) => todo!(),
            tree::Expr::DictLiteral(_) => todo!(),
            tree::Expr::Assignment(_, _) => todo!(),
            tree::Expr::Attr(_) => todo!(),
            tree::Expr::StaticAttr(_) => todo!(),
            tree::Expr::UnaryOps(o) => o.traverse_rt(),
            tree::Expr::BinaryOp(o) => o.traverse_rt(),
            tree::Expr::Comparison { left, right, extra } => {
                let mut cmps = vec![right];
                cmps.extend(extra);

                let mut lval = left.traverse_rt()?;
                // for cmp a < b < c < d < e,
                // break it up into a < b && b < c && c < d && d < e
                // do each comparison. if any ever returns false, short circuit and return
                for (cmp, rexpr) in cmps {
                    let rval = rexpr.traverse_rt()?;

                    if lval.apply_cmp(cmp, &rval)? {
                        lval = rval;
                    } else {
                        return Ok(Value::Bool(false));
                    }
                }

                Ok(Value::Bool(true))
            },
            tree::Expr::Range { left, right, step } => todo!(),
            tree::Expr::If(e) => e.traverse_rt(),
            tree::Expr::While { condition, block } => todo!(),
            tree::Expr::For { ident, iterator, block } => todo!(),
        }
    }
}

impl TraverseRt for tree::Literal {
    fn traverse_rt(&self) -> TRReturn {
        Ok(self.clone().into())
    }
}

impl TraverseRt for tree::UnaryOps {
    fn traverse_rt(&self) -> TRReturn {
        let tree::UnaryOps {ops, expr} = self;

        let mut ops_iter = ops.iter().rev();
        
        // ops should always have at least 1 unary op, so this should always be true
        let mut e = if let Some(op) = ops_iter.next() {
            expr.apply_unary(op)
        } else {
            // should never happen, but in case it does
            expr.traverse_rt()
        }?;

        // apply the rest:
        for op in ops_iter {
            e = e.apply_unary(op)?;
        }

        Ok(e)
    }
}

impl TraverseRt for tree::BinaryOp {
    fn traverse_rt(&self) -> TRReturn {
        let tree::BinaryOp { op, left, right } = self;

        left.apply_binary(op, right)
    }
}

impl TraverseRt for tree::Program {
    fn traverse_rt(&self) -> TRReturn {
        todo!()
    }
}

impl TraverseRt for tree::If {
    fn traverse_rt(&self) -> TRReturn {
        let tree::If { condition, if_true, if_false } = self;

        if condition.traverse_rt()?.truth() {
            if_true.traverse_rt()
        } else {
            // if this is none, what should this return?
            // a Value::Unit? None?
            todo!()
        }
    }
}

impl TraverseRt for tree::Else {
    fn traverse_rt(&self) -> TRReturn {
        match self {
            tree::Else::If(e) => e.traverse_rt(),
            tree::Else::Block(e) => e.traverse_rt(),
        }
    }
}
