pub(crate) mod tree;

pub enum Value {
    Int(isize),
    Float(f64),
    Char(char),
    Str(String),
    Bool(bool)
}

impl Value {
    /// Truthiness of a value: when it is cast to bool, what truth value should it have?
    /// 
    /// Numerics: Non-zero => true
    /// Collections: Non-empty => true
    fn truth(&self) -> bool {
        match self {
            Value::Int(v) => v != &0,
            Value::Float(v) => v != &0.0,
            Value::Char(_) => true,
            Value::Str(v) => !v.is_empty(),
            Value::Bool(v) => *v
        }
    }
}

impl From<tree::Literal> for Value {
    fn from(literal: tree::Literal) -> Self {
        match literal {
            tree::Literal::Int(v)   => Value::Int(v),
            tree::Literal::Float(v) => Value::Float(v),
            tree::Literal::Char(v)  => Value::Char(v),
            tree::Literal::Str(v)   => Value::Str(v),
            tree::Literal::Bool(v)  => Value::Bool(v),
        }
    }
}
pub enum RuntimeErr {

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
            tree::Expr::ListLiteral(_) => todo!(),
            tree::Expr::SetLiteral(_) => todo!(),
            tree::Expr::DictLiteral(_) => todo!(),
            tree::Expr::Assignment(_, _) => todo!(),
            tree::Expr::Attr(_) => todo!(),
            tree::Expr::StaticAttr(_) => todo!(),
            tree::Expr::UnaryOps(o) => o.traverse_rt(),
            tree::Expr::BinaryOp(o) => o.traverse_rt(),
            tree::Expr::Comparison { left, right, extra } => todo!(),
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

        let e = expr.traverse_rt()?;
        todo!();
    }
}

impl TraverseRt for tree::BinaryOp {
    fn traverse_rt(&self) -> TRReturn {
        let tree::BinaryOp { op, left, right } = self;

        let l = left.traverse_rt()?;
        let r = right.traverse_rt()?;
        todo!();
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