pub(crate) mod tree;

pub enum Value {
    Int(isize),
    Float(f64),
    Char(char),
    Str(String)
}

pub enum RuntimeErr {

}

trait TraverseRt {
    fn traverse_rt(&self) -> Result<Value, RuntimeErr>;
}

impl TraverseRt for tree::Expr {
    fn traverse_rt(&self) -> Result<Value, RuntimeErr> {
        match self {
            tree::Expr::Ident(_) => todo!(),
            tree::Expr::Block(_) => todo!(),
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
            tree::Expr::If(_) => todo!(),
            tree::Expr::While { condition, block } => todo!(),
            tree::Expr::For { ident, iterator, block } => todo!(),
        }
    }
}

impl TraverseRt for tree::Literal {
    fn traverse_rt(&self) -> Result<Value, RuntimeErr> {
        let val = match self {
            tree::Literal::Int(v) => Value::Int(*v),
            tree::Literal::Float(v) => Value::Float(*v),
            tree::Literal::Char(v) => Value::Char(*v),
            tree::Literal::Str(v) => Value::Str(v.clone()),
        };

        Ok(val)
    }
}

impl TraverseRt for tree::UnaryOps {
    fn traverse_rt(&self) -> Result<Value, RuntimeErr> {
        let tree::UnaryOps {ops, expr} = self;

        let e = expr.traverse_rt()?;
        todo!();
    }
}

impl TraverseRt for tree::BinaryOp {
    fn traverse_rt(&self) -> Result<Value, RuntimeErr> {
        let tree::BinaryOp { op, left, right } = self;

        let l = left.traverse_rt()?;
        let r = right.traverse_rt()?;
        todo!();
    }
}