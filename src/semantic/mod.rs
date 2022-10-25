use crate::tree;

pub trait TraverseResolve {
    fn traverse_rs(&self) -> ();
}

impl<T: TraverseResolve> TraverseResolve for [T] {
    fn traverse_rs(&self) -> () {
        for t in self {
            t.traverse_rs()
        }
    }
}
impl<T: TraverseResolve> TraverseResolve for Option<T> {
    fn traverse_rs(&self) -> () {
        if let Some(t) = self {
            t.traverse_rs()
        }
    }
}

impl TraverseResolve for tree::Program {
    fn traverse_rs(&self) -> () {
        self.0.traverse_rs()
    }
}

impl TraverseResolve for tree::Stmt {
    fn traverse_rs(&self) -> () {
        match self {
            tree::Stmt::Decl(d) => d.traverse_rs(),
            tree::Stmt::Return(e) => e.traverse_rs(),
            tree::Stmt::Break => (),
            tree::Stmt::Continue => (),
            tree::Stmt::FunDecl(f) => f.traverse_rs(),
            tree::Stmt::Expr(e) => e.traverse_rs(),
        }
    }
}

impl TraverseResolve for tree::Expr {
    fn traverse_rs(&self) -> () {
        match self {
            tree::Expr::Ident(_) => todo!(),
            tree::Expr::Block(_) => todo!(),
            tree::Expr::Literal(_) => todo!(),
            tree::Expr::ListLiteral(l) => l.traverse_rs(),
            tree::Expr::SetLiteral(s) => s.traverse_rs(),
            tree::Expr::DictLiteral(d) => {
                for (k, v) in d {
                    k.traverse_rs();
                    v.traverse_rs();
                }
            },
            tree::Expr::Assign(_, _) => todo!(),
            tree::Expr::Attr(_) => todo!(),
            tree::Expr::StaticAttr(_) => todo!(),
            tree::Expr::UnaryOps(op) => op.traverse_rs(),
            tree::Expr::BinaryOp(op) => op.traverse_rs(),
            tree::Expr::Comparison { left, rights } => {
                left.traverse_rs();
                for (_, e) in rights {
                    e.traverse_rs();
                }
            },
            tree::Expr::Range { left, right, step } => {
                left.traverse_rs();
                right.traverse_rs();
                if let Some(t) = step {
                    t.traverse_rs();
                }
            },
            tree::Expr::If(e) => e.traverse_rs(),
            tree::Expr::While { condition, block } => {
                condition.traverse_rs();
                block.traverse_rs();
            },
            tree::Expr::For { ident, iterator, block } => {
                iterator.traverse_rs();
                block.traverse_rs();
            },
            tree::Expr::Call { funct, params } => {
                funct.traverse_rs();
                params.traverse_rs();
            },
            tree::Expr::Index(idx) => idx.traverse_rs(),
        }
    }
}

impl TraverseResolve for tree::Decl {
    fn traverse_rs(&self) -> () {
        todo!()
    }
}

impl TraverseResolve for tree::FunDecl {
    fn traverse_rs(&self) -> () {
        todo!()
    }
}

impl TraverseResolve for tree::UnaryOps {
    fn traverse_rs(&self) -> () {
        self.expr.traverse_rs()
    }
}
impl TraverseResolve for tree::BinaryOp {
    fn traverse_rs(&self) -> () {
        self.left.traverse_rs();
        self.right.traverse_rs()
    }
}

impl TraverseResolve for tree::If {
    fn traverse_rs(&self) -> () {
        for (e, p) in &self.conditionals {
            e.traverse_rs();
            p.traverse_rs();
        }

        self.last.traverse_rs()
    }
}

impl TraverseResolve for tree::Index {
    fn traverse_rs(&self) -> () {
        self.expr.traverse_rs();
        self.index.traverse_rs();
    }
}