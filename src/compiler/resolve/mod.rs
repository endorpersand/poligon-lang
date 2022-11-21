use crate::tree;

pub mod plir;

trait TraversePLIR {
    type Output;

    fn traverse_plir(self) -> Self::Output;
}

impl TraversePLIR for tree::Program {
    type Output = plir::Program;

    fn traverse_plir(self) -> Self::Output {
        plir::Program(self.0.traverse_plir())
    }
}

impl TraversePLIR for tree::Block {
    type Output = plir::Block;

    fn traverse_plir(self) -> Self::Output {
        todo!()
    }
}

impl TraversePLIR for tree::Stmt {
    type Output = plir::Stmt;

    fn traverse_plir(self) -> Self::Output {
        match self {
            tree::Stmt::Decl(d)    => plir::Stmt::Decl(d.traverse_plir()),
            tree::Stmt::Return(me) => plir::Stmt::Return(me.map(TraversePLIR::traverse_plir)),
            tree::Stmt::Break      => plir::Stmt::Break,
            tree::Stmt::Continue   => plir::Stmt::Continue,
            tree::Stmt::FunDecl(f) => plir::Stmt::FunDecl(f.traverse_plir()),
            tree::Stmt::Expr(e)    => plir::Stmt::Expr(e.traverse_plir()),
        }
    }
}

impl TraversePLIR for tree::Decl {
    type Output = plir::Decl;

    fn traverse_plir(self) -> Self::Output {
        todo!()
    }
}

impl TraversePLIR for tree::FunDecl {
    type Output = plir::FunDecl;

    fn traverse_plir(self) -> Self::Output {
        todo!()
    }
}

impl TraversePLIR for tree::Expr {
    type Output = plir::Expr;

    fn traverse_plir(self) -> Self::Output {
        match self {
            tree::Expr::Ident(_) => todo!(),
            tree::Expr::Block(_) => todo!(),
            tree::Expr::Literal(_) => todo!(),
            tree::Expr::ListLiteral(_) => todo!(),
            tree::Expr::SetLiteral(_) => todo!(),
            tree::Expr::DictLiteral(_) => todo!(),
            tree::Expr::Assign(_, _) => todo!(),
            tree::Expr::Path(_) => todo!(),
            tree::Expr::UnaryOps { ops, expr } => todo!(),
            tree::Expr::BinaryOp { op, left, right } => todo!(),
            tree::Expr::Comparison { left, rights } => todo!(),
            tree::Expr::Range { left, right, step } => todo!(),
            tree::Expr::If { conditionals, last } => todo!(),
            tree::Expr::While { condition, block } => todo!(),
            tree::Expr::For { ident, iterator, block } => todo!(),
            tree::Expr::Call { funct, params } => todo!(),
            tree::Expr::Index(_) => todo!(),
            tree::Expr::Spread(_) => todo!(),
        }
    }
}