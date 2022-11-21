use crate::tree;

pub mod plir;

enum PLIRErr {}
type PLIRResult<T> = Result<T, PLIRErr>;
type InsertBlock = Vec<plir::Stmt>;

struct CodeGenerator {
    program: InsertBlock,
    blocks: Vec<InsertBlock>
}

impl CodeGenerator {
    fn new() -> Self {
        Self { program: vec![], blocks: vec![] }
    }
    fn unwrap(self) -> plir::Program {
        plir::Program(self.program)
    }

    fn push_block(&mut self) {
        self.blocks.push(vec![])
    }
    fn pop_block(&mut self) -> Option<InsertBlock> {
        self.blocks.pop()
    }
    fn peek_block(&mut self) -> &mut InsertBlock {
        self.blocks.last_mut().unwrap_or(&mut self.program)
    }
    fn push_stmt(&mut self, stmt: plir::Stmt) {
        self.peek_block().push(stmt);
    }

    fn consume_program(&mut self, prog: tree::Program) -> PLIRResult<()> {
        for stmt in prog.0.0 {
            self.consume_stmt(stmt)?;
        }

        Ok(())
    }

    fn consume_stmt(&mut self, stmt: tree::Stmt) -> PLIRResult<()> {
        match stmt {
            tree::Stmt::Decl(d) => self.consume_decl(d),
            tree::Stmt::Return(me) => {
                let maybe_expr = match me {
                    Some(e) => Some(self.consume_expr(e)?),
                    None => None,
                };
                self.push_stmt(plir::Stmt::Return(maybe_expr));
                Ok(())
            },
            tree::Stmt::Break => {
                self.push_stmt(plir::Stmt::Break);
                Ok(())
            },
            tree::Stmt::Continue => {
                self.push_stmt(plir::Stmt::Continue);
                Ok(())
            },
            tree::Stmt::FunDecl(f) => self.consume_fun_decl(f),
            tree::Stmt::Expr(e) => {
                let e = self.consume_expr(e)?;
                self.push_stmt(plir::Stmt::Expr(e));
                Ok(())
            },
        }
    }

    fn consume_decl(&mut self, decl: tree::Decl) -> PLIRResult<()> {
        todo!()
    }

    fn consume_fun_decl(&mut self, decl: tree::FunDecl) -> PLIRResult<()> {
        todo!()
    }

    fn consume_expr(&mut self, expr: tree::Expr) -> PLIRResult<plir::Expr> {
        match expr {
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