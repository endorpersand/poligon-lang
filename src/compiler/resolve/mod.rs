use crate::tree;

pub mod plir;

pub fn codegen(t: tree::Program) -> PLIRResult<plir::Program> {
    let mut cg = CodeGenerator::new();
    cg.consume_program(t)?;
    Ok(cg.unwrap())
}

pub enum PLIRErr {
    ExpectedType(plir::Type /* expected */, plir::Type /* found */)
}
pub type PLIRResult<T> = Result<T, PLIRErr>;
type InsertBlock = Vec<plir::Stmt>;
type PartialDecl = (tree::ReasgType, Option<tree::Type>);

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

    fn create_decl(&mut self, pd: PartialDecl, pat: tree::DeclPat, e: plir::Expr) -> PLIRResult<()> {
        let (rt, mty) = pd;
        match pat {
            tree::Pat::Unit(unit) => match unit {
                tree::DeclUnit::Ident(ident, mt) => {
                    self.push_stmt(plir::Stmt::Decl(plir::Decl {
                        rt,
                        mt,
                        ident,
                        ty: mty.map_or_else(|| e.ty.clone(), plir::Type::from),
                        val: e,
                    }));

                    Ok(())
                },
                tree::DeclUnit::Expr(_) => todo!(),
            },
            tree::Pat::Spread(_) => todo!(),
            tree::Pat::List(_) => todo!(),
        }
    }
    fn consume_decl(&mut self, decl: tree::Decl) -> PLIRResult<()> {
        let tree::Decl { rt, pat, ty, val } = decl;

        match pat {
            tree::Pat::Unit(_) => todo!(),
            tree::Pat::Spread(_) => todo!(),
            tree::Pat::List(_) => todo!(),
        }
    }

    fn consume_fun_decl(&mut self, decl: tree::FunDecl) -> PLIRResult<()> {
        todo!()
    }

    fn consume_expr(&mut self, expr: tree::Expr) -> PLIRResult<plir::Expr> {
        match expr {
            tree::Expr::Ident(_) => todo!(),
            tree::Expr::Block(_) => todo!(),
            tree::Expr::Literal(literal) => {
                let expr = match literal {
                    tree::Literal::Int(_)   => plir::Expr::new(plir::Type::int(),   plir::ExprType::Literal(literal)),
                    tree::Literal::Float(_) => plir::Expr::new(plir::Type::float(), plir::ExprType::Literal(literal)),
                    tree::Literal::Char(_)  => plir::Expr::new(plir::Type::char(),  plir::ExprType::Literal(literal)),
                    tree::Literal::Str(_)   => plir::Expr::new(plir::Type::str(),   plir::ExprType::Literal(literal)),
                    tree::Literal::Bool(_)  => plir::Expr::new(plir::Type::bool(),  plir::ExprType::Literal(literal)),
                };

                Ok(expr)
            },
            tree::Expr::ListLiteral(lst) => {
                let newlst: Vec<_> = lst.into_iter()
                    .map(|e| self.consume_expr(e))
                    .collect::<Result<_, _>>()?;

                // type resolution
                // TODO: union?
                // TODO: resolve this:
                match newlst.split_first() {
                    Some((head, tail)) => {
                        if tail.iter().all(|e| e.ty == head.ty) {
                            let e = plir::Expr::new(
                                plir::Type::list(head.ty.clone()),
                                plir::ExprType::ListLiteral(newlst)
                            );
                            Ok(e)
                        } else {
                            todo!()
                        }
                    },
                    None => todo!(),
                }
            },
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