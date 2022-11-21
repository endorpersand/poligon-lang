use crate::tree;

pub mod plir;

pub fn codegen(t: tree::Program) -> PLIRResult<plir::Program> {
    let mut cg = CodeGenerator::new();
    cg.consume_program(t)?;
    Ok(cg.unwrap())
}

pub enum PLIRErr {
    ExpectedType(plir::Type /* expected */, plir::Type /* found */),
    CannotBreak,
    CannotContinue,
    CannotReturn,
    CannotResolveType
}
pub type PLIRResult<T> = Result<T, PLIRErr>;
type PartialDecl = (tree::ReasgType, Option<tree::Type>);

enum BlockExit {
    Return(plir::Type),
    Break,
    Continue,
    Exit(plir::Type)
}
enum BlockBehavior {
    Function,
    Loop,
    Bare
}
enum BlockExitHandle {
    /// Continue running in the upper loop.
    /// 
    /// If a block exits here, it has a known value type.
    Continue(plir::Type),
    
    /// This is either an `break` or `continue`.
    /// 
    /// If a block exits here, there is no type.
    LoopExit,

    /// Propagate the exit to the upper loop.
    Propagate(BlockExit)
}

impl BlockBehavior {
    fn unpack_exit(&self, exit: BlockExit) -> PLIRResult<BlockExitHandle> {
        match self {
            BlockBehavior::Function => match exit {
                BlockExit::Return(t) => Ok(BlockExitHandle::Continue(t)),
                BlockExit::Break     => Err(PLIRErr::CannotBreak),
                BlockExit::Continue  => Err(PLIRErr::CannotContinue),
                BlockExit::Exit(t)   => Ok(BlockExitHandle::Continue(t)),
            },
            BlockBehavior::Loop => match exit {
                BlockExit::Return(_) => Ok(BlockExitHandle::Propagate(exit)),
                BlockExit::Break     => Ok(BlockExitHandle::LoopExit),
                BlockExit::Continue  => Ok(BlockExitHandle::LoopExit),
                BlockExit::Exit(t)   => Ok(BlockExitHandle::Continue(t)),
            },
            BlockBehavior::Bare => match exit {
                BlockExit::Return(_) => Ok(BlockExitHandle::Propagate(exit)),
                BlockExit::Break     => Ok(BlockExitHandle::Propagate(exit)),
                BlockExit::Continue  => Ok(BlockExitHandle::Propagate(exit)),
                BlockExit::Exit(t)   => Ok(BlockExitHandle::Continue(t)),
            },
        }
    }
}

struct InsertBlock {
    block: Vec<plir::Stmt>,
    exits: Vec<BlockExit>
}
impl InsertBlock {
    fn new() -> Self {
        Self {
            block: vec![],
            exits: vec![]
        }
    }

    fn push_stmt(&mut self, stmt: plir::Stmt) {
        match stmt {
            plir::Stmt::Return(e) => self.push_return(e),
            plir::Stmt::Break => self.push_break(),
            plir::Stmt::Continue => self.push_cont(),
            st => self.block.push(st)
        }
    }

    fn push_return(&mut self, me: Option<plir::Expr>) {
        let ty = match me {
            Some(ref e) => e.ty.clone(),
            None => plir::Type::void(),
        };
        self.block.push(plir::Stmt::Return(me));
        self.exits.push(BlockExit::Return(ty));
    }

    fn push_break(&mut self) {
        self.block.push(plir::Stmt::Break);
        self.exits.push(BlockExit::Break);
    }

    fn push_cont(&mut self) {
        self.block.push(plir::Stmt::Continue);
        self.exits.push(BlockExit::Continue);
    }
}

struct CodeGenerator {
    program: InsertBlock,
    blocks: Vec<InsertBlock>
}

impl CodeGenerator {
    fn new() -> Self {
        Self { program: InsertBlock::new(), blocks: vec![] }
    }
    fn unwrap(self) -> plir::Program {
        // TODO: handle return, break, etc
        plir::Program(self.program.block)
    }

    fn push_block(&mut self) {
        self.blocks.push(InsertBlock::new())
    }
    fn pop_block(&mut self) -> Option<InsertBlock> {
        self.blocks.pop()
    }
    fn peek_block(&mut self) -> &mut InsertBlock {
        self.blocks.last_mut().unwrap_or(&mut self.program)
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
                self.peek_block().push_return(maybe_expr);
                Ok(())
            },
            tree::Stmt::Break => {
                self.peek_block().push_break();
                Ok(())
            },
            tree::Stmt::Continue => {
                self.peek_block().push_cont();
                Ok(())
            },
            tree::Stmt::FunDecl(f) => self.consume_fun_decl(f),
            tree::Stmt::Expr(e) => {
                let e = self.consume_expr(e)?;
                self.peek_block().push_stmt(plir::Stmt::Expr(e));
                Ok(())
            },
        }
    }

    fn consume_block(&mut self, block: tree::Block, btype: BlockBehavior) -> PLIRResult<plir::Block> {
        // collect all the statements from this block
        self.push_block();
        for stmt in block.0 {
            self.consume_stmt(stmt)?;
        }
        let mut insert_block = self.pop_block().unwrap();
        
        // check the last statement and add BlockExit::Exit if necessary
        let maybe_exit_type = match insert_block.block.last() {
            Some(stmt) => match stmt {
                plir::Stmt::Decl(_)    => Some(plir::Type::void()),
                plir::Stmt::Return(_)  => None,
                plir::Stmt::Break      => None,
                plir::Stmt::Continue   => None,
                plir::Stmt::FunDecl(_) => Some(plir::Type::void()),
                plir::Stmt::Expr(e)    => Some(e.ty.clone()),
            },
            None => Some(plir::Type::void()),
        };

        if let Some(exit_type) = maybe_exit_type {
            insert_block.exits.push(BlockExit::Exit(exit_type));
        };

        // resolve block's type
        let InsertBlock { block, exits } = insert_block;

        let mut block_type = None;
        for exit in exits {
            match btype.unpack_exit(exit)? {
                BlockExitHandle::Continue(ty) => match block_type.as_ref() {
                    Some(t1) => if t1 != &ty {
                        // TODO: union?
                        Err(PLIRErr::CannotResolveType)?
                    },
                    None => {
                        block_type.replace(ty);
                    },
                },
                BlockExitHandle::LoopExit => {},
                BlockExitHandle::Propagate(p) => {
                    self.peek_block().exits.push(p);
                },
            }
        }
        
        match block_type {
            Some(bty) => Ok(plir::Block(bty, block)),
            None => Err(PLIRErr::CannotResolveType),
        }
    }

    fn create_decl(&mut self, pd: PartialDecl, pat: tree::DeclPat, e: plir::Expr) -> PLIRResult<()> {
        let (rt, mty) = pd;
        match pat {
            tree::Pat::Unit(unit) => match unit {
                tree::DeclUnit::Ident(ident, mt) => {
                    self.peek_block().push_stmt(plir::Stmt::Decl(plir::Decl {
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