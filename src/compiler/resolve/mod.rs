use crate::tree::{self, ReasgType, MutType};

pub mod plir;

pub fn codegen(t: tree::Program) -> PLIRResult<plir::Program> {
    let mut cg = CodeGenerator::new();
    cg.consume_program(t)?;
    Ok(cg.unwrap())
}

// Generic over &Type and Type
fn resolve_type<T: PartialEq>(into_it: impl IntoIterator<Item=T>) -> Option<T> {
    let mut it = into_it.into_iter();

    let ty = it.next()?;
    // TODO: union?
    if it.all(|u| ty == u) {
        Some(ty)
    } else {
        None
    }
}

fn split_var_expr(e: plir::Expr, splits: &[plir::Split]) -> PLIRResult<Vec<plir::Expr>> {
    let plir::Expr { ty, expr: ety } = e;
    match ety {
        plir::ExprType::Ident(ident) => {
            splits.into_iter().map(|sp| {
                Ok(plir::Expr::new(
                    ty.split(sp)?,
                    plir::ExprType::Split(ident.clone(), sp)
                ))
            })
            .collect()
        }
        _ => panic!("Cannot split this expression"),
    }
}
pub enum PLIRErr {
    ExpectedType(plir::Type /* expected */, plir::Type /* found */),
    CannotBreak,
    CannotContinue,
    CannotReturn,
    CannotResolveType,
    PoisonedTree,
    CannotSpread,
    CannotSpreadMultiple,
    CannotSplitType,
    InvalidSplit(plir::Type, plir::Split)
}
pub type PLIRResult<T> = Result<T, PLIRErr>;
type PartialDecl = (ReasgType, plir::Type);

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
    blocks: Vec<InsertBlock>,
    var_id: usize
}

impl CodeGenerator {
    fn new() -> Self {
        Self { program: InsertBlock::new(), blocks: vec![], var_id: 0 }
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

    fn var_name(&mut self, ident: &str) -> String {
        let string = format!("_{ident}_{}", self.var_id);
        self.var_id += 1;
        string
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
                plir::Stmt::VerifyPresence(_, _, _) => Some(plir::Type::void()),
            },
            None => Some(plir::Type::void()),
        };

        if let Some(exit_type) = maybe_exit_type {
            insert_block.exits.push(BlockExit::Exit(exit_type));
        };

        // resolve block's type
        let InsertBlock { block, exits } = insert_block;

        let mut branch_types = vec![];
        for exit in exits {
            match btype.unpack_exit(exit)? {
                BlockExitHandle::Continue(ty) => branch_types.push(ty),
                BlockExitHandle::LoopExit => {},
                BlockExitHandle::Propagate(p) => {
                    self.peek_block().exits.push(p);
                },
            }
        }
        
        match resolve_type(branch_types) {
            Some(bty) => Ok(plir::Block(bty, block)),
            None => Err(PLIRErr::CannotResolveType),
        }
    }

    fn create_decl(&mut self, pd: PartialDecl, pat: tree::DeclPat, e: plir::Expr) -> PLIRResult<()> {
        let (rt, ty) = pd;
        match pat {
            tree::Pat::Unit(unit) => match unit {
                tree::DeclUnit::Ident(ident, mt) => {
                    self.peek_block().push_stmt(plir::Stmt::Decl(plir::Decl {
                        rt,
                        mt,
                        ident,
                        ty,
                        val: e,
                    }));

                    Ok(())
                },
                tree::DeclUnit::Expr(_) => todo!(),
            },
            tree::Pat::Spread(spread) => match spread {
                // insert value into the pattern
                Some(pat) => self.create_decl((rt, ty), *pat, e),
                // drop value
                None => Ok(()),
            },
            tree::Pat::List(pats) => {
                let spread_pos = pats.iter().position(|p| matches!(p, tree::Pat::Spread(_)));
                match spread_pos {
                    Some(pos) => {
                        let mut left = pats;
                        let right = left.split_off(pos + 1);
                        let spread = left.pop().expect("expected spread");
                    },

                    // NO SPREAD
                    None => for (i, pat) in pats.iter().enumerate() {
                        todo!()
                    },
                }
                todo!()
            },
        }
    }
    fn consume_decl(&mut self, decl: tree::Decl) -> PLIRResult<()> {
        let tree::Decl { rt, pat, ty, val } = decl;

        let expr = self.consume_expr(val)?;
        let ident = self.var_name("declare");

        let expr_ty = expr.ty.clone();
        let decl_ty = ty.map_or_else(
            || expr_ty.clone(),
            plir::Type::from 
        );

        self.peek_block().push_stmt(plir::Stmt::Decl(plir::Decl {
            rt: ReasgType::Const, 
            mt: MutType::Immut, 
            ident: ident.clone(), 
            ty: expr_ty.clone(), 
            val: expr
        }));

        let var_access = plir::Expr::new(expr_ty, plir::ExprType::Ident(ident));
        self.create_decl((rt, decl_ty), pat, var_access)
    }

    fn consume_fun_decl(&mut self, decl: tree::FunDecl) -> PLIRResult<()> {
        let tree::FunDecl { ident, params, ret, block } = decl;

        let params: Vec<_> = params.into_iter()
            .map(|p| {
                let tree::Param { rt, mt, ident, ty } = p;
                let ty = ty.map_or(
                    plir::Type::unk(),
                    plir::Type::from
                );

                plir::Param { rt, mt, ident, ty }
            })
            .collect();
        
        let ret = ret.map_or(
            plir::Type::void(),
            plir::Type::from
        );

        let old_block = std::rc::Rc::try_unwrap(block)
            .map_err(|_| PLIRErr::PoisonedTree)?;
        let block = self.consume_block(old_block, BlockBehavior::Function)?;

        // TODO, type check block
        let fun_decl = plir::FunDecl { ident, params, ret, block };
        self.peek_block().push_stmt(plir::Stmt::FunDecl(fun_decl));
        Ok(())
    }

    fn consume_expr(&mut self, expr: tree::Expr) -> PLIRResult<plir::Expr> {
        match expr {
            tree::Expr::Ident(_) => todo!(),
            tree::Expr::Block(b) => {
                let block = self.consume_block(b, BlockBehavior::Bare)?;
                
                Ok(plir::Expr::new(block.0.clone(), plir::ExprType::Block(block)))
            },
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
                match newlst.split_first() {
                    Some((head, tail)) => {
                        if tail.iter().all(|e| e.ty == head.ty) {
                            let e = plir::Expr::new(
                                plir::Type::list(head.ty.clone()),
                                plir::ExprType::ListLiteral(newlst)
                            );
                            Ok(e)
                        } else {
                            // TODO: union?
                            Err(PLIRErr::CannotResolveType)
                        }
                    },
                    // TODO: resolve type of []
                    None => Err(PLIRErr::CannotResolveType),
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
            tree::Expr::If { conditionals, last } => {
                let conditionals: Vec<_> = conditionals.into_iter()
                    .map(|(cond, block)| {
                        let c = self.consume_expr(cond)?;
                        let b = self.consume_block(block, BlockBehavior::Bare)?;
                        Ok((c, b))
                    })
                    .collect::<Result<_, _>>()?;
                
                let last = match last {
                    Some(blk) => Some(self.consume_block(blk, BlockBehavior::Bare)?),
                    None => None,
                };

                let type_iter = conditionals.iter()
                    .map(|(_, block)| &block.0)
                    .chain(last.iter().map(|b| &b.0));

                let if_type = resolve_type(type_iter)
                    .ok_or(PLIRErr::CannotResolveType)?
                    .clone();

                Ok(plir::Expr::new(
                    if_type,
                    plir::ExprType::If { conditionals, last }
                ))
            },
            tree::Expr::While { condition, block } => {
                let condition = self.consume_expr(*condition)?;
                let block = self.consume_block(block, BlockBehavior::Loop)?;

                Ok(plir::Expr::new(
                    plir::Type::list(block.0.clone()), 
                    plir::ExprType::While { condition: Box::new(condition), block }
                ))
            },
            tree::Expr::For { ident, iterator, block } => todo!(),
            tree::Expr::Call { funct, params } => todo!(),
            tree::Expr::Index(_) => todo!(),
            tree::Expr::Spread(_) => Err(PLIRErr::CannotSpread),
        }
    }
}