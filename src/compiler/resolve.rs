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

#[derive(Clone, PartialEq, Eq)]
struct Var {
    ident: String,
    ty: plir::Type
}
impl Var {
    fn to_expr(self) -> plir::Expr {
        plir::Expr::new(self.ty, plir::ExprType::Ident(self.ident))
    }

    fn split(self, sp: plir::Split) -> PLIRResult<plir::Expr> {
        let t = self.ty.split(sp)?;
        let e = plir::Expr::new(t, plir::ExprType::Split(self.ident, sp));
        Ok(e)
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

    fn push_tmp_decl(&mut self, ident: &str, e: plir::Expr) -> Var {
        let ident = self.var_name(ident);
        let ety = e.ty.clone();

        let decl = plir::Decl {
            rt: ReasgType::Const,
            mt: MutType::Immut,
            ident: ident.clone(),
            ty: ety.clone(),
            val: e,
        };

        self.peek_block().push_stmt(plir::Stmt::Decl(decl));

        Var {
            ident,
            ty: ety
        }
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

    fn unwrap_decl(&mut self, rt: ReasgType, pat: tree::DeclPat, ty: Option<plir::Type>, e: plir::Expr) -> PLIRResult<()> {
        match pat {
            tree::Pat::Unit(unit) => match unit {
                tree::DeclUnit::Ident(ident, mt) => {
                    let ty = ty.unwrap_or_else(|| e.ty.clone());
                    let decl = plir::Decl { rt, mt, ident, ty, val: e };
                    self.peek_block().push_stmt(plir::Stmt::Decl(decl));
                    Ok(())
                },
                tree::DeclUnit::Expr(_) => todo!(),
            },
            tree::Pat::Spread(spread) => match spread {
                Some(pat) => self.unwrap_decl(rt, *pat, ty, e),
                None => Ok(()),
            },
            tree::Pat::List(pats) => {
                let var = self.push_tmp_decl("decl", e);

                match pats.iter().position(|p| matches!(p, tree::Pat::Spread(_))) {
                    Some(pos) => {
                        let len = pats.len();
                        let left = pos;
                        let right = len - pos - 1;

                        // let [a0, a1, a2, a3, .., b4, b3, b2, b1, b0] 
                        //      = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11];
                        // pattern has 10 elements, spread at position 4
                        // left: [0, 1, 2, 3], spread: [4, 4], right: [4, 3, 2, 1, 0]

                        // LEFT--- M---- RIGHT------
                        // 0 1 2 3 4 5 6 7 8 9 10 11
                        // 0 1 2 3 4---4=4 3 2  1  0

                        let indexes = (0..left).map(plir::Split::Left)
                            .chain(std::iter::once(plir::Split::Middle(left, right - 1)))
                            .chain((0..right).map(|i| plir::Split::Right(right - 1 - i /* last index - i */)));

                        for (idx, pat) in std::iter::zip(indexes, pats) {
                            let rhs = var.clone().split(idx)?;
                            let ty  = match ty {
                                Some(ref t) => Some(t.split(idx)?),
                                None => None,
                            };

                            self.unwrap_decl(rt, pat, ty, rhs)?;
                        }

                        Ok(())
                    },
                    None => {
                        for (i, pat) in pats.into_iter().enumerate() {
                            let idx = plir::Split::Left(i);

                            let rhs = var.clone().split(idx)?;
                            let ty  = match ty {
                                Some(ref t) => Some(t.split(idx)?),
                                None => None,
                            };

                            self.unwrap_decl(rt, pat, ty, rhs)?;
                        }

                        Ok(())
                    },
                }
            },
        }
    }

    fn consume_decl(&mut self, decl: tree::Decl) -> PLIRResult<()> {
        let tree::Decl { rt, pat, ty, val } = decl;

        let e = self.consume_expr(val)?;
        let ty = ty.map(plir::Type::from);
        self.unwrap_decl(rt, pat, ty, e)
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
            tree::Expr::Comparison { left, rights } => {
                let lval = self.consume_expr(*left)?;
                let lvar = self.push_tmp_decl("cmp_val", lval);
                for (cmp, right) in rights {
                    let rval = self.consume_expr(right)?;
                    let rvar = self.push_tmp_decl("cmp_val", rval);
                }

                todo!()
            },
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