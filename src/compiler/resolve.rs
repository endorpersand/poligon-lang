use crate::tree::{self, ReasgType, MutType, op};

pub mod plir;

pub fn codegen(t: tree::Program) -> PLIRResult<plir::Program> {
    let mut cg = CodeGenerator::new();
    cg.consume_program(t)?;
    Ok(cg.unwrap())
}

#[derive(Debug)]
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
    InvalidSplit(plir::Type, plir::Split),
    CannotUnary(op::Unary, plir::Type),
    CannotBinary(op::Binary, plir::Type, plir::Type),
    CannotCmp(op::Cmp, plir::Type, plir::Type),
    CannotIndex(plir::Type),
    CannotIndexWith(plir::Type, plir::Type),
}
pub type PLIRResult<T> = Result<T, PLIRErr>;

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
            None => plir::ty!(plir::Type::S_VOID),
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
    fn into_expr(self) -> plir::Expr {
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
                plir::Stmt::Decl(_)    => Some(plir::ty!(plir::Type::S_VOID)),
                plir::Stmt::Return(_)  => None,
                plir::Stmt::Break      => None,
                plir::Stmt::Continue   => None,
                plir::Stmt::FunDecl(_) => Some(plir::ty!(plir::Type::S_VOID)),
                plir::Stmt::Expr(e)    => Some(e.ty.clone())
            },
            None => Some(plir::ty!(plir::Type::S_VOID)),
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
        
        let bty = plir::Type::resolve_branches(&branch_types)
            .ok_or(PLIRErr::CannotResolveType)?;
        Ok(plir::Block(bty, block))
    }

    fn unwrap_decl(&mut self, rt: ReasgType, pat: tree::DeclPat, ty: Option<plir::Type>, e: plir::Expr) -> PLIRResult<()> {
        match pat {
            tree::Pat::Unit(tree::DeclUnit(ident, mt)) => {
                let ty = ty.unwrap_or_else(|| e.ty.clone());
                let decl = plir::Decl { rt, mt, ident, ty, val: e };
                self.peek_block().push_stmt(plir::Stmt::Decl(decl));
                Ok(())
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
                    plir::ty!(plir::Type::S_UNK),
                    plir::Type::from
                );

                plir::Param { rt, mt, ident, ty }
            })
            .collect();
        
        let ret = ret.map_or(
            plir::ty!(plir::Type::S_VOID),
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
                let ty = match literal {
                    tree::Literal::Int(_)   => plir::ty!(plir::Type::S_INT),
                    tree::Literal::Float(_) => plir::ty!(plir::Type::S_FLOAT),
                    tree::Literal::Char(_)  => plir::ty!(plir::Type::S_CHAR),
                    tree::Literal::Str(_)   => plir::ty!(plir::Type::S_STR),
                    tree::Literal::Bool(_)  => plir::ty!(plir::Type::S_BOOL)
                };

                Ok(plir::Expr::new(
                    ty, plir::ExprType::Literal(literal)
                ))
            },
            tree::Expr::ListLiteral(lst) => {
                let new_inner: Vec<_> = lst.into_iter()
                    .map(|e| self.consume_expr(e))
                    .collect::<Result<_, _>>()?;

                let elem_ty = plir::Type::resolve_collection_ty(new_inner.iter().map(|e| &e.ty))
                    .ok_or(PLIRErr::CannotResolveType)?;

                Ok(plir::Expr::new(
                    plir::ty!(plir::Type::S_LIST, [elem_ty]),
                    plir::ExprType::ListLiteral(new_inner)
                ))
            },
            tree::Expr::SetLiteral(set) => {
                let new_inner: Vec<_> = set.into_iter()
                    .map(|e| self.consume_expr(e))
                    .collect::<Result<_, _>>()?;

                let elem_ty = plir::Type::resolve_collection_ty(new_inner.iter().map(|e| &e.ty))
                    .ok_or(PLIRErr::CannotResolveType)?;

                Ok(plir::Expr::new(
                    plir::ty!(plir::Type::S_SET, [elem_ty]),
                    plir::ExprType::SetLiteral(new_inner)
                ))
            },
            tree::Expr::DictLiteral(entries) => {
                let new_inner: Vec<_> = entries.into_iter()
                    .map(|(k, v)| Ok((self.consume_expr(k)?, self.consume_expr(v)?)))
                    .collect::<Result<_, _>>()?;

                let (key_tys, val_tys): (Vec<_>, Vec<_>) = new_inner.iter()
                    .map(|(k, v)| (&k.ty, &v.ty))
                    .unzip();
                let key_ty = plir::Type::resolve_collection_ty(key_tys)
                    .ok_or(PLIRErr::CannotResolveType)?;
                let val_ty = plir::Type::resolve_collection_ty(val_tys)
                    .ok_or(PLIRErr::CannotResolveType)?;

                Ok(plir::Expr::new(
                    plir::ty!(plir::Type::S_DICT, [key_ty, val_ty]),
                    plir::ExprType::DictLiteral(new_inner)
                ))
            },
            tree::Expr::Assign(pat, expr) => {
                let expr = self.consume_expr(*expr)?;

                todo!()
            },
            tree::Expr::Path(_) => todo!(),
            tree::Expr::UnaryOps { ops, expr } => {
                let expr = self.consume_expr_and_box(*expr)?;
                
                let mut top_ty = expr.ty.clone();
                let mut op_stack = vec![];

                for op in ops.into_iter().rev() {
                    top_ty = plir::Type::resolve_unary_type(&op, &top_ty)
                        .ok_or_else(|| PLIRErr::CannotUnary(op, top_ty.clone()))?;
                    op_stack.push((op, top_ty.clone()));
                }

                let ops = op_stack.into_iter().rev().collect();
                
                Ok(plir::Expr::new(
                    top_ty,
                    plir::ExprType::UnaryOps { ops, expr }
                ))
            },
            tree::Expr::BinaryOp { op, left, right } => {
                let left = self.consume_expr_and_box(*left)?;
                let right = self.consume_expr_and_box(*right)?;

                let ty = plir::Type::resolve_binary_type(&op, &left.ty, &right.ty)
                    .ok_or_else(|| PLIRErr::CannotBinary(op, left.ty.clone(), right.ty.clone()))?;
                
                    Ok(plir::Expr::new(
                    ty,
                    plir::ExprType::BinaryOp { op, left, right }
                ))
            },
            tree::Expr::Comparison { left, rights } => {
                let left = self.consume_expr_and_box(*left)?;
                let rights = rights.into_iter()
                    .map(|(op, right)| Ok((op, self.consume_expr(right)?)))
                    .collect::<Result<_, _>>()?;

                Ok(plir::Expr::new(
                    plir::ty!(plir::Type::S_BOOL),
                    plir::ExprType::Comparison { left, rights }
                ))
            },
            tree::Expr::Range { left, right, step } => {
                let left = self.consume_expr_and_box(*left)?;
                let right = self.consume_expr_and_box(*right)?;
                let step = match step {
                    Some(st) => Some(self.consume_expr_and_box(*st)?),
                    None => None,
                };

                let ty = plir::Type::resolve_collection_ty([&left.ty, &right.ty])
                    .ok_or(PLIRErr::CannotResolveType)?;

                Ok(plir::Expr::new(
                    plir::ty!(plir::Type::S_RANGE, [ty]),
                    plir::ExprType::Range { left, right, step }
                ))
            },
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

                Ok(plir::Expr::new(
                    plir::Type::resolve_branches(type_iter).ok_or(PLIRErr::CannotResolveType)?,
                    plir::ExprType::If { conditionals, last }
                ))
            },
            tree::Expr::While { condition, block } => {
                let condition = self.consume_expr(*condition)?;
                let block = self.consume_block(block, BlockBehavior::Loop)?;

                Ok(plir::Expr::new(
                    plir::ty!(plir::Type::S_LIST, [block.0.clone()]),
                    plir::ExprType::While { condition: Box::new(condition), block }
                ))
            },
            tree::Expr::For { ident, iterator, block } => {
                let iterator = self.consume_expr_and_box(*iterator)?;
                let block = self.consume_block(block, BlockBehavior::Loop)?;

                Ok(plir::Expr::new(
                    plir::ty!(plir::Type::S_LIST, [block.0.clone()]),
                    plir::ExprType::For { ident, iterator, block }
                ))
            },
            tree::Expr::Call { funct, params } => {
                let funct = self.consume_expr_and_box(*funct)?;
                let params = params.into_iter()
                    .map(|expr| self.consume_expr(expr))
                    .collect::<Result<_, _>>()?;
                
                Ok(plir::Expr::new(
                    todo!(),
                    plir::ExprType::Call { funct, params }
                ))
            },
            tree::Expr::Index(tree::Index { expr, index }) => {
                let expr = self.consume_expr_and_box(*expr)?;
                let index = self.consume_expr_and_box(*index)?;

                let idx_ty = plir::Type::resolve_index_type(&expr.ty, &index)
                    .ok_or_else(|| PLIRErr::CannotIndexWith(expr.ty.clone(), index.ty.clone()))?;
                
                Ok(plir::Expr::new(
                    idx_ty,
                    plir::ExprType::Index(plir::Index { expr, index })
                ))
            },
            tree::Expr::Spread(_) => Err(PLIRErr::CannotSpread),
        }
    }

    fn consume_expr_and_box(&mut self, expr: tree::Expr) -> PLIRResult<Box<plir::Expr>> {
        self.consume_expr(expr).map(Box::new)
    }
}

#[cfg(test)]
mod test {
    use crate::{lexer, parser};

    use super::*;

    fn assert_plir_pass(input: &str) {
        let lexed = lexer::tokenize(input).unwrap();
        let parsed = parser::parse(lexed).unwrap();
        println!("{}", codegen(parsed).unwrap())
    }

    #[test]
    fn get_display() {
        assert_plir_pass("if true {
            2;
        } else {
            3;
        }");

        assert_plir_pass("fun a(b: int, c: string) {
            let d = if 0 {
                return 1;
            } else {
                3;
            };

            4;
        }");

        assert_plir_pass("
            let [a, .., b] = [1, 2, 3, 4];
        ")
    }
}