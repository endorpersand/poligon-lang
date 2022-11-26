use crate::tree::{self, ReasgType, MutType};

pub mod plir;

pub fn codegen(t: tree::Program) -> PLIRResult<plir::Program> {
    let mut cg = CodeGenerator::new();
    cg.consume_program(t)?;
    Ok(cg.unwrap()?)
}

fn create_splits<T>(pats: &[tree::Pat<T>]) -> Vec<plir::Split> {
    let mut splits = Vec::with_capacity(pats.len());
    let mut pats = pats.iter();

    let left = pats.by_ref()
        .take_while(|pat| !matches!(pat, tree::Pat::Spread(_)))
        .enumerate()
        .map(|(i, _)| plir::Split::Left(i));
    splits.extend(left);
    
    let mut right = pats.rev()
        .enumerate()
        .map(|(i, _)| plir::Split::Right(i))
        .rev();
    
    if let Some(first_right) = right.next() {
        let lidx = splits.last().map_or(0, |split| match split {
            plir::Split::Left(idx) => *idx + 1,
            _ => unreachable!()
        });
        let ridx = match first_right {
            plir::Split::Right(idx) => idx,
            _ => unreachable!()
        };

        splits.push(plir::Split::Middle(lidx, ridx));
        splits.push(first_right);
        splits.extend(right);
    };

    splits
}

#[derive(Debug)]
pub enum PLIRErr {
    CannotBreak,
    CannotContinue,
    CannotReturn,
    UnclosedBlock,
    ExpectedType(plir::Type /* expected */, plir::Type /* found */),
    CannotResolveType,
    PoisonedTree,
    CannotSpread,
    CannotSpreadMultiple,
    OpErr(plir::OpErr)
}
pub type PLIRResult<T> = Result<T, PLIRErr>;

impl From<plir::OpErr> for PLIRErr {
    fn from(err: plir::OpErr) -> Self {
        Self::OpErr(err)
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
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
    fn unwrap(self) -> PLIRResult<plir::Program> {
        if !self.blocks.is_empty() {
            Err(PLIRErr::UnclosedBlock)?;
        }
        let InsertBlock { block, exits } = self.program;

        match exits.last() {
            None => Ok(plir::Program(block)),
            Some(BlockExit::Break)     => Err(PLIRErr::CannotBreak),
            Some(BlockExit::Continue)  => Err(PLIRErr::CannotContinue),
            Some(BlockExit::Return(_)) => Err(PLIRErr::CannotReturn),
            Some(BlockExit::Exit(_))   => Err(PLIRErr::CannotReturn),
        }
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

    fn consume_insert_block(&mut self, block: InsertBlock, btype: BlockBehavior) -> PLIRResult<plir::Block> {
        let InsertBlock { block, mut exits } = block;
        
        // check the last statement and add BlockExit::Exit if necessary
        let maybe_exit_type = match block.last() {
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
            exits.push(BlockExit::Exit(exit_type));
        };

        // resolve block's type
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
    fn consume_tree_block(&mut self, block: tree::Block, btype: BlockBehavior) -> PLIRResult<plir::Block> {
        // collect all the statements from this block
        self.push_block();
        for stmt in block.0 {
            self.consume_stmt(stmt)?;
        }
        
        let insert_block = self.pop_block().unwrap();
        self.consume_insert_block(insert_block, btype)
    }

    fn unpack_pat<T, U, FS, FM>(
        &mut self, 
        pat: tree::Pat<T>, 
        e: plir::Expr, 
        extra: U, 
        mut split_extra: FS, 
        mut map: FM,
        consume_var: bool
    ) -> PLIRResult<()> 
        where FS: FnMut(&U, plir::Split) -> PLIRResult<U>,
              FM: FnMut(&mut Self, T, plir::Expr, U) -> PLIRResult<()>,
    {
        self.unpack_pat_inner(pat, e, extra, &mut split_extra, &mut map, consume_var)
    }

    fn unpack_pat_inner<T, U, FS, FM>(
        &mut self, 
        pat: tree::Pat<T>, 
        e: plir::Expr, 
        extra: U, 
        split_extra: &mut FS, 
        map: &mut FM,
        consume_var: bool
    ) -> PLIRResult<()> 
        where FS: FnMut(&U, plir::Split) -> PLIRResult<U>,
              FM: FnMut(&mut Self, T, plir::Expr, U) -> PLIRResult<()>
    {
        match pat {
            tree::Pat::Unit(t) => map(self, t, e, extra),
            tree::Pat::Spread(spread) => match spread {
                Some(pat) => self.unpack_pat_inner(*pat, e, extra, split_extra, map, consume_var),
                None => Ok(()),
            },
            tree::Pat::List(pats) => {
                let var = self.push_tmp_decl("decl", e);

                for (idx, pat) in std::iter::zip(create_splits(&pats), pats) {
                    let rhs = var.clone().split(idx)?;
                    let extr = split_extra(&extra, idx)?;
                    self.unpack_pat_inner(pat, rhs, extr, split_extra, map, false)?;
                }

                if consume_var {
                    self.peek_block().push_stmt(plir::Stmt::Expr(var.into_expr()));
                }
                Ok(())
            },
        }
    }

    fn consume_decl(&mut self, decl: tree::Decl) -> PLIRResult<()> {
        let tree::Decl { rt, pat, ty, val } = decl;

        let e = self.consume_expr(val)?;
        let ty = ty.map(plir::Type::from);

        self.unpack_pat(pat, e, (rt, ty), 
            |(rt, mty), idx| {
                Ok((*rt, match mty {
                    Some(t) => Some(t.split(idx)?),
                    None => None,
                }))
            }, 
            |this, unit, e, extra| {
                let tree::DeclUnit(ident, mt) = unit;
                let (rt, ty) = extra;

                let ty = ty.unwrap_or_else(|| e.ty.clone());
                let decl = plir::Decl { rt, mt, ident, ty, val: e };
                this.peek_block().push_stmt(plir::Stmt::Decl(decl));
                Ok(())
            },
            false
        )
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
        let block = self.consume_tree_block(old_block, BlockBehavior::Function)?;

        // TODO, type check block
        let fun_decl = plir::FunDecl { ident, params, ret, block };
        self.peek_block().push_stmt(plir::Stmt::FunDecl(fun_decl));
        Ok(())
    }

    fn consume_expr(&mut self, expr: tree::Expr) -> PLIRResult<plir::Expr> {
        match expr {
            tree::Expr::Ident(ident) => {
                Ok(plir::Expr::new(
                    todo!(),
                    plir::ExprType::Ident(ident)
                ))
            },
            tree::Expr::Block(b) => {
                let block = self.consume_tree_block(b, BlockBehavior::Bare)?;
                
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
                    .collect::<PLIRResult<_>>()?;

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

                self.push_block();
                self.unpack_pat(pat, expr, (), |_, _| Ok(()),
                    |this, unit, e, _| {
                        let unit = match unit {
                            tree::AsgUnit::Ident(ident) => plir::AsgUnit::Ident(ident),
                            tree::AsgUnit::Path(p) => {
                                let (_, p) = this.consume_path(p)?;
                                plir::AsgUnit::Path(p)
                            },
                            tree::AsgUnit::Index(idx) => {
                                let (_, idx) = this.consume_index(idx)?;
                                plir::AsgUnit::Index(idx)
                            },
                        };
                        
                        let asg = plir::Expr::new(
                            e.ty.clone(),
                            plir::ExprType::Assign(unit, Box::new(e))
                        );

                        this.peek_block().push_stmt(plir::Stmt::Expr(asg));
                        Ok(())
                    },
                    true
                )?;
                
                let insert_block = self.pop_block().unwrap();
                self.consume_insert_block(insert_block, BlockBehavior::Bare)
                    .map(|b| plir::Expr::new(b.0.clone(), plir::ExprType::Block(b)))
            },
            tree::Expr::Path(p) => {
                self.consume_path(p)
                    .map(|(ty, path)| plir::Expr::new(ty, plir::ExprType::Path(path)))
            },
            tree::Expr::UnaryOps { ops, expr } => {
                let expr = self.consume_expr_and_box(*expr)?;
                
                let mut top_ty = expr.ty.clone();
                let mut op_stack = vec![];

                for op in ops.into_iter().rev() {
                    top_ty = plir::Type::resolve_unary_type(&op, &top_ty)?;
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

                let ty = plir::Type::resolve_binary_type(&op, &left.ty, &right.ty)?;
                Ok(plir::Expr::new(
                    ty,
                    plir::ExprType::BinaryOp { op, left, right }
                ))
            },
            tree::Expr::Comparison { left, rights } => {
                let left = self.consume_expr_and_box(*left)?;
                let rights = rights.into_iter()
                    .map(|(op, right)| Ok((op, self.consume_expr(right)?)))
                    .collect::<PLIRResult<_>>()?;

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
                        let b = self.consume_tree_block(block, BlockBehavior::Bare)?;
                        Ok((c, b))
                    })
                    .collect::<PLIRResult<_>>()?;
                
                let last = match last {
                    Some(blk) => Some(self.consume_tree_block(blk, BlockBehavior::Bare)?),
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
                let block = self.consume_tree_block(block, BlockBehavior::Loop)?;

                Ok(plir::Expr::new(
                    plir::ty!(plir::Type::S_LIST, [block.0.clone()]),
                    plir::ExprType::While { condition: Box::new(condition), block }
                ))
            },
            tree::Expr::For { ident, iterator, block } => {
                let iterator = self.consume_expr_and_box(*iterator)?;
                let block = self.consume_tree_block(block, BlockBehavior::Loop)?;

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
            tree::Expr::Index(idx) => {
                self.consume_index(idx)
                    .map(|(ty, index)| plir::Expr::new(ty, plir::ExprType::Index(index)))
            },
            tree::Expr::Spread(_) => Err(PLIRErr::CannotSpread),
        }
    }

    fn consume_expr_and_box(&mut self, expr: tree::Expr) -> PLIRResult<Box<plir::Expr>> {
        self.consume_expr(expr).map(Box::new)
    }

    fn consume_path(&mut self, p: tree::Path) -> PLIRResult<(plir::Type, plir::Path)> {
        todo!()
    }
    fn consume_index(&mut self, idx: tree::Index) -> PLIRResult<(plir::Type, plir::Index)> {
        let tree::Index { expr, index } = idx;
        let expr = self.consume_expr_and_box(*expr)?;
        let index = self.consume_expr_and_box(*index)?;

        let idx_ty = plir::Type::resolve_index_type(&expr.ty, &index)?;
        Ok((idx_ty, plir::Index { expr, index }))
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
            let [a, ..b, c] = [1, 2, 3, 4];

            a = 4;
            [a, b, c, .., d, e, f] = [1, 2, 3, 4, 5, 6, 7, 8, 9];
        ")
    }
}