use crate::compiler::plir::{self, Expr, Block, ExprType};


pub(in crate::compiler::plir_codegen) struct ExprBlockIter<'e> {
    exprs: Vec<(&'e Expr, usize)>
}
impl<'e> ExprBlockIter<'e> {
    pub(in crate::compiler::plir_codegen) fn new(e: &'e Expr) -> Self {
        Self { exprs: vec![(e, 0)] }
    }
    fn add_to_stack(&mut self, expr: &'e Expr) {
        self.exprs.push((expr, 0));
    }
}
impl<'e> Iterator for ExprBlockIter<'e> {
    type Item = &'e Block;

    fn next(&mut self) -> Option<Self::Item> {
        let out = loop {
            let (expr, i) = self.exprs.last_mut()?;
            *i += 1; // the next index
            let j = *i - 1; // the current index

            match &expr.expr {
                ExprType::Block(b) if j == 0 => break b,
                
                ExprType::ListLiteral(exprs) 
                | ExprType::SetLiteral(exprs) 
                | ExprType::ClassLiteral(_, exprs)
                    if j < exprs.len() => {
                        self.add_to_stack(&exprs[j]);
                    },
                ExprType::DictLiteral(d) 
                    if (j / 2) < d.len() => {
                        let (vec_idx, pair_idx) = (j / 2, j % 2);
                        
                        let pair = &d[vec_idx];
                        self.add_to_stack(if pair_idx == 0 { &pair.0 } else { &pair.1 });
                    },
                ExprType::Assign(_, e) if j == 0 => { self.add_to_stack(e); }
                
                ExprType::Path(plir::Path::Struct(e, _))
                | ExprType::Path(plir::Path::Method(e, _, _))
                    if j == 0 => {
                        self.add_to_stack(e);
                    },
                
                ExprType::UnaryOps { ops: _, expr } if j == 0 => self.add_to_stack(expr),

                ExprType::BinaryOp { op: _, left, right: _ } if j == 0 => self.add_to_stack(left),
                ExprType::BinaryOp { op: _, left: _, right } if j == 1 => self.add_to_stack(right),
                
                ExprType::Comparison { left, rights: _ } if j == 0 => self.add_to_stack(left),
                ExprType::Comparison { left: _, rights } 
                    if 1 <= j && j < rights.len() + 1 => {
                        self.add_to_stack(&rights[j - 1].1);
                    },
                
                ExprType::Range { left, right: _, step: _ } if j == 0 => self.add_to_stack(left),
                ExprType::Range { left: _, right, step: _ } if j == 1 => self.add_to_stack(right),
                ExprType::Range { left: _, right: _, step } 
                    if step.is_some() && j == 2 => {
                        self.add_to_stack(step.as_ref().unwrap());
                    },
                
                ExprType::If { conditionals, last: _ }
                    if (j / 2) < conditionals.len() => {
                        let (vec_idx, pair_idx) = (j / 2, j % 2);
                        
                        let pair = &conditionals[vec_idx];
                        if pair_idx == 0 {
                            self.add_to_stack(&pair.0);
                        } else {
                            break &pair.1;
                        }
                    },
                ExprType::If { conditionals, last }
                    if last.is_some() && j == conditionals.len() * 2 => {
                        break last.as_ref().unwrap();
                    }

                ExprType::While { condition, block: _ } if j == 0 => self.add_to_stack(condition),
                ExprType::While { condition: _, block } if j == 1 => break block,

                ExprType::For { ident: _, element_type: _, iterator, block: _ } 
                    if j == 0 => {
                        self.add_to_stack(iterator);
                    },
                ExprType::For { ident: _, element_type: _, iterator: _, block } 
                    if j == 1 => {
                        break block;
                    },

                ExprType::Call { funct, params: _ } if j == 0 => self.add_to_stack(funct),
                ExprType::Call { funct: _, params } 
                    if 1 <= j && j < params.len() + 1 => {
                        self.add_to_stack(&params[j - 1]);
                    },

                ExprType::Index(plir::Index { expr, index: _ }) if j == 0 => self.add_to_stack(expr),
                ExprType::Index(plir::Index { expr: _, index }) if j == 1 => self.add_to_stack(index),

                ExprType::Spread(s) if s.is_some() && j == 0 => {
                    self.add_to_stack(s.as_ref().unwrap());
                },

                ExprType::Cast(expr) if j == 0 => self.add_to_stack(expr),

                ExprType::Deref(plir::IDeref { expr, ty: _ }) if j == 0 => self.add_to_stack(expr),

                ExprType::GEP(_, ptr, _) if j == 0 => self.add_to_stack(ptr),
                ExprType::GEP(_, _, indices) 
                    if 1 <= j && j < indices.len() + 1 => {
                        self.add_to_stack(&indices[j - 1]);
                    },

                | ExprType::Ident(_)
                | ExprType::Block(_)
                | ExprType::Literal(_)
                | ExprType::ListLiteral(_)
                | ExprType::SetLiteral(_)
                | ExprType::DictLiteral(_)
                | ExprType::ClassLiteral(_, _)
                | ExprType::Assign(_, _)
                | ExprType::Path(_)
                | ExprType::UnaryOps { .. }
                | ExprType::BinaryOp { .. }
                | ExprType::Comparison { .. }
                | ExprType::Range { .. }
                | ExprType::If { .. }
                | ExprType::While { .. }
                | ExprType::For { .. }
                | ExprType::Call { .. }
                | ExprType::Index(_)
                | ExprType::Spread(_)
                | ExprType::Split(_, _)
                | ExprType::Cast(_)
                | ExprType::Deref(_)
                | ExprType::GEP(_, _, _)
                | ExprType::Alloca(_)
                | ExprType::SizeOf(_)
                => { self.exprs.pop(); }
            }
        };

        Some(out)
    }
}

enum ExprOrBlock<'e> {
    Expr(&'e mut Expr),
    Block(&'e mut Block)
}
pub(in crate::compiler::plir_codegen) struct ExprBlockIterMut<'e> {
    frontier: Vec<ExprOrBlock<'e>>
}
impl<'e> ExprBlockIterMut<'e> {
    pub(in crate::compiler::plir_codegen) fn new(e: &'e mut Expr) -> Self {
        Self { frontier: vec![ExprOrBlock::Expr(e)] }
    }
    fn add_expr(&mut self, expr: &'e mut Expr) {
        self.frontier.push(ExprOrBlock::Expr(expr));
    }
    fn add_exprs_in_rev<I>(&mut self, expr: I) 
        where I: IntoIterator<Item=&'e mut Expr>,
              I::IntoIter: DoubleEndedIterator
    {
        self.frontier.extend(expr.into_iter().map(ExprOrBlock::Expr).rev());
    }
    fn add_block(&mut self, block: &'e mut Block) {
        self.frontier.push(ExprOrBlock::Block(block));
    }
}
impl<'e> Iterator for ExprBlockIterMut<'e> {
    type Item = &'e mut Block;

    fn next(&mut self) -> Option<Self::Item> {
        let out = loop {
            match self.frontier.pop()? {
                ExprOrBlock::Expr(e) => match &mut e.expr {
                    | ExprType::Ident(_)
                    | ExprType::Literal(_)
                    => {},
    
                    ExprType::Block(b) => break b,
    
                    | ExprType::ListLiteral(exprs)
                    | ExprType::SetLiteral(exprs)
                    | ExprType::ClassLiteral(_, exprs) 
                    => {
                        self.add_exprs_in_rev(exprs);
                    },
                    
                    ExprType::DictLiteral(entries) => {
                        self.add_exprs_in_rev(
                            entries.iter_mut()
                                .flat_map(|(k, v)| [k, v])
                        );
                    },

                    ExprType::Assign(_, e) => self.add_expr(e),
                    
                    ExprType::Path(p) => match p {
                        plir::Path::Static(_, _, _) => {},
                        plir::Path::Struct(e, _) => self.add_expr(e),
                        plir::Path::Method(m, _, _) => self.add_expr(m),
                    },
                    ExprType::UnaryOps { ops: _, expr } => self.add_expr(expr),
                    ExprType::BinaryOp { op: _, left, right } => {
                        self.add_expr(right);
                        self.add_expr(left);
                    },
                    ExprType::Comparison { left, rights } => {
                        self.add_exprs_in_rev(
                            rights.iter_mut()
                                .map(|(_, e)| e)
                        );
                        self.add_expr(left);
                    },
                    ExprType::Range { left, right, step } => {
                        if let Some(step) = step {
                            self.add_expr(step);
                        }
                        self.add_expr(right);
                        self.add_expr(left);
                    },
                    ExprType::If { conditionals, last } => {
                        if let Some(last) = last {
                            self.add_block(last);
                        }
                        self.frontier.extend(
                            conditionals.iter_mut()
                                .flat_map(|(c, b)| [ExprOrBlock::Expr(c), ExprOrBlock::Block(b)])
                                .rev()
                        );
                    },
                    ExprType::While { condition, block } => {
                        self.add_block(block);
                        self.add_expr(condition);
                    },
                    ExprType::For { ident: _, element_type: _, iterator, block } => {
                        self.add_block(block);
                        self.add_expr(iterator);
                    },
                    ExprType::Call { funct, params } => {
                        self.add_exprs_in_rev(params);
                        self.add_expr(funct);
                    },
                    ExprType::Index(plir::Index { expr, index }) => {
                        self.add_expr(index);
                        self.add_expr(expr);
                    },
                    ExprType::Spread(s) => {
                        if let Some(expr) = s {
                            self.add_expr(expr);
                        }
                    },
                    ExprType::Split(_, _) => {},
                    ExprType::Cast(e) => self.add_expr(e),
                    ExprType::Deref(plir::IDeref { expr, ty: _ }) => self.add_expr(expr),
                    ExprType::GEP(_, ptr, idx) => {
                        self.add_exprs_in_rev(idx);
                        self.add_expr(ptr);
                    },
                    ExprType::Alloca(_) => {},
                    ExprType::SizeOf(_) => {},
                },
                ExprOrBlock::Block(b) => break b,
            }
        };

        Some(out)
    }
}
