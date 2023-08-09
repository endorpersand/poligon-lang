use crate::ast::Literal;

use super::own_cow;

pub(crate) trait Walker {
    type Err;

    fn visit_hoisted_stmt(&mut self, _el: &super::HoistedStmt) -> Result<(), Self::Err>;
    fn visit_fun_decl(&mut self, el: &super::FunDecl) -> Result<(), Self::Err>;
    fn visit_fun_sig(&mut self, el: &super::FunSignature) -> Result<(), Self::Err>;
    fn visit_class_decl(&mut self, el: &super::Class) -> Result<(), Self::Err>;
    fn visit_decl(&mut self, el: &super::Decl) -> Result<(), Self::Err>;

    fn visit_proc_stmt(&mut self, el: &super::ProcStmt) -> Result<(), Self::Err>;

    fn visit_expr(&mut self, el: &super::Expr) -> Result<(), Self::Err>;
    fn visit_ident(&mut self, el: &str) -> Result<(), Self::Err>;
    fn visit_fun_ident(&mut self, el: &super::FunIdent) -> Result<(), Self::Err>;
    fn visit_block(&mut self, el: &super::Block) -> Result<(), Self::Err>;
    fn visit_literal(&mut self, el: &Literal) -> Result<(), Self::Err>;
    fn visit_asg_unit(&mut self, _el: &super::AsgUnit) -> Result<(), Self::Err> {
        // Assignment unit is typically just glue for the variants.
        Ok(())
    }
    fn visit_path(&mut self, el: &super::Path) -> Result<(), Self::Err>;
    fn visit_index(&mut self, el: &super::Index) -> Result<(), Self::Err>;
    fn visit_deref(&mut self, el: &super::IDeref) -> Result<(), Self::Err>;

    fn visit_type(&mut self, el: &super::Type) -> Result<(), Self::Err>;
    fn visit_fun_type(&mut self, el: &super::FunType) -> Result<(), Self::Err>;
    fn visit_param(&mut self, el: &super::Param) -> Result<(), Self::Err>;
    fn visit_field(&mut self, el: &super::Field) -> Result<(), Self::Err>;

    fn walk_program(&mut self, el: &super::Program) -> Result<(), Self::Err> {
        let super::Program(hoisted) = el;
        for st in hoisted {
            self.walk_hoisted_stmt(st)?;
        }
        Ok(())
    }
    fn walk_hoisted_stmt(&mut self, el: &super::HoistedStmt) -> Result<(), Self::Err> {
        self.visit_hoisted_stmt(el)?;
        match el {
            super::HoistedStmt::FunDecl(f) => self.walk_fun_decl(f),
            super::HoistedStmt::ExternFunDecl(f) => self.walk_fun_sig(f),
            super::HoistedStmt::ClassDecl(c) => self.walk_class_decl(c),
            super::HoistedStmt::IGlobal(id, _) => self.visit_ident(id),
        }
    }
    fn walk_proc_stmt(&mut self, el: &super::ProcStmt) -> Result<(), Self::Err> {
        self.visit_proc_stmt(el)?;
        match el {
            super::ProcStmt::Decl(d) => self.walk_decl(d),
            super::ProcStmt::Return(me) | super::ProcStmt::Exit(me) => {
                match me {
                    Some(e) => self.walk_expr(e),
                    None => Ok(()),
                }
            },
            super::ProcStmt::Break => Ok(()),
            super::ProcStmt::Continue => Ok(()),
            super::ProcStmt::Throw(_) => Ok(()),
            super::ProcStmt::Expr(e) => self.walk_expr(e),
        }
    }

    fn walk_fun_decl(&mut self, el: &super::FunDecl) -> Result<(), Self::Err> {
        self.visit_fun_decl(el)?;
        
        let super::FunDecl { sig, block } = el;
        self.walk_fun_sig(sig)?;
        self.walk_block(block)?;
        Ok(())
    }
    fn walk_fun_sig(&mut self, el: &super::FunSignature) -> Result<(), Self::Err> {
        self.visit_fun_sig(el)?;
        let super::FunSignature { private: _, ident, params, varargs: _, ret } = el;
        self.visit_fun_ident(ident)?;
        for p in params {
            self.walk_param(p)?;
        }
        self.walk_type(ret)
    }

    fn walk_type(&mut self, el: &super::Type) -> Result<(), Self::Err> {
        self.visit_type(el)?;
        match el {
            super::Type::Unk(_) | super::Type::Prim(_) | super::Type::TypeVar(_, _) => Ok(()),
            super::Type::Generic(_, t, ()) | super::Type::Tuple(t, ()) => {
                for ty in t.iter() {
                    self.walk_type(ty)?;
                }
                Ok(())
            },
            super::Type::Fun(ft) => self.walk_fun_type(ft),
        }
    }
    fn walk_fun_type(&mut self, el: &super::FunType) -> Result<(), Self::Err> {
        self.visit_fun_type(el)?;
        let super::FunType { params, ret, varargs: _ } = el;
        for p in params.iter() {
            self.walk_type(p)?;
        }
        self.walk_type(ret)
    }
    fn walk_param(&mut self, el: &super::Param) -> Result<(), Self::Err> {
        self.visit_param(el)?;
        let super::Param { rt: _, mt: _, ident, ty } = el;
        self.visit_ident(ident)?;
        self.walk_type(ty)
    }
    fn walk_class_decl(&mut self, el: &super::Class) -> Result<(), Self::Err> {
        self.visit_class_decl(el)?;
        let super::Class { ty, fields } = el;
        self.walk_type(ty)?;
        for field in fields.values() {
            self.walk_field(field)?;
        }
        Ok(())
    }
    fn walk_field(&mut self, el: &super::Field) -> Result<(), Self::Err> {
        self.visit_field(el)?;
        let super::Field { rt: _, mt: _, ty } = el;
        self.walk_type(ty)
    }
    fn walk_decl(&mut self, el: &super::Decl) -> Result<(), Self::Err> {
        self.visit_decl(el)?;
        let super::Decl { rt: _, mt: _, ident, ty, val } = el;
        self.visit_ident(ident)?;
        self.walk_type(ty)?;
        self.walk_expr(val)
    }
    fn walk_expr(&mut self, el: &super::Expr) -> Result<(), Self::Err> {
        self.visit_expr(el)?;
        let super::Expr { ty, expr } = el;
        self.walk_type(ty)?;
        match expr {
            super::ExprType::Ident(id) => self.visit_ident(id),
            super::ExprType::Block(el) => self.walk_block(el),
            super::ExprType::Literal(lit) => self.visit_literal(lit),
            super::ExprType::ListLiteral(l) => {
                for e in l {
                    self.walk_expr(e)?;
                }
                Ok(())
            },
            super::ExprType::SetLiteral(s) => {
                for e in s {
                    self.walk_expr(e)?;
                }
                Ok(())
            },
            super::ExprType::DictLiteral(d) => {
                for (k, v) in d {
                    self.walk_expr(k)?;
                    self.walk_expr(v)?;
                }
                Ok(())
            },
            super::ExprType::ClassLiteral(ty, fields) => {
                self.walk_type(ty)?;
                for f in fields {
                    self.walk_expr(f)?;
                }
                Ok(())
            },
            super::ExprType::Assign(u, e) => {
                self.walk_asg_unit(u)?;
                self.walk_expr(e)
            },
            super::ExprType::Path(p) => self.walk_path(p),
            super::ExprType::UnaryOps { ops, expr } => {
                for (_, ty) in ops {
                    self.walk_type(ty)?;
                }
                self.walk_expr(expr)
            },
            super::ExprType::BinaryOp { op: _, left, right } => {
                self.walk_expr(left)?;
                self.walk_expr(right)
            },
            super::ExprType::Comparison { left, rights } => {
                self.walk_expr(left)?;
                for (_, right) in rights {
                    self.walk_expr(right)?
                }
                Ok(())
            },
            super::ExprType::Range { left, right, step } => {
                self.walk_expr(left)?;
                self.walk_expr(right)?;
                match step {
                    Some(e) => self.walk_expr(e),
                    None => Ok(()),
                }
            },
            super::ExprType::If { conditionals, last } => {
                for (cond, block) in conditionals {
                    self.walk_expr(cond)?;
                    self.walk_block(block)?;
                }
                match last {
                    Some(b) => self.walk_block(b),
                    None => Ok(()),
                }
            },
            super::ExprType::While { condition, block } => {
                self.walk_expr(condition)?;
                self.walk_block(block)
            },
            super::ExprType::For { ident, element_type, iterator, block } => {
                self.visit_ident(ident)?;
                self.walk_type(element_type)?;
                self.walk_expr(iterator)?;
                self.walk_block(block)
            },
            super::ExprType::Call { funct, params } => {
                self.walk_expr(funct)?;
                for p in params {
                    self.walk_expr(p)?;
                }
                Ok(())
            },
            super::ExprType::Index(idx) => self.walk_index(idx),
            super::ExprType::Spread(s) => {
                match s {
                    Some(e) => self.walk_expr(e),
                    None => Ok(()),
                }
            },
            super::ExprType::Split(id, _) => self.visit_ident(id),
            super::ExprType::Cast(e) => self.walk_expr(e),
            super::ExprType::Deref(d) => self.walk_deref(d),
            super::ExprType::GEP(ty, ptr, idxs) => {
                self.walk_type(ty)?;
                self.walk_expr(ptr)?;
                for idx in idxs {
                    self.walk_expr(idx)?;
                }
                Ok(())
            },
            super::ExprType::Alloca(ty) => self.walk_type(ty),
            super::ExprType::SizeOf(ty) => self.walk_type(ty),
        }
    }
    fn walk_block(&mut self, el: &super::Block) -> Result<(), Self::Err> {
        self.visit_block(el)?;
        let super::Block(ty, stmts) = el;
        self.walk_type(ty)?;
        for st in stmts {
            self.walk_proc_stmt(st)?;
        }
        Ok(())
    }

    fn walk_asg_unit(&mut self, el: &super::AsgUnit) -> Result<(), Self::Err> {
        self.visit_asg_unit(el)?;
        match el {
            super::AsgUnit::Ident(id) => self.visit_ident(id),
            super::AsgUnit::Path(p)   => self.walk_path(p),
            super::AsgUnit::Index(i)  => self.walk_index(i),
            super::AsgUnit::Deref(d)  => self.walk_deref(d),
        }
    }
    fn walk_path(&mut self, el: &super::Path) -> Result<(), Self::Err> {
        self.visit_path(el)?;
        match el {
            super::Path::Static(t, id, pty) => {
                self.walk_type(t)?;
                self.visit_ident(id)?;
                self.walk_type(pty)
            },
            super::Path::Struct(e, idxs) => {
                self.walk_expr(e)?;
                for (_, ty) in idxs {
                    self.walk_type(ty)?;
                }
                Ok(())
            },
            super::Path::Method(e, id, ty) => {
                self.walk_expr(e)?;
                self.visit_ident(id)?;
                self.walk_fun_type(ty)
            },
        }
    }
    fn walk_index(&mut self, el: &super::Index) -> Result<(), Self::Err> {
        self.visit_index(el)?;

        let super::Index { expr, index } = el;
        self.walk_expr(expr)?;
        self.walk_expr(index)
    }
    fn walk_deref(&mut self, el: &super::IDeref) -> Result<(), Self::Err> {
        self.visit_deref(el)?;
        
        let super::IDeref { expr, ty } = el;
        self.walk_expr(expr)?;
        self.walk_type(ty)
    }
}

pub(crate) trait WalkerMut {
    type Err;

    fn visit_hoisted_stmt(&mut self, _el: &mut super::HoistedStmt) -> Result<(), Self::Err>;
    fn visit_fun_decl(&mut self, el: &mut super::FunDecl) -> Result<(), Self::Err>;
    fn visit_fun_sig(&mut self, el: &mut super::FunSignature) -> Result<(), Self::Err>;
    fn visit_class_decl(&mut self, el: &mut super::Class) -> Result<(), Self::Err>;
    fn visit_decl(&mut self, el: &mut super::Decl) -> Result<(), Self::Err>;

    fn visit_proc_stmt(&mut self, el: &mut super::ProcStmt) -> Result<(), Self::Err>;

    fn visit_expr(&mut self, el: &mut super::Expr) -> Result<(), Self::Err>;
    fn visit_ident(&mut self, el: &mut str) -> Result<(), Self::Err>;
    fn visit_fun_ident(&mut self, el: &mut super::FunIdent) -> Result<(), Self::Err>;
    fn visit_block(&mut self, el: &mut super::Block) -> Result<(), Self::Err>;
    fn visit_literal(&mut self, el: &mut Literal) -> Result<(), Self::Err>;
    fn visit_asg_unit(&mut self, _el: &mut super::AsgUnit) -> Result<(), Self::Err> {
        // Assignment unit is typically just glue for the variants.
        Ok(())
    }
    fn visit_path(&mut self, el: &mut super::Path) -> Result<(), Self::Err>;
    fn visit_index(&mut self, el: &mut super::Index) -> Result<(), Self::Err>;
    fn visit_deref(&mut self, el: &mut super::IDeref) -> Result<(), Self::Err>;

    fn visit_type(&mut self, el: &mut super::Type) -> Result<(), Self::Err>;
    fn visit_fun_type(&mut self, el: &mut super::FunType) -> Result<(), Self::Err>;
    fn visit_param(&mut self, el: &mut super::Param) -> Result<(), Self::Err>;
    fn visit_field(&mut self, el: &mut super::Field) -> Result<(), Self::Err>;

    fn walk_program(&mut self, el: &mut super::Program) -> Result<(), Self::Err> {
        let super::Program(hoisted) = el;
        for st in hoisted {
            self.walk_hoisted_stmt(st)?;
        }
        Ok(())
    }
    fn walk_hoisted_stmt(&mut self, el: &mut super::HoistedStmt) -> Result<(), Self::Err> {
        self.visit_hoisted_stmt(el)?;
        match el {
            super::HoistedStmt::FunDecl(f) => self.walk_fun_decl(f),
            super::HoistedStmt::ExternFunDecl(f) => self.walk_fun_sig(f),
            super::HoistedStmt::ClassDecl(c) => self.walk_class_decl(c),
            super::HoistedStmt::IGlobal(id, _) => self.visit_ident(id),
        }
    }
    fn walk_proc_stmt(&mut self, el: &mut super::ProcStmt) -> Result<(), Self::Err> {
        self.visit_proc_stmt(el)?;
        match el {
            super::ProcStmt::Decl(d) => self.walk_decl(d),
            super::ProcStmt::Return(me) | super::ProcStmt::Exit(me) => {
                match me {
                    Some(e) => self.walk_expr(e),
                    None => Ok(()),
                }
            },
            super::ProcStmt::Break => Ok(()),
            super::ProcStmt::Continue => Ok(()),
            super::ProcStmt::Throw(_) => Ok(()),
            super::ProcStmt::Expr(e) => self.walk_expr(e),
        }
    }

    fn walk_fun_decl(&mut self, el: &mut super::FunDecl) -> Result<(), Self::Err> {
        self.visit_fun_decl(el)?;
        
        let super::FunDecl { sig, block } = el;
        self.walk_fun_sig(sig)?;
        self.walk_block(block)?;
        Ok(())
    }
    fn walk_fun_sig(&mut self, el: &mut super::FunSignature) -> Result<(), Self::Err> {
        self.visit_fun_sig(el)?;
        let super::FunSignature { private: _, ident, params, varargs: _, ret } = el;
        self.visit_fun_ident(ident)?;
        for p in params {
            self.walk_param(p)?;
        }
        self.walk_type(ret)
    }

    fn walk_type(&mut self, el: &mut super::Type) -> Result<(), Self::Err> {
        self.visit_type(el)?;
        match el {
            super::Type::Unk(_) | super::Type::Prim(_) | super::Type::TypeVar(_, _) => Ok(()),
            super::Type::Generic(_, cow, ()) | super::Type::Tuple(cow, ()) => {
                let tys = own_cow(cow);

                for ty in tys {
                    self.walk_type(ty)?;
                }
                Ok(())
            },
            super::Type::Fun(ft) => self.walk_fun_type(ft),
        }
    }
    fn walk_fun_type(&mut self, el: &mut super::FunType) -> Result<(), Self::Err> {
        self.visit_fun_type(el)?;
        let super::FunType { params, ret, varargs: _ } = el;

        let par = own_cow(params);
        let ret = own_cow(ret);

        for p in par {
            self.walk_type(p)?;
        }
        self.walk_type(ret)
    }
    fn walk_param(&mut self, el: &mut super::Param) -> Result<(), Self::Err> {
        self.visit_param(el)?;
        let super::Param { rt: _, mt: _, ident, ty } = el;
        self.visit_ident(ident)?;
        self.walk_type(ty)
    }
    fn walk_class_decl(&mut self, el: &mut super::Class) -> Result<(), Self::Err> {
        self.visit_class_decl(el)?;
        let super::Class { ty, fields } = el;
        self.walk_type(ty)?;
        for field in fields.values_mut() {
            self.walk_field(field)?;
        }
        Ok(())
    }
    fn walk_field(&mut self, field: &mut super::Field) -> Result<(), Self::Err> {
        self.visit_field(field)?;
        let super::Field { rt: _, mt: _, ty } = field;
        self.walk_type(ty)
    }
    fn walk_decl(&mut self, el: &mut super::Decl) -> Result<(), Self::Err> {
        self.visit_decl(el)?;
        let super::Decl { rt: _, mt: _, ident, ty, val } = el;
        self.visit_ident(ident)?;
        self.walk_type(ty)?;
        self.walk_expr(val)
    }
    fn walk_expr(&mut self, el: &mut super::Expr) -> Result<(), Self::Err> {
        self.visit_expr(el)?;
        let super::Expr { ty, expr } = el;
        self.walk_type(ty)?;
        match expr {
            super::ExprType::Ident(id) => self.visit_ident(id),
            super::ExprType::Block(el) => self.walk_block(el),
            super::ExprType::Literal(lit) => self.visit_literal(lit),
            super::ExprType::ListLiteral(l) => {
                for e in l {
                    self.walk_expr(e)?;
                }
                Ok(())
            },
            super::ExprType::SetLiteral(s) => {
                for e in s {
                    self.walk_expr(e)?;
                }
                Ok(())
            },
            super::ExprType::DictLiteral(d) => {
                for (k, v) in d {
                    self.walk_expr(k)?;
                    self.walk_expr(v)?;
                }
                Ok(())
            },
            super::ExprType::ClassLiteral(ty, fields) => {
                self.walk_type(ty)?;
                for f in fields {
                    self.walk_expr(f)?;
                }
                Ok(())
            },
            super::ExprType::Assign(u, e) => {
                self.walk_asg_unit(u)?;
                self.walk_expr(e)
            },
            super::ExprType::Path(p) => self.walk_path(p),
            super::ExprType::UnaryOps { ops, expr } => {
                for (_, ty) in ops {
                    self.walk_type(ty)?;
                }
                self.walk_expr(expr)
            },
            super::ExprType::BinaryOp { op: _, left, right } => {
                self.walk_expr(left)?;
                self.walk_expr(right)
            },
            super::ExprType::Comparison { left, rights } => {
                self.walk_expr(left)?;
                for (_, right) in rights {
                    self.walk_expr(right)?
                }
                Ok(())
            },
            super::ExprType::Range { left, right, step } => {
                self.walk_expr(left)?;
                self.walk_expr(right)?;
                match step {
                    Some(e) => self.walk_expr(e),
                    None => Ok(()),
                }
            },
            super::ExprType::If { conditionals, last } => {
                for (cond, block) in conditionals {
                    self.walk_expr(cond)?;
                    self.walk_block(block)?;
                }
                match last {
                    Some(b) => self.walk_block(b),
                    None => Ok(()),
                }
            },
            super::ExprType::While { condition, block } => {
                self.walk_expr(condition)?;
                self.walk_block(block)
            },
            super::ExprType::For { ident, element_type, iterator, block } => {
                self.visit_ident(ident)?;
                self.walk_type(element_type)?;
                self.walk_expr(iterator)?;
                self.walk_block(block)
            },
            super::ExprType::Call { funct, params } => {
                self.walk_expr(funct)?;
                for p in params {
                    self.walk_expr(p)?;
                }
                Ok(())
            },
            super::ExprType::Index(idx) => self.walk_index(idx),
            super::ExprType::Spread(s) => {
                match s {
                    Some(e) => self.walk_expr(e),
                    None => Ok(()),
                }
            },
            super::ExprType::Split(id, _) => self.visit_ident(id),
            super::ExprType::Cast(e) => self.walk_expr(e),
            super::ExprType::Deref(d) => self.walk_deref(d),
            super::ExprType::GEP(ty, ptr, idxs) => {
                self.walk_type(ty)?;
                self.walk_expr(ptr)?;
                for idx in idxs {
                    self.walk_expr(idx)?;
                }
                Ok(())
            },
            super::ExprType::Alloca(ty) => self.walk_type(ty),
            super::ExprType::SizeOf(ty) => self.walk_type(ty),
        }
    }
    fn walk_block(&mut self, el: &mut super::Block) -> Result<(), Self::Err> {
        self.visit_block(el)?;
        let super::Block(ty, stmts) = el;
        self.walk_type(ty)?;
        for st in stmts {
            self.walk_proc_stmt(st)?;
        }
        Ok(())
    }

    fn walk_asg_unit(&mut self, el: &mut super::AsgUnit) -> Result<(), Self::Err> {
        self.visit_asg_unit(el)?;
        match el {
            super::AsgUnit::Ident(id) => self.visit_ident(id),
            super::AsgUnit::Path(p)   => self.walk_path(p),
            super::AsgUnit::Index(i)  => self.walk_index(i),
            super::AsgUnit::Deref(d)  => self.walk_deref(d),
        }
    }
    fn walk_path(&mut self, el: &mut super::Path) -> Result<(), Self::Err> {
        self.visit_path(el)?;
        match el {
            super::Path::Static(t, id, pty) => {
                self.walk_type(t)?;
                self.visit_ident(id)?;
                self.walk_type(pty)
            },
            super::Path::Struct(e, idxs) => {
                self.walk_expr(e)?;
                for (_, ty) in idxs {
                    self.walk_type(ty)?;
                }
                Ok(())
            },
            super::Path::Method(e, id, ty) => {
                self.walk_expr(e)?;
                self.visit_ident(id)?;
                self.walk_fun_type(ty)
            },
        }
    }
    fn walk_index(&mut self, el: &mut super::Index) -> Result<(), Self::Err> {
        self.visit_index(el)?;

        let super::Index { expr, index } = el;
        self.walk_expr(expr)?;
        self.walk_expr(index)
    }
    fn walk_deref(&mut self, el: &mut super::IDeref) -> Result<(), Self::Err> {
        self.visit_deref(el)?;
        
        let super::IDeref { expr, ty } = el;
        self.walk_expr(expr)?;
        self.walk_type(ty)
    }
}