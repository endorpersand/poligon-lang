use crate::compiler::plir;

use super::{TypeResolver, CannotResolve};

pub(super) struct TypeApplier<'a>(pub(super) &'a mut TypeResolver);

impl plir::walk::WalkerMut for TypeApplier<'_> {
    type Err = CannotResolve;

    fn visit_hoisted_stmt(&mut self, _: &mut plir::HoistedStmt) -> Result<(), Self::Err> {
        Ok(())
    }
    fn visit_fun_decl(&mut self, _: &mut plir::FunDecl) -> Result<(), Self::Err> {
        Ok(())
    }
    fn visit_fun_sig(&mut self, _: &mut plir::FunSignature) -> Result<(), Self::Err> {
        Ok(())
    }
    fn visit_class_decl(&mut self, _: &mut plir::Class) -> Result<(), Self::Err> {
        Ok(())
    }
    fn visit_decl(&mut self, _: &mut plir::Decl) -> Result<(), Self::Err> {
        Ok(())
    }
    fn visit_proc_stmt(&mut self, _: &mut plir::ProcStmt) -> Result<(), Self::Err> {
        Ok(())
    }
    fn visit_expr(&mut self, _: &mut plir::Expr) -> Result<(), Self::Err> {
        Ok(())
    }
    fn visit_ident(&mut self, _: &mut str) -> Result<(), Self::Err> {
        Ok(())
    }
    fn visit_fun_ident(&mut self, _: &mut plir::FunIdent) -> Result<(), Self::Err> {
        Ok(())
    }
    fn visit_block(&mut self, _: &mut plir::Block) -> Result<(), Self::Err> {
        Ok(())
    }
    fn visit_literal(&mut self, _: &mut crate::ast::Literal) -> Result<(), Self::Err> {
        Ok(())
    }
    fn visit_path(&mut self, _: &mut plir::Path) -> Result<(), Self::Err> {
        Ok(())
    }
    fn visit_index(&mut self, _: &mut plir::Index) -> Result<(), Self::Err> {
        Ok(())
    }
    fn visit_deref(&mut self, _: &mut plir::IDeref) -> Result<(), Self::Err> {
        Ok(())
    }
    fn visit_type(&mut self, el: &mut plir::Type) -> Result<(), Self::Err> {
        let ty = self.0.deep_normalize(el.clone())?;
        *el = ty;
        Ok(())
    }
    fn visit_fun_type(&mut self, _: &mut plir::FunType) -> Result<(), Self::Err> {
        Ok(())
    }
    fn visit_param(&mut self, _: &mut plir::Param) -> Result<(), Self::Err> {
        Ok(())
    }
    fn visit_field(&mut self, _: &mut plir::Field) -> Result<(), Self::Err> {
        Ok(())
    }
    fn walk_type(&mut self, el: &mut plir::Type) -> Result<(), Self::Err> {
        self.visit_type(el)
    }
}