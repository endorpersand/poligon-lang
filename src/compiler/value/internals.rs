//! Internally defined structs for LLVM representation

use inkwell::types::StructType;

use crate::compiler::Compiler;

impl<'ctx> Compiler<'ctx> {
    /// [`super::TypeLayout::Str`]
    pub(super) fn string_type(&self) -> StructType<'ctx> {
        self.ctx.get_struct_type("String").unwrap_or_else(|| {
            let str_ty = self.ctx.opaque_struct_type("String");
    
            str_ty.set_body(&[
                self.ctx.i8_type().ptr_type(Default::default()).into(),
                self.ctx.i64_type().into()
            ], false);
    
            str_ty
        })
    }
}