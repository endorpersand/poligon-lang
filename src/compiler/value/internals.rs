//! Internally defined structs for LLVM representation

use inkwell::builder::Builder;
use inkwell::types::{StructType, FunctionType};
use inkwell::values::{FunctionValue};

use crate::compiler::{Compiler, CompileResult};

use super::TypeLayout;

macro_rules! std_map {
    ($($l:literal: $i:ident),+) => {
        fn lookup(&self, name: &str) -> bool {
            matches!(name, $($l)|+)
        }

        fn register(&self, name: &str, builder: Builder<'ctx>, fun: FunctionValue<'ctx>) -> CompileResult<'ctx, ()> {
            match name {
                $($l => self.$i(builder, fun)),+,
                _ => unimplemented!()
            }
        }
    }
}
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

    fn std_print(&self, builder: Builder<'ctx>, fun: FunctionValue<'ctx>) -> CompileResult<'ctx, ()> {
        let p0 = fun.get_first_param().unwrap().into_struct_value();
        let buf = builder.build_extract_value(p0, 0, "buf").unwrap();
    
        let puts = self.import_fun("puts",
            TypeLayout::Int.fn_type(self, &[self.ctx.i8_type().ptr_type(Default::default()).into()], false),
        )?;
    
        builder.build_call(puts, &[buf.into()], "");
        builder.build_return(None);
    
        Ok(())
    }

    fn std_printc(&self, builder: Builder<'ctx>, fun: FunctionValue<'ctx>) -> CompileResult<'ctx, ()> {
        let p0 = fun.get_first_param().unwrap();
    
        let printf = self.import_fun("printf",
            TypeLayout::Int.fn_type(self, &[
                self.ctx.i8_type().ptr_type(Default::default()).into()
            ], true),
        )?;
    
        let template = unsafe { builder.build_global_string("%c\0", "printc") };
        builder.build_call(printf, &[template.as_pointer_value().into(), p0.into()], "");
        builder.build_return(None);
    
        Ok(())
    }

    // HACK
    std_map! {
        "print": std_print,
        "printc": std_printc
    }

    pub fn import_fun(&self, s: &str, ty: FunctionType<'ctx>) -> CompileResult<'ctx, FunctionValue<'ctx>> {
        let fun = self.module.get_function(s).unwrap_or_else(|| self.module.add_function(s, ty, None));

        if self.lookup(s) && fun.count_basic_blocks() < 1 {
            let builder = self.ctx.create_builder();
            let bb = self.ctx.append_basic_block(fun, "body");
            builder.position_at_end(bb);

            self.register(s, builder, fun)?;

            if !fun.verify(true) {
                // SAFETY: Not used after.
                unsafe { fun.delete() }
                panic!("could not import fun '{s}'");
            }
        }

        Ok(fun)
    }
}