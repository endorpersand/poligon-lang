//! Internally defined structs for LLVM representation

use inkwell::types::{FunctionType, BasicType};
use inkwell::values::FunctionValue;

use crate::compiler::llvm::Builder2;
use crate::compiler::{Compiler, CompileResult, layout, params};

macro_rules! std_map {
    ($($l:literal: $i:ident),+) => {
        fn lookup(&self, name: &str) -> bool {
            matches!(name, $($l)|+)
        }

        fn register(&self, name: &str, builder: Builder2<'ctx>, fun: FunctionValue<'ctx>) -> CompileResult<'ctx, ()> {
            match name {
                $($l => self.$i(builder, fun)),+,
                _ => unimplemented!()
            }
        }
    }
}

/// A macro that makes the syntax for creating [`FunctionType`]s simpler.
macro_rules! fn_type {
    (($($e:expr),*) -> $r:expr) => {
        $r.fn_type(params![$($e),*], false)
    };
    ((~) -> $r:expr) => {
        $r.fn_type(params![], true)
    };
    (($($e:expr),+,~) -> $r:expr) => {
        $r.fn_type(params![$($e),+], true)
    };
}
pub(in crate::compiler) use fn_type;

impl<'ctx> Compiler<'ctx> {
    fn std_print(&self, builder: Builder2<'ctx>, fun: FunctionValue<'ctx>) -> CompileResult<'ctx, ()> {
        let _ptr = self.ptr_type(Default::default());
        
        let ptr = fun.get_first_param().unwrap().into_pointer_value();
        let dynarray_ptr = builder.build_struct_gep(layout!(self, S_STR), ptr, 0, "").unwrap();
        let buf_ptr = builder.build_struct_gep(layout!(self, "#dynarray"), dynarray_ptr, 0, "").unwrap();

        let puts = self.import_fun("puts", fn_type![(_ptr) -> layout!(self, S_INT)])?;
    
        let buf = builder.build_load(_ptr, buf_ptr, "buf");
        builder.build_call(puts, &[buf.into()], "");
        builder.build_return(None);
    
        Ok(())
    }

    fn std_printc(&self, builder: Builder2<'ctx>, fun: FunctionValue<'ctx>) -> CompileResult<'ctx, ()> {
        let _ptr = self.ptr_type(Default::default());
        let _int = layout!(self, S_INT);

        let p0 = fun.get_first_param().unwrap();
        let printf = self.import_fun("printf", fn_type!((_ptr, ~) -> _int))?;
    
        let template = unsafe { builder.build_global_string("%c\0", "printc") };
        builder.build_call(printf, params![template.as_pointer_value(), p0], "");
        builder.build_return(None);
    
        Ok(())
    }
    
    fn std_printd(&self, builder: Builder2<'ctx>, fun: FunctionValue<'ctx>) -> CompileResult<'ctx, ()> {
        let _ptr = self.ptr_type(Default::default());
        let _int = layout!(self, S_INT);

        let p0 = fun.get_first_param().unwrap();
        let printf = self.import_fun("printf", fn_type!((_ptr, ~) -> _int))?;
    
        let template = unsafe { builder.build_global_string("%d\0", "printd") };
        builder.build_call(printf, params![template.as_pointer_value(), p0], "");
        builder.build_return(None);
    
        Ok(())
    }

    fn dynarray_new(&self, builder: Builder2<'ctx>, fun: FunctionValue<'ctx>) -> CompileResult<'ctx, ()> {
        let _i64 = self.ctx.i64_type();
        let _dynarray = layout!(self, "#dynarray")
            .into_struct_type();
        
        let cap = fun.get_first_param().unwrap().into_int_value();

        self.import_heap();
        let malloc = self.module.get_function("malloc").unwrap();

        let buf = builder.build_call(malloc, &[cap.into()], "dynarray_buf")
            .try_as_basic_value()
            .unwrap_left()
            .into_pointer_value();

        let val = builder.create_struct_value(_dynarray, &[
            buf.into(), 
            _i64.const_zero().into(), 
            cap.into()
        ])?;

        builder.build_return(Some(&val));

        Ok(())
    }

    fn dynarray_resize(&self, builder: Builder2<'ctx>, fun: FunctionValue<'ctx>) -> CompileResult<'ctx, ()> {
        use inkwell::IntPredicate::UGT;

        let _dynarray = layout!(self, "#dynarray");
        let _int = layout!(self, S_INT);

        self.import_heap();
        let memcpy = self.module.get_function("memcpy").unwrap();
        let free = self.module.get_function("free").unwrap();
        
        let [dynarray_ptr, new_cap]: [_; 2] = *Box::try_from(fun.get_params()).unwrap();
        let dynarray_ptr = dynarray_ptr.into_pointer_value();
        let new_cap = new_cap.into_int_value();

        let dynarray = builder.build_load(_dynarray, dynarray_ptr, "")
            .into_struct_value();
        
        let old_buf = builder.build_extract_value(dynarray, 0, "old_buf")
            .expect("#dynarray buf")
            .into_pointer_value();
        let len = builder.build_extract_value(dynarray, 1, "len")
            .expect("#dynarray len")
            .into_int_value();
        let old_cap = builder.build_extract_value(dynarray, 2, "old_cap")
            .expect("#dynarray cap")
            .into_int_value();
    
        let needs_realloc = builder.build_int_compare(UGT, new_cap, old_cap, "");
        let realloc_bb = self.ctx.append_basic_block(fun, "realloc");
        let merge_bb = self.ctx.append_basic_block(fun, "merge");

        builder.build_conditional_branch(needs_realloc, realloc_bb, merge_bb);
        
        builder.position_at_end(realloc_bb);

        let dn = self.import_fun("#dynarray::new", fn_type![(_int) -> _dynarray])?;
        let alloc = builder.build_call(dn, params![new_cap], "alloc")
            .try_as_basic_value()
            .unwrap_left()
            .into_struct_value();

        let new_buf = builder.build_extract_value(alloc, 0, "new_buf")
            .expect("#dynarray buf")
            .into_pointer_value();

        builder.build_call(memcpy, params![new_buf, old_buf, len], "copy_buf");
        builder.build_insert_value(alloc, len, 1, "alloc");
        
        builder.build_store(dynarray_ptr, alloc);
        builder.build_call(free, params![old_buf], "free_old_buf");

        builder.build_unconditional_branch(merge_bb);
        
        builder.position_at_end(merge_bb);
        builder.build_return(None);
        Ok(())
    }

    fn dynarray_append(&self, builder: Builder2<'ctx>, fun: FunctionValue<'ctx>) -> CompileResult<'ctx, ()> {
        todo!()
    }

    fn dynarray_extend(&self, builder: Builder2<'ctx>, fun: FunctionValue<'ctx>) -> CompileResult<'ctx, ()> {
        let _dynarray = layout!(self, "#dynarray").into_struct_type();
        let _int = layout!(self, S_INT).into_int_type();
        let _ptr = self.ptr_type(Default::default());
        let _void = self.ctx.void_type();

        let [dynarray_ptr, bytes, add_len]: [_; 3] = *Box::try_from(fun.get_params()).unwrap();
        let dynarray_ptr = dynarray_ptr.into_pointer_value();

        let bytes = bytes.into_pointer_value();
        let add_len = add_len.into_int_value();

        let old_len_ptr = builder.build_struct_gep(_dynarray, dynarray_ptr, 1, "").unwrap();
        let old_len = builder.build_load(_int, old_len_ptr, "").into_int_value();
        let new_len = builder.build_int_add(old_len, add_len, "len");

        let dynarray_resize = self.import_fun("#dynarray::resize", fn_type![(_ptr, _int) -> _void])?;
        builder.build_call(dynarray_resize, params![dynarray_ptr, new_len], "");

        self.import_heap();
        let memcpy = self.module.get_function("memcpy").unwrap();
        let buf_ptr = builder.build_struct_gep(_dynarray, dynarray_ptr, 0, "").unwrap();
        let buf = builder.build_load(_ptr, buf_ptr, "buf").into_pointer_value();
        let shift_buf = unsafe {
            builder.build_gep(self.ctx.i8_type().array_type(0), buf, &[_int.const_zero(), old_len], "") 
        };
        builder.build_call(memcpy, params![shift_buf, bytes, add_len], "");
        builder.build_return(None);
        Ok(())
    }

    // HACK
    std_map! {
        "print": std_print,
        "printc": std_printc,
        "printd": std_printd,
        "#dynarray::new": dynarray_new,
        "#dynarray::resize": dynarray_resize,
        "#dynarray::append": dynarray_append,
        "#dynarray::extend": dynarray_extend
    }

    /// Import a defined internal function or a libc function.
    /// 
    /// Internal functions are currently defined in [`compiler::value::internals`].
    /// The type signature and identifier need to match exactly, or else defined internals may fail 
    /// or a segmentation fault may occur.
    pub(crate) fn import_fun(&self, s: &str, ty: FunctionType<'ctx>) -> CompileResult<'ctx, FunctionValue<'ctx>> {
        let fun = self.module.get_function(s).unwrap_or_else(|| self.module.add_function(s, ty, None));

        if self.lookup(s) && fun.count_basic_blocks() < 1 {
            let builder = Builder2::new(self.ctx.create_builder());
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

    /// Imports malloc, free, memcpy from libc
    pub(crate) fn import_heap(&self) {
        let _int = self.ptr_type(Default::default());
        let _i64 = layout!(self, S_INT);
        let _void = self.ctx.void_type();

        if self.module.get_function("malloc").is_none() {
            let malloc = fn_type![(_i64) -> _int];
            self.module.add_function("malloc", malloc, None);
        }
        if self.module.get_function("free").is_none() {
            let free = fn_type![(_int) -> _void];
            self.module.add_function("free", free, None);
        }
        if self.module.get_function("memcpy").is_none() {
            let memcpy = fn_type![(_int, _int, _i64) -> _int];
            self.module.add_function("memcpy", memcpy, None);
        }
    }
}