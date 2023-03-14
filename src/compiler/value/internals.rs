//! Internally defined structs for LLVM representation

use std::collections::HashMap;

use inkwell::types::{FunctionType, BasicType};
use inkwell::values::FunctionValue;
use lazy_static::lazy_static;

use crate::compiler::llvm::Builder2;
use crate::compiler::llvm::types::{FnTypeS, IntTypeS, PtrTypeS, VoidTypeS, RetTypeS, Concretize};
use crate::compiler::{Compiler, CompileResult, layout, params, CompileErr, fn_type};

macro_rules! map {
    () => { HashMap::new() };
    ($($k:expr => $v:expr),+) => {{
        let mut m = HashMap::new();
        $(
            m.insert($k, $v);
        )+
        m
    }}
}

macro_rules! fn_type_s {
    (($($e:expr),*) -> $r:expr) => {
        FnTypeS::new(params![$($e),*], false, RetTypeS::from($r))
    };
    ((~) -> $r:expr) => {
        FnTypeS::new(params![], true, RetTypeS::from($r))
    };
    (($($e:expr),+,~) -> $r:expr) => {
        FnTypeS::new(params![$($e),+], true, RetTypeS::from($r))
    };
}

lazy_static! {
    static ref CHAR: IntTypeS  = IntTypeS::I32;
    static ref INT:  IntTypeS  = IntTypeS::I64;
    static ref PTR:  PtrTypeS  = PtrTypeS;
    static ref VOID: VoidTypeS = VoidTypeS;

    static ref C_INTRINSICS: HashMap<&'static str, FnTypeS> = map! {
        "putwchar"  => fn_type_s![(*CHAR) -> *INT],
        "printf"    => fn_type_s![(*PTR /* i8* */, ~) -> *INT],
        "malloc"    => fn_type_s![(*INT) -> *PTR],
        "free"      => fn_type_s![(*PTR) -> *VOID],
        "memcpy"    => fn_type_s![(*PTR, *PTR, *INT) -> *PTR],
        "asprintf"  => fn_type_s![(*PTR /* i8** */, *PTR /* i8* */, ~) -> *INT],
        "setlocale" => fn_type_s![(*INT, *PTR /* i8* */) -> *PTR]
    };
}

macro_rules! std_map {
    (use $compiler:ident; $($(let $t:ident = $u:expr;)+)? $($l:literal: $i:ident, $closure:expr),+) => {
        fn lookup(&self, name: &str) -> Option<FunctionType<'ctx>> {
            let $compiler = self;
            $(
                $(let $t = $u;)+
            )?
            match name {
                $($l => Some($closure)),+,
                _ => None
            }
        }

        fn init_body(&self, name: &str, builder: Builder2<'ctx>, fun: FunctionValue<'ctx>) -> CompileResult<'ctx, ()> {
            match name {
                $($l => self.$i(builder, fun)),+,
                s => panic!("non-intrinsic {s} was imported, but does not have a defined body")
            }
        }
    }
}

impl<'ctx> Compiler<'ctx> {
    fn std_print(&self, builder: Builder2<'ctx>, fun: FunctionValue<'ctx>) -> CompileResult<'ctx, ()> {
        let _ptr = self.ptr_type(Default::default());
        let _str = layout!(self, S_STR);
        let _dynarray = layout!(self, "#dynarray");
        let _int = layout!(self, S_INT);
        
        let str_ptr = fun.get_first_param().unwrap().into_pointer_value();
        let dynarray_ptr = builder.build_struct_gep(_str, str_ptr, 0, "").unwrap();
        
        let buf_ptr = builder.build_struct_gep(_dynarray, dynarray_ptr, 0, "").unwrap();
        let len_ptr = builder.build_struct_gep(_dynarray, dynarray_ptr, 1, "").unwrap();
        let printf = self.std_import("printf")?;
        let template = unsafe { builder.build_global_string("%.*s\n\0", "_tmpl_print") };
    
        let buf = builder.build_load(_ptr, buf_ptr, "buf");
        let len = builder.build_load(_int, len_ptr, "len");
        builder.build_call(printf, params![template.as_pointer_value(), len, buf], "");
        
        builder.build_return(None);
        Ok(())
    }

    fn dynarray_new(&self, builder: Builder2<'ctx>, fun: FunctionValue<'ctx>) -> CompileResult<'ctx, ()> {
        let _i64 = self.ctx.i64_type();
        let _dynarray = layout!(self, "#dynarray")
            .into_struct_type();
        
        let malloc = self.std_import("malloc")?;

        let cap = fun.get_first_param().unwrap().into_int_value();
        
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

        let _dynarray = layout!(self, "#dynarray").into_struct_type();
        let _int = layout!(self, S_INT).into_int_type();
        let _ptr = self.ptr_type(Default::default());

        let malloc = self.std_import("malloc")?;
        let memcpy = self.std_import("memcpy")?;
        let free = self.std_import("free")?;
        
        let [dynarray_ptr, new_cap]: [_; 2] = *Box::try_from(fun.get_params()).unwrap();
        let dynarray_ptr = dynarray_ptr.into_pointer_value();
        let new_cap = new_cap.into_int_value();

        let dynarray = builder.build_typed_load(_dynarray, dynarray_ptr, "");
        
        let buf_ptr = builder.build_struct_gep(_dynarray, dynarray_ptr, 0, "")
            .expect("#dynarray buf");
        let old_buf = builder.build_typed_load(_ptr, buf_ptr, "old_buf");
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

        let new_buf = builder.build_call(malloc, params![new_cap], "dynarray_buf")
            .try_as_basic_value()
            .unwrap_left()
            .into_pointer_value();

        builder.build_call(memcpy, params![new_buf, old_buf, len], "copy_buf");

        builder.build_store(buf_ptr, new_buf);
        builder.build_call(free, params![old_buf], "free_old_buf");

        builder.build_unconditional_branch(merge_bb);
        
        builder.position_at_end(merge_bb);
        builder.build_return(None);
        Ok(())
    }

    fn dynarray_push(&self, builder: Builder2<'ctx>, fun: FunctionValue<'ctx>) -> CompileResult<'ctx, ()> {
        let _dynarray = layout!(self, "#dynarray");
        let _int = layout!(self, S_INT).into_int_type();
        let _i8 = self.ctx.i8_type();
        let _bytearr = _i8.array_type(0);

        let [dynarray_ptr, new_byte]: [_; 2] = *Box::try_from(fun.get_params()).unwrap();
        let dynarray_ptr = dynarray_ptr.into_pointer_value();
        let new_byte = new_byte.into_int_value();

        let len_ptr = builder.build_struct_gep(_dynarray, dynarray_ptr, 1, "").unwrap();
        let len = builder.build_typed_load(_int, len_ptr, "len");
        let len_p1 = builder.build_int_add(len, _int.const_int(1, false), "");
        
        let dynarray_resize = self.std_import("#dynarray::resize")?;
        builder.build_call(dynarray_resize, &[len_p1.into()], "");

        let buf_ptr = builder.build_struct_gep(_dynarray, dynarray_ptr, 0, "").unwrap();
        let push_ptr = unsafe { 
            builder.build_gep(_bytearr, buf_ptr, &[
                _int.const_int(0, false),
                len
            ], "")
        };
        builder.build_store(push_ptr, new_byte);
        builder.build_return(None);
        Ok(())
    }

    fn dynarray_pop(&self, builder: Builder2<'ctx>, fun: FunctionValue<'ctx>) -> CompileResult<'ctx, ()> {
        use inkwell::IntPredicate::EQ;
        
        let _dynarray = layout!(self, "#dynarray").into_struct_type();
        let _int = layout!(self, S_INT).into_int_type();
        let _i8 = self.ctx.i8_type();
        let _bytearr = _i8.array_type(0);

        let dynarray_ptr = fun.get_first_param().unwrap().into_pointer_value();
        
        let len_ptr = builder.build_struct_gep(_dynarray, dynarray_ptr, 1, "").unwrap();
        let len = builder.build_typed_load(_int, len_ptr, "len");
        let len_m1 = builder.build_int_sub(len, _int.const_int(1, false), "");
        
        let len_iz = builder.build_int_compare(EQ, len, _int.const_zero(), "");
        let new_len = builder.build_typed_select1(len_iz, len, len_m1, "");
        
        let buf_ptr = builder.build_struct_gep(_dynarray, dynarray_ptr, 0, "").unwrap();
        let popped_ptr = unsafe { 
            builder.build_gep(_bytearr, buf_ptr, &[
                _int.const_int(0, false),
                new_len
            ], "")
        };
        let nz_popped = builder.build_typed_load(_i8, popped_ptr, "nz_pop");
        let popped = builder.build_select(len_iz, _int.const_zero(), nz_popped, ""); // TODO: null
        
        builder.build_return(Some(&popped));
        Ok(())
    }

    fn dynarray_extend(&self, builder: Builder2<'ctx>, fun: FunctionValue<'ctx>) -> CompileResult<'ctx, ()> {
        let _dynarray = layout!(self, "#dynarray").into_struct_type();
        let _int = layout!(self, S_INT).into_int_type();
        let _ptr = self.ptr_type(Default::default());
        let _void = self.ctx.void_type();
        let _bytearr = self.ctx.i8_type().array_type(0);

        let dynarray_resize = self.std_import("#dynarray::resize")?;
        let memcpy = self.std_import("memcpy")?;

        let [dynarray_ptr, bytes, add_len]: [_; 3] = *Box::try_from(fun.get_params()).unwrap();
        let dynarray_ptr = dynarray_ptr.into_pointer_value();

        let bytes = bytes.into_pointer_value();
        let add_len = add_len.into_int_value();

        let old_len_ptr = builder.build_struct_gep(_dynarray, dynarray_ptr, 1, "").unwrap();
        let old_len = builder.build_typed_load(_int, old_len_ptr, "");
        let new_len = builder.build_int_add(old_len, add_len, "len");

        builder.build_call(dynarray_resize, params![dynarray_ptr, new_len], "");

        let buf_ptr = builder.build_struct_gep(_dynarray, dynarray_ptr, 0, "").unwrap();
        let buf = builder.build_typed_load(_ptr, buf_ptr, "buf");
        let shift_buf = unsafe {
            builder.build_gep(_bytearr, buf, &[_int.const_zero(), old_len], "") 
        };
        builder.build_call(memcpy, params![shift_buf, bytes, add_len], "");
        let len_ptr = builder.build_struct_gep(_dynarray, dynarray_ptr, 1, "").unwrap();
        builder.build_store(len_ptr, new_len);

        builder.build_return(None);
        Ok(())
    }

    fn x_to_string(&self, builder: Builder2<'ctx>, fun: FunctionValue<'ctx>, fmt: &str, tmpl_name: &str) -> CompileResult<'ctx, ()> {
        let _int = layout!(self, S_INT).into_int_type();
        let _ptr = self.ptr_type(Default::default());
        let _dynarray = layout!(self, "#dynarray").into_struct_type();
        let _str = layout!(self, S_STR).into_struct_type();
    
        let val = fun.get_first_param().unwrap();
        let buf_ptr = builder.build_alloca(_ptr, "buf_ptr");
        
        let template = unsafe { builder.build_global_string(fmt, tmpl_name) };
        let asprintf = self.std_import("asprintf")?;
        
        let len = builder.build_call(asprintf, params![buf_ptr, template.as_pointer_value(), val], "len")
            .try_as_basic_value()
            .unwrap_left()
            .into_int_value();
        let buf = builder.build_typed_load(_ptr, buf_ptr, "buf");
        let cap = builder.build_int_add(len, _int.const_int(1, false), "cap");
    
        // bc asprintf creates a malloc'd ptr, we can build dynarray by ourselves.
        // Additionally, Poligon strings are represented without the null terminator,
        // so, the extra null terminator is free capacity.
        let dynarray = builder.create_struct_value(_dynarray, params![buf, len, cap])?;
        let string = builder.create_struct_value(_str, params![dynarray])?;
    
        builder.build_return(Some(&string));
        Ok(())
    }

    fn int_to_string(&self, builder: Builder2<'ctx>, fun: FunctionValue<'ctx>) -> CompileResult<'ctx, ()> {
        self.x_to_string(builder, fun, "%d\0", "_tmpl_int_to_string")
    }
    fn float_to_string(&self, builder: Builder2<'ctx>, fun: FunctionValue<'ctx>) -> CompileResult<'ctx, ()> {
        self.x_to_string(builder, fun, "%#f\0", "_tmpl_float_to_string")
    }
    fn char_to_string(&self, builder: Builder2<'ctx>, fun: FunctionValue<'ctx>) -> CompileResult<'ctx, ()> {
        self.x_to_string(builder, fun, "%lc\0", "_tmpl_char_to_string")
    }

    // HACK
    std_map! {
        use c;
        let _str   = layout!(c, S_STR);
        let _char  = layout!(c, S_CHAR);
        let _int   = layout!(c, S_INT);
        let _float = layout!(c, S_FLOAT);
        let _i8    = c.ctx.i8_type();
        let _void  = c.ctx.void_type();
        let _dynarray = layout!(c, "#dynarray");
        let _ptr = c.ptr_type(Default::default());

        "print": std_print, fn_type![(_str)  -> _void],
        "#dynarray::new": dynarray_new, fn_type![(_int) -> _dynarray],
        "#dynarray::resize": dynarray_resize, fn_type![(_ptr /* "#dynarray"* */, _int) -> _void],
        "#dynarray::push": dynarray_push, fn_type![(_ptr /* "#dynarray"* */, _i8)  -> _void],
        "#dynarray::extend": dynarray_extend, fn_type![(_ptr /* "#dynarray"* */, _ptr /* i8* */, _int) -> _void],
        "#dynarray::pop": dynarray_pop, fn_type![(_ptr /* "#dynarray"* */) -> _i8],
        "int__to_string": int_to_string, fn_type![(_int) -> _str],
        "float__to_string": float_to_string, fn_type![(_float) -> _str],
        "char__to_string": char_to_string, fn_type![(_char) -> _str]
    }

    /// Import a defined internal function or a libc function.
    /// 
    /// Internal functions are currently defined in [`compiler::value::internals`].
    /// The type signature and identifier need to match exactly, or else defined internals may fail 
    /// or a segmentation fault may occur.
    pub(crate) fn std_import(&self, s: &str) -> CompileResult<'ctx, FunctionValue<'ctx>> {
        let intrinsic = C_INTRINSICS.get(s).map(|f| f.as_concrete(self));
        let fun = match self.module.get_function(s) {
            Some(fun) => fun,
            None => {
                let ty = intrinsic.or_else(|| self.lookup(s))
                    .ok_or_else(|| CompileErr::CannotImport(String::from(s)))?;

                self.module.add_function(s, ty, None)
            }
        };

        if intrinsic.is_none() && fun.count_basic_blocks() < 1 {
            let builder = Builder2::new(self.ctx.create_builder());
            let bb = self.ctx.append_basic_block(fun, "body");
            builder.position_at_end(bb);

            self.init_body(s, builder, fun)?;

            if !fun.verify(true) {
                // SAFETY: Not used after.
                unsafe { fun.delete() }
                panic!("could not import fun '{s}'");
            }
        }

        Ok(fun)
    }
}