//! Internally defined structs for LLVM representation

use std::collections::HashMap;

use inkwell::types::{FunctionType, BasicType};
use inkwell::values::FunctionValue;
use lazy_static::lazy_static;

use crate::compiler::llvm::Builder2;
use crate::compiler::llvm::types::{FnTypeS, IntTypeS, PtrTypeS, VoidTypeS, RetTypeS, Concretize};
use crate::compiler::{LLVMCodegen, LLVMResult, LLVMErr, plir};

use super::llvm_codegen::{layout, params, fn_type};

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

macro_rules! fun_type {
    (($($e:expr),*) -> $r:expr) => {
        plir::FunType::new(vec![$($e),*], $r, false)
    };
    ((~) -> $r:expr) => {
        plir::FunType::new(vec![], $r, true)
    };
    (($($e:expr),+,~) -> $r:expr) => {
        plir::FunType::new(vec![$($e),+], $r, true)
    };
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

macro_rules! c_intrinsics {
    ($($c:ident: {$f1:expr, $f2:expr}),*) => {
        lazy_static! {
            pub(in crate::compiler) static ref C_INTRINSICS_LLVM: HashMap<&'static str, FnTypeS> = map! {
                $(stringify!($c) => $f2),*
            };
        }
        lazy_static! {
            pub(in crate::compiler) static ref C_INTRINSICS_PLIR: HashMap<&'static str, plir::FunType> = map! {
                $(stringify!($c) => $f1),*
            };
        }
    }
}

lazy_static! {
    static ref CHAR_L: IntTypeS = IntTypeS::I32;
    static ref INT_L: IntTypeS = IntTypeS::I64;
    static ref PTR_L: PtrTypeS = PtrTypeS;
    static ref VOID_L: VoidTypeS = VoidTypeS;
    
    static ref INT_P: plir::Type = plir::ty!(plir::Type::S_INT);
    static ref CHAR_P: plir::Type = plir::ty!(plir::Type::S_CHAR);
    static ref PTR_P: plir::Type = plir::ty!("#ptr");
    static ref VOID_P: plir::Type = plir::ty!(plir::Type::S_VOID);
}
c_intrinsics! {
    putwchar: { // (char) -> int
        fun_type![(CHAR_P.clone()) -> INT_P.clone()],
        fn_type_s![(*CHAR_L) -> *INT_L]
    },
    printf: { // (char*, ..) -> int
        fun_type![(PTR_P.clone(), ~) -> INT_P.clone()],
        fn_type_s![(*PTR_L, ~) -> *INT_L]
    },
    malloc: { // (int) -> void*
        fun_type![(INT_P.clone()) -> PTR_P.clone()],
        fn_type_s![(*INT_L) -> *PTR_L]
    },
    free: { // (void*) -> void
        fun_type![(PTR_P.clone()) -> VOID_P.clone()],
        fn_type_s![(*PTR_L) -> *VOID_L]
    },
    memcpy: { // (void*, void*, int) -> void* /// (dest, src, len) -> dest
        fun_type![(PTR_P.clone(), PTR_P.clone(), INT_P.clone()) -> PTR_P.clone()],
        fn_type_s![(*PTR_L, *PTR_L, *INT_L) -> *PTR_L]
    },
    asprintf: { // (char**, char*, ..) -> int
        fun_type![(PTR_P.clone(), PTR_P.clone(), ~) -> INT_P.clone()],
        fn_type_s![(*PTR_L, *PTR_L, ~) -> *INT_L]
    },
    setlocale: { // (int, char*) -> char* /// (cat, locale) -> locale
        fun_type![(INT_P.clone(), PTR_P.clone()) -> PTR_P.clone()],
        fn_type_s![(*INT_L, *PTR_L) -> *PTR_L]
    }
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

        fn init_body(&self, name: &str, builder: Builder2<'ctx>, fun: FunctionValue<'ctx>) -> LLVMResult<'ctx, ()> {
            match name {
                $($l => self.$i(builder, fun)),+,
                s => panic!("non-intrinsic {s} was imported, but does not have a defined body")
            }
        }
    }
}

impl<'ctx> LLVMCodegen<'ctx> {
    fn x_to_string(&self, builder: Builder2<'ctx>, fun: FunctionValue<'ctx>, fmt: &str, tmpl_name: &str) -> LLVMResult<'ctx, ()> {
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

    fn int_to_string(&self, builder: Builder2<'ctx>, fun: FunctionValue<'ctx>) -> LLVMResult<'ctx, ()> {
        self.x_to_string(builder, fun, "%d\0", "_tmpl_int_to_string")
    }
    fn float_to_string(&self, builder: Builder2<'ctx>, fun: FunctionValue<'ctx>) -> LLVMResult<'ctx, ()> {
        self.x_to_string(builder, fun, "%#f\0", "_tmpl_float_to_string")
    }
    fn char_to_string(&self, builder: Builder2<'ctx>, fun: FunctionValue<'ctx>) -> LLVMResult<'ctx, ()> {
        self.x_to_string(builder, fun, "%lc\0", "_tmpl_char_to_string")
    }

    // HACK
    std_map! {
        use c;
        let _str   = layout!(c, S_STR);
        let _char  = layout!(c, S_CHAR);
        let _int   = layout!(c, S_INT);
        let _float = layout!(c, S_FLOAT);

        "int__to_string": int_to_string, fn_type![(_int) -> _str],
        "float__to_string": float_to_string, fn_type![(_float) -> _str],
        "char__to_string": char_to_string, fn_type![(_char) -> _str]
    }

    /// Import a defined internal function or a libc function.
    /// 
    /// Internal functions are currently defined in [`compiler::value::internals`].
    /// The type signature and identifier need to match exactly, or else defined internals may fail 
    /// or a segmentation fault may occur.
    pub(crate) fn std_import(&self, s: &str) -> LLVMResult<'ctx, FunctionValue<'ctx>> {
        let intrinsic = C_INTRINSICS_LLVM.get(s)
            .map(|f| f.as_concrete(self));
        let fun = match self.module.get_function(s) {
            Some(fun) => fun,
            None => {
                let ty = intrinsic.or_else(|| self.lookup(s))
                    .ok_or_else(|| LLVMErr::CannotImport(String::from(s)))?;

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