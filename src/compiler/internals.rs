//! Internally defined structs for LLVM representation

use std::collections::HashMap;

use inkwell::values::FunctionValue;
use lazy_static::lazy_static;

use crate::compiler::llvm::types::{FnTypeS, IntTypeS, PtrTypeS, VoidTypeS, RetTypeS, Concretize};
use crate::compiler::{LLVMCodegen, LLVMResult, LLVMErr, plir};

use super::llvm_codegen::params;

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

impl<'ctx> LLVMCodegen<'ctx> {
    /// Import a defined internal function or a libc function.
    /// 
    /// Internal functions are currently defined in [`compiler::value::internals`].
    /// The type signature and identifier need to match exactly, or else defined internals may fail 
    /// or a segmentation fault may occur.
    pub(crate) fn import_libc(&self, s: &str) -> LLVMResult<'ctx, FunctionValue<'ctx>> {
        let intrinsic = C_INTRINSICS_LLVM.get(s)
            .map(|f| f.as_concrete(self));
        let fun = match self.module.get_function(s) {
            Some(fun) => fun,
            None => {
                let ty = intrinsic
                    .ok_or_else(|| LLVMErr::CannotImport(String::from(s)))?;

                self.module.add_function(s, ty, None)
            }
        };

        Ok(fun)
    }
}