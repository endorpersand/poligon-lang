//! Internally defined structs for LLVM representation

use std::collections::HashMap;

use inkwell::values::FunctionValue;
use lazy_static::lazy_static;

use crate::compiler::llvm::types::{FnTypeS, IntTypeS, PtrTypeS, VoidTypeS, RetTypeS, Concretize, FloatTypeS};
use crate::compiler::{LLVMCodegen, LLVMResult, LLVMErr, plir};

use super::llvm_codegen::params;

macro_rules! map {
    () => { HashMap::new() };
    ($($k:expr => $v:expr),+ $(,)?) => {{
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
    ($($c:ident: {$alias:expr, $f1:expr, $f2:expr}),* $(,)?) => {
        lazy_static! {
            pub(in crate::compiler) static ref C_INTRINSICS_LLVM: HashMap<&'static str, (&'static str, FnTypeS)> = map! {
                $(concat!("#", stringify!($c)) => ($alias, $f2)),*
            };
        }
        lazy_static! {
            pub(in crate::compiler) static ref C_INTRINSICS_PLIR: HashMap<&'static str, plir::FunType> = map! {
                $(concat!("#", stringify!($c)) => $f1),*
            };
        }
    }
}

lazy_static! {
    static ref BOOL_L:  IntTypeS   = IntTypeS::Bool;
    static ref CHAR_L:  IntTypeS   = IntTypeS::I32;
    static ref INT_L:   IntTypeS   = IntTypeS::I64;
    static ref FLOAT_L: FloatTypeS = FloatTypeS;
    static ref PTR_L:   PtrTypeS   = PtrTypeS;
    static ref VOID_L:  VoidTypeS  = VoidTypeS;
    
    static ref BOOL_P:  plir::Type = plir::ty!(plir::Type::S_BOOL);
    static ref CHAR_P:  plir::Type = plir::ty!(plir::Type::S_CHAR);
    static ref INT_P:   plir::Type = plir::ty!(plir::Type::S_INT);
    static ref FLOAT_P: plir::Type = plir::ty!(plir::Type::S_FLOAT);
    static ref PTR_P:   plir::Type = plir::ty!("#ptr");
    static ref VOID_P:  plir::Type = plir::ty!(plir::Type::S_VOID);
}
c_intrinsics! {
    putwchar: { // (char) -> int
        "putwchar",
        fun_type![(CHAR_P.clone()) -> INT_P.clone()],
        fn_type_s![(*CHAR_L) -> *INT_L]
    },
    printf: { // (char*, ..) -> int
        "printf",
        fun_type![(PTR_P.clone(), ~) -> INT_P.clone()],
        fn_type_s![(*PTR_L, ~) -> *INT_L]
    },
    malloc: { // (int) -> void*
        "malloc",
        fun_type![(INT_P.clone()) -> PTR_P.clone()],
        fn_type_s![(*INT_L) -> *PTR_L]
    },
    free: { // (void*) -> void
        "free",
        fun_type![(PTR_P.clone()) -> VOID_P.clone()],
        fn_type_s![(*PTR_L) -> *VOID_L]
    },
    memcpy: { // (void*, void*, int) -> void* /// (dest, src, len) -> dest
        "memcpy",
        fun_type![(PTR_P.clone(), PTR_P.clone(), INT_P.clone()) -> PTR_P.clone()],
        fn_type_s![(*PTR_L, *PTR_L, *INT_L) -> *PTR_L]
    },
    asprintf: { // (char**, char*, ..) -> int
        "asprintf",
        fun_type![(PTR_P.clone(), PTR_P.clone(), ~) -> INT_P.clone()],
        fn_type_s![(*PTR_L, *PTR_L, ~) -> *INT_L]
    },
    setlocale: { // (int, char*) -> char* /// (cat, locale) -> locale
        "setlocale",
        fun_type![(INT_P.clone(), PTR_P.clone()) -> PTR_P.clone()],
        fn_type_s![(*INT_L, *PTR_L) -> *PTR_L]
    },
    abs: { // (int) -> int
        "llvm.abs.i64",
        fun_type![(INT_P.clone()) -> INT_P.clone()],
        fn_type_s![(*INT_L) -> *INT_L]
    },
    fabs: { // (float) -> float
        "llvm.fabs.f64",
        fun_type![(FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L) -> *FLOAT_L]
    },
    // ldiv: { // (int, int) -> (int, int)

    // }
    fma: { // (float, float, float) -> float
        "llvm.fma.f64",
        fun_type![(FLOAT_P.clone(), FLOAT_P.clone(), FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L, *FLOAT_L, *FLOAT_L) -> *FLOAT_L]
    },
    smax: { // (int, int) -> int
        "llvm.smax.i64",
        fun_type![(INT_P.clone(), INT_P.clone()) -> INT_P.clone()],
        fn_type_s![(*INT_L, *INT_L) -> *INT_L]
    },
    smin: { // (int, int) -> int
        "llvm.smin.i64",
        fun_type![(INT_P.clone(), INT_P.clone()) -> INT_P.clone()],
        fn_type_s![(*INT_L, *INT_L) -> *INT_L]
    },
    fmax: { // (float, float) -> float
        "llvm.fmax.f64",
        fun_type![(FLOAT_P.clone(), FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L, *FLOAT_L) -> *FLOAT_L]
    },
    fmin: { // (float, float) -> float
        "llvm.fmin.f64",
        fun_type![(FLOAT_P.clone(), FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L, *FLOAT_L) -> *FLOAT_L]
    },
    // nanl: {

    // },
    exp: { // (float) -> float
        "llvm.exp.f64",
        fun_type![(FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L) -> *FLOAT_L]
    },
    exp2: { // (float) -> float
        "llvm.exp2.f64",
        fun_type![(FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L) -> *FLOAT_L]
    },
    expm1: { // (float) -> float
        "expm1",
        fun_type![(FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L) -> *FLOAT_L]
    },
    log: { // (float) -> float
        "llvm.log.f64",
        fun_type![(FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L) -> *FLOAT_L]
    },
    log2: { // (float) -> float
        "llvm.log2.f64",
        fun_type![(FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L) -> *FLOAT_L]
    },
    log10: { // (float) -> float
        "llvm.log10.f64",
        fun_type![(FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L) -> *FLOAT_L]
    },
    log1p: { // (float) -> float
        "log1p",
        fun_type![(FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L) -> *FLOAT_L]
    },
    sqrt: { // (float) -> float
        "llvm.sqrt.f64",
        fun_type![(FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L) -> *FLOAT_L]
    },
    cbrt: { // (float) -> float
        "cbrt",
        fun_type![(FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L) -> *FLOAT_L]
    },
    hypot: { // (float, float) -> float
        "hypot",
        fun_type![(FLOAT_P.clone(), FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L, *FLOAT_L) -> *FLOAT_L]
    },
    pow: { // (float, float) -> float
        "llvm.pow.f64",
        fun_type![(FLOAT_P.clone(), FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L, *FLOAT_L) -> *FLOAT_L]
    },
    powi: { // (float int) -> float
        "llvm.powi.f64.i64",
        fun_type![(FLOAT_P.clone(), INT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L, *INT_L) -> *FLOAT_L]
    },
    sin: { // (float) -> float
        "llvm.sin.f64",
        fun_type![(FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L) -> *FLOAT_L]
    },
    cos: { // (float) -> float
        "llvm.cos.f64",
        fun_type![(FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L) -> *FLOAT_L]
    },
    tan: { // (float) -> float
        "tan",
        fun_type![(FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L) -> *FLOAT_L]
    },
    asin: { // (float) -> float
        "asin",
        fun_type![(FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L) -> *FLOAT_L]
    },
    acos: { // (float) -> float
        "acos",
        fun_type![(FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L) -> *FLOAT_L]
    },
    atan: { // (float) -> float
        "atan",
        fun_type![(FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L) -> *FLOAT_L]
    },
    atan2: { // (float, float) -> float
        "atan2",
        fun_type![(FLOAT_P.clone(), INT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L, *INT_L) -> *FLOAT_L]
    },
    sinh: { // (float) -> float
        "sinh",
        fun_type![(FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L) -> *FLOAT_L]
    },
    cosh: { // (float) -> float
        "cosh",
        fun_type![(FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L) -> *FLOAT_L]
    },
    tanh: { // (float) -> float
        "tanh",
        fun_type![(FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L) -> *FLOAT_L]
    },
    asinh: { // (float) -> float
        "asinh",
        fun_type![(FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L) -> *FLOAT_L]
    },
    acosh: { // (float) -> float
        "acosh",
        fun_type![(FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L) -> *FLOAT_L]
    },
    atanh: { // (float) -> float
        "atanh",
        fun_type![(FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L) -> *FLOAT_L]
    },
    erf: { // (float) -> float
        "erf",
        fun_type![(FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L) -> *FLOAT_L]
    },
    erfc: { // (float) -> float
        "erfc",
        fun_type![(FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L) -> *FLOAT_L]
    },
    lgamma: { // (float) -> float
        "lgamma",
        fun_type![(FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L) -> *FLOAT_L]
    },
    tgamma: { // (float) -> float
        "tgamma",
        fun_type![(FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L) -> *FLOAT_L]
    },
    ceil: { // (float) -> float
        "llvm.ceil.f64",
        fun_type![(FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L) -> *FLOAT_L]
    },
    floor: { // (float) -> float
        "llvm.floor.f64",
        fun_type![(FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L) -> *FLOAT_L]
    },
    trunc: { // (float) -> float
        "llvm.trunc.f64",
        fun_type![(FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L) -> *FLOAT_L]
    },
    round: { // (float) -> float
        "round",
        fun_type![(FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L) -> *FLOAT_L]
    },
    lround: { // (float) -> int
        "lround",
        fun_type![(FLOAT_P.clone()) -> INT_P.clone()],
        fn_type_s![(*FLOAT_L) -> *INT_L]
    },
    // nearbyint: {

    // },
    // lrint: {

    // },
    // frexp: {

    // },
    // ldexp: {

    // },
    // modf: {

    // },
    // scalbln: {

    // },
    nexttoward: { // (float, float) -> float
        "nexttoward",
        fun_type![(FLOAT_P.clone(), FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L, *INT_L) -> *FLOAT_L]
    },
    copysign: { // (float, float) -> float
        "llvm.copysign.f64",
        fun_type![(FLOAT_P.clone(), FLOAT_P.clone()) -> FLOAT_P.clone()],
        fn_type_s![(*FLOAT_L, *INT_L) -> *FLOAT_L]
    },
    // fpclassify: {

    // },
    isfinite: { // (float) -> bool
        "isfinite",
        fun_type![(FLOAT_P.clone()) -> BOOL_P.clone()],
        fn_type_s![(*FLOAT_L) -> *BOOL_L]
    },
    isinf: { // (float) -> bool
        "isinf",
        fun_type![(FLOAT_P.clone()) -> BOOL_P.clone()],
        fn_type_s![(*FLOAT_L) -> *BOOL_L]
    },
    isnan: { // (float) -> bool
        "isnan",
        fun_type![(FLOAT_P.clone()) -> BOOL_P.clone()],
        fn_type_s![(*FLOAT_L) -> *BOOL_L]
    },
    isnormal: { // (float) -> bool
        "isnormal",
        fun_type![(FLOAT_P.clone()) -> BOOL_P.clone()],
        fn_type_s![(*FLOAT_L) -> *BOOL_L]
    },
    // signbit: {

    // }
    bitreverse: { // (int) -> int
        "llvm.bitreverse.i64",
        fun_type![(INT_P.clone()) -> INT_P.clone()],
        fn_type_s![(*INT_L) -> *INT_L]
    },
    bswap: { // (int) -> int
        "llvm.bswap.i64",
        fun_type![(INT_P.clone()) -> INT_P.clone()],
        fn_type_s![(*INT_L) -> *INT_L]
    },
    ctlz: { // (int, bool) -> int
        "llvm.ctlz.i64",
        fun_type![(INT_P.clone(), BOOL_P.clone()) -> INT_P.clone()],
        fn_type_s![(*INT_L, *BOOL_L) -> *INT_L]
    },
    cttz: { // (int, bool) -> int
        "llvm.cttz.i64",
        fun_type![(INT_P.clone(), BOOL_P.clone()) -> INT_P.clone()],
        fn_type_s![(*INT_L, *BOOL_L) -> *INT_L]
    },
}

impl<'ctx> LLVMCodegen<'ctx> {
    /// Import a defined internal function or a libc function.
    /// 
    /// Internal functions are currently defined in [`compiler::value::internals`].
    /// The type signature and identifier need to match exactly, or else defined internals may fail 
    /// or a segmentation fault may occur.
    pub(crate) fn import_intrinsic(&self, s: &str) -> LLVMResult<'ctx, FunctionValue<'ctx>> {
        // lookup intrinsic on HashMap
        let (alias, ty) = C_INTRINSICS_LLVM.get(s)
            .map(|(ident, ty)| (*ident, ty.as_concrete(self)))
            .ok_or_else(|| LLVMErr::CannotImport(String::from(s)))?;

        // access that fn
        let fun = self.module.get_function(alias)
            .unwrap_or_else(|| {
                self.module.add_function(alias, ty, None)
            });

        Ok(fun)
    }
}