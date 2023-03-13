use inkwell::types::{IntType, FloatType, PointerType, StructType, FunctionType, BasicTypeEnum, VoidType};

use crate::compiler::{Compiler, ReturnableType};

pub(in crate::compiler) trait Concretize<'ctx> {
    type Type;
    fn as_concrete(&self, compiler: &Compiler<'ctx>) -> Self::Type;
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntTypeS {
    #[allow(dead_code)]
    Bool,
    #[allow(dead_code)]
    I8,
    // I16,
    I32,
    I64
}
impl<'ctx> Concretize<'ctx> for IntTypeS {
    type Type = IntType<'ctx>;

    fn as_concrete(&self, compiler: &Compiler<'ctx>) -> Self::Type {
        match self {
            IntTypeS::Bool => compiler.ctx.bool_type(),
            IntTypeS::I8   => compiler.ctx.i8_type(),
            IntTypeS::I32  => compiler.ctx.i32_type(),
            IntTypeS::I64  => compiler.ctx.i64_type(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct FloatTypeS;
impl<'ctx> Concretize<'ctx> for FloatTypeS {
    type Type = FloatType<'ctx>;

    fn as_concrete(&self, compiler: &Compiler<'ctx>) -> Self::Type {
        compiler.ctx.f64_type()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct PtrTypeS;
impl<'ctx> Concretize<'ctx> for PtrTypeS {
    type Type = PointerType<'ctx>;

    fn as_concrete(&self, compiler: &Compiler<'ctx>) -> Self::Type {
        compiler.ptr_type(Default::default())
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct StructTypeS(String);
impl<'ctx> Concretize<'ctx> for StructTypeS {
    type Type = StructType<'ctx>;

    fn as_concrete(&self, compiler: &Compiler<'ctx>) -> Self::Type {
        compiler.ctx.get_struct_type(&self.0)
            .unwrap_or_else(|| panic!("expected struct with name {}", self.0))
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct VoidTypeS;
impl<'ctx> Concretize<'ctx> for VoidTypeS {
    type Type = VoidType<'ctx>;

    fn as_concrete(&self, compiler: &Compiler<'ctx>) -> Self::Type {
        compiler.ctx.void_type()
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct FnTypeS {
    params: Vec<BasicTypeEnumS>, 
    ret: RetTypeS, 
    var_args: bool
}
impl FnTypeS {
    pub fn new(params: &[BasicTypeEnumS], var_args: bool, ret: RetTypeS) -> Self {
        Self {
            params: params.to_vec(),
            ret,
            var_args,
        }
    }
}
impl<'ctx> Concretize<'ctx> for FnTypeS {
    type Type = FunctionType<'ctx>;

    fn as_concrete(&self, compiler: &Compiler<'ctx>) -> Self::Type {
        let params: Vec<_> = self.params.iter()
            .map(|t| t.as_concrete(compiler))
            .map(Into::into)
            .collect();
        self.ret.as_concrete(compiler).fn_type(&params, self.var_args)
    }
}

macro_rules! enum_s {
    ($name:ident => $into:ident: $($t:ident),*) => {
        #[derive(Clone, PartialEq, Eq, Hash)]
        pub enum $name {
            $(
                $t($t)
            ),*
        }

        $(
            impl From<$t> for $name {
                fn from(value: $t) -> Self {
                    Self::$t(value)
                }
            }
        )*

        impl<'ctx> Concretize<'ctx> for $name {
            type Type = $into<'ctx>;
        
            fn as_concrete(&self, compiler: &Compiler<'ctx>) -> Self::Type {
                match self {
                    $(
                        Self::$t(t) => t.as_concrete(compiler).into(),
                    )*
                }
            }
        }
    }
}
macro_rules! btes_to_rtes {
    ($($t:ident),*) => {
        $(
            impl From<$t> for RetTypeS {
                fn from(value: $t) -> Self {
                    Self::from(BasicTypeEnumS::from(value))
                }
            }
        )*
    }
}

enum_s!(BasicTypeEnumS => BasicTypeEnum: IntTypeS, FloatTypeS, PtrTypeS, StructTypeS);
btes_to_rtes!(IntTypeS, FloatTypeS, PtrTypeS, StructTypeS);

enum_s!(RetTypeS => ReturnableType: BasicTypeEnumS, VoidTypeS);