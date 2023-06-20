use std::fmt::{Display, Formatter};

use inkwell::types::{IntType, FloatType, PointerType, StructType, FunctionType, BasicTypeEnum, VoidType, BasicType, BasicMetadataTypeEnum, ArrayType, VectorType};

use crate::compiler::{LLVMCodegen, to_str};

pub(in crate::compiler) trait Concretize<'ctx> {
    type Type;
    fn as_concrete(&self, compiler: &LLVMCodegen<'ctx>) -> Self::Type;
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
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

    fn as_concrete(&self, compiler: &LLVMCodegen<'ctx>) -> Self::Type {
        match self {
            IntTypeS::Bool => compiler.ctx.bool_type(),
            IntTypeS::I8   => compiler.ctx.i8_type(),
            IntTypeS::I32  => compiler.ctx.i32_type(),
            IntTypeS::I64  => compiler.ctx.i64_type(),
        }
    }
}
impl<'ctx> From<IntType<'ctx>> for IntTypeS {
    fn from(value: IntType<'ctx>) -> Self {
        match value.get_bit_width() {
            1  => IntTypeS::Bool,
            8  => IntTypeS::I8,
            32 => IntTypeS::I32,
            64 => IntTypeS::I64,
            s  => unimplemented!("IntTypeS for size {s}")
        }
    }
}
impl Display for IntTypeS {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            IntTypeS::Bool => write!(f, "i1"),
            IntTypeS::I8   => write!(f, "i8"),
            IntTypeS::I32  => write!(f, "i32"),
            IntTypeS::I64  => write!(f, "i64"),
        }
    }
}
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct FloatTypeS;
impl<'ctx> Concretize<'ctx> for FloatTypeS {
    type Type = FloatType<'ctx>;

    fn as_concrete(&self, compiler: &LLVMCodegen<'ctx>) -> Self::Type {
        compiler.ctx.f64_type()
    }
}
impl<'ctx> From<FloatType<'ctx>> for FloatTypeS {
    fn from(_: FloatType<'ctx>) -> Self {
        // FIXME: obviously does not work for non-f64's
        FloatTypeS
    }
}
impl Display for FloatTypeS {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "f64")
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct PtrTypeS;
impl<'ctx> Concretize<'ctx> for PtrTypeS {
    type Type = PointerType<'ctx>;

    fn as_concrete(&self, compiler: &LLVMCodegen<'ctx>) -> Self::Type {
        compiler.ptr_type(Default::default())
    }
}
impl<'ctx> From<PointerType<'ctx>> for PtrTypeS {
    fn from(_: PointerType<'ctx>) -> Self {
        PtrTypeS
    }
}
impl Display for PtrTypeS {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "ptr")
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct StructTypeS(String);
impl<'ctx> Concretize<'ctx> for StructTypeS {
    type Type = StructType<'ctx>;

    fn as_concrete(&self, compiler: &LLVMCodegen<'ctx>) -> Self::Type {
        compiler.ctx.get_struct_type(&self.0)
            .unwrap_or_else(|| panic!("expected struct with name {}", self.0))
    }
}
impl<'ctx> From<StructType<'ctx>> for StructTypeS {
    fn from(value: StructType<'ctx>) -> Self {
        let name = to_str! { value.get_name().unwrap() }
            .to_string();
        StructTypeS(name)
    }
}
impl Display for StructTypeS {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct ArrayTypeS(Box<BasicTypeEnumS>, u32);
impl<'ctx> Concretize<'ctx> for ArrayTypeS {
    type Type = ArrayType<'ctx>;

    fn as_concrete(&self, compiler: &LLVMCodegen<'ctx>) -> Self::Type {
        let bv = self.0.as_concrete(compiler);
        bv.array_type(self.1)
    }
}
impl<'ctx> From<ArrayType<'ctx>> for ArrayTypeS {
    fn from(value: ArrayType<'ctx>) -> Self {
        let ty = value.get_element_type();
        let len = value.len();

        ArrayTypeS(Box::new(ty.into()), len)
    }
}
impl Display for ArrayTypeS {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{} x {}]", self.1, self.0)
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct VectorTypeS(Box<BasicTypeEnumS>, u32);
impl<'ctx> Concretize<'ctx> for VectorTypeS {
    type Type = VectorType<'ctx>;

    fn as_concrete(&self, compiler: &LLVMCodegen<'ctx>) -> Self::Type {
        match self.0.as_concrete(compiler) {
            BasicTypeEnum::FloatType(t) => t.vec_type(self.1),
            BasicTypeEnum::IntType(t) => t.vec_type(self.1),
            BasicTypeEnum::PointerType(t) => t.vec_type(self.1),
            _ => unimplemented!("this type doesn't have a vec version")
        }
    }
}
impl<'ctx> From<VectorType<'ctx>> for VectorTypeS {
    fn from(value: VectorType<'ctx>) -> Self {
        let ty = value.get_element_type();
        let len = value.get_size();

        VectorTypeS(Box::new(ty.into()), len)
    }
}
impl Display for VectorTypeS {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{} x {}>", self.1, self.0)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct VoidTypeS;
impl<'ctx> Concretize<'ctx> for VoidTypeS {
    type Type = VoidType<'ctx>;

    fn as_concrete(&self, compiler: &LLVMCodegen<'ctx>) -> Self::Type {
        compiler.ctx.void_type()
    }
}
impl<'ctx> From<VoidType<'ctx>> for VoidTypeS {
    fn from(_: VoidType<'ctx>) -> Self {
        VoidTypeS
    }
}
impl Display for VoidTypeS {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "void")
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
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

    fn as_concrete(&self, compiler: &LLVMCodegen<'ctx>) -> Self::Type {
        let params: Vec<_> = self.params.iter()
            .map(|t| t.as_concrete(compiler))
            .map(Into::into)
            .collect();
        self.ret.as_concrete(compiler).fn_type(&params, self.var_args)
    }
}
impl<'ctx> From<FunctionType<'ctx>> for FnTypeS {
    fn from(value: FunctionType<'ctx>) -> Self {
        let ret = match value.get_return_type() {
            Some(t) => RetTypeS::BasicTypeEnumS(t.into()),
            None => RetTypeS::VoidTypeS(VoidTypeS),
        };
        let params = value.get_param_types()
            .into_iter()
            .map(BasicTypeEnumS::from)
            .collect();
        let var_args = value.is_var_arg();

        FnTypeS {
            params,
            ret,
            var_args,
        }
    }
}
impl Display for FnTypeS {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} (", self.ret)?;
        if let Some((last, rest)) = self.params.split_last() {
            for p in rest {
                write!(f, "{p}, ")?;
            }
            write!(f, "{last}")?;
            if self.var_args {
                write!(f, ", ..")?;
            }
        } else if self.var_args {
            write!(f, "..")?
        };
        write!(f, ")")
    }
}

macro_rules! enum_s {
    ($EnumS:ident => $Enum:ident: $($t:ident),*) => {
        #[derive(Clone, PartialEq, Eq, Hash, Debug)]
        pub enum $EnumS {
            $(
                $t($t)
            ),*
        }

        $(
            impl From<$t> for $EnumS {
                fn from(value: $t) -> Self {
                    Self::$t(value)
                }
            }
        )*

        impl Display for $EnumS {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(
                        Self::$t(t) => t.fmt(f)
                    ),*
                }
            }
        }

        impl<'ctx> Concretize<'ctx> for $EnumS {
            type Type = $Enum<'ctx>;
        
            fn as_concrete(&self, compiler: &LLVMCodegen<'ctx>) -> Self::Type {
                match self {
                    $(
                        Self::$t(t) => t.as_concrete(compiler).into(),
                    )*
                }
            }
        }
    }
}
macro_rules! into_enum_s {
    ($EnumS:ident => $Enum:ident: $($TS:ident => $T:ident),*) => {
        impl<'ctx> From<$Enum<'ctx>> for $EnumS {
            fn from(value: $Enum<'ctx>) -> $EnumS {
                match value {
                    $(
                        $Enum::$T(t) => Self::$TS(t.into())
                    ),*
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

enum_s!(BasicTypeEnumS => BasicTypeEnum: IntTypeS, FloatTypeS, PtrTypeS, StructTypeS, ArrayTypeS, VectorTypeS);
into_enum_s!(BasicTypeEnumS => BasicTypeEnum: 
    IntTypeS    => IntType, 
    FloatTypeS  => FloatType, 
    PtrTypeS    => PointerType, 
    StructTypeS => StructType,
    ArrayTypeS  => ArrayType,
    VectorTypeS => VectorType
);
btes_to_rtes!(IntTypeS, FloatTypeS, PtrTypeS, StructTypeS);

enum_s!(RetTypeS => ReturnableType: BasicTypeEnumS, VoidTypeS);
into_enum_s!(RetTypeS => ReturnableType:
    BasicTypeEnumS => BasicTypeEnum,
    VoidTypeS => VoidType
);

pub enum ReturnableType<'ctx> {
    BasicTypeEnum(BasicTypeEnum<'ctx>),
    VoidType(VoidType<'ctx>)
}
impl<'ctx> ReturnableType<'ctx> {
    pub fn fn_type(self, param_types: &[BasicMetadataTypeEnum<'ctx>], is_var_args: bool) -> FunctionType<'ctx> {
        match self {
            ReturnableType::BasicTypeEnum(t) => t.fn_type(param_types, is_var_args),
            ReturnableType::VoidType(t)  => t.fn_type(param_types, is_var_args),
        }
    }
}
impl<'ctx> From<BasicTypeEnum<'ctx>> for ReturnableType<'ctx> {
    fn from(value: BasicTypeEnum<'ctx>) -> Self {
        ReturnableType::BasicTypeEnum(value)
    }
}
impl<'ctx> From<VoidType<'ctx>> for ReturnableType<'ctx> {
    fn from(value: VoidType<'ctx>) -> Self {
        ReturnableType::VoidType(value)
    }
}