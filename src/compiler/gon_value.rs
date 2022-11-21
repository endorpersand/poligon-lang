use inkwell::types::BasicTypeEnum;
use inkwell::values::{IntValue, FloatValue, BasicValueEnum, BasicValue};

use crate::tree;

use super::Compiler;

#[derive(Clone, Copy, Debug)]
pub enum GonValue<'ctx> {
    Float(FloatValue<'ctx> /* f64 */),
    Int(IntValue<'ctx> /* i? */),
    Bool(IntValue<'ctx> /* i1 */)
}

impl<'ctx> GonValue<'ctx> {
    pub fn typed(&self) -> GonValueType {
        match self {
            GonValue::Float(_) => GonValueType::Float,
            GonValue::Int(_)   => GonValueType::Int,
            GonValue::Bool(_)  => GonValueType::Bool,
        }
    }

    pub fn new_int(c: &Compiler<'ctx>, v: isize) -> Self {
        Self::Int(c.ctx.i64_type().const_int(v as u64, true))
    }
    pub fn new_bool(c: &Compiler<'ctx>, v: bool) -> Self {
        Self::Bool(c.ctx.bool_type().const_int(v as u64, true))
    }
    pub fn new_float(c: &Compiler<'ctx>, f: f64) -> Self {
        Self::Float(c.ctx.f64_type().const_float(f))
    }

    pub fn basic_enum(self) -> BasicValueEnum<'ctx> {
        match self {
            GonValue::Float(f) => f.as_basic_value_enum(),
            GonValue::Int(i)   => i.as_basic_value_enum(),
            GonValue::Bool(b)  => b.as_basic_value_enum(),
        }
    }

    pub fn reconstruct(t: GonValueType, v: BasicValueEnum<'ctx>) -> Self {
        match t {
            GonValueType::Float => Self::Float(v.into_float_value()),
            GonValueType::Int   => Self::Int(v.into_int_value()),
            GonValueType::Bool  => Self::Bool(v.into_int_value()),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum GonValueType {
    Float, Int, Bool
}

impl GonValueType {
    pub fn lookup(ty: &tree::Type) -> Option<Self> {
        match ty.0.as_str() {
            "float" => Some(GonValueType::Float),
            "int"   => Some(GonValueType::Int),
            "bool"  => Some(GonValueType::Bool),
            _       => None
        }
    }

    pub fn basic_enum<'ctx>(&self, c: &Compiler<'ctx>) -> BasicTypeEnum<'ctx> {
        match self {
            GonValueType::Float => c.ctx.f64_type().into(),
            GonValueType::Int   => c.ctx.i64_type().into(),
            GonValueType::Bool  => c.ctx.bool_type().into(),
        }
    }
}

// macro_rules! apply_gv {
//     ($i:ident as $e:expr => $fn:expr) => {
//         match $e {
//             GonValueEnum::Int($i)   => $fn,
//             GonValueEnum::Float($i) => $fn,
//             GonValueEnum::Bool($i)  => $fn,
//         }
//     }
// }
// pub(crate) use apply_gv;

macro_rules! apply_bv {
    ($i:ident as $e:expr => $fn:expr) => {
        match $e {
            BasicValueEnum::ArrayValue($i)   => $fn,
            BasicValueEnum::IntValue($i)     => $fn,
            BasicValueEnum::FloatValue($i)   => $fn,
            BasicValueEnum::PointerValue($i) => $fn,
            BasicValueEnum::StructValue($i)  => $fn,
            BasicValueEnum::VectorValue($i)  => $fn,
        }
    }
}
pub(crate) use apply_bv;