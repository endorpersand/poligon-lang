mod op_impl;

use inkwell::types::{BasicTypeEnum, BasicMetadataTypeEnum, FunctionType};
use inkwell::values::{IntValue, FloatValue, BasicValueEnum, BasicValue};

use super::Compiler;
use super::resolve::plir;

#[derive(Clone, Copy, Debug)]
pub enum GonValue<'ctx> {
    Float(FloatValue<'ctx> /* f64 */),
    Int(IntValue<'ctx> /* i? */),
    Bool(IntValue<'ctx> /* i1 */),
    Unit
}

impl<'ctx> GonValue<'ctx> {
    pub fn plir_type(&self) -> plir::Type {
        match self {
            GonValue::Float(_) => plir::ty!(plir::Type::S_FLOAT),
            GonValue::Int(_)   => plir::ty!(plir::Type::S_INT),
            GonValue::Bool(_)  => plir::ty!(plir::Type::S_BOOL),
            GonValue::Unit     => plir::ty!(plir::Type::S_VOID),
        }
    }

    pub fn type_layout(&self) -> TypeLayout {
        match self {
            GonValue::Float(_) => TypeLayout::Float,
            GonValue::Int(_)   => TypeLayout::Int,
            GonValue::Bool(_)  => TypeLayout::Bool,
            GonValue::Unit     => TypeLayout::Unit,
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

    pub fn basic_value(self, c: &Compiler<'ctx>) -> BasicValueEnum<'ctx> {
        match self {
            GonValue::Float(f) => f.as_basic_value_enum(),
            GonValue::Int(i)   => i.as_basic_value_enum(),
            GonValue::Bool(b)  => b.as_basic_value_enum(),
            GonValue::Unit     => c.ctx.struct_type(&[], true).const_zero().as_basic_value_enum(),
        }
    }

    pub fn reconstruct(t: &plir::Type, v: BasicValueEnum<'ctx>) -> Self {
        match t.as_ref() {
            plir::TypeRef::Prim(plir::Type::S_FLOAT) => Self::Float(v.into_float_value()),
            plir::TypeRef::Prim(plir::Type::S_INT)   => Self::Int(v.into_int_value()),
            plir::TypeRef::Prim(plir::Type::S_BOOL)  => Self::Bool(v.into_int_value()),
            plir::TypeRef::Prim(plir::Type::S_VOID)  => Self::Unit,
            // TODO: not panic
            _ => panic!("Cannot reconstruct value from type")
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum TypeLayout {
    Float, Int, Bool, Unit
}

impl TypeLayout {
    pub fn of(ty: &plir::Type) -> Option<Self> {
        match ty {
            plir::Type::Prim(prim) => match prim.as_str() {
                plir::Type::S_FLOAT => Some(TypeLayout::Float),
                plir::Type::S_INT   => Some(TypeLayout::Int),
                plir::Type::S_BOOL  => Some(TypeLayout::Bool),
                plir::Type::S_VOID  => Some(TypeLayout::Unit),
                _ => todo!("type layout of {ty}")
            },
            _ => todo!("type layout of {ty}")
        }
    }

    pub fn basic_type<'ctx>(&self, c: &Compiler<'ctx>) -> BasicTypeEnum<'ctx> {
        match self {
            TypeLayout::Float => c.ctx.f64_type().into(),
            TypeLayout::Int   => c.ctx.i64_type().into(),
            TypeLayout::Bool  => c.ctx.bool_type().into(),
            TypeLayout::Unit  => c.ctx.struct_type(&[], true).into(),
        }
    }

    pub fn fn_type<'ctx>(&self, c: &Compiler<'ctx>, params: &[BasicMetadataTypeEnum<'ctx>], is_var_args: bool) -> FunctionType<'ctx> {
        match self {
            TypeLayout::Float => c.ctx.f64_type().fn_type(params, is_var_args),
            TypeLayout::Int   => c.ctx.i64_type().fn_type(params, is_var_args),
            TypeLayout::Bool  => c.ctx.bool_type().fn_type(params, is_var_args),
            TypeLayout::Unit  => c.ctx.void_type().fn_type(params, is_var_args),
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