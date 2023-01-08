mod op_impl;

use inkwell::types::{BasicTypeEnum, BasicMetadataTypeEnum, FunctionType, VoidType};
use inkwell::values::{IntValue, FloatValue, BasicValueEnum, BasicValue, ArrayValue, BasicMetadataValueEnum};

use super::Compiler;
use super::plir;

macro_rules! apply_bv {
    (let $i:ident = $e:expr => $fn:expr) => {
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
macro_rules! apply_btv {
    (let $i:ident = $e:expr => $fn:expr) => {
        match $e {
            BasicTypeEnum::ArrayType($i)   => $fn,
            BasicTypeEnum::IntType($i)     => $fn,
            BasicTypeEnum::FloatType($i)   => $fn,
            BasicTypeEnum::PointerType($i) => $fn,
            BasicTypeEnum::StructType($i)  => $fn,
            BasicTypeEnum::VectorType($i)  => $fn,
        }
    }
}

pub(crate) use apply_bv;

#[derive(Clone, Copy, Debug)]
pub enum GonValue<'ctx> {
    Float(FloatValue<'ctx> /* f64 */),
    Int(IntValue<'ctx> /* i? */),
    Bool(IntValue<'ctx> /* i1 */),
    Str(ArrayValue<'ctx> /* [n x i8] */),
    Unit,
}

impl<'ctx> GonValue<'ctx> {
    pub fn plir_type(&self) -> plir::Type {
        match self {
            GonValue::Float(_) => plir::ty!(plir::Type::S_FLOAT),
            GonValue::Int(_)   => plir::ty!(plir::Type::S_INT),
            GonValue::Bool(_)  => plir::ty!(plir::Type::S_BOOL),
            GonValue::Str(_)   => plir::ty!(plir::Type::S_STR),
            GonValue::Unit     => plir::ty!(plir::Type::S_VOID),
        }
    }

    pub fn type_layout(&self) -> TypeLayout {
        match self {
            GonValue::Float(_) => TypeLayout::Float,
            GonValue::Int(_)   => TypeLayout::Int,
            GonValue::Bool(_)  => TypeLayout::Bool,
            GonValue::Str(a)   => TypeLayout::Str(Some(a.get_type().len())),
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
    pub fn new_str(c: &Compiler<'ctx>, s: &str) -> Self {
        // todo: null-terminated fix
        // todo: UTF-8 unicode support
        // i love c

        let bytes: Vec<_> = s.as_bytes().iter()
            .copied()
            .chain(std::iter::once(0))
            .map(|byte| c.ctx.i8_type().const_int(byte as u64, false))
            .collect();
        Self::Str(c.ctx.i8_type().const_array(&bytes))
    }

    pub fn basic_value(self, c: &Compiler<'ctx>) -> BasicValueEnum<'ctx> {
        match self {
            GonValue::Float(f) => f.as_basic_value_enum(),
            GonValue::Int(i)   => i.as_basic_value_enum(),
            GonValue::Bool(b)  => b.as_basic_value_enum(),
            GonValue::Str(s)   => s.as_basic_value_enum(),
            GonValue::Unit     => c.ctx.struct_type(&[], true).const_zero().as_basic_value_enum(),
        }
    }

    pub fn param_value(self, c: &mut Compiler<'ctx>) -> BasicMetadataValueEnum<'ctx> {
        match self {
            GonValue::Str(_) => {
                // HACK: Forgive me father, for I have sinned.

                // Okay, now seriously,
                // All str parameters should be i8*, so any str values (which are of the type [n x i8])
                // need to be converted to i8*, which can be done through GEP.
                // That being said, there's a definitely a better way than storing a new pointer every time.

                let ptr = c.alloca_and_store("str", self);
                let zero = c.ctx.i32_type().const_zero();
                unsafe {
                    c.builder.build_in_bounds_gep(ptr, &[zero, zero], "gep").into()
                }
            },
            val => val.basic_value(c).into()
        }
    }

    pub fn basic_value_or_void(self, c: &Compiler<'ctx>) -> Option<BasicValueEnum<'ctx>> {
        match self {
            GonValue::Unit => None,
            val => Some(val.basic_value(c))
        }
    }

    pub fn reconstruct(t: &plir::Type, v: BasicValueEnum<'ctx>) -> Self {
        match t.as_ref() {
            plir::TypeRef::Prim(plir::Type::S_FLOAT) => Self::Float(v.into_float_value()),
            plir::TypeRef::Prim(plir::Type::S_INT)   => Self::Int(v.into_int_value()),
            plir::TypeRef::Prim(plir::Type::S_BOOL)  => Self::Bool(v.into_int_value()),
            plir::TypeRef::Prim(plir::Type::S_VOID)  => Self::Unit,
            plir::TypeRef::Prim(plir::Type::S_STR)   => Self::Str(v.into_array_value()),
            // TODO: not panic
            _ => panic!("Cannot reconstruct value from type")
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum TypeLayout {
    Float, Int, Bool, Unit, Str(Option<u32>)
}

impl TypeLayout {
    pub fn of(ty: &plir::Type) -> Option<Self> {
        match ty {
            plir::Type::Prim(prim) => match prim.as_str() {
                plir::Type::S_FLOAT => Some(TypeLayout::Float),
                plir::Type::S_INT   => Some(TypeLayout::Int),
                plir::Type::S_BOOL  => Some(TypeLayout::Bool),
                plir::Type::S_VOID  => Some(TypeLayout::Unit),
                plir::Type::S_STR   => Some(TypeLayout::Str(None)),
                _ => todo!("type layout of {ty}")
            },
            _ => todo!("type layout of {ty}")
        }
    }

    pub fn basic_type<'ctx>(&self, c: &Compiler<'ctx>) -> BasicTypeEnum<'ctx> {
        match self {
            TypeLayout::Float     => c.ctx.f64_type().into(),
            TypeLayout::Int       => c.ctx.i64_type().into(),
            TypeLayout::Bool      => c.ctx.bool_type().into(),
            TypeLayout::Str(mlen) => match mlen.as_ref() {
                Some(&len) => c.ctx.i8_type().array_type(len).into(),
                None => c.ctx.i8_type().ptr_type(Default::default()).into(),
            },
            TypeLayout::Unit => c.ctx.struct_type(&[], true).into(),
        }
    }

    fn basic_type_or_void<'ctx>(&self, c: &Compiler<'ctx>) -> Result<BasicTypeEnum<'ctx>, VoidType<'ctx>> {
        match self {
            TypeLayout::Unit => Err(c.ctx.void_type()),
            ty => Ok(ty.basic_type(c))
        }
    }

    pub fn fn_type<'ctx>(&self, c: &Compiler<'ctx>, params: &[BasicMetadataTypeEnum<'ctx>], is_var_args: bool) -> FunctionType<'ctx> {
        match self.basic_type_or_void(c) {
            Ok(ty) => apply_btv!(let t = ty => t.fn_type(params, is_var_args)),
            Err(ty) => ty.fn_type(params, is_var_args)
        }
    }
}