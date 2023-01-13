mod op_impl;

use inkwell::types::{BasicTypeEnum, BasicMetadataTypeEnum, FunctionType, VoidType};
use inkwell::values::{IntValue, FloatValue, BasicValueEnum, BasicValue, ArrayValue, BasicMetadataValueEnum};

use super::Compiler;
use super::plir;

/// Apply a function to a basic value enum 
/// if every variant has defined that function.
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
/// Apply a function to a basic type enum
/// if every variant has defined that function.
macro_rules! apply_bt {
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

/// An LLVM representation of the a possible value in Poligon.
/// 
/// This enum holds all the information to keep track of a type in Poligon,
/// and can be converted to an LLVM value via [`GonValue::basic_value`].
#[derive(Clone, Copy, Debug)]
pub enum GonValue<'ctx> {
    /// A `float`.
    Float(FloatValue<'ctx> /* f64 */),
    /// An `int`.
    Int(IntValue<'ctx> /* iX */),
    /// A `bool`.
    Bool(IntValue<'ctx> /* i1 */),
    /// A `string`.
    Str(ArrayValue<'ctx> /* [n x i8] */),
    /// A `void`. 
    /// 
    /// This can either be represented 
    /// as `void` or `()` depending 
    /// on where it is.
    Unit,
}

impl<'ctx> GonValue<'ctx> {
    /// The PLIR type for this value.
    /// 
    /// This can be used to reconstruct a GonValue from a LLVM representation. 
    /// See [`GonValue::reconstruct`].
    pub fn plir_type(&self) -> plir::Type {
        match self {
            GonValue::Float(_) => plir::ty!(plir::Type::S_FLOAT),
            GonValue::Int(_)   => plir::ty!(plir::Type::S_INT),
            GonValue::Bool(_)  => plir::ty!(plir::Type::S_BOOL),
            GonValue::Str(_)   => plir::ty!(plir::Type::S_STR),
            GonValue::Unit     => plir::ty!(plir::Type::S_VOID),
        }
    }

    /// The type layout for this value.
    /// 
    /// See [`TypeLayout`] for more information.
    pub fn type_layout(&self) -> TypeLayout {
        match self {
            GonValue::Float(_) => TypeLayout::Float,
            GonValue::Int(_)   => TypeLayout::Int,
            GonValue::Bool(_)  => TypeLayout::Bool,
            GonValue::Str(a)   => TypeLayout::Str(Some(a.get_type().len())),
            GonValue::Unit     => TypeLayout::Unit,
        }
    }

    /// Create a new int value using an int in Rust.
    pub fn new_int(c: &Compiler<'ctx>, v: isize) -> Self {
        Self::Int(c.ctx.i64_type().const_int(v as u64, true))
    }
    /// Create a new bool value using a bool in Rust.
    pub fn new_bool(c: &Compiler<'ctx>, v: bool) -> Self {
        Self::Bool(c.ctx.bool_type().const_int(v as u64, true))
    }
    /// Create a new float value using a float in Rust.
    pub fn new_float(c: &Compiler<'ctx>, f: f64) -> Self {
        Self::Float(c.ctx.f64_type().const_float(f))
    }
    /// Create a new string value using a string slice in Rust.
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

    /// Produce a basic LLVM value for this `GonValue`.
    /// 
    /// Depending on context, [`GonValue::param_value`] 
    /// or [`GonValue::basic_value_or_void`] may be more suitable.
    pub fn basic_value(self, c: &Compiler<'ctx>) -> BasicValueEnum<'ctx> {
        match self {
            GonValue::Float(f) => f.as_basic_value_enum(),
            GonValue::Int(i)   => i.as_basic_value_enum(),
            GonValue::Bool(b)  => b.as_basic_value_enum(),
            GonValue::Str(s)   => s.as_basic_value_enum(),
            GonValue::Unit     => c.ctx.struct_type(&[], true).const_zero().as_basic_value_enum(),
        }
    }

    /// Produce a basic LLVM value for this `GonValue`.
    /// 
    /// This should be used instead of [`GonValue::basic_value`]
    /// when the value is being inserted into a function call.
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

    /// Produce a basic LLVM value (or void) for this `GonValue`.
    /// 
    /// This should be used instead of [`GonValue::basic_value`]
    /// when being inserted into a `return` statement or 
    /// other similar statements where an Option\<BasicValue\> is accepted.
    pub fn basic_value_or_void(self, c: &Compiler<'ctx>) -> Option<BasicValueEnum<'ctx>> {
        match self {
            GonValue::Unit => None,
            val => Some(val.basic_value(c))
        }
    }

    /// Create a [`GonValue`] from a given LLVM value.
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

/// The specific representation for a type in LLVM.
/// 
/// Each `GonValue` is represented in some way in LLVM.
/// This enum defines which representations are needed 
/// and how they are defined in LLVM.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum TypeLayout {
    /// The float format.
    /// 
    /// This is formatted as a float in LLVM.
    Float, 
    /// The int format.
    /// 
    /// This is formatted as an `iX` in LLVM.
    Int, 
    /// THe bool format.
    /// 
    /// This is formatted as a bool (`i1`) in LLVM.
    Bool, 
    /// The unit format.
    /// 
    /// This is formatted as a `void` or `()` in LLVM.
    Unit, 
    /// The fixed string format.
    /// 
    /// This is formatted as a fixed array of chars (`[n x i8]`) or a char pointer (`i8*`) in LLVM.
    Str(Option<u32>)
}

impl TypeLayout {
    /// Determine the type layout of a given PLIR type (if it exists).
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

    /// Convert the type layout into an LLVM basic type representation.
    /// 
    /// In certain contexts, [`TypeLayout::basic_type_or_void`] may be more applicable.
    pub fn basic_type<'ctx>(&self, c: &Compiler<'ctx>) -> BasicTypeEnum<'ctx> {
        match self {
            TypeLayout::Float     => c.ctx.f64_type().into(),
            TypeLayout::Int       => c.ctx.i64_type().into(),
            TypeLayout::Bool      => c.ctx.bool_type().into(),
            TypeLayout::Str(mlen) => match mlen.as_ref() {
                Some(&len) => c.ctx.i8_type().array_type(len).into(), // [n x i8]
                None => c.ctx.i8_type().ptr_type(Default::default()).into(), // *i8
            },
            TypeLayout::Unit => c.ctx.struct_type(&[], true).into(),
        }
    }

    /// Convert the type into an LLVM basic type (or `void` if unit).
    /// 
    /// This should be used over [`TypeLayout::basic_type`] 
    /// when dealing with function return types.
    pub fn basic_type_or_void<'ctx>(&self, c: &Compiler<'ctx>) -> Result<BasicTypeEnum<'ctx>, VoidType<'ctx>> {
        match self {
            TypeLayout::Unit => Err(c.ctx.void_type()),
            ty => Ok(ty.basic_type(c))
        }
    }

    /// Create a function type with this type layout as the return type.
    pub fn fn_type<'ctx>(&self, c: &Compiler<'ctx>, params: &[BasicMetadataTypeEnum<'ctx>], is_var_args: bool) -> FunctionType<'ctx> {
        match self.basic_type_or_void(c) {
            Ok(ty) => apply_bt!(let t = ty => t.fn_type(params, is_var_args)),
            Err(ty) => ty.fn_type(params, is_var_args)
        }
    }
}