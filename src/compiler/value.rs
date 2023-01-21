mod op_impl;
mod internals;

use inkwell::types::{BasicTypeEnum, BasicMetadataTypeEnum, FunctionType, VoidType};
use inkwell::values::{IntValue, FloatValue, BasicValueEnum, BasicValue, StructValue};

use super::Compiler;
use super::plir;

/// Apply a function to a basic value enum 
/// if every variant has defined that function.
macro_rules! apply_bv {
    (let $i:ident = $e:expr => $fn:expr) => {
        match $e {
            inkwell::values::BasicValueEnum::ArrayValue($i)   => $fn,
            inkwell::values::BasicValueEnum::IntValue($i)     => $fn,
            inkwell::values::BasicValueEnum::FloatValue($i)   => $fn,
            inkwell::values::BasicValueEnum::PointerValue($i) => $fn,
            inkwell::values::BasicValueEnum::StructValue($i)  => $fn,
            inkwell::values::BasicValueEnum::VectorValue($i)  => $fn,
        }
    }
}
/// Apply a function to a basic type enum
/// if every variant has defined that function.
macro_rules! apply_bt {
    (let $i:ident = $e:expr => $fn:expr) => {
        match $e {
            inkwell::types::BasicTypeEnum::ArrayType($i)   => $fn,
            inkwell::types::BasicTypeEnum::IntType($i)     => $fn,
            inkwell::types::BasicTypeEnum::FloatType($i)   => $fn,
            inkwell::types::BasicTypeEnum::PointerType($i) => $fn,
            inkwell::types::BasicTypeEnum::StructType($i)  => $fn,
            inkwell::types::BasicTypeEnum::VectorType($i)  => $fn,
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
    Str(StructValue<'ctx> /* internals::string_type */),
    /// `void`. 
    /// 
    /// This can either be represented 
    /// as `void` or `()` depending 
    /// on where it is.
    Unit,
}

impl<'ctx> Compiler<'ctx> {
    /// Create a new int value using an int from Rust.
    pub fn new_int(&self, v: isize) -> GonValue<'ctx> {
        GonValue::Int(self.ctx.i64_type().const_int(v as u64, true))
    }
    /// Create a new bool value using a bool from Rust.
    pub fn new_bool(&self, v: bool) -> GonValue<'ctx> {
        GonValue::Bool(self.ctx.bool_type().const_int(v as u64, true))
    }
    /// Create a new float value using a float from Rust.
    pub fn new_float(&self, f: f64) -> GonValue<'ctx> {
        GonValue::Float(self.ctx.f64_type().const_float(f))
    }
    /// Create a new string value using a string slice from Rust.
    pub fn new_str(&self, s: &str) -> GonValue<'ctx> {
        // todo: null-terminated fix
        // i love c
        let array = self.ctx.const_string(s.as_bytes(), true);

        let array_ptr = self.builder.build_alloca(array.get_type(), "strstore");
        self.builder.build_store(array_ptr, array);

        let ptr = self.builder.build_bitcast(
            array_ptr, 
            self.ctx.i8_type().ptr_type(Default::default()), 
            "strptr"
        );
        
        GonValue::Str(self.create_struct_value(self.string_type(), &[
            ptr,
            self.ctx.i64_type().const_int(s.len() as u64, true).into()
        ]).unwrap())
    }

    /// Cast a GonValue to another type.
    /// 
    /// The only successful casts here are int to float, char to string, anything to unit
    pub fn cast(&self, v: GonValue<'ctx>, ty: &plir::Type) -> Option<GonValue<'ctx>> {
        match (v, ty.as_ref()) {
            (GonValue::Int(i), plir::TypeRef::Prim(plir::Type::S_FLOAT)) => {
                let ft = TypeLayout::of(ty)?.basic_type(self).into_float_type();
                let fv = self.builder.build_signed_int_to_float(i, ft, "cast");
                
                Some(GonValue::Float(fv))
            },
            (_, plir::TypeRef::Prim(plir::Type::S_VOID)) => Some(GonValue::Unit),
            _ => None
        }
    }
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
            GonValue::Str(_)   => TypeLayout::Str,
            GonValue::Unit     => TypeLayout::Unit,
        }
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
            GonValue::Unit     => c.ctx.const_struct(&[], true).as_basic_value_enum(),
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
            plir::TypeRef::Prim(plir::Type::S_STR)   => Self::Str(v.into_struct_value()),
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
    /// The string format.
    /// 
    /// This is formatted as the following struct in LLVM:
    /// ```text
    /// %string = {
    ///     i8*,  ; buffer
    ///     i64   ; length
    /// }
    /// ```
    Str
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
                plir::Type::S_STR   => Some(TypeLayout::Str),
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
            TypeLayout::Float => c.ctx.f64_type().into(),
            TypeLayout::Int   => c.ctx.i64_type().into(),
            TypeLayout::Bool  => c.ctx.bool_type().into(),
            TypeLayout::Str   => c.string_type().into(),
            TypeLayout::Unit  => c.ctx.struct_type(&[], true).into()
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