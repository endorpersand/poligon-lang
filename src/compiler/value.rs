mod op_impl;
mod internals;

use std::collections::HashMap;

use inkwell::types::{BasicTypeEnum, BasicMetadataTypeEnum, FunctionType, VoidType, BasicType, StructType};
use inkwell::values::{IntValue, FloatValue, BasicValueEnum, StructValue, FunctionValue};

use super::{Compiler, CompileResult, CompileErr};
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
pub(crate) use apply_bt;

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
    pub fn new_str(&mut self, s: &str) -> GonValue<'ctx> {
        // todo: null-terminated fix
        // i love c
        let array = self.ctx.const_string(s.as_bytes(), true);

        let array_ptr = self.builder.build_alloca(array.get_type(), "strstore");
        self.builder.build_store(array_ptr, array);
        
        let str_ty = self.string_type().struct_type();
        GonValue::Str(self.create_struct_value(str_ty, &[
            array_ptr.into(),
            self.ctx.i64_type().const_int(s.len() as u64 + 1, true).into()
        ]).unwrap())
    }

    /// Cast a GonValue to another type.
    /// 
    /// The only successful casts here: 
    /// - int to float
    /// - char to string
    /// - anything to unit
    /// - anything to bool
    pub fn cast(&mut self, v: GonValue<'ctx>, ty: &plir::Type) -> CompileResult<'ctx, GonValue<'ctx>> {
        use plir::{Type, TypeRef};

        match (v, ty.as_ref()) {
            (GonValue::Int(i), TypeRef::Prim(Type::S_FLOAT)) => {
                let ft = self.load_layout(ty)?.basic_type(self).into_float_type();
                let fv = self.builder.build_signed_int_to_float(i, ft, "cast");
                
                Ok(GonValue::Float(fv))
            },
            // TODO: impl char -> str
            (_, TypeRef::Prim(Type::S_BOOL)) => Ok(GonValue::Bool(self.truth(v))),
            (_, TypeRef::Prim(Type::S_VOID)) => Ok(GonValue::Unit),
            _ => Err(CompileErr::CannotCast(v.plir_type(), ty.clone()))
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
            GonValue::Str(_)   => TypeLayout::struct_layout("String"),
            GonValue::Unit     => TypeLayout::Unit,
        }
    }

    /// Produce a basic LLVM value for this `GonValue`.
    /// 
    /// Depending on context, [`GonValue::basic_value_or_void`] may be more suitable.
    pub fn basic_value(self, c: &Compiler<'ctx>) -> BasicValueEnum<'ctx> {
        match self {
            GonValue::Float(f) => f.into(),
            GonValue::Int(i)   => i.into(),
            GonValue::Bool(b)  => b.into(),
            GonValue::Str(s)   => s.into(),
            GonValue::Unit     => c.void_value_type().const_zero().into(),
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
        use plir::{TypeRef, Type};
        
        match t.as_ref() {
            TypeRef::Prim(Type::S_FLOAT) => Self::Float(v.into_float_value()),
            TypeRef::Prim(Type::S_INT)   => Self::Int(v.into_int_value()),
            TypeRef::Prim(Type::S_BOOL)  => Self::Bool(v.into_int_value()),
            TypeRef::Prim(Type::S_VOID)  => Self::Unit,
            TypeRef::Prim(Type::S_STR)   => Self::Str(v.into_struct_value()),
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
#[derive(Clone, PartialEq, Eq, Debug)]
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

    /// A custom-defined struct.
    /// 
    /// All structs with a given type layout have to be registered
    /// using [`Compiler::register_struct`], as this accesses the
    /// struct using the compiler's defined structs.
    Struct(String),

    /// A tuple of types.
    Tuple(Vec<TypeLayout>)
}

impl TypeLayout {
    /// Determine the type layout of a given PLIR type (if it exists).
    pub fn of(ty: &plir::Type) -> Option<Self> {
        use plir::{TypeRef, Type};
        match ty.as_ref() {
            TypeRef::Prim(Type::S_FLOAT) => Some(TypeLayout::Float),
            TypeRef::Prim(Type::S_INT)   => Some(TypeLayout::Int),
            TypeRef::Prim(Type::S_BOOL)  => Some(TypeLayout::Bool),
            TypeRef::Prim(Type::S_VOID)  => Some(TypeLayout::Unit),
            TypeRef::Prim(Type::S_STR)   => Some(TypeLayout::struct_layout("String")),
            TypeRef::Tuple(t) => {
                t.iter().map(TypeLayout::of)
                    .collect::<Option<Vec<_>>>()
                    .map(TypeLayout::Tuple)
            }
            _ => todo!("type layout of {ty}")
        }
    }

    fn struct_layout(s: &str) -> TypeLayout { TypeLayout::Struct(String::from(s)) }

    /// Convert the type layout into an LLVM basic type representation.
    /// 
    /// In certain contexts, [`TypeLayout::basic_type_or_void`] may be more applicable.
    pub fn basic_type<'ctx>(&self, c: &Compiler<'ctx>) -> BasicTypeEnum<'ctx> {
        match self {
            TypeLayout::Float     => c.ctx.f64_type().into(),
            TypeLayout::Int       => c.ctx.i64_type().into(),
            TypeLayout::Bool      => c.ctx.bool_type().into(),
            TypeLayout::Unit      => c.void_value_type().into(),
            TypeLayout::Struct(s) => c.get_struct(s).struct_type().into(),
            TypeLayout::Tuple(t)  => {
                let fty = t.iter().map(|t| t.basic_type(c))
                    .collect::<Vec<_>>();

                c.ctx.struct_type(&fty, false).into()
            },
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
            Ok(ty) => ty.fn_type(params, is_var_args),
            Err(ty) => ty.fn_type(params, is_var_args)
        }
    }
}

pub(crate) struct GonStruct<'ctx> {
    pub(crate) name: String,
    ty: StructType<'ctx>,
    methods: HashMap<String, FunctionValue<'ctx>>
}

impl<'ctx> GonStruct<'ctx> {
    pub fn new(c: &Compiler<'ctx>, name: &str, fields: &[BasicTypeEnum<'ctx>]) -> Self {
        let ty = c.ctx.opaque_struct_type(name);
        ty.set_body(fields, false);

        Self { name: name.to_string(), ty, methods: HashMap::new() }
    }

    pub fn add_method(&mut self, name: &str, f: FunctionValue<'ctx>) {
        self.methods.insert(name.to_string(), f);
    }

    pub fn struct_type(&self) -> StructType<'ctx> {
        self.ty
    }
}