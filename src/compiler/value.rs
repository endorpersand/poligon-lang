mod op_impl;
mod internals;

use inkwell::values::{IntValue, FloatValue, BasicValueEnum, StructValue, BasicValue};

use super::{Compiler, CompileResult, CompileErr, layout};
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
/// and can be converted to an LLVM value via [`Compiler::basic_value_of`].
#[derive(Clone, Copy, Debug)]
pub enum GonValue<'ctx> {
    /// A `float`.
    Float(FloatValue<'ctx> /* f64 */),
    /// An `int`.
    Int(IntValue<'ctx> /* iX */),
    /// A `bool`.
    Bool(IntValue<'ctx> /* i1 */),
    /// A `char`.
    /// 
    /// Following Rust's rules, `char` is stored as a u32.
    Char(IntValue<'ctx> /* i32 */),
    /// Any value represented by a struct in LLVM.
    /// 
    /// For example, `String`.
    Struct(StructValue<'ctx>),
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
    /// Create a new char value using a char from Rust.
    pub fn new_char(&self, c: char) -> GonValue<'ctx> {
        GonValue::Int(self.ctx.i32_type().const_int(c as u32 as u64, true))
    }
    /// Create a new string value using a string slice from Rust.
    pub fn new_str(&mut self, s: &str) -> GonValue<'ctx> {
        // todo: null-terminated fix
        // i love c
        let array = self.ctx.const_string(s.as_bytes(), true);

        let array_ptr = self.builder.build_alloca(array.get_type(), "strstore");
        self.builder.build_store(array_ptr, array);
        
        let str_type = layout!(self, S_STR).into_struct_type();
        GonValue::Struct(self.create_struct_value(str_type, &[
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
                let ft = self.get_layout(ty)?.into_float_type();
                let fv = self.builder.build_signed_int_to_float(i, ft, "cast");
                
                Ok(GonValue::Float(fv))
            },
            // TODO: impl char -> str
            (_, TypeRef::Prim(Type::S_BOOL)) => Ok(GonValue::Bool(self.truth(v))),
            (_, TypeRef::Prim(Type::S_VOID)) => Ok(GonValue::Unit),
            _ => Err(CompileErr::CannotCast(self.plir_type_of(v), ty.clone()))
        }
    }

    /// Create a [`GonValue`] from a given LLVM value.
    pub fn reconstruct(&self, t: &plir::Type, v: impl BasicValue<'ctx>) -> CompileResult<'ctx, GonValue<'ctx>> {
        use plir::{TypeRef, Type};
        use inkwell::types::BasicTypeEnum;
        
        let v = v.as_basic_value_enum();
        match t.as_ref() {
            TypeRef::Prim(Type::S_FLOAT) => Ok(GonValue::Float(v.into_float_value())),
            TypeRef::Prim(Type::S_INT)   => Ok(GonValue::Int(v.into_int_value())),
            TypeRef::Prim(Type::S_BOOL)  => Ok(GonValue::Bool(v.into_int_value())),
            TypeRef::Prim(Type::S_CHAR)  => Ok(GonValue::Char(v.into_int_value())),
            TypeRef::Prim(Type::S_VOID)  => Ok(GonValue::Unit),
            _ => {
                if let BasicTypeEnum::StructType(_) = self.get_layout(t)? {
                    Ok(GonValue::Struct(v.into_struct_value()))
                } else {
                    Err(CompileErr::UnresolvedType(t.clone()))
                }
            }
        }
    }

    /// Obtain the basic LLVM value represented by this [`GonValue`].
    /// 
    /// Depending on context, [`Compiler::returnable_value_of`] may be more suitable.
    pub fn basic_value_of(&self, value: GonValue<'ctx>) -> BasicValueEnum<'ctx> {
        match value {
            GonValue::Float(f)  => f.into(),
            GonValue::Int(i)    => i.into(),
            GonValue::Bool(b)   => b.into(),
            GonValue::Char(c)   => c.into(),
            GonValue::Struct(s) => s.into(),
            GonValue::Unit      => layout!(self, S_VOID).const_zero(),
        }
    }

    /// Obtain the LLVM value used for return statements for this [`GonValue`].
    /// 
    /// This should be used instead of [`Compiler::basic_value_of`]
    /// when being inserted into a `return` statement or 
    /// other similar statements where an `Option\<&dyn BasicValue\>` is accepted.
    pub fn returnable_value_of(&self, value: GonValue<'ctx>) -> Option<BasicValueEnum<'ctx>> {
        match value {
            GonValue::Unit => None,
            val => Some(self.basic_value_of(val))
        }
    }

    /// The PLIR type for this value.
    /// 
    /// This can be used to reconstruct a GonValue from a LLVM representation. 
    /// See [`Compiler::reconstruct`].
    pub fn plir_type_of(&self, value: GonValue<'ctx>) -> plir::Type {
        match value {
            GonValue::Float(_)  => plir::ty!(plir::Type::S_FLOAT),
            GonValue::Int(_)    => plir::ty!(plir::Type::S_INT),
            GonValue::Bool(_)   => plir::ty!(plir::Type::S_BOOL),
            GonValue::Char(_)   => plir::ty!(plir::Type::S_CHAR),
            GonValue::Struct(s) => {
                let st = s.get_type();
                let name = st.get_name()
                    .expect("Expected struct to have name")
                    .to_str()
                    .expect("Expected struct name to be Rust-compatible");
                
                plir::ty!(name)
            },
            GonValue::Unit      => plir::ty!(plir::Type::S_VOID),
        }
    }
}