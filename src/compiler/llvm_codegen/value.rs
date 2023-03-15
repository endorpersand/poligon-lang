use inkwell::values::{IntValue, FloatValue, BasicValueEnum, BasicValue};

use super::{LLVMCodegen, LLVMResult, LLVMErr, layout, params};
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
    /// A miscellaneous value with no special tracking.
    /// 
    /// For example, `String`.
    Default(BasicValueEnum<'ctx>),
    /// `void`. 
    /// 
    /// This can either be represented 
    /// as `void` or `()` depending 
    /// on where it is.
    Unit,
}

impl<'ctx> LLVMCodegen<'ctx> {
    /// Create a new int value using an int from Rust.
    pub fn new_int(&self, v: isize) -> GonValue<'ctx> {
        GonValue::Int(layout!(self, S_INT).into_int_type().const_int(v as u64, true))
    }
    /// Create a new bool value using a bool from Rust.
    pub fn new_bool(&self, v: bool) -> GonValue<'ctx> {
        GonValue::Bool(layout!(self, S_BOOL).into_int_type().const_int(v as u64, true))
    }
    /// Create a new float value using a float from Rust.
    pub fn new_float(&self, f: f64) -> GonValue<'ctx> {
        GonValue::Float(layout!(self, S_FLOAT).into_float_type().const_float(f))
    }
    /// Create a new char value using a char from Rust.
    pub fn new_char(&self, c: char) -> GonValue<'ctx> {
        GonValue::Char(layout!(self, S_CHAR).into_int_type().const_int(c as u64, true))
    }
    /// Create a new string value using a string slice from Rust.
    pub fn new_str(&self, s: &str) -> GonValue<'ctx> {
        // todo: support \0
        let _ptr = self.ptr_type(Default::default());
        let _int = layout!(self, S_INT).into_int_type();
        let _dynarray = layout!(self, "#dynarray").into_struct_type();
        let _str = layout!(self, S_STR).into_struct_type();

        let arr_new = self.std_import("#dynarray::new").unwrap();
        let arr_ext = self.std_import("#dynarray::extend").unwrap();

        let string = self.ctx.const_string(s.as_bytes(), false);
        let len = _int.const_int(string.get_type().len() as _, false);

        let dynarray_ptr = self.builder.build_alloca(_dynarray, "");
        let dynarray = self.builder.build_call(arr_new, params![len], "dynarray_new")
            .try_as_basic_value()
            .unwrap_left()
            .into_struct_value();
        self.builder.build_store(dynarray_ptr, dynarray);
        
        let string_ptr = self.builder.build_alloca(string.get_type(), "");
        self.builder.build_store(string_ptr, string);

        self.builder.build_call(arr_ext, params![dynarray_ptr, string_ptr, len], "");

        let dynarray = self.builder.build_load(_dynarray, dynarray_ptr, "");
        GonValue::Default(self.builder.create_struct_value(_str, &[dynarray]).unwrap().into())
    }

    /// Cast a GonValue to another type.
    /// 
    /// The only successful casts here: 
    /// - int to float
    /// - char to string
    /// - anything to unit
    /// - anything to bool
    pub fn cast(&mut self, v: GonValue<'ctx>, ty: &plir::Type) -> LLVMResult<'ctx, GonValue<'ctx>> {
        use plir::{Type, TypeRef};

        match (v, ty.as_ref()) {
            (GonValue::Int(i), TypeRef::Prim(Type::S_FLOAT)) => {
                let ft = self.get_layout(ty)?.into_float_type();
                let fv = self.builder.build_signed_int_to_float(i, ft, "cast");
                
                Ok(GonValue::Float(fv))
            },
            (GonValue::Char(c), TypeRef::Prim(Type::S_STR)) => {
                let to_str = self.std_import("char__to_string")?;
                let string = self.builder.build_call(to_str, params![c], "cast")
                    .try_as_basic_value()
                    .unwrap_left();

                Ok(GonValue::Default(string))
            },
            (_, TypeRef::Prim(Type::S_BOOL)) => Ok(GonValue::Bool(self.truth(v))),
            (_, TypeRef::Prim(Type::S_VOID)) => Ok(GonValue::Unit),
            _ => Err(LLVMErr::CannotCast(self.plir_type_of(v), ty.clone()))
        }
    }

    /// Create a [`GonValue`] from a given LLVM value.
    pub fn reconstruct(&self, t: &plir::Type, v: impl BasicValue<'ctx>) -> LLVMResult<'ctx, GonValue<'ctx>> {
        use plir::{TypeRef, Type};
        
        let v = v.as_basic_value_enum();
        match t.as_ref() {
            TypeRef::Prim(Type::S_FLOAT) => Ok(GonValue::Float(v.into_float_value())),
            TypeRef::Prim(Type::S_INT)   => Ok(GonValue::Int(v.into_int_value())),
            TypeRef::Prim(Type::S_BOOL)  => Ok(GonValue::Bool(v.into_int_value())),
            TypeRef::Prim(Type::S_CHAR)  => Ok(GonValue::Char(v.into_int_value())),
            TypeRef::Prim(Type::S_VOID)  => Ok(GonValue::Unit),
            _ => Ok(GonValue::Default(v))
        }
    }

    /// Obtain the basic LLVM value represented by this [`GonValue`].
    /// 
    /// Depending on context, [`Compiler::returnable_value_of`] may be more suitable.
    pub fn basic_value_of(&self, value: GonValue<'ctx>) -> BasicValueEnum<'ctx> {
        match value {
            GonValue::Float(f)   => f.into(),
            GonValue::Int(i)     => i.into(),
            GonValue::Bool(b)    => b.into(),
            GonValue::Char(c)    => c.into(),
            GonValue::Default(s) => s,
            GonValue::Unit       => layout!(self, S_VOID).const_zero(),
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
        use inkwell::types::BasicTypeEnum;
        use plir::{ty, Type};

        match value {
            GonValue::Float(_)   => ty!(Type::S_FLOAT),
            GonValue::Int(_)     => ty!(Type::S_INT),
            GonValue::Bool(_)    => ty!(Type::S_BOOL),
            GonValue::Char(_)    => ty!(Type::S_CHAR),
            GonValue::Default(s) => {
                let st = s.get_type();
                
                match st {
                    BasicTypeEnum::ArrayType(t) => unimplemented!("{t} does not have a PLIR representation"),
                    BasicTypeEnum::FloatType(_) => ty!(Type::S_FLOAT),
                    BasicTypeEnum::IntType(t)   => match t.get_bit_width() {
                        1 => ty!(Type::S_BOOL),
                        8 => ty!("#byte"),
                        32 => ty!(Type::S_CHAR),
                        64 => ty!(Type::S_INT),
                        _ => unimplemented!("{t} does not have a PLIR representation")
                    },
                    BasicTypeEnum::PointerType(_) => ty!("#ptr"),
                    BasicTypeEnum::StructType(t)  => {
                        let name = t.get_name()
                            .expect("Expected struct to have name")
                            .to_str()
                            .expect("Expected struct name to be Rust-compatible");
                        ty!(name)
                    },
                    BasicTypeEnum::VectorType(t)  => unimplemented!("{t} does not have a PLIR representation")
                }
                
            },
            GonValue::Unit => ty!(Type::S_VOID),
        }
    }
}