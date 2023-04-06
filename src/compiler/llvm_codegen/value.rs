use inkwell::values::BasicValueEnum;

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
/// and can be converted to an LLVM value via [`LLVMCodegen::basic_value_of`].
#[derive(Clone, Copy, Debug)]
pub enum GonValue<'ctx> {
    /// A non-unit type (which can be represented as an LLVM basic type)
    Basic(BasicValueEnum<'ctx>),
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
        GonValue::Basic(layout!(self, S_INT).into_int_type().const_int(v as u64, true).into())
    }
    /// Create a new bool value using a bool from Rust.
    pub fn new_bool(&self, v: bool) -> GonValue<'ctx> {
        GonValue::Basic(layout!(self, S_BOOL).into_int_type().const_int(v as u64, true).into())
    }
    /// Create a new float value using a float from Rust.
    pub fn new_float(&self, f: f64) -> GonValue<'ctx> {
        GonValue::Basic(layout!(self, S_FLOAT).into_float_type().const_float(f).into())
    }
    /// Create a new char value using a char from Rust.
    pub fn new_char(&self, c: char) -> GonValue<'ctx> {
        GonValue::Basic(layout!(self, S_CHAR).into_int_type().const_int(c as u64, true).into())
    }
    /// Create a new string value using a string slice from Rust.
    pub fn new_str(&self, s: &str) -> GonValue<'ctx> {
        // todo: support \0
        let _int = layout!(self, S_INT).into_int_type();
        let _str = layout!(self, S_STR).into_struct_type();

        let ident = plir::FunIdent::new_static(&plir::ty!(plir::Type::S_STR), "from_raw");
        let str_from_raw = self.get_fn_by_plir_ident(&ident).unwrap();

        // get ptr to content:
        let content_arr = self.ctx.const_string(s.as_bytes(), false);
        let content = self.builder.build_alloca(content_arr.get_type(), "");
        self.builder.build_store(content, content_arr);
        // get len:
        let len = _int.const_int(content_arr.get_type().len() as _, false);

        let string = self.builder.build_call(str_from_raw, params![content, len], "string_literal")
            .try_as_basic_value()
            .unwrap_left();
        
        GonValue::Basic(string)
    }

    /// Cast a GonValue to another type.
    /// 
    /// The only successful casts here: 
    /// - int to float
    /// - anything to unit
    /// - anything to bool
    pub fn cast(&mut self, v: GonValue<'ctx>, src: &plir::Type, dest: &plir::Type) -> LLVMResult<'ctx, GonValue<'ctx>> {
        use plir::{Type, TypeRef};

        match (src.as_ref(), dest.as_ref()) {
            (TypeRef::Prim(Type::S_INT), TypeRef::Prim(Type::S_FLOAT)) => {
                let _float = self.get_layout(dest)?.into_float_type();
                let GonValue::Basic(bv) = v else { unreachable!() };
                let val = self.builder.build_signed_int_to_float(bv.into_int_value(), _float, "cast");
                
                Ok(GonValue::Basic(val.into()))
            },
            (_, TypeRef::Prim(Type::S_BOOL)) => Ok(GonValue::Basic(self.truth(v).into())),
            (_, TypeRef::Prim(Type::S_VOID)) => Ok(GonValue::Unit),
            _ => Err(LLVMErr::CannotCast(src.clone(), dest.clone()))
        }
    }

    /// Obtain the basic LLVM value represented by this [`GonValue`].
    /// 
    /// For return statements, this function should not be used. 
    /// Instead, an `Option<BasicValueEnum<'ctx>>` should be created by checking
    /// the `plir::Type` of the GonValue.
    pub fn basic_value_of(&self, value: GonValue<'ctx>) -> BasicValueEnum<'ctx> {
        match value {
            GonValue::Basic(b) => b,
            GonValue::Unit     => layout!(self, S_VOID).const_zero()
        }
    }
}

impl<'ctx> From<Option<BasicValueEnum<'ctx>>> for GonValue<'ctx> {
    fn from(value: Option<BasicValueEnum<'ctx>>) -> Self {
        match value {
            Some(t) => GonValue::Basic(t),
            None => GonValue::Unit,
        }
    }
}