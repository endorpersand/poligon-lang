pub(crate) mod types;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::types::{StructType, BasicType};
use inkwell::values::{BasicValueEnum, StructValue, BasicValue, PointerValue, IntValue};

use super::{LLVMResult, LLVMErr};


pub(crate) struct Builder2<'ctx>(Builder<'ctx>);

impl<'ctx> std::ops::Deref for Builder2<'ctx> {
    type Target = Builder<'ctx>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<'ctx> std::ops::DerefMut for Builder2<'ctx> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'ctx> Builder2<'ctx> {
    pub fn new(b: Builder<'ctx>) -> Self {
        Self(b)
    }

    /// Initializes a new struct value 
    /// and assigns all the fields of that struct.
    pub fn create_struct_value(
        &self, 
        ty: StructType<'ctx>,
        values: &[BasicValueEnum<'ctx>]
    ) -> LLVMResult<'ctx, StructValue<'ctx>> {
        let mut result = ty.const_zero();
        
        for (i, &fval) in values.iter().enumerate() {
            result = self.build_insert_value(result, fval, i as u32, "")
                .ok_or_else(|| LLVMErr::StructIndexOOB(i))?
                .try_into()
                .unwrap();
        }

        Ok(result)
    }

    pub(crate) fn branch_and_goto(&self, bb: BasicBlock<'ctx>) {
        self.build_unconditional_branch(bb);
        self.position_at_end(bb);
    }

    /// [`Builder::build_load`] with implicit type coercion
    pub fn build_typed_load<T: BasicTypeT<'ctx>>(
        &self,
        pointee_ty: T,
        ptr: PointerValue<'ctx>,
        name: &str,
    ) -> T::Value {
        T::Value::try_from(self.build_load(pointee_ty, ptr, name))
            .ok()
            .expect("value to match pointee type in build_typed_load")
    }

    /// [`Builder::build_select`] with implicit type coercion.
    /// Unlike `build_select`, this can only use boolean parameters.
    #[allow(unused)]
    pub fn build_typed_select1<BV: BasicValue<'ctx> + TryFrom<BasicValueEnum<'ctx>>>(
        &self,
        condition: IntValue<'ctx>,
        then: BV,
        else_: BV,
        name: &str,
    ) -> BV {
        BV::try_from(self.build_select(condition, then, else_, name))
            .ok()
            .expect("value to match parameter types in build_typed_select1")
    }
}

pub trait BasicTypeT<'ctx>: BasicType<'ctx> {
    type Value: BasicValue<'ctx> + TryFrom<BasicValueEnum<'ctx>>;
}
macro_rules! btt_impl {
    ($($BT:ident: $BV:ident;)*) => {
        $(
            impl<'ctx> BasicTypeT<'ctx> for inkwell::types::$BT<'ctx> {
                type Value = inkwell::values::$BV<'ctx>;
            }
        )*
    }
}

btt_impl! {
    ArrayType: ArrayValue;
    IntType: IntValue;
    FloatType: FloatValue;
    PointerType: PointerValue;
    StructType: StructValue;
    VectorType: VectorValue;
    BasicTypeEnum: BasicValueEnum;
}