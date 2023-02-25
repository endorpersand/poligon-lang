use inkwell::builder::Builder;
use inkwell::types::StructType;
use inkwell::values::{BasicValueEnum, StructValue};

use super::{CompileResult, CompileErr};


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
    ) -> CompileResult<'ctx, StructValue<'ctx>> {
        let mut result = ty.const_zero();
        
        for (i, &fval) in values.iter().enumerate() {
            result = self.build_insert_value(result, fval, i as u32, "")
                .ok_or_else(|| CompileErr::StructIndexOOB(i))?
                .try_into()
                .unwrap();
        }

        Ok(result)
    }
}