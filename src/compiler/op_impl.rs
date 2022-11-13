use inkwell::values::{BasicValueEnum, FloatValue, IntValue};

use crate::tree::op;

use super::{Compiler, Value};

enum NumericArgs<'ctx> {
    Float(FloatValue<'ctx>, FloatValue<'ctx>),
    Int(IntValue<'ctx>, IntValue<'ctx>),
    None(Value<'ctx>, Value<'ctx>)
}
impl<'ctx> NumericArgs<'ctx> {
    fn from_args(c: &Compiler<'ctx>, a: Value<'ctx>, b: Value<'ctx>) -> Self {
        match (a, b) {
            (Value::Float(f1), Value::Float(f2)) => Self::Float(f1, f2),
            (Value::Float(f1), Value::Int(i2) | Value::Bool(i2)) => {
                let f2 = c.builder.build_signed_int_to_float(i2, c.ctx.f64_type(), "i_castf");
                Self::Float(f1, f2)
            },
            (Value::Int(i1) | Value::Bool(i1), Value::Float(f2)) => {
                let f1 = c.builder.build_signed_int_to_float(i1, c.ctx.f64_type(), "i_castf");
                Self::Float(f1, f2)
            },
            (Value::Int(i1) | Value::Bool(i1), Value::Int(i2) | Value::Bool(i2)) => Self::Int(i1, i2),
        }
    }
}
impl<'ctx> Compiler<'ctx> {
    fn apply_binary(&self, b: &op::Binary, left: Value<'ctx>, right: Value<'ctx>) -> Value<'ctx> {
        match NumericArgs::from_args(self, left, right) {
            NumericArgs::Float(f1, f2) => Value::Float(f1.apply_binary(b, f2, self)),
            NumericArgs::Int(i1, i2)   => Value::Int(i1.apply_binary(b, i2, self)),
            NumericArgs::None(_, _)    => todo!(),
        }
    }
}

pub(super) trait Binary<'ctx, Rhs=Self> {
    type Output;

    fn apply_binary(self, b: &op::Binary, right: Rhs, c: &Compiler<'ctx>) -> Self::Output;
}

impl<'ctx> Binary<'ctx> for FloatValue<'ctx> {
    type Output = FloatValue<'ctx>;

    fn apply_binary(self, b: &op::Binary, right: FloatValue<'ctx>, c: &Compiler<'ctx>) -> Self::Output {
        match b {
            op::Binary::Add => c.builder.build_float_add(self, right, "f_add"),
            op::Binary::Sub => c.builder.build_float_sub(self, right, "f_sub"),
            op::Binary::Mul => c.builder.build_float_mul(self, right, "f_mul"),
            op::Binary::Div => c.builder.build_float_div(self, right, "f_div"),
            op::Binary::Mod => c.builder.build_float_rem(self, right, "f_rem"),
            op::Binary::Shl => todo!(),
            op::Binary::Shr => todo!(),
            op::Binary::BitOr => todo!(),
            op::Binary::BitAnd => todo!(),
            op::Binary::BitXor => todo!(),
            op::Binary::LogAnd => todo!(),
            op::Binary::LogOr => todo!(),
        }
    }
}

impl<'ctx> Binary<'ctx> for IntValue<'ctx> {
    type Output = IntValue<'ctx>;

    fn apply_binary(self, b: &op::Binary, right: IntValue<'ctx>, c: &Compiler<'ctx>) -> Self::Output {
        match b {
            op::Binary::Add => todo!(),
            op::Binary::Sub => todo!(),
            op::Binary::Mul => todo!(),
            op::Binary::Div => todo!(),
            op::Binary::Mod => todo!(),
            op::Binary::Shl => todo!(),
            op::Binary::Shr => todo!(),
            op::Binary::BitOr => todo!(),
            op::Binary::BitAnd => todo!(),
            op::Binary::BitXor => todo!(),
            op::Binary::LogAnd => todo!(),
            op::Binary::LogOr => todo!(),
        }
    }
}