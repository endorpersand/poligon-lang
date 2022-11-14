use inkwell::FloatPredicate;
use inkwell::values::{FloatValue, IntValue};

use crate::tree::{op, self};

use super::{Compiler, Value, TraverseIR};

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
    fn apply_binary(&mut self, b: &op::Binary, left: Value<'ctx>, right: Value<'ctx>) -> Value<'ctx> {
        match NumericArgs::from_args(self, left, right) {
            NumericArgs::Float(f1, f2) => Value::Float(f1.apply_binary(b, f2, self)),
            NumericArgs::Int(i1, i2)   => Value::Int(i1.apply_binary(b, i2, self)),
            NumericArgs::None(_, _)    => todo!(),
        }
    }
}

pub(super) trait GonValue<'ctx>: Binary<'ctx> + Cmp<'ctx> 
    where Self: Sized
{
    fn truth(self, c: &Compiler<'ctx>) -> IntValue<'ctx> /* bool */;
}

pub(super) trait Binary<'ctx, Rhs=Self> {
    type Output;

    fn apply_binary(self, b: &op::Binary, right: Rhs, c: &mut Compiler<'ctx>) -> Self::Output;
}
pub(super) trait Cmp<'ctx, Rhs=Self> {
    type Output;

    fn apply_cmp(self, op: &op::Cmp, right: Rhs, c: &mut Compiler<'ctx>) -> Self::Output;
}

impl<'ctx> Binary<'ctx> for tree::Expr {
    type Output = super::IRResult<FloatValue<'ctx>>;

    fn apply_binary(self, b: &op::Binary, right: Self, c: &mut Compiler<'ctx>) -> Self::Output {
        match b {
            // TODO: lazy LogAnd, LogOr
            // op::Binary::LogAnd => {
            //     let parent = c.parent_fn();

            //     let lhs = c.;
            // },
            // op::Binary::LogOr  => {
            //     let parent = c.parent_fn();

            //     let lhs = c.;
            // },

            // eager eval
            b => {
                let left = self.write_ir(c)?;
                let right = right.write_ir(c)?;

                Ok(left.apply_binary(b, right, c))
            }
        }
    }
}

impl<'ctx> Binary<'ctx> for FloatValue<'ctx> {
    type Output = FloatValue<'ctx>;

    fn apply_binary(self, b: &op::Binary, right: FloatValue<'ctx>, c: &mut Compiler<'ctx>) -> Self::Output {
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
            op::Binary::LogOr  => todo!(),
        }
    }
}

impl<'ctx> Cmp<'ctx> for FloatValue<'ctx> {
    type Output = IntValue<'ctx> /* bool */;

    fn apply_cmp(self, op: &op::Cmp, right: Self, c: &mut Compiler<'ctx>) -> Self::Output {
        // todo: investigate OXX and UXX
        let (pred, name) = match op {
            op::Cmp::Lt => (FloatPredicate::OLT, "f_lt"),
            op::Cmp::Gt => (FloatPredicate::OGT, "f_gt"),
            op::Cmp::Le => (FloatPredicate::OLE, "f_le"),
            op::Cmp::Ge => (FloatPredicate::OGE, "f_ge"),
            op::Cmp::Eq => (FloatPredicate::OEQ, "f_eq"),
            op::Cmp::Ne => (FloatPredicate::ONE, "f_ne"),
        };

        c.builder.build_float_compare(pred, self, right, name)
    }
}

impl<'ctx> GonValue<'ctx> for FloatValue<'ctx> {
    fn truth(self, c: &Compiler<'ctx>) -> IntValue<'ctx> {
        let zero = c.ctx.f64_type().const_zero();
        c.builder.build_float_compare(FloatPredicate::ONE, self, zero, "cond_cmp")
    }
}

impl<'ctx> Binary<'ctx> for IntValue<'ctx> {
    type Output = IntValue<'ctx>;

    fn apply_binary(self, b: &op::Binary, right: IntValue<'ctx>, c: &mut Compiler<'ctx>) -> Self::Output {
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