use inkwell::FloatPredicate;
use inkwell::values::{FloatValue, IntValue};

use crate::tree::{op, self};

use super::{Compiler, Value, TraverseIR};

pub(super) trait GonValue<'ctx>: Unary<'ctx> + Binary<'ctx> + Cmp<'ctx> 
    where Self: Sized
{
    fn truth(self, c: &Compiler<'ctx>) -> IntValue<'ctx> /* bool */;
}

pub(super) trait Unary<'ctx, Rhs=Self> {
    type Output;

    fn apply_unary(self, op: &op::Unary, c: &mut Compiler<'ctx>) -> Self::Output;
}
pub(super) trait Binary<'ctx, Rhs=Self> {
    type Output;

    fn apply_binary(self, op: &op::Binary, right: Rhs, c: &mut Compiler<'ctx>) -> Self::Output;
}
pub(super) trait Cmp<'ctx, Rhs=Self> {
    type Output;

    fn apply_cmp(self, op: &op::Cmp, right: Rhs, c: &mut Compiler<'ctx>) -> Self::Output;
}

impl<'ctx> Binary<'ctx> for &tree::Expr {
    type Output = super::IRResult<FloatValue<'ctx>>;

    fn apply_binary(self, op: &op::Binary, right: Self, c: &mut Compiler<'ctx>) -> Self::Output {
        match op {
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

impl<'ctx> Unary<'ctx> for FloatValue<'ctx> {
    type Output = FloatValue<'ctx>;

    fn apply_unary(self, op: &op::Unary, c: &mut Compiler<'ctx>) -> Self::Output {
        match op {
            op::Unary::Plus   => self,
            op::Unary::Minus  => c.builder.build_float_neg(self, "f_neg"),
            op::Unary::LogNot => todo!(),
            op::Unary::BitNot => todo!(),
        }
    }
}

impl<'ctx> Binary<'ctx> for FloatValue<'ctx> {
    type Output = FloatValue<'ctx>;

    fn apply_binary(self, op: &op::Binary, right: FloatValue<'ctx>, c: &mut Compiler<'ctx>) -> Self::Output {
        match op {
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

    fn apply_binary(self, op: &op::Binary, right: IntValue<'ctx>, c: &mut Compiler<'ctx>) -> Self::Output {
        match op {
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