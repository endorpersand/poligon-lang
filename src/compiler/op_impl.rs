use inkwell::FloatPredicate;
use inkwell::values::{FloatValue, IntValue};

use crate::tree::{op, self};

use super::{Compiler, GonValueEnum, TraverseIR, IRResult};

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
    type Output = super::IRResult<GonValueEnum<'ctx>>;

    fn apply_binary(self, op: &op::Binary, right: Self, c: &mut Compiler<'ctx>) -> Self::Output {
        match op {
            op::Binary::LogAnd => {
                let parent = c.parent_fn();
                let bb = c.get_insert_block();

                let lhs = self.write_ir(c)?;

                // lhs ? rhs : lhs
                let mut then_bb = c.ctx.append_basic_block(parent, "logand_true");
                let merge_bb = c.ctx.append_basic_block(parent, "logand_merge");
                c.builder.build_conditional_branch(lhs.truth(c), then_bb, merge_bb);
                
                c.builder.position_at_end(then_bb);
                let rhs = right.write_ir(c)?;
                c.builder.build_unconditional_branch(merge_bb);
                then_bb = c.builder.get_insert_block().unwrap();

                c.builder.position_at_end(merge_bb);
                let phi = c.builder.build_phi(lhs.typed().basic_enum(c), "logand_result"); // TODO, properly type
                phi.add_incoming(&[
                    // if LHS was true
                    (&rhs.basic_enum(), then_bb),
                    // if LHS was false
                    (&lhs.basic_enum(), bb),
                ]);

                Ok(GonValueEnum::reconstruct(lhs.typed(), phi.as_basic_value()))
            },
            op::Binary::LogOr  => {
                let parent = c.parent_fn();
                let bb = c.get_insert_block();

                let lhs = self.write_ir(c)?;

                // lhs ? lhs : rhs
                let mut else_bb = c.ctx.append_basic_block(parent, "logor_false");
                let merge_bb = c.ctx.append_basic_block(parent, "logor_merge");
                c.builder.build_conditional_branch(lhs.truth(c), merge_bb, else_bb);
                
                c.builder.position_at_end(else_bb);
                let rhs = right.write_ir(c)?;
                c.builder.build_unconditional_branch(merge_bb);
                else_bb = c.builder.get_insert_block().unwrap();

                c.builder.position_at_end(merge_bb);
                let phi = c.builder.build_phi(lhs.typed().basic_enum(c), "logor_result"); // TODO, properly type
                phi.add_incoming(&[
                    // if LHS was true
                    (&lhs.basic_enum(), bb),
                    // if LHS was false
                    (&rhs.basic_enum(), else_bb)
                ]);

                Ok(GonValueEnum::reconstruct(lhs.typed(), phi.as_basic_value()))
            },

            // eager eval
            b => {
                let left = self.write_ir(c)?;
                let right = right.write_ir(c)?;

                left.apply_binary(b, right, c)
            }
        }
    }
}

impl<'ctx> Unary<'ctx> for GonValueEnum<'ctx> {
    type Output = IRResult<Self>;

    fn apply_unary(self, op: &op::Unary, c: &mut Compiler<'ctx>) -> Self::Output {
        match op {
            op::Unary::Plus => todo!(),
            op::Unary::Minus => todo!(),
            op::Unary::LogNot => todo!(),
            op::Unary::BitNot => todo!(),
        }
    }
}
impl<'ctx> Binary<'ctx> for GonValueEnum<'ctx> {
    type Output = IRResult<Self>;

    fn apply_binary(self, op: &op::Binary, right: Self, c: &mut Compiler<'ctx>) -> Self::Output {
        match op {
            // numeric add
            op::Binary::Add => todo!(),

            // numeric sub
            op::Binary::Sub => todo!(),

            // numeric mul, collection repeat
            op::Binary::Mul => todo!(),

            // numeric div
            op::Binary::Div => todo!(),

            // numeric modulo
            op::Binary::Mod => todo!(),

            // int shift left
            op::Binary::Shl => todo!(),

            // int shift right
            op::Binary::Shr => todo!(),

            // bitwise or, collection concat
            op::Binary::BitOr => todo!(),

            // bitwise and
            op::Binary::BitAnd => todo!(),

            // bitwise xor
            op::Binary::BitXor => todo!(),

            // logical and
            op::Binary::LogAnd => {
                let fun = c.parent_fn();
                let mut bb = c.get_insert_block();
                let merge_bb = c.ctx.append_basic_block(fun, "post_logand_eager");

                let mut rhs_bb = c.ctx.append_basic_block(fun, "logand_eager_true");
                c.update_block(&mut rhs_bb, |_, c| {
                    c.builder.build_unconditional_branch(merge_bb);
                    Ok(())
                })?;
                c.update_block(&mut bb, |_, c| {
                    c.builder.build_conditional_branch(self.truth(c), rhs_bb, merge_bb);
                    Ok(())
                })?;

                c.builder.position_at_end(merge_bb);
                c.builder.build_phi(self.typed().basic_enum(c), "logand_eager_result");
                todo!()
            },

            // logical or
            op::Binary::LogOr => {
                let fun = c.parent_fn();
                let mut bb = c.get_insert_block();
                let merge_bb = c.ctx.append_basic_block(fun, "post_logor_eager");

                let mut rhs_bb = c.ctx.append_basic_block(fun, "logor_eager_false");
                c.update_block(&mut rhs_bb, |_, c| {
                    c.builder.build_unconditional_branch(merge_bb);
                    Ok(())
                })?;
                c.update_block(&mut bb, |_, c| {
                    c.builder.build_conditional_branch(self.truth(c), rhs_bb, merge_bb);
                    Ok(())
                })?;

                c.builder.position_at_end(merge_bb);
                c.builder.build_phi(self.typed().basic_enum(c), "logor_eager_result");
                todo!()
            },
        }
    }
}

impl<'ctx> Cmp<'ctx> for GonValueEnum<'ctx> {
    type Output = IntValue<'ctx>;

    fn apply_cmp(self, op: &op::Cmp, right: Self, c: &mut Compiler<'ctx>) -> Self::Output {
        match op {
            op::Cmp::Lt => todo!(),
            op::Cmp::Gt => todo!(),
            op::Cmp::Le => todo!(),
            op::Cmp::Ge => todo!(),
            op::Cmp::Eq => todo!(),
            op::Cmp::Ne => todo!(),
        }
    }
}

impl<'ctx> Unary<'ctx> for FloatValue<'ctx> {
    type Output = IRResult<Self>;

    fn apply_unary(self, op: &op::Unary, c: &mut Compiler<'ctx>) -> Self::Output {
        match op {
            op::Unary::Plus   => Ok(self),
            op::Unary::Minus  => Ok(c.builder.build_float_neg(self, "f_neg")),
            op::Unary::LogNot => todo!(),
            op::Unary::BitNot => todo!(),
        }
    }
}

impl<'ctx> Binary<'ctx> for FloatValue<'ctx> {
    type Output = IRResult<Self>;

    fn apply_binary(self, op: &op::Binary, right: FloatValue<'ctx>, c: &mut Compiler<'ctx>) -> Self::Output {
        match op {
            op::Binary::Add => Ok(c.builder.build_float_add(self, right, "f_add")),
            op::Binary::Sub => Ok(c.builder.build_float_sub(self, right, "f_sub")),
            op::Binary::Mul => Ok(c.builder.build_float_mul(self, right, "f_mul")),
            op::Binary::Div => Ok(c.builder.build_float_div(self, right, "f_div")),
            op::Binary::Mod => Ok(c.builder.build_float_rem(self, right, "f_rem")),
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