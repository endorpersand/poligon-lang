use inkwell::values::IntValue;

use crate::tree::{op, self};

use super::{Compiler, GonValueEnum, TraverseIR, IRResult};
use internal::*;

pub trait Unary<'ctx> {
    type Output;

    fn apply_unary(self, op: &op::Unary, c: &mut Compiler<'ctx>) -> Self::Output;
}
pub trait Binary<'ctx, Rhs=Self> {
    type Output;

    fn apply_binary(self, op: &op::Binary, right: Rhs, c: &mut Compiler<'ctx>) -> Self::Output;
}
pub trait Cmp<'ctx, Rhs=Self> {
    type Output;

    fn apply_cmp(self, op: &op::Cmp, right: Rhs, c: &mut Compiler<'ctx>) -> Self::Output;
}
pub trait Truth<'ctx> {
    fn truth(self, c: &Compiler<'ctx>) -> IntValue<'ctx> /* bool */;
}

impl<'ctx> Compiler<'ctx> {
    pub fn apply_unary<T: Unary<'ctx>>(&mut self, left: T, op: &op::Unary) -> T::Output {
        left.apply_unary(op, self)
    }
    pub fn apply_binary<T: Binary<'ctx, U>, U>(&mut self, left: T, op: &op::Binary, right: U) -> T::Output {
        left.apply_binary(op, right, self)
    }
    pub fn apply_cmp<T: Cmp<'ctx, U>, U>(&mut self, left: T, op: &op::Cmp, right: U) -> T::Output {
        left.apply_cmp(op, right, self)
    }
    pub fn truth<T: Truth<'ctx>>(&self, left: T) -> IntValue<'ctx> /* bool */ {
        left.truth(self)
    }
}

impl<'ctx> Unary<'ctx> for GonValueEnum<'ctx> {
    type Output = IRResult<GonValueEnum<'ctx>>;

    fn apply_unary(self, op: &op::Unary, c: &mut Compiler<'ctx>) -> Self::Output {
        match op {
            op::Unary::LogNot => Ok(GonValueEnum::Bool(c.builder.build_not(self.truth(c), "log_not"))),
            op => match self {
                GonValueEnum::Float(v) => v.unary_internal(op, c).map(GonValueEnum::Float),
                GonValueEnum::Int(v)   => Ok(GonValueEnum::Int(v.unary_internal(op, c))),
                GonValueEnum::Bool(v)  => Ok(GonValueEnum::Int(v.unary_internal(op, c))), // TODO: separate bool int
            }
        }
    }
}
impl<'ctx> Binary<'ctx> for GonValueEnum<'ctx> {
    type Output = IRResult<GonValueEnum<'ctx>>;

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
    type Output = IRResult<IntValue<'ctx> /* bool */>;

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
impl<'ctx> Truth<'ctx> for GonValueEnum<'ctx> {
    fn truth(self, c: &Compiler<'ctx>) -> IntValue<'ctx> /* bool */ {
        match self {
            GonValueEnum::Float(f) => f.truth_internal(c),
            GonValueEnum::Int(i)   => i.truth_internal(c),
            GonValueEnum::Bool(b)  => b,
        }
    }
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

mod internal {
    use inkwell::{FloatPredicate, IntPredicate};
    use inkwell::values::{FloatValue, IntValue};

    use crate::compiler::{Compiler, IRResult};
    use crate::tree::op;

    pub(super) trait ValueUnary<'ctx> {
        type Output;
    
        fn unary_internal(self, op: &op::Unary, c: &mut Compiler<'ctx>) -> Self::Output;
    }
    pub(super) trait ValueBinary<'ctx, Rhs=Self> {
        type Output;
    
        fn binary_internal(self, op: &op::Binary, right: Rhs, c: &mut Compiler<'ctx>) -> Self::Output;
    }
    pub(super) trait ValueCmp<'ctx, Rhs=Self> {
        type Output;
    
        fn cmp_internal(self, op: &op::Cmp, right: Rhs, c: &mut Compiler<'ctx>) -> Self::Output;
    }
    pub(super) trait ValueTruth<'ctx> {
        fn truth_internal(self, c: &Compiler<'ctx>) -> IntValue<'ctx> /* bool */;
    }

    impl<'ctx> ValueUnary<'ctx> for FloatValue<'ctx> {
        type Output = IRResult<Self>;
    
        fn unary_internal(self, op: &op::Unary, c: &mut Compiler<'ctx>) -> Self::Output {
            match op {
                op::Unary::Plus   => Ok(self),
                op::Unary::Minus  => Ok(c.builder.build_float_neg(self, "f_neg")),
                op::Unary::LogNot => unreachable!("logical not was directly computed on {}", self.get_type()),
                op::Unary::BitNot => todo!(),
            }
        }
    }
    
    impl<'ctx> ValueBinary<'ctx> for FloatValue<'ctx> {
        type Output = IRResult<Self>;
    
        fn binary_internal(self, op: &op::Binary, right: FloatValue<'ctx>, c: &mut Compiler<'ctx>) -> Self::Output {
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
                op::Binary::LogAnd => unreachable!("logical and was directly computed on {}", self.get_type()),
                op::Binary::LogOr  => unreachable!("logical or was directly computed on {}", self.get_type()),
            }
        }
    }
    
    impl<'ctx> ValueCmp<'ctx> for FloatValue<'ctx> {
        type Output = IntValue<'ctx> /* bool */;
    
        fn cmp_internal(self, op: &op::Cmp, right: Self, c: &mut Compiler<'ctx>) -> Self::Output {
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
    
    impl<'ctx> ValueTruth<'ctx> for FloatValue<'ctx> {
        fn truth_internal(self, c: &Compiler<'ctx>) -> IntValue<'ctx> {
            let zero = c.ctx.f64_type().const_zero();
            c.builder.build_float_compare(FloatPredicate::ONE, self, zero, "truth")
        }
    }
    
    impl<'ctx> ValueUnary<'ctx> for IntValue<'ctx> {
        type Output = IntValue<'ctx>;
    
        fn unary_internal(self, op: &op::Unary, c: &mut Compiler<'ctx>) -> Self::Output {
            match op {
                op::Unary::Plus   => self,
                op::Unary::Minus  => c.builder.build_int_neg(self, "i_neg"),
                op::Unary::LogNot => unreachable!("logical not was directly computed on {}", self.get_type()),
                op::Unary::BitNot => c.builder.build_not(self, "bit_not"),
            }
        }
    }
    
    impl<'ctx> ValueBinary<'ctx> for IntValue<'ctx> {
        type Output = IntValue<'ctx>;
    
        fn binary_internal(self, op: &op::Binary, right: IntValue<'ctx>, c: &mut Compiler<'ctx>) -> Self::Output {
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
                op::Binary::LogAnd => unreachable!("logical and was directly computed on {}", self.get_type()),
                op::Binary::LogOr => unreachable!("logical or was directly computed on {}", self.get_type()),
            }
        }
    }
    
    impl<'ctx> ValueCmp<'ctx> for IntValue<'ctx> {
        type Output = IntValue<'ctx>;
    
        fn cmp_internal(self, op: &op::Cmp, right: Self, c: &mut Compiler<'ctx>) -> Self::Output {
            let (pred, name) = match op {
                op::Cmp::Lt => (IntPredicate::SLT, "i_lt"),
                op::Cmp::Gt => (IntPredicate::SGT, "i_gt"),
                op::Cmp::Le => (IntPredicate::SLE, "i_le"),
                op::Cmp::Ge => (IntPredicate::SGE, "i_ge"),
                op::Cmp::Eq => (IntPredicate::EQ,  "i_eq"),
                op::Cmp::Ne => (IntPredicate::NE,  "i_ne"),
            };
    
            c.builder.build_int_compare(pred, self, right, name)
        }
    }
    
    impl<'ctx> ValueTruth<'ctx> for IntValue<'ctx> {
        fn truth_internal(self, c: &Compiler<'ctx>) -> IntValue<'ctx> {
            let zero = c.ctx.i64_type().const_zero();
            c.builder.build_int_compare(IntPredicate::NE, self, zero, "truth")
        }
    }
}