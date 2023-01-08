use inkwell::values::{IntValue, FloatValue};

use crate::compiler::resolve::plir;
use crate::compiler::{Compiler, IRResult, IRErr, TraverseIR};
use crate::tree::op;

use self::internal::*;

use super::GonValue;

pub trait Unary<'ctx> {
    type Output;

    fn apply_unary(self, op: op::Unary, c: &mut Compiler<'ctx>) -> Self::Output;
}
pub trait Binary<'ctx, Rhs=Self> {
    type Output;

    fn apply_binary(self, op: op::Binary, right: Rhs, c: &mut Compiler<'ctx>) -> Self::Output;
}
pub trait Cmp<'ctx, Rhs=Self> {
    type Output;

    fn apply_cmp(self, op: op::Cmp, right: Rhs, c: &mut Compiler<'ctx>) -> Self::Output;
}
pub trait Truth<'ctx> {
    fn truth(self, c: &Compiler<'ctx>) -> IntValue<'ctx> /* bool */;
}

impl<'ctx> Compiler<'ctx> {
    pub fn apply_unary<T: Unary<'ctx>>(&mut self, left: T, op: op::Unary) -> T::Output {
        left.apply_unary(op, self)
    }
    pub fn apply_binary<T: Binary<'ctx, U>, U>(&mut self, left: T, op: op::Binary, right: U) -> T::Output {
        left.apply_binary(op, right, self)
    }
    pub fn apply_cmp<T: Cmp<'ctx, U>, U>(&mut self, left: T, op: op::Cmp, right: U) -> T::Output {
        left.apply_cmp(op, right, self)
    }
    pub fn truth<T: Truth<'ctx>>(&self, left: T) -> IntValue<'ctx> /* bool */ {
        left.truth(self)
    }
}

enum NumericArgs<'ctx> {
    Float(FloatValue<'ctx>, FloatValue<'ctx>),
    Int(IntValue<'ctx>, IntValue<'ctx>),
    Other(GonValue<'ctx>, GonValue<'ctx>)
}

impl<'ctx> NumericArgs<'ctx> {
    fn new(c: &Compiler<'ctx>, lhs: GonValue<'ctx>, rhs: GonValue<'ctx>) -> Self {
        match (lhs, rhs) {
            (GonValue::Float(f1), GonValue::Float(f2)) => Self::Float(f1, f2),
            
            (GonValue::Float(f1), GonValue::Int(i2)) => {
                let f2 = c.builder.build_signed_int_to_float(i2, f1.get_type(), "num_op_cast_implicit");
                Self::Float(f1, f2)
            },
            (GonValue::Int(i1), GonValue::Float(f2)) => {
                let f1 = c.builder.build_signed_int_to_float(i1, f2.get_type(), "num_op_cast_implicit");
                Self::Float(f1, f2)
            },
            
            (GonValue::Int(i1), GonValue::Int(i2)) => Self::Int(i1, i2),
            (a, b) => Self::Other(a, b)
        }
    }
}
impl<'ctx> Unary<'ctx> for GonValue<'ctx> {
    type Output = IRResult<GonValue<'ctx>>;

    fn apply_unary(self, op: op::Unary, c: &mut Compiler<'ctx>) -> Self::Output {
        match op {
            op::Unary::LogNot => Ok(GonValue::Bool(c.builder.build_not(self.truth(c), "log_not"))),
            op => match self {
                GonValue::Float(v) => match v.unary_internal(op, c) {
                    Ok(f) => Ok(GonValue::Float(f)),
                    Err(IOpErr::WrongType) => Err(IRErr::CannotUnary(op, self.type_layout())),
                },
                GonValue::Int(v)   => Ok(GonValue::Int(v.unary_internal(op, c))),
                GonValue::Bool(v)  => Ok(GonValue::Int(v.unary_internal(op, c))), // TODO: separate int/bool
                GonValue::Str(_)   => todo!(),
                GonValue::Unit     => Err(IRErr::CannotUnary(op, self.type_layout())),
            }
        }
    }
}
impl<'ctx> Binary<'ctx> for GonValue<'ctx> {
    type Output = IRResult<GonValue<'ctx>>;

    fn apply_binary(self, op: op::Binary, right: Self, c: &mut Compiler<'ctx>) -> Self::Output {
        macro_rules! num_args_else {
            ($($p:pat => $e:expr),*) => {
                match NumericArgs::new(c, self, right) {
                    NumericArgs::Float(f1, f2) => match f1.binary_internal(op, f2, c) {
                        Ok(t) => Ok(GonValue::Float(t)),
                        Err(IOpErr::WrongType) => Err(IRErr::CannotBinary(op, self.type_layout(), right.type_layout())),
                    },
                    NumericArgs::Int(i1, i2) => {
                        Ok(GonValue::Int(i1.binary_internal(op, i2, c)))
                    },
                    $($p => $e),*
                }
            }
        }
        match op {
            // numeric add
            | op::Binary::Add 
            | op::Binary::Sub
            | op::Binary::Div
            | op::Binary::Mod
            | op::Binary::Shl
            | op::Binary::Shr
            => num_args_else!{
                NumericArgs::Other(o1, o2) => Err(IRErr::CannotBinary(op, o1.type_layout(), o2.type_layout()))
            },

            // numeric mul, collection repeat
            op::Binary::Mul => num_args_else! {
                NumericArgs::Other(_, _) => todo!()
            },

            // bitwise or, collection concat
            op::Binary::BitOr => num_args_else! {
                NumericArgs::Other(_, _) => todo!()
            },

            // bitwise and
            op::Binary::BitAnd => num_args_else! {
                NumericArgs::Other(_, _) => todo!()
            },

            // bitwise xor
            op::Binary::BitXor => num_args_else! {
                NumericArgs::Other(_, _) => todo!()
            },

            // logical and
            op::Binary::LogAnd => {
                let fun = c.parent_fn();
                let mut bb = c.get_insert_block();
                let merge_bb = c.ctx.append_basic_block(fun, "post_logand_eager");

                let mut rhs_bb = c.ctx.append_basic_block(fun, "logand_eager_true");
                
                c.builder.position_at_end(rhs_bb);
                c.builder.build_unconditional_branch(merge_bb);
                rhs_bb = c.builder.get_insert_block().unwrap();
                
                c.builder.position_at_end(bb);
                c.builder.build_conditional_branch(self.truth(c), rhs_bb, merge_bb);
                bb = c.builder.get_insert_block().unwrap();

                c.builder.position_at_end(merge_bb);

                let phi = c.builder.build_phi(self.type_layout().basic_type(c), "logand_eager_result");
                phi.add_incoming(&[
                    (&self.basic_value(c), bb),
                    (&right.basic_value(c), rhs_bb),
                ]);
                Ok(GonValue::reconstruct(&self.plir_type(), phi.as_basic_value()))
            },

            // logical or
            op::Binary::LogOr => {
                let fun = c.parent_fn();
                let mut bb = c.get_insert_block();
                let merge_bb = c.ctx.append_basic_block(fun, "post_logor_eager");

                let mut rhs_bb = c.ctx.append_basic_block(fun, "logor_eager_false");

                c.builder.position_at_end(rhs_bb);
                c.builder.build_unconditional_branch(merge_bb);
                rhs_bb = c.builder.get_insert_block().unwrap();
                
                c.builder.position_at_end(bb);
                c.builder.build_conditional_branch(self.truth(c), merge_bb, rhs_bb);
                bb = c.builder.get_insert_block().unwrap();

                c.builder.position_at_end(merge_bb);

                let phi = c.builder.build_phi(self.type_layout().basic_type(c), "logor_eager_result");
                phi.add_incoming(&[
                    (&self.basic_value(c), bb),
                    (&right.basic_value(c), rhs_bb),
                ]);
                Ok(GonValue::reconstruct(&self.plir_type(), phi.as_basic_value()))
            },
        }
    }
}
impl<'ctx> Cmp<'ctx> for GonValue<'ctx> {
    type Output = IRResult<IntValue<'ctx> /* bool */>;

    fn apply_cmp(self, op: op::Cmp, right: Self, c: &mut Compiler<'ctx>) -> Self::Output {
        match NumericArgs::new(c, self, right) {
            NumericArgs::Float(f1, f2) => Ok(f1.cmp_internal(op, f2, c)),
            NumericArgs::Int(i1, i2)   => Ok(i1.cmp_internal(op, i2, c)),
            NumericArgs::Other(_, _)   => todo!(),
        }
    }
}
impl<'ctx> Truth<'ctx> for GonValue<'ctx> {
    fn truth(self, c: &Compiler<'ctx>) -> IntValue<'ctx> /* bool */ {
        match self {
            GonValue::Float(f) => f.truth_internal(c),
            GonValue::Int(i)   => i.truth_internal(c),
            GonValue::Bool(b)  => b,
            GonValue::Str(_)   => todo!(),
            GonValue::Unit     => c.ctx.bool_type().const_int(0, true),
        }
    }
}

impl<'ctx> Unary<'ctx> for &plir::Expr {
    type Output = IRResult<GonValue<'ctx>>;

    fn apply_unary(self, op: op::Unary, c: &mut Compiler<'ctx>) -> Self::Output {
        let val = self.write_ir(c)?;
        c.apply_unary(val, op)
    }
}
impl<'ctx> Binary<'ctx> for &plir::Expr {
    type Output = IRResult<GonValue<'ctx>>;

    fn apply_binary(self, op: op::Binary, right: Self, c: &mut Compiler<'ctx>) -> Self::Output {
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
                let phi = c.builder.build_phi(lhs.type_layout().basic_type(c), "logand_result"); // TODO, properly type
                phi.add_incoming(&[
                    // if LHS was true
                    (&rhs.basic_value(c), then_bb),
                    // if LHS was false
                    (&lhs.basic_value(c), bb),
                ]);

                Ok(GonValue::reconstruct(&lhs.plir_type(), phi.as_basic_value()))
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
                let phi = c.builder.build_phi(lhs.type_layout().basic_type(c), "logor_result"); // TODO, properly type
                phi.add_incoming(&[
                    // if LHS was true
                    (&lhs.basic_value(c), bb),
                    // if LHS was false
                    (&rhs.basic_value(c), else_bb)
                ]);

                Ok(GonValue::reconstruct(&lhs.plir_type(), phi.as_basic_value()))
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

    use crate::compiler::Compiler;
    use crate::tree::op;

    pub(super) enum IOpErr {
        WrongType
    }

    type IOpResult<T> = Result<T, IOpErr>;

    pub(super) trait ValueUnary<'ctx> {
        type Output;
    
        fn unary_internal(self, op: op::Unary, c: &mut Compiler<'ctx>) -> Self::Output;
    }
    pub(super) trait ValueBinary<'ctx, Rhs=Self> {
        type Output;
    
        fn binary_internal(self, op: op::Binary, right: Rhs, c: &mut Compiler<'ctx>) -> Self::Output;
    }
    pub(super) trait ValueCmp<'ctx, Rhs=Self> {
        type Output;
    
        fn cmp_internal(self, op: op::Cmp, right: Rhs, c: &mut Compiler<'ctx>) -> Self::Output;
    }
    pub(super) trait ValueTruth<'ctx> {
        fn truth_internal(self, c: &Compiler<'ctx>) -> IntValue<'ctx> /* bool */;
    }

    impl<'ctx> ValueUnary<'ctx> for FloatValue<'ctx> {
        type Output = IOpResult<Self>;
    
        fn unary_internal(self, op: op::Unary, c: &mut Compiler<'ctx>) -> Self::Output {
            match op {
                op::Unary::Plus   => Ok(self),
                op::Unary::Minus  => Ok(c.builder.build_float_neg(self, "f_neg")),
                op::Unary::LogNot => unreachable!("logical not was directly computed on {}", self.get_type()),
                op::Unary::BitNot => Err(IOpErr::WrongType),
            }
        }
    }
    
    impl<'ctx> ValueBinary<'ctx> for FloatValue<'ctx> {
        type Output = IOpResult<Self>;
    
        fn binary_internal(self, op: op::Binary, right: FloatValue<'ctx>, c: &mut Compiler<'ctx>) -> Self::Output {
            match op {
                op::Binary::Add => Ok(c.builder.build_float_add(self, right, "f_add")),
                op::Binary::Sub => Ok(c.builder.build_float_sub(self, right, "f_sub")),
                op::Binary::Mul => Ok(c.builder.build_float_mul(self, right, "f_mul")),
                op::Binary::Div => Ok(c.builder.build_float_div(self, right, "f_div")),
                op::Binary::Mod => Ok(c.builder.build_float_rem(self, right, "f_mod")),
                op::Binary::Shl => Err(IOpErr::WrongType),
                op::Binary::Shr => Err(IOpErr::WrongType),
                op::Binary::BitOr => Err(IOpErr::WrongType),
                op::Binary::BitAnd => Err(IOpErr::WrongType),
                op::Binary::BitXor => Err(IOpErr::WrongType),
                op::Binary::LogAnd => unreachable!("logical and was directly computed on {}", self.get_type()),
                op::Binary::LogOr  => unreachable!("logical or was directly computed on {}", self.get_type()),
            }
        }
    }
    
    impl<'ctx> ValueCmp<'ctx> for FloatValue<'ctx> {
        type Output = IntValue<'ctx> /* bool */;
    
        fn cmp_internal(self, op: op::Cmp, right: Self, c: &mut Compiler<'ctx>) -> Self::Output {
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
    
        fn unary_internal(self, op: op::Unary, c: &mut Compiler<'ctx>) -> Self::Output {
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
    
        fn binary_internal(self, op: op::Binary, right: IntValue<'ctx>, c: &mut Compiler<'ctx>) -> Self::Output {
            // TODO: _add vs _nsw_add vs _nuw_add
            match op {
                op::Binary::Add => c.builder.build_int_add(self, right, "i_add"),
                op::Binary::Sub => c.builder.build_int_sub(self, right, "i_sub"),
                op::Binary::Mul => c.builder.build_int_mul(self, right, "i_mul"),
                op::Binary::Div => {
                    // TODO: deal with div by 0
                    c.builder.build_int_signed_div(self, right, "i_div")
                },
                op::Binary::Mod => {
                    // TODO: deal with div by 0
                    c.builder.build_int_signed_rem(self, right, "i_mod")
                },
                op::Binary::Shl => c.builder.build_left_shift(self, right, "i_shl"),
                // TODO, arith shr vs logical shr
                op::Binary::Shr => c.builder.build_right_shift(self, right, true, "i_shr"),
                op::Binary::BitOr => c.builder.build_or(self, right, "i_or"),
                op::Binary::BitAnd => c.builder.build_and(self, right, "i_and"),
                op::Binary::BitXor => c.builder.build_xor(self, right, "i_xor"),
                op::Binary::LogAnd => unreachable!("logical and was directly computed on {}", self.get_type()),
                op::Binary::LogOr => unreachable!("logical or was directly computed on {}", self.get_type()),
            }
        }
    }
    
    impl<'ctx> ValueCmp<'ctx> for IntValue<'ctx> {
        type Output = IntValue<'ctx>;
    
        fn cmp_internal(self, op: op::Cmp, right: Self, c: &mut Compiler<'ctx>) -> Self::Output {
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