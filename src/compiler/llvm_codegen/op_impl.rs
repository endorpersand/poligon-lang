use inkwell::types::BasicType;
use inkwell::{FloatPredicate, IntPredicate};
use inkwell::values::{IntValue, FloatValue, BasicValueEnum as BV, PointerValue, VectorValue, StructValue, ArrayValue};

use crate::compiler::plir;
use crate::compiler::{LLVMCodegen, LLVMResult, LLVMErr};
use crate::ast::op;

use super::{GonValue, apply_bv, apply_bt};

pub trait AsBV<'ctx> {
    fn into_bv(self, c: &mut LLVMCodegen<'ctx>) -> LLVMResult<BV<'ctx>>;
}
pub trait AsBVInfallible<'ctx> {
    fn into_bvi(self, c: &LLVMCodegen<'ctx>) -> BV<'ctx>;
}

impl<'ctx, V: AsBVInfallible<'ctx>> AsBV<'ctx> for V {
    fn into_bv(self, c: &mut LLVMCodegen<'ctx>) -> LLVMResult<BV<'ctx>> {
        Ok(self.into_bvi(c))
    }
}
impl<'ctx> AsBV<'ctx> for &plir::Expr {
    fn into_bv(self, c: &mut LLVMCodegen<'ctx>) -> LLVMResult<BV<'ctx>> {
        c.compile(self).and_then(|gv| gv.into_bv(c))
    }
}
impl<'ctx> AsBVInfallible<'ctx> for GonValue<'ctx> {
    fn into_bvi(self, c: &LLVMCodegen<'ctx>) -> BV<'ctx> {
        c.basic_value_of(self)
    }
}
impl<'ctx> AsBVInfallible<'ctx> for BV<'ctx> {
    fn into_bvi(self, _: &LLVMCodegen<'ctx>) -> BV<'ctx> {
        self
    }
}

pub trait Unary<'ctx> {
    type Output;

    /// Create an instruction computing the unary operation on a given value.
    fn apply_unary(self, op: op::Unary, c: &mut LLVMCodegen<'ctx>) -> Self::Output;
}
pub trait Binary<'ctx, Rhs=Self> {
    type Output;

    /// Create an instruction computing the binary operation on two values.
    fn apply_binary(self, op: op::Binary, right: Rhs, c: &mut LLVMCodegen<'ctx>) -> Self::Output;
}
pub trait Cmp<'ctx, Rhs=Self> {
    type Output;

    /// Create an instruction comparing two values.
    fn apply_cmp(self, op: op::Cmp, right: Rhs, c: &mut LLVMCodegen<'ctx>) -> Self::Output;
}
pub trait Truth<'ctx> {
    /// Calculate the boolean value (the truth value) of some given value.
    fn truth(self, c: &LLVMCodegen<'ctx>) -> IntValue<'ctx> /* bool */;
}
trait TruthBool<'ctx> {
    fn truth_bool(self) -> bool;
}
impl <'ctx, T: TruthBool<'ctx>> Truth<'ctx> for T {
    fn truth(self, c: &LLVMCodegen<'ctx>) -> IntValue<'ctx>  {
        c.ctx.bool_type().const_int(self.truth_bool() as _, false)
    }
}

fn cannot_unary<'ctx, T>(op: op::Unary, left: impl Into<BV<'ctx>>) -> LLVMResult<T> {
    Err(LLVMErr::CannotUnary(op, left.into().get_type().into()))
}
fn cannot_binary<'ctx, T>(op: op::Binary, left: impl Into<BV<'ctx>>, right: impl Into<BV<'ctx>>) -> LLVMResult<T> {
    Err(LLVMErr::CannotBinary(op, left.into().get_type().into(), right.into().get_type().into()))
}
fn cannot_cmp<'ctx, T>(op: op::Cmp, left: impl Into<BV<'ctx>>, right: impl Into<BV<'ctx>>) -> LLVMResult<T> {
    Err(LLVMErr::CannotCmp(op, left.into().get_type().into(), right.into().get_type().into()))
}

impl<'ctx> LLVMCodegen<'ctx> {
    /// Create an instruction computing the unary operation on a given value.
    pub(crate) fn apply_unary<T: AsBV<'ctx>>(
        &mut self, 
        left: T, 
        op: op::Unary
    ) -> <BV<'ctx> as Unary<'ctx>>::Output {
        left.into_bv(self)?.apply_unary(op, self)
    }

    /// Create an instruction computing the binary operation on two values.
    pub(crate) fn apply_binary<T: AsBV<'ctx>, U: AsBV<'ctx>>(
        &mut self, 
        left: T, 
        op: op::Binary, 
        right: U
    ) -> <BV<'ctx> as Binary<'ctx, U>>::Output {
        left.into_bv(self)?.apply_binary(op, right, self)
    }

    /// Create an instruction comparing two values.
    pub(crate) fn apply_cmp<T: AsBV<'ctx>, U: AsBV<'ctx>>(
        &mut self, 
        left: T, 
        op: op::Cmp, 
        right: U
    ) -> <BV<'ctx> as Cmp<'ctx, U>>::Output {
        left.into_bv(self)?.apply_cmp(op, right, self)
    }

    /// Calculate the boolean value (the truth value) of some given value.
    pub(crate) fn truth<T: AsBVInfallible<'ctx>>(&self, left: T) -> IntValue<'ctx> /* bool */ {
        left.into_bvi(self).truth(self)
    }
}

impl<'ctx> Unary<'ctx> for BV<'ctx> {
    type Output = LLVMResult<BV<'ctx>>;

    fn apply_unary(self, op: op::Unary, c: &mut LLVMCodegen<'ctx>) -> Self::Output {
        match op {
            op::Unary::LogNot => Ok(c.builder.build_not(self.truth(c), "l_not").into()),
            _ => match self {
                BV::IntValue(v)     => Ok(v.apply_unary(op, c).into()),
                BV::FloatValue(v)   => v.apply_unary(op, c).map(Into::into),
                BV::ArrayValue(_)   => cannot_unary(op, self),
                BV::PointerValue(_) => cannot_unary(op, self),
                BV::StructValue(_)  => cannot_unary(op, self),
                BV::VectorValue(_)  => cannot_unary(op, self),
            }
        }
    }
}
impl<'ctx, T: AsBV<'ctx>> Binary<'ctx, T> for BV<'ctx> {
    type Output = LLVMResult<BV<'ctx>>;

    fn apply_binary(self, op: op::Binary, right: T, c: &mut LLVMCodegen<'ctx>) -> Self::Output {
        match op {
            op::Binary::LogAnd => {
                let bb = c.get_insert_block();

                // lhs ? rhs : lhs
                let mut then_bb = c.ctx.insert_basic_block_after(bb, "and_true");
                let merge_bb = c.ctx.insert_basic_block_after(then_bb, "and_merge");
                c.builder.build_conditional_branch(self.truth(c), then_bb, merge_bb);
                
                c.builder.position_at_end(then_bb);
                let rhs = right.into_bv(c)?;
                c.builder.build_unconditional_branch(merge_bb);
                then_bb = c.builder.get_insert_block().unwrap();

                c.builder.position_at_end(merge_bb);
                let phi = c.builder.build_phi(self.get_type(), "and_result");
                phi.add_incoming(&[
                    // if LHS was true
                    (&rhs, then_bb),
                    // if LHS was false
                    (&self, bb),
                ]);

                Ok(phi.as_basic_value())
            },
            op::Binary::LogOr  => {
                let bb = c.get_insert_block();

                // lhs ? lhs : rhs
                let mut else_bb = c.ctx.insert_basic_block_after(bb, "or_false");
                let merge_bb = c.ctx.insert_basic_block_after(else_bb, "or_merge");
                c.builder.build_conditional_branch(self.truth(c), merge_bb, else_bb);
                
                c.builder.position_at_end(else_bb);
                let rhs = right.into_bv(c)?;
                c.builder.build_unconditional_branch(merge_bb);
                else_bb = c.builder.get_insert_block().unwrap();

                c.builder.position_at_end(merge_bb);
                let phi = c.builder.build_phi(self.get_type(), "or_result");
                phi.add_incoming(&[
                    // if LHS was true
                    (&self, bb),
                    // if LHS was false
                    (&rhs, else_bb)
                ]);

                Ok(phi.as_basic_value())
            },
            _ => {
                let rhs = right.into_bv(c)?;
                
                macro_rules! cast_rhs {
                    () => {{
                        match rhs.try_into() {
                            Ok(t) => t,
                            Err(_) => cannot_binary(op, self, rhs)?
                        }
                    }}
                }

                match self {
                    BV::ArrayValue(v)   => v.apply_binary(op, cast_rhs!(), c).map(Into::into),
                    BV::IntValue(v)     => Ok(v.apply_binary(op, cast_rhs!(), c).into()),
                    BV::FloatValue(v)   => v.apply_binary(op, cast_rhs!(), c).map(Into::into),
                    BV::PointerValue(_) => cannot_binary(op, self, rhs),
                    BV::StructValue(_)  => cannot_binary(op, self, rhs),
                    BV::VectorValue(_)  => cannot_binary(op, self, rhs),
                }
            }
        }
    }
}
impl<'ctx, T: AsBV<'ctx>> Cmp<'ctx, T> for BV<'ctx> {
    type Output = LLVMResult<IntValue<'ctx>>;

    fn apply_cmp(self, op: op::Cmp, right: T, c: &mut LLVMCodegen<'ctx>) -> Self::Output {
        let rhs = right.into_bv(c)?;

        macro_rules! cast_rhs {
            () => {{
                match rhs.try_into() {
                    Ok(t) => t,
                    Err(_) => cannot_cmp(op, self, rhs)?
                }
            }}
        }

        match self {
            BV::ArrayValue(_)   => cannot_cmp(op, self, rhs),
            BV::IntValue(v)     => Ok(v.apply_cmp(op, cast_rhs!(), c)),
            BV::FloatValue(v)   => Ok(v.apply_cmp(op, cast_rhs!(), c)),
            BV::PointerValue(v) => Ok(v.apply_cmp(op, cast_rhs!(), c)),
            BV::StructValue(_)  => cannot_cmp(op, self, rhs),
            BV::VectorValue(_)  => cannot_cmp(op, self, rhs),
        }
    }
}
impl<'ctx> Truth<'ctx> for BV<'ctx> {
    fn truth(self, c: &LLVMCodegen<'ctx>) -> IntValue<'ctx>  {
        apply_bv!(let bv = self => bv.truth(c))
    }
}

impl<'ctx> Unary<'ctx> for FloatValue<'ctx> {
    type Output = LLVMResult<Self>;

    fn apply_unary(self, op: op::Unary, c: &mut LLVMCodegen<'ctx>) -> Self::Output {
        match op {
            op::Unary::Plus   => Ok(self),
            op::Unary::Minus  => Ok(c.builder.build_float_neg(self, "f_neg")),
            op::Unary::LogNot => unreachable!("logical not was directly computed on {}", self.get_type()),
            op::Unary::BitNot => cannot_unary(op, self),
        }
    }
}

impl<'ctx> Unary<'ctx> for IntValue<'ctx> {
    type Output = IntValue<'ctx>;

    fn apply_unary(self, op: op::Unary, c: &mut LLVMCodegen<'ctx>) -> Self::Output {
        match op {
            op::Unary::Plus   => self,
            op::Unary::Minus  => c.builder.build_int_neg(self, "i_neg"),
            op::Unary::LogNot => unreachable!("logical not was directly computed on {}", self.get_type()),
            op::Unary::BitNot => c.builder.build_not(self, "bit_not"),
        }
    }
}

impl<'ctx> Binary<'ctx> for FloatValue<'ctx> {
    type Output = LLVMResult<Self>;

    fn apply_binary(self, op: op::Binary, right: FloatValue<'ctx>, c: &mut LLVMCodegen<'ctx>) -> Self::Output {
        match op {
            op::Binary::Add => Ok(c.builder.build_float_add(self, right, "f_add")),
            op::Binary::Sub => Ok(c.builder.build_float_sub(self, right, "f_sub")),
            op::Binary::Mul => Ok(c.builder.build_float_mul(self, right, "f_mul")),
            op::Binary::Div => Ok(c.builder.build_float_div(self, right, "f_div")),
            op::Binary::Mod => Ok(c.builder.build_float_rem(self, right, "f_mod")),
            op::Binary::Shl => cannot_binary(op, self, right),
            op::Binary::Shr => cannot_binary(op, self, right),
            op::Binary::BitOr => cannot_binary(op, self, right),
            op::Binary::BitAnd => cannot_binary(op, self, right),
            op::Binary::BitXor => cannot_binary(op, self, right),
            op::Binary::LogAnd => unreachable!("logical and was directly computed on {}", self.get_type()),
            op::Binary::LogOr  => unreachable!("logical or was directly computed on {}", self.get_type()),
        }
    }
}

impl<'ctx> Binary<'ctx> for IntValue<'ctx> {
    type Output = IntValue<'ctx>;

    fn apply_binary(self, op: op::Binary, right: IntValue<'ctx>, c: &mut LLVMCodegen<'ctx>) -> Self::Output {
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

impl<'ctx> Binary<'ctx> for ArrayValue<'ctx> {
    type Output = LLVMResult<Self>;

    fn apply_binary(self, op: op::Binary, right: Self, c: &mut LLVMCodegen<'ctx>) -> Self::Output {
        match op {
            op::Binary::Add    => {
                let ty1  = self.get_type();
                let ty2 = right.get_type();

                let ety1  = ty1.get_element_type();
                let ety2 = ty2.get_element_type();

                if ety1 == ety2 {
                    let result_len = ty1.len().saturating_add(ty2.len());
                    let extra_len = result_len - ty2.len();
                    let tyr = apply_bt!(let ty = ety1 => ty.array_type(result_len));
                    
                    let slice2_ptr = c.builder.build_alloca(ty2, "slice_alloc");
                    let slice2 = c.builder.build_load(ety2.array_type(extra_len), slice2_ptr, "slice_load");

                    let result_ptr = c.builder.build_alloca(tyr, "concat");
                    c.builder.build_store(result_ptr, tyr.const_zero());

                    // insert L elements into the buffer
                    c.builder.build_store(result_ptr, self);
                    
                    // insert R elements into the buffer
                    let zero = c.ctx.i32_type().const_zero();
                    let insert_idx = c.ctx.i32_type().const_int(ty1.len() as _, false);
                    let insert_ptr = unsafe { 
                        c.builder.build_in_bounds_gep(tyr, result_ptr, &[zero, insert_idx], "insert") 
                    };
                    c.builder.build_store(insert_ptr, slice2);

                    Ok(c.builder.build_typed_load(tyr, result_ptr, "concat_load"))
                } else {
                    cannot_binary(op, self, right)
                }
            },
            op::Binary::Sub    => cannot_binary(op, self, right),
            op::Binary::Mul    => cannot_binary(op, self, right),
            op::Binary::Div    => cannot_binary(op, self, right),
            op::Binary::Mod    => cannot_binary(op, self, right),
            op::Binary::Shl    => cannot_binary(op, self, right),
            op::Binary::Shr    => cannot_binary(op, self, right),
            op::Binary::BitOr  => cannot_binary(op, self, right),
            op::Binary::BitAnd => cannot_binary(op, self, right),
            op::Binary::BitXor => cannot_binary(op, self, right),
            op::Binary::LogAnd => unreachable!("logical and was directly computed on {}", self.get_type()),
            op::Binary::LogOr  => unreachable!("logical or was directly computed on {}", self.get_type()),
        }
    }
}

impl<'ctx> Cmp<'ctx> for FloatValue<'ctx> {
    type Output = IntValue<'ctx> /* bool */;

    fn apply_cmp(self, op: op::Cmp, right: Self, c: &mut LLVMCodegen<'ctx>) -> Self::Output {
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

impl<'ctx> Cmp<'ctx> for IntValue<'ctx> {
    type Output = IntValue<'ctx>;

    fn apply_cmp(self, op: op::Cmp, right: Self, c: &mut LLVMCodegen<'ctx>) -> Self::Output {
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

impl<'ctx> Cmp<'ctx> for PointerValue<'ctx> {
    type Output = IntValue<'ctx>;

    fn apply_cmp(self, op: op::Cmp, right: Self, c: &mut LLVMCodegen<'ctx>) -> Self::Output {
        let diff = c.builder.build_ptr_diff(c.ctx.i8_type(), self, right, "");
        diff.apply_cmp(op, diff.get_type().const_zero(), c)
    }
}

impl<'ctx> Truth<'ctx> for FloatValue<'ctx> {
    fn truth(self, c: &LLVMCodegen<'ctx>) -> IntValue<'ctx> {
        let zero = self.get_type().const_zero();
        c.builder.build_float_compare(FloatPredicate::ONE, self, zero, "truth")
    }
}

impl<'ctx> Truth<'ctx> for IntValue<'ctx> {
    fn truth(self, c: &LLVMCodegen<'ctx>) -> IntValue<'ctx> {
        let ty = self.get_type();

        // special exception for bools:
        if ty.get_bit_width() == 1 { 
            self 
        } else {
            let zero = ty.const_zero();
            c.builder.build_int_compare(IntPredicate::NE, self, zero, "truth")
        }
    }
}

impl<'ctx> TruthBool<'ctx> for ArrayValue<'ctx> {
    fn truth_bool(self) -> bool {
        // Arrays can go beyond the length, but we will just avoid doing that in Poligon.
        self.get_type().len() != 0
    }
}
impl<'ctx> TruthBool<'ctx> for StructValue<'ctx> {
    fn truth_bool(self) -> bool {
        !self.is_null()
    }
}
impl<'ctx> TruthBool<'ctx> for VectorValue<'ctx> {
    fn truth_bool(self) -> bool {
        self.get_type().get_size() != 0
    }
}
impl<'ctx> TruthBool<'ctx> for PointerValue<'ctx> {
    fn truth_bool(self) -> bool {
        !self.is_null()
    }
}