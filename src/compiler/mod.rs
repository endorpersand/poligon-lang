mod op_impl;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::values::{BasicValueEnum, FloatValue, IntValue};

use crate::tree;

pub struct Compiler<'ctx> {
    ctx: &'ctx Context,
    builder: Builder<'ctx>
}

impl<'ctx> Compiler<'ctx> {
    pub fn from_ctx(ctx: &'ctx Context) -> Self {
        Self {
            ctx,
            builder: ctx.create_builder()
        }
    }
}

enum Value<'ctx> {
    Float(FloatValue<'ctx>),
    Int(IntValue<'ctx>),
    Bool(IntValue<'ctx>)
}
impl<'ctx> Value<'ctx> {
    fn new_int(c: &Compiler<'ctx>, v: isize) -> Self {
        Self::Int(c.ctx.i64_type().const_int(v as u64, true))
    }
    fn new_bool(c: &Compiler<'ctx>, v: bool) -> Self {
        Self::Bool(c.ctx.bool_type().const_int(v as u64, true))
    }
    fn new_float(c: &Compiler<'ctx>, f: f64) -> Self {
        Self::Float(c.ctx.f64_type().const_float(f))
    }
}

trait TraverseIR<'ctx> {
    type Return;
    fn traverse_ir(&self, compiler: &Compiler<'ctx>) -> Self::Return;
}

impl<'ctx> TraverseIR<'ctx> for tree::Expr {
    type Return = ();

    fn traverse_ir(&self, compiler: &Compiler<'ctx>) -> Self::Return {
        match self {
            tree::Expr::Ident(_) => todo!(),
            tree::Expr::Block(_) => todo!(),
            tree::Expr::Literal(literal) => todo!(),
            tree::Expr::ListLiteral(_) => todo!(),
            tree::Expr::SetLiteral(_) => todo!(),
            tree::Expr::DictLiteral(_) => todo!(),
            tree::Expr::Assign(_, _) => todo!(),
            tree::Expr::Path(_) => todo!(),
            tree::Expr::UnaryOps(_) => todo!(),
            tree::Expr::BinaryOp(_) => todo!(),
            tree::Expr::Comparison { left, rights } => todo!(),
            tree::Expr::Range { left, right, step } => todo!(),
            tree::Expr::If(_) => todo!(),
            tree::Expr::While { condition, block } => todo!(),
            tree::Expr::For { ident, iterator, block } => todo!(),
            tree::Expr::Call { funct, params } => todo!(),
            tree::Expr::Index(_) => todo!(),
            tree::Expr::Spread(_) => todo!(),
        }
    }
}

impl<'ctx> TraverseIR<'ctx> for tree::Literal {
    type Return = Value<'ctx>;

    fn traverse_ir(&self, compiler: &Compiler<'ctx>) -> Self::Return {
        let ctx = compiler.ctx;
        match self {
            tree::Literal::Int(i)   => Value::new_int(compiler, *i),
            tree::Literal::Float(f) => Value::new_float(compiler, *f),
            tree::Literal::Char(_)  => todo!(),
            tree::Literal::Str(_)   => todo!(),
            tree::Literal::Bool(b)  => Value::new_bool(compiler, *b),
        }
    }
}

#[cfg(test)]
mod tests {
    use inkwell::context::Context;

    use crate::tree;

    use super::Compiler;

    #[test]
    fn what_am_i_doing() {
        let ctx = Context::create();
        let compiler = Compiler::from_ctx(&ctx);

        let i99 = ctx.i64_type().const_int(999, true);
        let btrue = ctx.bool_type().const_int(1, true);
        compiler.builder.build_int_add(i99, btrue, "add");
    }
}