mod op_impl;

use std::collections::HashMap;

use inkwell::FloatPredicate;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{FloatValue, IntValue, FunctionValue, BasicValue, BasicValueEnum};

use crate::tree;

pub struct Compiler<'ctx> {
    ctx: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,

    vars: HashMap<String, BasicValueEnum<'ctx>>
}

impl<'ctx> Compiler<'ctx> {
    pub fn from_ctx(ctx: &'ctx Context) -> Self {
        Self {
            ctx,
            builder: ctx.create_builder(),
            module: ctx.create_module("eval"),
            vars: HashMap::new()
        }
    }

    fn compile<T: TraverseIR<'ctx>>(&mut self, t: &T) -> T::Return {
        t.traverse_ir(self)
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

#[derive(Debug)]
enum IRErr {
    UndefinedVariable(String),
    UndefinedFunction(String),
    WrongArity(usize /* expected */, usize /* got */),
    CallWasInstruction,
    InvalidFunction
}
type IRResult<T> = Result<T, IRErr>;

trait TraverseIR<'ctx> {
    type Return;
    fn traverse_ir(&self, compiler: &mut Compiler<'ctx>) -> Self::Return;
}

impl<'ctx> TraverseIR<'ctx> for tree::Program {
    type Return = IRResult<FloatValue<'ctx>>;

    fn traverse_ir(&self, compiler: &mut Compiler<'ctx>) -> Self::Return {
        if let [tree::Stmt::Expr(e)] = &self.0[..] {
            e.traverse_ir(compiler)
        } else {
            todo!()
        }
        // let Compiler { ctx, module, .. } = compiler;
        
        // let main_ret = ctx.void_type().fn_type(&[], false);
        // let fun = module.add_function("main", main_ret, None);
        // let block = ctx.append_basic_block(fun, "main_block");

        // for stmt in &self.0 {
        //     compiler.builder.position_at_end(block);
        //     stmt.traverse_ir(compiler)?;
        // }
        // compiler.builder.position_at_end(block);
        // compiler.builder.build_return(None);

        // if fun.verify(true) {
        //     Ok(fun)
        // } else {
        //     // SAFETY: Not used after.
        //     unsafe { fun.delete() }
        //     Err(IRErr::InvalidFunction)
        // }
    }
}

impl<'ctx> TraverseIR<'ctx> for tree::Stmt {
    type Return = IRResult<()>;

    fn traverse_ir(&self, compiler: &mut Compiler<'ctx>) -> <Self as TraverseIR>::Return {
        match self {
            tree::Stmt::Decl(_) => todo!(),
            tree::Stmt::Return(_) => todo!(),
            tree::Stmt::Break => todo!(),
            tree::Stmt::Continue => todo!(),
            tree::Stmt::FunDecl(d) => { d.traverse_ir(compiler)?; },
            tree::Stmt::Expr(e) => {
                let expr = e.traverse_ir(compiler)?;
                todo!()
                // compiler.builder.insert_instruction(&expr.as_instruction().unwrap(), None);
            },
        };

        Ok(())
    }
}

impl<'ctx> TraverseIR<'ctx> for tree::Expr {
    type Return = IRResult<FloatValue<'ctx>>;

    fn traverse_ir(&self, compiler: &mut Compiler<'ctx>) -> Self::Return {
        match self {
            tree::Expr::Ident(ident) => match compiler.vars.get(ident) {
                Some(var) => Ok(var.into_float_value()),
                None => Err(IRErr::UndefinedVariable(ident.clone())),
            },
            tree::Expr::Block(_) => todo!(),
            tree::Expr::Literal(literal) => literal.traverse_ir(compiler),
            tree::Expr::ListLiteral(_) => todo!(),
            tree::Expr::SetLiteral(_) => todo!(),
            tree::Expr::DictLiteral(_) => todo!(),
            tree::Expr::Assign(_, _) => todo!(),
            tree::Expr::Path(_) => todo!(),
            tree::Expr::UnaryOps(_) => todo!(),
            tree::Expr::BinaryOp(tree::BinaryOp { op, left, right }) => {
                // todo, make lazy
                let lval = left.traverse_ir(compiler)?;
                let rval = right.traverse_ir(compiler)?;

                Ok(op_impl::Binary::apply_binary(lval, op, rval, compiler))
            },
            tree::Expr::Comparison { left, rights } => todo!(),
            tree::Expr::Range { left, right, step } => todo!(),
            tree::Expr::If(e) => e.traverse_ir(compiler),
            tree::Expr::While { condition, block } => todo!(),
            tree::Expr::For { ident, iterator, block } => todo!(),
            tree::Expr::Call { funct, params } => {
                if let tree::Expr::Ident(ident) = &**funct {
                    match compiler.module.get_function(ident) {
                        Some(fun) => {
                            let fun_params = fun.count_params() as usize;
                            let expr_params = params.len();
                            if fun_params == expr_params {
                                let resolved_params: Vec<_> = params.iter()
                                    .map(|p| p.traverse_ir(compiler).map(Into::into))
                                    .collect::<Result<_, _>>()?;

                                let call = compiler.builder.build_call(fun, &resolved_params, "call");
                                
                                match call.try_as_basic_value().left() {
                                    Some(basic) => Ok(basic.into_float_value()),
                                    None => Err(IRErr::CallWasInstruction),
                                }
                            } else {
                                Err(IRErr::WrongArity(fun_params, expr_params))
                            }
                        },
                        None => Err(IRErr::UndefinedFunction(ident.clone())),
                    }
                } else {
                    todo!()
                }
            },
            tree::Expr::Index(_) => todo!(),
            tree::Expr::Spread(_) => todo!(),
        }
    }
}

impl<'ctx> TraverseIR<'ctx> for tree::Literal {
    type Return = IRResult<FloatValue<'ctx>>;

    fn traverse_ir(&self, compiler: &mut Compiler<'ctx>) -> Self::Return {
        let ctx = compiler.ctx;
        match self {
            tree::Literal::Int(i)   => todo!(),
            tree::Literal::Float(f) => Ok(ctx.f64_type().const_float(*f)),
            tree::Literal::Char(_)  => todo!(),
            tree::Literal::Str(_)   => todo!(),
            tree::Literal::Bool(b)  => todo!(),
        }
    }
}

impl<'ctx> TraverseIR<'ctx> for tree::If {
    type Return = IRResult<FloatValue<'ctx>>;

    fn traverse_ir(&self, compiler: &mut Compiler<'ctx>) -> Self::Return {
        let tree::If { conditionals, last } = self;
        let parent = compiler.builder.get_insert_block().and_then(|bb| bb.get_parent())
            .expect("Expected if to be generated in function");
        
        let mut incoming = vec![];
        let mut prev_else = None;
        let merge_bb = compiler.ctx.append_basic_block(parent, "merge");

        for (cmp, block) in conditionals {
            // if there was a previous else, reposition us there
            if let Some(bb) = prev_else.take() {
                compiler.builder.position_at_end(bb);
            }
            // comparison value
            // TODO, handle non-float
            let cmp_fval = cmp.traverse_ir(compiler)?;
            let zero = compiler.ctx.f64_type().const_zero();
            let cmp_val = compiler.builder.build_float_compare(FloatPredicate::ONE, cmp_fval, zero, "cond_cmp");
                
            // create blocks and branch
            let mut then_bb = compiler.ctx.prepend_basic_block(merge_bb, "then");
            let else_bb     = compiler.ctx.prepend_basic_block(merge_bb, "else");
    
            compiler.builder.build_conditional_branch(cmp_val, then_bb, else_bb);

            // build then block
            compiler.builder.position_at_end(then_bb);
            let then_result = block.traverse_ir(compiler)?;
            compiler.builder.build_unconditional_branch(merge_bb);
            //update then block
            then_bb = compiler.builder.get_insert_block().unwrap();

            incoming.push((then_result, then_bb));
            prev_else.replace(else_bb);
        }

        // handle last
        match (prev_else, last) {
            (Some(mut else_bb), Some(block)) => {
                // build else block
                compiler.builder.position_at_end(else_bb);
                let else_result = block.traverse_ir(compiler)?;
                compiler.builder.build_unconditional_branch(merge_bb);
                //update else block
                else_bb = compiler.builder.get_insert_block().unwrap();

                incoming.push((else_result, else_bb));
            },
            (None, Some(_))    => unreachable!(),
            (_, None)          => todo!("Support if with no else"),
        }

        compiler.builder.position_at_end(merge_bb);
        let phi = compiler.builder.build_phi(compiler.ctx.f64_type(), "ifval");
        
        let (incoming_results, incoming_blocks): (Vec<_>, Vec<_>) = incoming.into_iter().unzip();
        let incoming: Vec<_> = std::iter::zip(incoming_results.iter(), incoming_blocks)
            .map(|(a, b)| (a as &dyn BasicValue, b))
            .collect();
        phi.add_incoming(&incoming);

        Ok(phi.as_basic_value().into_float_value())
    }
}

impl<'ctx> TraverseIR<'ctx> for tree::FunDecl {
    type Return = IRResult<FunctionValue<'ctx>>;

    fn traverse_ir(&self, compiler: &mut Compiler<'ctx>) -> Self::Return {
        let tree::FunDecl { ident, params, ret: _, block } = self;
        let Compiler { ctx, builder, module, vars } = compiler;

        // Function signature
        let arg_types = params.iter()
            .map(|_| ctx.f64_type().into())
            .collect::<Vec<_>>();
        let fun_type = ctx.f64_type().fn_type(&arg_types, false);

        let fun = module.add_function(ident, fun_type, None);

        // set arguments names
        for (param, arg) in std::iter::zip(params, fun.get_param_iter()) {
            arg.into_float_value().set_name(&param.ident);
        }

        // Body
        let bb = ctx.append_basic_block(fun, "block");
        builder.position_at_end(bb);

        // store params
        for (param, arg) in std::iter::zip(params, fun.get_param_iter()) {
            // todo: see if there's a more efficient way of allocating
            vars.insert(param.ident.clone(), arg);
        }

        // TODO: expand beyond 1 expr
        let tree::Program(inner) = &**block;
        let ret_value = if let [tree::Stmt::Expr(e)] = &inner[..] {
            e.traverse_ir(compiler)?
        } else {
            todo!()
        };
        
        compiler.builder.build_return(Some(&ret_value));
        
        if fun.verify(true) {
            Ok(fun)
        } else {
            // SAFETY: Not used after.
            unsafe { fun.delete() }
            Err(IRErr::InvalidFunction)
        }
    }
}

#[cfg(test)]
mod tests {
    use inkwell::context::Context;

    use crate::lexer;
    use crate::parser;
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

    #[test]
    fn what_am_i_doing_2() {
        let ctx = Context::create();
        let mut compiler = Compiler::from_ctx(&ctx);

        let lexed = lexer::tokenize("fun hello() {
            2. + 3.;
        }").unwrap();
        
        let tree::Program(parsed) = parser::parse(lexed).unwrap();

        if let [tree::Stmt::FunDecl(fdcl)] = &parsed[..] {
            let fun = compiler.compile(fdcl).unwrap();
            fun.print_to_stderr();
        } else {
            panic!(":(");
        };
    }

    #[test]
    fn what_am_i_doing_3() {
        let ctx = Context::create();
        let mut compiler = Compiler::from_ctx(&ctx);

        let lexed = lexer::tokenize("fun double(a) {
            a * 2.;
        }").unwrap();
        
        let tree::Program(parsed) = parser::parse(lexed).unwrap();

        if let [tree::Stmt::FunDecl(fdcl)] = &parsed[..] {
            let fun = compiler.compile(fdcl).unwrap();
            fun.print_to_stderr();
        }
    }

    #[test]
    fn what_am_i_doing_4() {
        let ctx = Context::create();
        let mut compiler = Compiler::from_ctx(&ctx);

        let lexed = lexer::tokenize("2. * 2.;").unwrap();
        let parsed = parser::parse(lexed).unwrap();
        let fun = compiler.compile(&parsed).unwrap();
        fun.print_to_stderr();
    }

    #[test]
    fn what_am_i_doing_5() {
        let ctx = Context::create();
        let mut compiler = Compiler::from_ctx(&ctx);

        let lexed = lexer::tokenize("fun main(a) {
            if a {
                main(0.); 
            } else {
                main(0.) + 1.;
            }
        }").unwrap();
        let parsed = parser::parse(lexed).unwrap();

        if let [tree::Stmt::FunDecl(fdcl)] = &parsed.0[..] {
            let fun = compiler.compile(fdcl).unwrap();
            fun.print_to_stderr();
        } else {
            panic!(":(");
        };

        let ctx = Context::create();
        let mut compiler = Compiler::from_ctx(&ctx);

        let lexed = lexer::tokenize("fun main(a) {
            if a {
                main(0.); 
            } else if a {
                main(0.) + 1.;
            } else {
                main(0.) + 2.;
            }
        }").unwrap();
        let parsed = parser::parse(lexed).unwrap();

        if let [tree::Stmt::FunDecl(fdcl)] = &parsed.0[..] {
            let fun = compiler.compile(fdcl).unwrap();
            fun.print_to_stderr();
        } else {
            panic!(":(");
        };

        let ctx = Context::create();
        let mut compiler = Compiler::from_ctx(&ctx);

        let lexed = lexer::tokenize("fun main(a) {
            if a {
                main(0.); 
            } else if a {
                main(0.) + 1.;
            } else if a {
                main(0.) + 2.;
            } else if a {
                main(0.) + 3.;
            } else if a {
                main(0.) + 4.;
            } else if a {
                main(0.) + 5.;
            } else if a {
                main(0.) + 6.;
            } else if a {
                main(0.) + 7.;
            } else if a {
                main(0.) + 8.;
            } else if a {
                main(0.) + 9.;
            } else if a {
                main(0.) + 10.;
            } else if a {
                main(0.) + 11.;
            } else {
                main(0.) + 12.;
            }
        }").unwrap();
        let parsed = parser::parse(lexed).unwrap();

        if let [tree::Stmt::FunDecl(fdcl)] = &parsed.0[..] {
            let fun = compiler.compile(fdcl).unwrap();
            fun.print_to_stderr();
        } else {
            panic!(":(");
        };
    }
}