mod op_impl;

use std::collections::HashMap;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{FloatValue, IntValue, FunctionValue, BasicValue, PointerValue, PhiValue};

use crate::tree;

use self::op_impl::{GonValue, Cmp};
pub struct Compiler<'ctx> {
    ctx: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,

    vars: HashMap<String, PointerValue<'ctx>>
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
        t.write_ir(self)
    }

    fn insert_block(&self) -> BasicBlock<'ctx> {
        self.builder.get_insert_block()
            .expect("No insert block found")
    }

    fn parent_fn(&self) -> FunctionValue<'ctx> {
        self.builder.get_insert_block()
            .and_then(BasicBlock::get_parent)
            .expect("No insert block found")
    }

    fn update_block<F, T>(&mut self, bb: &mut BasicBlock<'ctx>, mut f: F) -> IRResult<T>
        where F: FnMut(&mut BasicBlock<'ctx>, &mut Self) -> IRResult<T>
    {
        self.builder.position_at_end(*bb);
        let t = f(bb, self)?;
        *bb = self.builder.get_insert_block().unwrap();
        Ok(t)
    }

    /// Create an alloca instruction in the entry block of
    /// the function.  This is used for mutable variables etc.
    fn create_entry_block_alloca(&mut self, ident: &str) -> PointerValue<'ctx> {
        let builder = self.ctx.create_builder();

        let fun_bb = self.parent_fn().get_first_basic_block().expect("Expected function to have block");
        // reposition this builder to the top of the first block
        match fun_bb.get_first_instruction() {
            Some(instr) => builder.position_before(&instr),
            None => builder.position_at_end(fun_bb),
        };

        // create alloca
        let alloca = builder.build_alloca(self.ctx.f64_type(), ident);
        self.vars.insert(String::from(ident), alloca);
        alloca
    }

    /// Store the value in the allocated position or return None if there is no allocated position
    fn store_val<V>(&mut self, ident: &str, val: V) -> Option<PointerValue<'ctx>> 
        where V: BasicValue<'ctx>
    {
        self.vars.get(ident)
            .map(|&alloca| {
                self.builder.build_store(alloca, val);
                alloca
            })
    }

    /// Create an alloca instruction at the top and also store the value at the current insert point
    fn alloca_and_store<V>(&mut self, ident: &str, val: V) -> PointerValue<'ctx> 
        where V: BasicValue<'ctx>
    {
        let alloca = self.create_entry_block_alloca(ident);

        self.builder.build_store(alloca, val);
        alloca
    }
}

fn add_incoming<'a, 'ctx, B: BasicValue<'ctx>>(
        phi: PhiValue<'ctx>,
        incoming: &'a [(B, BasicBlock<'ctx>)]
    ) {
    let (incoming_results, incoming_blocks): (Vec<&B>, Vec<_>) = incoming.iter()
        .map(|(a, b)| (a, *b))
        .unzip();

    let vec: Vec<_> = std::iter::zip(incoming_results, incoming_blocks)
        .map(|(a, b)| (a as _, b))
        .collect();
    
    phi.add_incoming(&vec);
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
    InvalidFunction,
    BlockHadFloatValue // TODO: remove
}
type IRResult<T> = Result<T, IRErr>;

trait TraverseIR<'ctx> {
    type Return;
    fn write_ir(&self, compiler: &mut Compiler<'ctx>) -> Self::Return;
}

impl<'ctx> TraverseIR<'ctx> for tree::Block {
    type Return = IRResult<Option<FloatValue<'ctx>>>;

    fn write_ir(&self, compiler: &mut Compiler<'ctx>) -> Self::Return {
        let mut stmts = self.0.iter();
        let maybe_last = stmts.next_back();

        match maybe_last {
            Some(last) => {
                for stmt in stmts {
                    stmt.write_ir(compiler)?;
                }
                last.write_ir(compiler)
            },
            None => {
                Ok(None)
            }
        }
    }
}

impl<'ctx> TraverseIR<'ctx> for tree::Program {
    type Return = IRResult<FloatValue<'ctx>>;

    fn write_ir(&self, compiler: &mut Compiler<'ctx>) -> Self::Return {
        todo!()
    }
}

impl<'ctx> TraverseIR<'ctx> for tree::Stmt {
    type Return = IRResult<Option<FloatValue<'ctx>>>;

    fn write_ir(&self, compiler: &mut Compiler<'ctx>) -> <Self as TraverseIR<'ctx>>::Return {
        match self {
            tree::Stmt::Decl(d) => {
                let tree::Decl { rt, pat, ty, val } = d;
                match pat {
                    tree::Pat::Unit(tree::DeclUnit::Ident(ident, mt)) => {
                        let val = val.write_ir(compiler)?;
                        compiler.alloca_and_store(ident, val);
                        Ok(None)
                    },
                    _ => todo!("pattern destructuring not implemented")
                }
            },
            tree::Stmt::Return(me) => {
                let expr = match me {
                    Some(e) => Some(e.write_ir(compiler)?),
                    None => None,
                };
                compiler.builder.build_return(expr.as_ref().map(|r| r as _));
                Ok(None)
            },
            tree::Stmt::Break => todo!(),
            tree::Stmt::Continue => todo!(),
            tree::Stmt::FunDecl(d) => {
                d.write_ir(compiler)?;
                Ok(None)
            },
            tree::Stmt::Expr(e) => {
                e.write_ir(compiler).map(|e| Some(e))
            },
        }
    }
}

impl<'ctx> TraverseIR<'ctx> for tree::Expr {
    type Return = IRResult<FloatValue<'ctx>>;

    fn write_ir(&self, compiler: &mut Compiler<'ctx>) -> Self::Return {
        match self {
            tree::Expr::Ident(ident) => match compiler.vars.get(ident) {
                Some(&ptr) => Ok(compiler.builder.build_load(ptr, "load").into_float_value()),
                None => Err(IRErr::UndefinedVariable(ident.clone())),
            },
            tree::Expr::Block(block) => {
                // wrap in block for clarity
                let fun = compiler.parent_fn();
                let orig_bb = compiler.insert_block();
                let mut expr_bb = compiler.ctx.append_basic_block(fun, "block");
                let exit_bb = compiler.ctx.append_basic_block(fun, "post_block");

                compiler.builder.position_at_end(orig_bb);
                compiler.builder.build_unconditional_branch(expr_bb);

                let t = compiler.update_block(&mut expr_bb, |_, c| {
                    let t = block.write_ir(c)?;
                    c.builder.build_unconditional_branch(exit_bb);
                    t.ok_or(IRErr::BlockHadFloatValue)
                })?;

                compiler.builder.position_at_end(exit_bb);
                Ok(t)
            },
            tree::Expr::Literal(literal) => literal.write_ir(compiler),
            tree::Expr::ListLiteral(_) => todo!(),
            tree::Expr::SetLiteral(_) => todo!(),
            tree::Expr::DictLiteral(_) => todo!(),
            tree::Expr::Assign(pat, expr) => {
                match pat {
                    tree::Pat::Unit(tree::AsgUnit::Ident(ident)) => {
                        let val = expr.write_ir(compiler)?;
                        compiler.store_val(ident, val)
                            .ok_or_else(|| IRErr::UndefinedVariable(ident.clone()))?;
                        Ok(val)
                    },
                    _ => todo!("pattern destructuring not implemented")
                }
            },
            tree::Expr::Path(_) => todo!(),
            tree::Expr::UnaryOps(_) => todo!(),
            tree::Expr::BinaryOp(tree::BinaryOp { op, left, right }) => {
                op_impl::Binary::apply_binary(&**left, op, &**right, compiler)
            },
            tree::Expr::Comparison { left, rights } => {
                let fun = compiler.parent_fn();
                let mut lval = left.write_ir(compiler)?;
                
                let mut riter = rights.iter();
                let last = riter.next_back();
                
                match last {
                    Some((last_cmp, last_rexpr)) => {
                        let mut incoming = vec![];
                        let post_bb = compiler.ctx.append_basic_block(fun, "post_cmp");

                        for (cmp, rexpr) in riter {
                            // eval comparison
                            let rval = rexpr.write_ir(compiler)?;
                            let result = lval.apply_cmp(cmp, rval, compiler);
                            
                            // branch depending on T/F
                            let then_bb = compiler.ctx.prepend_basic_block(post_bb, "cmp_true");
                            compiler.builder.build_conditional_branch(result, then_bb, post_bb);
                            
                            // update phi
                            incoming.push((result, compiler.insert_block()));

                            // prepare for next comparison
                            compiler.builder.position_at_end(then_bb);
                            lval = rval;
                        }
                        
                        // last block
                        let rval = last_rexpr.write_ir(compiler)?;
                        let result = lval.apply_cmp(last_cmp, rval, compiler);
                        // go to post block
                        compiler.builder.build_unconditional_branch(post_bb);
                        // add to phi
                        incoming.push((result, compiler.insert_block()));

                        compiler.builder.position_at_end(post_bb);
                        let phi = compiler.builder.build_phi(compiler.ctx.bool_type(), "cmp_result");
                        add_incoming(phi, &incoming);

                        let result = phi.as_basic_value().into_int_value();
                        let fresult = compiler.builder
                            .build_signed_int_to_float(result, compiler.ctx.f64_type(), "cmp_cast");
                        Ok(fresult)
                    },
                    None => Ok(lval),
                }
            },
            tree::Expr::Range { left, right, step } => todo!(),
            tree::Expr::If(e) => e.write_ir(compiler),
            tree::Expr::While { condition, block } => {
                let bb = compiler.insert_block();
                let fun = compiler.parent_fn();

                let cond_bb = compiler.ctx.append_basic_block(fun, "while_cond");
                let loop_bb = compiler.ctx.append_basic_block(fun, "while");
                let exit_loop_bb = compiler.ctx.append_basic_block(fun, "post_while");

                // end BB by going into loop
                compiler.builder.position_at_end(bb);
                compiler.builder.build_unconditional_branch(cond_bb);

                // if cond is true, go into the loop. otherwise, exit
                compiler.builder.position_at_end(cond_bb);
                let cond = condition.write_ir(compiler)?.truth(compiler);
                compiler.builder.build_conditional_branch(cond, loop_bb, exit_loop_bb);

                compiler.builder.position_at_end(loop_bb);
                block.write_ir(compiler)?; // todo, use value
                compiler.builder.build_unconditional_branch(cond_bb);

                compiler.builder.position_at_end(exit_loop_bb);
                Ok(compiler.ctx.f64_type().const_zero()) // TODO

            },
            tree::Expr::For { ident, iterator, block } => todo!(),
            tree::Expr::Call { funct, params } => {
                if let tree::Expr::Ident(ident) = &**funct {
                    match compiler.module.get_function(ident) {
                        Some(fun) => {
                            let fun_params = fun.count_params() as usize;
                            let expr_params = params.len();
                            if fun_params == expr_params {
                                let resolved_params: Vec<_> = params.iter()
                                    .map(|p| p.write_ir(compiler).map(Into::into))
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

    fn write_ir(&self, compiler: &mut Compiler<'ctx>) -> Self::Return {
        let ctx = compiler.ctx;
        match self {
            tree::Literal::Int(_)   => todo!("int literal"),
            tree::Literal::Float(f) => Ok(ctx.f64_type().const_float(*f)),
            tree::Literal::Char(_)  => todo!("char literal"),
            tree::Literal::Str(_)   => todo!("str literal"),
            tree::Literal::Bool(_)  => todo!("bool literal"),
        }
    }
}

impl<'ctx> TraverseIR<'ctx> for tree::If {
    type Return = IRResult<FloatValue<'ctx>>;

    fn write_ir(&self, compiler: &mut Compiler<'ctx>) -> Self::Return {
        let tree::If { conditionals, last } = self;
        let parent = compiler.parent_fn();
        
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
            let cmp_val = cmp.write_ir(compiler)?.truth(compiler);
                
            // create blocks and branch
            let mut then_bb = compiler.ctx.prepend_basic_block(merge_bb, "then");
            let else_bb     = compiler.ctx.prepend_basic_block(merge_bb, "else");
    
            compiler.builder.build_conditional_branch(cmp_val, then_bb, else_bb);

            // build then block
            compiler.builder.position_at_end(then_bb);
            let then_result = block.write_ir(compiler)?
                .unwrap_or_else(|| todo!("Accept blocks that do not return float"));
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
                let else_result = block.write_ir(compiler)?
                    .unwrap_or_else(|| todo!("Accept blocks that do not return float"));
                compiler.builder.build_unconditional_branch(merge_bb);
                //update else block
                else_bb = compiler.builder.get_insert_block().unwrap();

                incoming.push((else_result, else_bb));
            },
            (None, Some(_)) => unreachable!(),
            (_, None)       => todo!("Support if with no else"),
        }

        compiler.builder.position_at_end(merge_bb);
        let phi = compiler.builder.build_phi(compiler.ctx.f64_type(), "ifval");
        
        add_incoming(phi, &incoming);
        Ok(phi.as_basic_value().into_float_value())
    }
}

impl<'ctx> TraverseIR<'ctx> for tree::FunDecl {
    type Return = IRResult<FunctionValue<'ctx>>;

    fn write_ir(&self, compiler: &mut Compiler<'ctx>) -> Self::Return {
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
        let bb = ctx.append_basic_block(fun, &format!("{ident}_body"));
        builder.position_at_end(bb);

        // store params
        for (param, arg) in std::iter::zip(params, fun.get_param_iter()) {
            compiler.alloca_and_store(&param.ident, arg);
        }

        let ret_value = block.write_ir(compiler)?;
        // write the last value in block as return
        compiler.builder.build_return(ret_value.as_ref().map(|t| t as _));
        
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

    /// Assert that a function declaration with an expression in it passes.
    /// Also prints the function to STDERR.
    fn assert_fun_pass(input: &str) {
        let ctx = Context::create();
        let mut compiler = Compiler::from_ctx(&ctx);

        let lexed  = lexer::tokenize(input).unwrap();
        let parsed = parser::parse(lexed).unwrap();

        match &parsed.0.0[..] {
            [tree::Stmt::FunDecl(fdcl)] => {
                let fun = compiler.compile(fdcl).unwrap();
                fun.print_to_stderr();
            }
            _ => {
                panic!("Program is not a singular function declaration");
            }
        }
    }

    #[test]
    fn what_am_i_doing_2() {
        assert_fun_pass("fun hello() {
            2. + 3.;
        }");

        assert_fun_pass("fun double(a) {
            a * 2.;
        }");
    }

    #[test]
    fn if_else_compile_test() {
        assert_fun_pass("fun main(a) {
            if a {
                main(0.); 
            } else {
                main(0.) + 1.;
            }
        }");

        assert_fun_pass("fun main(a) {
            if a {
                main(0.); 
            } else if a {
                main(0.) + 1.;
            } else {
                main(0.) + 2.;
            }
        }");
        
        assert_fun_pass("fun main(a) {
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
        }");
    }

    #[test]
    fn while_ir() {
        assert_fun_pass("fun main(a) {
            while a {
                main(a);
            };
        }");
    }

    #[test]
    fn var_test() {
        assert_fun_pass("fun main(a) {
            a = 2.;
        }");

        assert_fun_pass("fun main(a) {
            let b = 2.;
        }");
    }

    #[test]
    fn log_and_log_or_test() {
        assert_fun_pass("fun main(a, b) {
            a && b;
        }");
        
        assert_fun_pass("fun main(a, b) {
            a || b;
        }");
    }

    #[test]
    fn cmp_test() {
        assert_fun_pass("fun main(a, b) {
            a < b;
        }");
        
        assert_fun_pass("fun main(a, b, c) {
            a < b < c;
        }");

        assert_fun_pass("fun main(a, b, c, d, e, f, g) {
            a < b < c < d == e < f > g;
        }");
    }

    #[test]
    fn fun_multi_stmt_test() {
        // test multiple declarations
        assert_fun_pass("fun main() {
            let a = 1.;
            let b = 2.;
            let c = 3.;
            a + b + c;
        }");

        // block test
        assert_fun_pass("fun main() {
            let b = {
                let a = 1.;
                a + 2.;
            };

            b;
        }");

        // multi expression
        assert_fun_pass("fun main() {
            1. + 2. + 3.;
            4. + 5. + 6.;
            7. + 8. + 9.;
        }");

        // // fast return
        // assert_fun_pass("fun main(a) {
        //     let b = if a {
        //         return 2.;
        //     } else {
        //         2. + 15.;
        //     };

        //     b;
        // }");
    }
}