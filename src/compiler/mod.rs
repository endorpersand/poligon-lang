//! The compiler, which takes an AST tree and reads it into LLVM code 
//! which can be compiled into an executable.
//! 
//! The compiler has two steps: converting into PLIR, then compiling into LLVM
//! 
//! # PLIR
//! PLIR, the intermediate language, simplifies the main language in order to make
//! it simpler to compiler to LLVM.
//! 
//! PLIR can be generated from AST via the [`codegen::codegen`] function,
//! or using the [`codegen::CodeGenerator`] struct.
//! 
//! # LLVM
//! The PLIR is then compiled to LLVM via the [`Compiler`] struct.

mod value;
pub mod codegen;
pub mod plir;

use std::collections::HashMap;
use std::ffi::CStr;
use std::iter;

use inkwell::OptimizationLevel;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::support::LLVMString;
use inkwell::types::StructType;
use inkwell::values::{FunctionValue, BasicValue, PointerValue, PhiValue, BasicValueEnum, StructValue};

use crate::ast::{op, Literal};
use crate::err::GonErr;

pub use self::value::*;
use self::value::apply_bv;

/// This struct converts from PLIR to LLVM.
pub struct Compiler<'ctx> {
    ctx: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,

    vars: HashMap<String, PointerValue<'ctx>>
}

impl<'ctx> Compiler<'ctx> {
    /// Create a new Compiler, using a [`Context`] from inkwell.
    pub fn from_ctx(ctx: &'ctx Context) -> Self {
        Self {
            ctx,
            builder: ctx.create_builder(),
            module: ctx.create_module("eval"),
            vars: HashMap::new()
        }
    }
    
    pub fn get_module(&self) -> &Module<'ctx> {
        &self.module
    }

    /// Executes a compiled program JIT, and returns the resulting value.
    /// 
    /// # Safety
    /// Currently, any type can be returned from this function. 
    /// Any calls to this function should ensure that the value returned in Poligon
    /// would align to the provided type in Rust.
    #[allow(unused)]
    pub unsafe fn jit_run<T>(&mut self, fun: FunctionValue<'ctx>) -> CompileResult<T> {
        let fn_name = fun.get_name()
            .to_str()
            .unwrap();

        let jit = self.module.create_jit_execution_engine(OptimizationLevel::Default)
            .map_err(CompileErr::LLVMErr)?;

        unsafe {
            let jit_fun = jit.get_function::<unsafe extern "C" fn() -> T>(fn_name).unwrap();
            Ok(jit_fun.call())
        }
    }

    /// Compiles the given item, 
    /// registers any defined variables or functions to the compiler,
    /// and returns the value generated by the write.
    pub fn compile<T: TraverseIR<'ctx>>(&mut self, t: &T) -> T::Return {
        t.write_ir(self)
    }

    fn get_insert_block(&self) -> BasicBlock<'ctx> {
        self.builder.get_insert_block()
            .expect("No insert block found")
    }

    fn parent_fn(&self) -> FunctionValue<'ctx> {
        self.builder.get_insert_block()
            .and_then(BasicBlock::get_parent)
            .expect("No insert block found")
    }

    /// Create an alloca instruction in the entry block of
    /// the function.  This is used for mutable variables etc.
    fn create_entry_block_alloca(&mut self, ident: &str, layout: TypeLayout) -> PointerValue<'ctx> {
        let builder = self.ctx.create_builder();

        let fun_bb = self.parent_fn().get_first_basic_block().expect("Expected function to have block");
        // reposition this builder to the top of the first block
        match fun_bb.get_first_instruction() {
            Some(instr) => builder.position_before(&instr),
            None => builder.position_at_end(fun_bb),
        };

        // create alloca
        let alloca = builder.build_alloca(layout.basic_type(self), ident);
        self.vars.insert(String::from(ident), alloca);
        alloca
    }

    /// Store the value in the allocated position or return None if there is no allocated position
    fn store_val(&self, ident: &str, val: GonValue<'ctx>) -> Option<PointerValue<'ctx>>
    {
        self.vars.get(ident)
            .map(|&alloca| {
                self.builder.build_store(alloca, val.basic_value(self));
                alloca
            })
    }

    /// Create an alloca instruction at the top and also store the value at the current insert point
    fn alloca_and_store(&mut self, ident: &str, val: GonValue<'ctx>) -> PointerValue<'ctx>
    {
        let alloca = self.create_entry_block_alloca(ident, val.type_layout());

        self.builder.build_store(alloca, val.basic_value(self));
        alloca
    }

    fn get_val(&self, ident: &str, ty: &plir::Type) -> CompileResult<GonValue<'ctx>> {
        match self.vars.get(ident) {
            Some(&ptr) => {
                let val = self.builder.build_load(ptr, "load");
                Ok(GonValue::reconstruct(ty, val))
            },
            None => Err(CompileErr::UndefinedVar(String::from(ident))),
        }
    }

    fn add_incoming_gv<'a>(&self, phi: PhiValue<'ctx>, incoming: &'a [(GonValue<'ctx>, BasicBlock<'ctx>)]) {
        let (incoming_results, incoming_blocks): (Vec<BasicValueEnum<'ctx>>, Vec<_>) = incoming.iter()
            .map(|(a, b)| (a.basic_value(self), *b))
            .unzip();
    
        let vec: Vec<_> = iter::zip(incoming_results.iter(), incoming_blocks)
            .map(|(a, b)| (a as _, b))
            .collect();
        
        phi.add_incoming(&vec);
    }

    /// Write a [`plir::Block`] into the LLVM representation.
    ///
    /// [`plir::Block`] does not implement [`TraverseIR`] 
    /// because how the block should be closed cannot be generalized with that trait.
    /// 
    /// The closer indicates how the block should be closed 
    /// if this block exits with the `exit` statement.
    /// This is useful, for example, for adding an unconditional branch to an LLVM block
    /// but only if there was no return statement already emitted.
    pub fn write_block<F>(&mut self, block: &plir::Block, close: F) -> CompileResult<GonValue<'ctx>>
        where F: FnOnce(&mut Self, GonValue<'ctx>)
    {
        match block.1.split_last() {
            Some((tail, head)) => {
                for stmt in head {
                    stmt.write_ir(self)?;
                }

                if let plir::Stmt::Exit(me) = tail {
                    let value = match me {
                        Some(e) => e.write_ir(self)?,
                        None => GonValue::Unit,
                    };
                    close(self, value);
                    Ok(value)
                } else {
                    tail.write_ir(self)
                }
            },
            None => Ok(GonValue::Unit), // {}
        }
    }

    fn branch_and_goto(&self, bb: BasicBlock<'ctx>) {
        self.builder.build_unconditional_branch(bb);
        self.builder.position_at_end(bb);
    }

    pub fn create_struct_value(
        &self, 
        ty: StructType<'ctx>,
        values: &[BasicValueEnum<'ctx>]
    ) -> CompileResult<StructValue<'ctx>> {
        let ptr = self.builder.build_alloca(ty, "struct");
        let result = ty.const_zero();
        self.builder.build_store(ptr, result);

        for (i, &fval) in values.iter().enumerate() {
            let field_ptr = self.builder.build_struct_gep(ptr, i as u32, "field")
                .map_err(|_| CompileErr::StructIndexOOB(i))?;
            self.builder.build_store(field_ptr, fval);
        }
        
        Ok(self.builder.build_load(ptr, "struct_load").into_struct_value())
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

/// Errors that occur during compilation to LLVM.
#[derive(Debug)]
pub enum CompileErr {
    /// Variable was not declared.
    UndefinedVar(String),
    /// Function was not declared.
    UndefinedFun(String),
    /// Wrong number of parameters were put into this function.
    WrongArity(usize /* expected */, usize /* got */),
    /// The function created was invalid.
    InvalidFun,
    /// The given PLIR type could not be resolved into a type in LLVM.
    UnresolvedType(plir::Type),
    /// The unary operator cannot be applied to this type.
    CannotUnary(op::Unary, TypeLayout),
    /// The binary operator cannot be applied between these two types.
    CannotBinary(op::Binary, TypeLayout, TypeLayout),
    /// These two types can't be compared using the given operation.
    CannotCmp(op::Cmp, TypeLayout, TypeLayout),
    /// Endpoint for LLVM (main function) could not be resolved.
    CannotDetermineMain,
    /// An error occurred within LLVM.
    LLVMErr(LLVMString),

    StructIndexOOB(usize)
}
/// A [`Result`] type for operations in compilation to LLVM.
pub type CompileResult<T> = Result<T, CompileErr>;

impl GonErr for CompileErr {
    fn err_name(&self) -> &'static str {
        match self {
            | CompileErr::UndefinedVar(_)
            | CompileErr::UndefinedFun(_)
            => "name error",

            | CompileErr::WrongArity(_, _)
            => "value error",

            | CompileErr::InvalidFun
            | CompileErr::CannotDetermineMain
            => "syntax error",

            CompileErr::UnresolvedType(_) => todo!(),
            | CompileErr::CannotUnary(_, _)
            | CompileErr::CannotBinary(_, _, _)
            | CompileErr::CannotCmp(_, _, _)
            | CompileErr::StructIndexOOB(_)
            => "type error",
            
            | CompileErr::LLVMErr(_) 
            => "llvm error",
        }
    }

    fn message(&self) -> String {
        match self {
            Self::UndefinedVar(name) => format!("could not find variable '{name}'"),
            Self::UndefinedFun(name) => format!("could not find function '{name}'"),
            Self::WrongArity(e, f) => format!("expected {e} parameters in function call, got {f}"),
            Self::InvalidFun => String::from("could not create function"),
            Self::UnresolvedType(t) => format!("'{t}' is missing an LLVM representation"),
            Self::CannotUnary(op, t1) => format!("cannot apply '{op}' to {t1:?}"),
            Self::CannotBinary(op, t1, t2) => format!("cannot apply '{op}' to {t1:?} and {t2:?}"),
            Self::CannotCmp(op, t1, t2) => format!("cannot compare '{op}' between {t1:?} and {t2:?}"),
            Self::StructIndexOOB(i) => format!("cannot index struct, does not have field {i}"),
            Self::CannotDetermineMain => String::from("could not determine entry point"),
            Self::LLVMErr(e) => format!("{e}"),
        }
    }
}

/// This trait is implemented for values that can be traversed in order to 
/// create an LLVM representation or write values into the compiler.
pub trait TraverseIR<'ctx> {
    /// The value returned in traversing.
    type Return;

    /// This function describes how the value is written into LLVM.
    fn write_ir(&self, compiler: &mut Compiler<'ctx>) -> Self::Return;
}

// impl<'ctx> TraverseIR<'ctx> for plir::Block {
//     type Return = IRResult<GonValue<'ctx>>;

//     fn write_ir(&self, compiler: &mut Compiler<'ctx>) -> Self::Return {
//         match self.1.split_last() {
//             Some((tail, head)) => {
//                 for stmt in head {
//                     stmt.write_ir(compiler)?;
//                 }
//                 tail.write_ir(compiler)
//             }
//             None => Ok(GonValue::Unit),
//         }
//     }
// }

impl<'ctx> TraverseIR<'ctx> for plir::Program {
    type Return = CompileResult<FunctionValue<'ctx>>;

    /// To create a program from a script, we must determine the given `main` endpoint.
    /// 
    /// This is how it is currently implemented:
    /// First, evaluate all of the function declarations. (TODO!: don't hoist functions?)
    /// 
    /// We can then determine the program entry point:
    /// 1. If there are any statements outside of function declarations, 
    /// then those statements are treated as a single program.
    ///     - In this case, there cannot be a function named `main`.
    /// 2. If there is only one function, then that function is the program.
    /// 3. If there are any functions named main, then that function is the program.
    fn write_ir(&self, compiler: &mut Compiler<'ctx>) -> Self::Return {
        fn is_main(cs: &CStr) -> bool {
            match cs.to_str() {
                Ok(s) => s == "main",
                Err(_) => false,
            }
        }

        // split the functions from everything else:
        let mut funs = vec![];
        let mut rest = vec![];
        
        for stmt in &self.0 {
            match stmt {
                plir::Stmt::FunDecl(dcl) => funs.push(dcl.write_ir(compiler)?),
                plir::Stmt::ExternFunDecl(dcl) => funs.push(import(compiler, dcl)?),
                stmt => rest.push(stmt)
            }
        }

        // evaluate type of program
        if rest.is_empty() {
            match *funs.as_slice() {
                // the program is an empty function
                [] => {
                    let main = compiler.module.add_function(
                        "main", 
                        compiler.ctx.void_type().fn_type(&[], false), 
                        None
                    );

                    let bb = compiler.ctx.append_basic_block(main, "body");
                    compiler.builder.position_at_end(bb);
                    compiler.builder.build_return(None);

                    if main.verify(false) {
                        Ok(main)
                    } else {
                        unreachable!("Creation of blank main function errored?");
                    }
                }

                // the program is the only function in fun_decls
                [fun] => Ok(fun),
                
                // the program is the "main" function in fun_decls
                _ => {
                    funs.into_iter().find(|f| is_main(f.get_name()))
                        .ok_or(CompileErr::CannotDetermineMain)
                }
            }
        } else {
            // the program is the anonymous statements
            if funs.iter().any(|f| is_main(f.get_name())) {
                Err(CompileErr::CannotDetermineMain)
            } else {
                let main = compiler.module.add_function(
                    "main", 
                    compiler.ctx.void_type().fn_type(&[], false), 
                    None
                );

                let bb = compiler.ctx.append_basic_block(main, "main_body");
                
                compiler.builder.position_at_end(bb);
                for stmt in rest {
                    stmt.write_ir(compiler)?;
                }
                compiler.builder.build_return(None);

                if main.verify(true) {
                    Ok(main)
                } else {
                    Err(CompileErr::InvalidFun)
                }
            }
        }
    }
}

impl<'ctx> TraverseIR<'ctx> for plir::Stmt {
    type Return = CompileResult<GonValue<'ctx>>;

    fn write_ir(&self, compiler: &mut Compiler<'ctx>) -> <Self as TraverseIR<'ctx>>::Return {
        match self {
            plir::Stmt::Decl(d) => {
                let plir::Decl { ident, val, .. } = d;
                // TODO: support rt, mt, ty

                let val = val.write_ir(compiler)?;
                compiler.alloca_and_store(ident, val);
                Ok(GonValue::Unit)
            },
            plir::Stmt::Return(me) => {
                match me {
                    Some(expr) => {
                        let e = expr.write_ir(compiler)?;
                        compiler.builder.build_return(Some(&e.basic_value(compiler)))
                    }
                    None => compiler.builder.build_return(None)
                };

                Ok(GonValue::Unit)
            },
            plir::Stmt::Exit(_) => {
                // Exits are resolved at the block level
                Ok(GonValue::Unit)
            },
            plir::Stmt::Break => todo!(),
            plir::Stmt::Continue => todo!(),
            plir::Stmt::FunDecl(d) => {
                d.write_ir(compiler)?;
                Ok(GonValue::Unit)
            },
            plir::Stmt::ExternFunDecl(fs) => {
                import(compiler, fs)?;
                Ok(GonValue::Unit)
            },
            plir::Stmt::Expr(e) => {
                e.write_ir(compiler)
            },
        }
    }
}

impl<'ctx> TraverseIR<'ctx> for plir::Expr {
    type Return = CompileResult<GonValue<'ctx>>;

    fn write_ir(&self, compiler: &mut Compiler<'ctx>) -> Self::Return {
        let plir::Expr { ty: expr_ty, expr } = self;
        let expr_layout = TypeLayout::of(expr_ty)
            .ok_or_else(|| CompileErr::UnresolvedType(expr_ty.clone()))?;

        match expr {
            plir::ExprType::Ident(ident) => compiler.get_val(ident, expr_ty),
            plir::ExprType::Block(block) => {
                // wrap in block for clarity
                let fun = compiler.parent_fn();
                let orig_bb = compiler.get_insert_block();
                let expr_bb = compiler.ctx.append_basic_block(fun, "block");
                let exit_bb = compiler.ctx.append_basic_block(fun, "post_block");

                compiler.builder.position_at_end(orig_bb);
                compiler.branch_and_goto(expr_bb);
                let bval = compiler.write_block(block, |c, _| {
                    c.builder.build_unconditional_branch(exit_bb);
                })?;

                compiler.builder.position_at_end(exit_bb);
                Ok(bval)
            },
            plir::ExprType::Literal(literal) => literal.write_ir(compiler),
            plir::ExprType::ListLiteral(_) => todo!(),
            plir::ExprType::SetLiteral(_) => todo!(),
            plir::ExprType::DictLiteral(_) => todo!(),
            plir::ExprType::Assign(target, expr) => {
                match target {
                    plir::AsgUnit::Ident(ident) => {
                        let val = expr.write_ir(compiler)?;
                        compiler.store_val(ident, val)
                            .ok_or_else(|| CompileErr::UndefinedVar(ident.clone()))?;
                        Ok(val)
                    },
                    plir::AsgUnit::Path(_)  => todo!(),
                    plir::AsgUnit::Index(_) => todo!(),
                }
            },
            plir::ExprType::Path(_) => todo!(),
            plir::ExprType::UnaryOps { ops, expr } => {
                match ops.split_last() {
                    Some((&(tail_op, _), head)) => {
                        let first = compiler.apply_unary(&**expr, tail_op)?;
                        head.iter()
                            .try_rfold(first, |e, &(op, _)| compiler.apply_unary(e, op))
                    },
                    None => expr.write_ir(compiler),
                }
            },
            plir::ExprType::BinaryOp { op, left, right } => {
                compiler.apply_binary(&**left, *op, &**right)
            },
            plir::ExprType::Comparison { left, rights } => {
                let fun = compiler.parent_fn();
                let mut lval = left.write_ir(compiler)?;
                
                match rights.split_last() {
                    Some(((last_cmp, last_rexpr), head)) => {
                        let mut incoming = vec![];
                        let post_bb = compiler.ctx.append_basic_block(fun, "post_cmp");

                        for (cmp, rexpr) in head {
                            // eval comparison
                            let rval = rexpr.write_ir(compiler)?;
                            let result = compiler.apply_cmp(lval, *cmp, rval)?;
                            
                            // branch depending on T/F
                            let then_bb = compiler.ctx.prepend_basic_block(post_bb, "cmp_true");
                            compiler.builder.build_conditional_branch(result, then_bb, post_bb);
                            
                            // update phi
                            incoming.push((result, compiler.get_insert_block()));

                            // prepare for next comparison
                            compiler.builder.position_at_end(then_bb);
                            lval = rval;
                        }
                        
                        // last block
                        let rval = last_rexpr.write_ir(compiler)?;
                        let result = compiler.apply_cmp(lval, *last_cmp, rval)?;
                        // add to phi
                        incoming.push((result, compiler.get_insert_block()));

                        // close block and go to post
                        compiler.branch_and_goto(post_bb);

                        let phi = compiler.builder.build_phi(compiler.ctx.bool_type(), "cmp_result");
                        add_incoming(phi, &incoming);

                        let result = phi.as_basic_value().into_int_value();
                        Ok(GonValue::Bool(result))
                    },
                    None => Ok(lval),
                }
            },
            plir::ExprType::Range { .. } => todo!(),
            plir::ExprType::If { conditionals, last } => {
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
                    let cmp_val = cmp.write_ir(compiler)?;
                    let cmp_val = compiler.truth(cmp_val);
                        
                    // create blocks and branch
                    let mut then_bb = compiler.ctx.prepend_basic_block(merge_bb, "then");
                    let else_bb     = compiler.ctx.prepend_basic_block(merge_bb, "else");
            
                    compiler.builder.build_conditional_branch(cmp_val, then_bb, else_bb);

                    // build then block
                    compiler.builder.position_at_end(then_bb);
                    // write ir from the block
                    compiler.write_block(block, |compiler, result| {
                        compiler.builder.build_unconditional_branch(merge_bb);
                        
                        // add block to phi
                        then_bb = compiler.builder.get_insert_block().unwrap();
                        incoming.push((result, then_bb));
                    })?;

                    prev_else.replace(else_bb);
                }

                // handle last
                let mut else_bb = prev_else.unwrap();

                // build else block
                compiler.builder.position_at_end(else_bb);

                let mut close = |compiler: &mut Compiler<'ctx>, result| {
                    compiler.builder.build_unconditional_branch(merge_bb);
    
                    //update else block
                    else_bb = compiler.builder.get_insert_block().unwrap();
                    incoming.push((result, else_bb));
                };
                match last {
                    Some(block) => { compiler.write_block(block, close)?; },
                    None => close(compiler, GonValue::Unit),
                };

                compiler.builder.position_at_end(merge_bb);

                let phi = compiler.builder.build_phi(expr_layout.basic_type(compiler), "if_result");
                compiler.add_incoming_gv(phi, &incoming);
                
                Ok(GonValue::reconstruct(expr_ty, phi.as_basic_value()))
            },
            plir::ExprType::While { condition, block } => {
                let bb = compiler.get_insert_block();
                let fun = compiler.parent_fn();

                let cond_bb = compiler.ctx.append_basic_block(fun, "while_cond");
                let loop_bb = compiler.ctx.append_basic_block(fun, "while");
                let exit_loop_bb = compiler.ctx.append_basic_block(fun, "post_while");

                // end BB by going into loop
                compiler.builder.position_at_end(bb);
                compiler.branch_and_goto(cond_bb);

                // if cond is true, go into the loop. otherwise, exit
                let condval = condition.write_ir(compiler)?;
                let cond = compiler.truth(condval);
                compiler.builder.build_conditional_branch(cond, loop_bb, exit_loop_bb);

                compiler.builder.position_at_end(loop_bb);
                compiler.write_block(block, |compiler, _| {
                    // todo, use value
                    compiler.builder.build_unconditional_branch(cond_bb);
                })?; 

                compiler.builder.position_at_end(exit_loop_bb);
                Ok(compiler.new_bool(true)) // TODO

            },
            plir::ExprType::For { .. } => todo!(),
            plir::ExprType::Call { funct, params } => {
                let fun = if let plir::ExprType::Ident(ident) = &funct.expr {
                    compiler.module.get_function(ident)
                        .ok_or_else(|| CompileErr::UndefinedFun(ident.clone()))?
                } else {
                    todo!()
                };

                let fun_ret = match &funct.ty {
                    plir::Type::Fun(_, ret) => &**ret,
                    _ => unreachable!()
                };
                
                let fun_params = fun.count_params() as usize;
                let expr_params = params.len();
                if fun_params == expr_params {
                    let resolved_params: Vec<_> = params.iter()
                        .map(|p| p.write_ir(compiler).map(|gv| gv.basic_value(compiler).into()))
                        .collect::<Result<_, _>>()?;

                    let call = compiler.builder.build_call(fun, &resolved_params, "call");
                    match call.try_as_basic_value().left() {
                        Some(basic) => Ok(GonValue::reconstruct(fun_ret, basic)),
                        None => Ok(GonValue::Unit),
                    }
                } else {
                    Err(CompileErr::WrongArity(fun_params, expr_params))
                }
            },
            plir::ExprType::Index(_) => todo!(),
            plir::ExprType::Spread(_) => todo!(),
            plir::ExprType::Split(_, _) => todo!(),
        }
    }
}

impl<'ctx> TraverseIR<'ctx> for Literal {
    type Return = CompileResult<GonValue<'ctx>>;

    fn write_ir(&self, compiler: &mut Compiler<'ctx>) -> Self::Return {
        let value = match self {
            Literal::Int(i)   => compiler.new_int(*i),
            Literal::Float(f) => compiler.new_float(*f),
            Literal::Char(_)  => todo!("char literal"),
            Literal::Str(s)   => compiler.new_str(s),
            Literal::Bool(b)  => compiler.new_bool(*b),
        };

        Ok(value)
    }
}

impl<'ctx> TraverseIR<'ctx> for plir::FunSignature {
    type Return = CompileResult<FunctionValue<'ctx>>;

    fn write_ir(&self, compiler: &mut Compiler<'ctx>) -> Self::Return {
        let plir::FunSignature { ident, params, ret } = self;

        let arg_tys: Vec<_> = params.iter()
            .map(|p| {
                let layout = TypeLayout::of(&p.ty)
                    .ok_or_else(|| CompileErr::UnresolvedType(p.ty.clone()))?;
                Ok(layout.basic_type(compiler).into())
            })
            .collect::<Result<_, _>>()?;

        let ret_ty = TypeLayout::of(ret)
            .ok_or_else(|| CompileErr::UnresolvedType(ret.clone()))?;

        let fun_ty = ret_ty.fn_type(compiler, &arg_tys, false);
        let fun = compiler.module.add_function(ident, fun_ty, None);

        // set arguments names
        for (param, arg) in iter::zip(params, fun.get_param_iter()) {
            apply_bv!(let v = arg => v.set_name(&param.ident));
        }

        Ok(fun)
    }
}

// TODO: clean up
fn import<'ctx>(compiler: &Compiler<'ctx>, sig: &plir::FunSignature) -> CompileResult<FunctionValue<'ctx>> {
    let plir::FunSignature { ident, params, ret } = sig;

    let arg_tys: Vec<_> = params.iter()
        .map(|p| {
            let layout = TypeLayout::of(&p.ty)
                .ok_or_else(|| CompileErr::UnresolvedType(p.ty.clone()))?;
            Ok(layout.basic_type(compiler).into())
        })
        .collect::<Result<_, _>>()?;

    let ret_ty = TypeLayout::of(ret)
        .ok_or_else(|| CompileErr::UnresolvedType(ret.clone()))?;

    let fun_ty = ret_ty.fn_type(compiler, &arg_tys, false);

    compiler.import_fun(ident, fun_ty)
}

impl<'ctx> TraverseIR<'ctx> for plir::FunDecl {
    type Return = CompileResult<FunctionValue<'ctx>>;

    fn write_ir(&self, compiler: &mut Compiler<'ctx>) -> Self::Return {
        let plir::FunDecl { sig, block } = self;

        let fun = sig.write_ir(compiler)?;

        let bb = compiler.ctx.append_basic_block(fun, "body");
        compiler.builder.position_at_end(bb);

        // store params
        for (param, val) in iter::zip(&sig.params, fun.get_param_iter()) {
            compiler.alloca_and_store(&param.ident, GonValue::reconstruct(&param.ty, val));
        }

        // return nothing if the return value is Unit
        compiler.write_block(block, |compiler, result| {
            let rval = result.basic_value_or_void(compiler);
            compiler.builder.build_return(rval.as_ref().map(|t| t as _));
        })?;
        
        if fun.verify(true) {
            Ok(fun)
        } else {
            println!();
            println!("=== the module ===");
            println!("{}", compiler.module.print_to_string().to_string());
            // SAFETY: Not used after.
            unsafe { fun.delete() }
            Err(CompileErr::InvalidFun)
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::Path;

    use inkwell::context::Context;

    use crate::lexer;
    use crate::parser;

    use super::Compiler;
    use super::codegen;
    use super::plir;

    fn file(input: impl AsRef<Path>) -> String {
        fs::read_to_string(input.as_ref()).unwrap()
    }
    /// Assert that a function declaration with an expression in it passes.
    /// Also prints the function to STDERR.
    fn assert_fun_pass(input: &str) {
        assert_fun_pass_vb(input, false)
    }

    fn assert_fun_pass_vb(input: &str, verbose: bool) {
        let ctx = Context::create();
        let mut compiler = Compiler::from_ctx(&ctx);

        let lexed  = lexer::tokenize(input).unwrap();
        let parsed = parser::parse(lexed).unwrap();
        let plired = codegen::codegen(parsed).unwrap();

        if verbose {
            println!("{plired}");
        }

        match &plired.0[..] {
            [plir::Stmt::FunDecl(fdcl)] => {
                let fun = compiler.compile(fdcl).unwrap();
                fun.print_to_stderr();
            }
            _ => {
                panic!("Program is not a singular function declaration");
            }
        }
    }

    fn assert_main_pass_vb(input: &str, verbose: bool) {
        let ctx = Context::create();
        let mut compiler = Compiler::from_ctx(&ctx);

        let lexed  = lexer::tokenize(input).unwrap();
        let parsed = parser::parse(lexed).unwrap();
        let plired = codegen::codegen(parsed).unwrap();

        if verbose {
            println!("{plired}");
        }

        compiler.compile(&plired).unwrap();
        for fun in compiler.module.get_functions() {
            fun.print_to_stderr();
        }
    }

    fn exec<T>(input: &str) -> T {
        let ctx = Context::create();
        let mut compiler = Compiler::from_ctx(&ctx);

        let lexed  = lexer::tokenize(input).unwrap();
        let parsed = parser::parse(lexed).unwrap();
        let plired = codegen::codegen(parsed).unwrap();
        let fun = compiler.compile(&plired).unwrap();

        unsafe { compiler.jit_run::<T>(fun).unwrap() }
    }

    #[test]
    fn mid_return() {
        assert_fun_pass_vb("fun main() -> float {
            return 2.0;
            main();
        }", true)
    }
    #[test]
    fn what_am_i_doing_2() {
        assert_fun_pass("fun hello() -> float {
            2. + 3.;
        }");

        assert_fun_pass("fun double(a: float) -> float {
            a * 2;
        }");
    }

    #[test]
    fn if_else_void_test() {
        assert_fun_pass("fun main(a: int) {
            if a {
                main(a);
            };
        }");
    
        assert_fun_pass("fun main(a: int) {
            if a {
                main(a);
            } else {
                main(a);
            };
        }");
    }

    #[test]
    fn if_with_fast_return() {
        // fast return
        assert_fun_pass("fun main(a: int) -> float {
            let b = if a {
                return 2.;
            } else {
                2. + 15.;
            };

            b;
        }");
    }

    #[test]
    fn if_else_chain_test() {
        assert_fun_pass("fun main(a: float) -> float {
            if a {
                main(0.); 
            } else {
                main(0.) + 1.;
            }
        }");

        assert_fun_pass("fun main(a: float) -> float {
            if a {
                main(0.); 
            } else if a {
                main(0.) + 1.;
            } else {
                main(0.) + 2.;
            }
        }");
        
        assert_fun_pass("fun main(a: float) -> float {
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
        assert_fun_pass("fun main(a: float) -> float {
            while a {
                main(a);
            };

            2.0;
        }");
    }

    #[test]
    fn var_test() {
        assert_fun_pass("fun main(a: float) -> float {
            a = 2.;
        }");

        assert_fun_pass("fun main() {
            let b = 2.;
        }");
    }

    #[test]
    fn log_and_log_or_test() {
        assert_fun_pass("fun main(a: bool, b: bool) -> bool {
            a && b;
        }");
        
        assert_fun_pass("fun main(a: bool, b: bool) -> bool {
            a || b;
        }");
    }

    #[test]
    fn cmp_test() {
        assert_fun_pass("fun main(a: int, b: int) -> bool {
            a < b;
        }");
        
        assert_fun_pass("fun main(a: int, b: int, c: int) -> bool {
            a < b < c;
        }");

        assert_fun_pass("fun main(a: int, b: int, c: int, d: int, e: int, f: int, g: int) -> bool {
            a < b < c < d == e < f > g;
        }");
    }

    #[test]
    fn multi_stmt_test() {
        // test multiple declarations
        assert_fun_pass("fun main() -> int {
            let a = 1;
            let b = 2;
            let c = 3;
            a + b + c;
        }");

        // block test
        assert_fun_pass("fun main() -> int {
            let b = {
                let a = 1;
                a + 2;
            };

            b;
        }");

        // multi expression
        assert_fun_pass("fun main() -> float {
            1. + 2. + 3.;
            4. + 5. + 6.;
            7. + 8. + 9.;
        }");
    }

    // #[test]
    // fn void_add() {
    //     assert_fun_pass("fun main() {
    //         main() + 1;
    //     }")
    // }
    #[test]
    fn jit_compile_test() {
        let value = exec::<f64>("
            fun double(a: float) -> float {
                a * 2;
            }
            
            fun main() -> float {
                double(15.);
            }
        ");
        println!("{}", value);

        let value = exec::<bool>("
            fun main() -> bool {
                true;
            }
        ");
        println!("{}", value);
    }

    #[test]
    fn plir_llvm_creation() {
        assert_main_pass_vb(&file("_test_files/plir_llvm_creation.gon"), true)
    }

    #[test]
    fn printing() {
        let code = "
        extern fun puts(s: string) -> int;

        fun main() {
            puts(\"Hello World!\");
            return;
        }
        ";

        assert_main_pass_vb(code, true);
        exec::<()>(code);
    }
    #[test]
    fn lexical_scope() {
        assert_main_pass_vb(&file("_test_files/lexical_scope_ll.gon"), true);
        let ctx = Context::create();
        let mut compiler = Compiler::from_ctx(&ctx);

        let lexed  = lexer::tokenize(&file("_test_files/lexical_scope_ll.gon")).unwrap();
        let parsed = parser::parse(lexed).unwrap();
        let plired = codegen::codegen(parsed).unwrap();
        let main_fun = compiler.compile(&plired).unwrap();

        unsafe { compiler.jit_run::<bool>(main_fun).unwrap(); }
    }
}