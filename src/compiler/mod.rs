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

use std::borrow::Cow;
use std::collections::HashMap;
use std::iter;

use inkwell::{OptimizationLevel, AddressSpace};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::support::LLVMString;
use inkwell::types::{StructType, BasicTypeEnum, PointerType, BasicType, BasicMetadataTypeEnum, FunctionType, VoidType};
use inkwell::values::{FunctionValue, BasicValue, PointerValue, PhiValue, BasicValueEnum, StructValue, InstructionValue};

use crate::ast::{op, Literal};
use crate::err::GonErr;

pub use self::value::*;
use self::value::apply_bv;

fn default_layouts(ctx: &Context) -> HashMap<String, BasicTypeEnum> {
    macro_rules! map {
        ($($k:expr => $v:expr),*$(,)?) => {{
            let mut m = HashMap::new();
            $(
                m.insert(String::from($k), $v.into());
            )*
            m
        }}
    }

    use plir::Type;

    map! {
        Type::S_INT   => ctx.i64_type(),
        Type::S_FLOAT => ctx.f64_type(),
        Type::S_BOOL  => ctx.bool_type(),
        Type::S_STR   => {
            let st = ctx.opaque_struct_type(Type::S_STR);
            st.set_body(&[
                ctx.i8_type().ptr_type(Default::default()).into(),
                ctx.i64_type().into()
            ], false);
            st
        },
        Type::S_VOID  => ctx.struct_type(&[], false)
    }
}

/// Computes a const layout.
/// The layouts are defined in [`default_layouts`].
/// 
/// If an unimplemented layout is accessed, this panics.
macro_rules! layout {
    ($compiler:expr, $i:ident) => {
        $compiler.get_layout_by_name($crate::compiler::plir::Type::$i).unwrap()
    }
}
pub(crate) use layout;

enum ReturnableType<'ctx> {
    Basic(BasicTypeEnum<'ctx>),
    Void(VoidType<'ctx>)
}
impl<'ctx> ReturnableType<'ctx> {
    fn fn_type(self, param_types: &[BasicMetadataTypeEnum<'ctx>], is_var_args: bool) -> FunctionType<'ctx> {
        match self {
            ReturnableType::Basic(t) => t.fn_type(param_types, is_var_args),
            ReturnableType::Void(t) => t.fn_type(param_types, is_var_args),
        }
    }
}
impl<'ctx> From<BasicTypeEnum<'ctx>> for ReturnableType<'ctx> {
    fn from(value: BasicTypeEnum<'ctx>) -> Self {
        ReturnableType::Basic(value)
    }
}
impl<'ctx> From<VoidType<'ctx>> for ReturnableType<'ctx> {
    fn from(value: VoidType<'ctx>) -> Self {
        ReturnableType::Void(value)
    }
}

/**
 * Pointers to indicate where each exit statement type should send program flow.
 * Used in [`Compiler::write_block`].
 */
#[derive(Default, Clone, Copy)]
pub struct ExitPointers<'ctx> {
    exit: Option<BasicBlock<'ctx>>,
    brk:  Option<BasicBlock<'ctx>>,
    cont: Option<BasicBlock<'ctx>>
}
impl<'ctx> ExitPointers<'ctx> {
    /**
     * Indicates this block is a bare (or conditional) block.
     * The only valid statements out of this block are through `exit`, which
     * should exit to the specified block.
     */
    pub fn bare(exit: BasicBlock<'ctx>) -> Self {
        Self { exit: Some(exit), brk: None, cont: None }
    }

    /**
     * Indicates this block is a loop block.
     * The specified blocks indicate where a `continue` or `break` statement 
     * should exit out of.
     */
    pub fn loopy(cont: BasicBlock<'ctx>, brk: BasicBlock<'ctx>) -> Self {
        Self { exit: Some(cont), brk: Some(brk), cont: Some(cont) }
    }
}

/// This struct converts from PLIR to LLVM.
pub struct Compiler<'ctx> {
    ctx: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    layouts: HashMap<String, BasicTypeEnum<'ctx>>,
    exit_pointers: Vec<ExitPointers<'ctx>>,

    vars: HashMap<String, PointerValue<'ctx>>
}

impl<'ctx> Compiler<'ctx> {
    /// Create a new Compiler, using a [`Context`] from inkwell.
    pub fn from_ctx(ctx: &'ctx Context) -> Self {
        Self {
            ctx,
            builder: ctx.create_builder(),
            module: ctx.create_module("eval"),
            layouts: default_layouts(ctx),
            exit_pointers: vec![],

            vars: HashMap::new()
        }
    }
    
    #[doc(hidden)]
    /// Obtains the LLVM module being created by the compiler.
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
    pub unsafe fn jit_run<T>(&mut self, fun: FunctionValue<'ctx>) -> CompileResult<'ctx, T> {
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
        t.write_value(self)
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

    /// Create an alloca instruction and also store the value in the allocated pointer
    fn alloca_and_store(&mut self, ident: &str, val: GonValue<'ctx>) -> CompileResult<'ctx, PointerValue<'ctx>>
    {
        let alloca = self.builder.build_alloca(self.get_layout(&self.plir_type_of(val))?, ident);
        self.vars.insert(String::from(ident), alloca);
        self.builder.build_store(alloca, self.basic_value_of(val));
        Ok(alloca)
    }

    fn get_ptr(&mut self, ident: &str) -> CompileResult<'ctx, PointerValue<'ctx>> {
        match self.vars.get(ident) {
            Some(&ptr) => Ok(ptr),
            None => Err(CompileErr::UndefinedVar(String::from(ident))),
        }
    }

    fn add_incoming_gv<'a>(&self, phi: PhiValue<'ctx>, incoming: &'a [(GonValue<'ctx>, BasicBlock<'ctx>)]) {
        let (incoming_results, incoming_blocks): (Vec<BasicValueEnum<'ctx>>, Vec<_>) = incoming.iter()
            .map(|&(a, b)| (self.basic_value_of(a), b))
            .unzip();
    
        let vec: Vec<_> = iter::zip(incoming_results.iter(), incoming_blocks)
            .map(|(a, b)| (a as _, b))
            .collect();
        
        phi.add_incoming(&vec);
    }

    fn get_exit_pointer<T>(&self, f: fn(ExitPointers<'ctx>) -> Option<T>) -> Option<T> {
        self.exit_pointers.iter().rev().copied().find_map(f)
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
    pub fn write_block<F>(&mut self, block: &plir::Block, exits: ExitPointers<'ctx>, close: F) -> CompileResult<'ctx, GonValue<'ctx>>
        where F: FnOnce(&mut Self, GonValue<'ctx>)
    {
        self.exit_pointers.push(exits);
        let (mcvalue, mjump) = match block.1.split_last() {
            Some((tail, head)) => {
                for stmt in head {
                    stmt.write_value(self)?;
                }

                match tail {
                    plir::Stmt::Exit(me) => {
                        let value = me.write_value(self)?;
                        (Some(value), self.get_exit_pointer(|ep| ep.exit))
                    },
                    plir::Stmt::Break => (None, self.get_exit_pointer(|ep| ep.brk)),
                    plir::Stmt::Continue => (None, self.get_exit_pointer(|ep| ep.cont)),
                    plir::Stmt::Return(me) => {
                        let value = me.write_value(self)?;
                        self.build_return(value);
                        (None, None)
                    }
                    _ => (Some(GonValue::Unit), None) // unreachable?
                }
            },
            None => (Some(GonValue::Unit), None), // {}
        };
        self.exit_pointers.pop();

        if let Some(jump) = mjump {
            self.builder.build_unconditional_branch(jump);
        }
        if let Some(close_value) = mcvalue {
            close(self, close_value);
        }

        Ok(mcvalue.unwrap_or(GonValue::Unit))
    }

    fn branch_and_goto(&self, bb: BasicBlock<'ctx>) {
        self.builder.build_unconditional_branch(bb);
        self.builder.position_at_end(bb);
    }

    /// Creates an opaque pointer type.
    pub fn ptr_type(&self, addr: AddressSpace) -> PointerType<'ctx> {
        self.ctx.i64_type().ptr_type(addr)
    }

    /// Initializes a new struct value 
    /// and assigns all the fields of that struct.
    pub fn create_struct_value(
        &self, 
        ty: StructType<'ctx>,
        values: &[BasicValueEnum<'ctx>]
    ) -> CompileResult<'ctx, StructValue<'ctx>> {
        let mut result = ty.const_zero();
        for (i, &fval) in values.iter().enumerate() {
            result = self.builder.build_insert_value(result, fval, i as u32, "")
                .ok_or_else(|| CompileErr::StructIndexOOB(i))?
                .try_into()
                .unwrap();
        }
        Ok(result)
    }

    /// Build a function return instruction using a GonValue.
    pub fn build_return(&self, gv: GonValue<'ctx>) -> InstructionValue<'ctx> {
        self.builder.build_return(self.returnable_value_of(gv).as_ref().map(|bv| bv as _))
    }

    /// Create a function value from the function's PLIR signature.
    fn define_fun(&self, sig: &plir::FunSignature) -> CompileResult<'ctx, (FunctionValue<'ctx>, FunctionType<'ctx>)> {
        let plir::FunSignature { ident, params, ret } = sig;

        let arg_tys: Vec<_> = params.iter()
            .map(|p| self.get_ref_layout(&p.ty).map(Into::into))
            .collect::<Result<_, _>>()?;

        let fun_ty = self.get_layout_or_void(ret)?
            .fn_type(&arg_tys, false);
        let fun = self.module.add_function(ident, fun_ty, None);

        // set arguments names
        for (param, arg) in iter::zip(params, fun.get_param_iter()) {
            apply_bv!(let v = arg => v.set_name(&param.ident));
        }

        Ok((fun, fun_ty))
    }

    /// Import a function using the provided PLIR function signature.
    fn import(&mut self, sig: &plir::FunSignature) -> CompileResult<'ctx, FunctionValue<'ctx>> {
        let (_, fun_ty) = self.define_fun(sig)?;
        self.import_fun(&sig.ident, fun_ty)
    }

    /// Define a type for the compiler to track.
    fn define_type(&mut self, ty: &str, layout: impl BasicType<'ctx>) {
        self.layouts.insert(ty.to_string(), layout.as_basic_type_enum());
    }

    /// Get the LLVM layout of a given PLIR type.
    fn get_layout(&self, ty: &plir::Type) -> CompileResult<'ctx, BasicTypeEnum<'ctx>> {
        ty.ident()
            .and_then(|ident| self.get_layout_by_name(ident))
            .ok_or_else(|| CompileErr::UnresolvedType(ty.clone()))
    }
    /// Get the LLVM layout using the layout's identifier.
    fn get_layout_by_name(&self, ident: &str) -> Option<BasicTypeEnum<'ctx>> {
        self.layouts.get(ident).copied()
    }
    /// Get the BasicTypeEnum for this PLIR type (or void if is void).
    /// 
    /// This is useful for function types returning.
    fn get_layout_or_void(&self, ty: &plir::Type) -> CompileResult<'ctx, ReturnableType<'ctx>> {
        use plir::{TypeRef, Type};

        if let TypeRef::Prim(Type::S_VOID) = ty.as_ref() {
            Ok(self.ctx.void_type()).map(Into::into)
        } else {
            self.get_layout(ty).map(Into::into)
        }
    }

    /// If this PLIR type is a copy-by-reference type, return `Ok(PointerType)` 
    /// to indicate this is holding a pointer.
    /// Otherwise, return `Err(BasicTypeEnum)` to say the conversion to pointer failed.
    /// 
    /// This should not be used, instead use [`Compiler::get_ref_layout`].
    fn try_get_ref_layout(&self, ty: &plir::Type) -> CompileResult<'ctx, Result<PointerType<'ctx>, BasicTypeEnum<'ctx>>> {
        use plir::{TypeRef, Type};
        
        self.get_layout(ty).map(|layout| {
            match (ty.as_ref(), layout) {
                (TypeRef::Prim(Type::S_VOID), _) => Err(layout),
                (_, BasicTypeEnum::StructType(_)) => Ok(self.ptr_type(Default::default())),
                _ => Err(layout)
            }
        })
    }

    /// Get the LLVM layout of a given PLIR type, keeping track of copy-by-reference.
    /// If a type is copy-by-reference, this type is LLVM `ptr`, otherwise it is its normal value.
    fn get_ref_layout(&self, ty: &plir::Type) -> CompileResult<'ctx, BasicTypeEnum<'ctx>> {
        self.try_get_ref_layout(ty)
            .map(|l| l.map_or_else(std::convert::identity, Into::into))
    }
    /// Similar to [`Expr::write_ir`], except writing in a pointer if this value is copy-by-reference.
    fn write_ref_value(&mut self, e: &plir::Expr) -> CompileResult<'ctx, BasicValueEnum<'ctx>> {
        if self.try_get_ref_layout(&e.ty)?.is_ok() {
            e.write_ptr(self).map(Into::into)
        } else {
            e.write_value(self).map(|gv| self.basic_value_of(gv))
        }
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
pub enum CompileErr<'ctx> {
    /// Variable was not declared.
    UndefinedVar(String),
    /// Function was not declared.
    UndefinedFun(String),
    /// The function created was invalid.
    InvalidFun,
    /// The given PLIR type could not be resolved into a type in LLVM.
    UnresolvedType(plir::Type),
    /// The unary operator cannot be applied to this LLVM type.
    CannotUnary(op::Unary, BasicTypeEnum<'ctx>),
    /// The binary operator cannot be applied between these two LLVM types.
    CannotBinary(op::Binary, BasicTypeEnum<'ctx>, BasicTypeEnum<'ctx>),
    /// These two LLVM types can't be compared using the given operation.
    CannotCmp(op::Cmp, BasicTypeEnum<'ctx>, BasicTypeEnum<'ctx>),
    /// Cannot perform a type cast from A to B
    CannotCast(plir::Type, plir::Type),
    /// Endpoint for LLVM (main function) could not be resolved.
    CannotDetermineMain,
    /// An error occurred within LLVM.
    LLVMErr(LLVMString),
    /// Index out of bounds access on struct occurred
    StructIndexOOB(usize),
    /// Some error occurred that there isn't a defined enum variant for
    Generic(&'static str, String)
}
/// A [`Result`] type for operations in compilation to LLVM.
pub type CompileResult<'ctx, T> = Result<T, CompileErr<'ctx>>;

impl<'ctx> GonErr for CompileErr<'ctx> {
    fn err_name(&self) -> &'static str {
        match self {
            | CompileErr::UndefinedVar(_)
            | CompileErr::UndefinedFun(_)
            => "name error",

            | CompileErr::InvalidFun
            | CompileErr::CannotDetermineMain
            => "syntax error",

            CompileErr::UnresolvedType(_)
            | CompileErr::CannotUnary(_, _)
            | CompileErr::CannotBinary(_, _, _)
            | CompileErr::CannotCmp(_, _, _)
            | CompileErr::CannotCast(_, _)
            | CompileErr::StructIndexOOB(_)
            => "llvm type error",
            
            | CompileErr::LLVMErr(_) 
            => "llvm error",

            | CompileErr::Generic(t, _)
            => t,
        }
    }

    fn message(&self) -> String {
        match self {
            CompileErr::UndefinedVar(name) => format!("could not find variable '{name}'"),
            CompileErr::UndefinedFun(name) => format!("could not find function '{name}'"),
            CompileErr::InvalidFun => String::from("could not create function"),
            CompileErr::UnresolvedType(t) => format!("missing type layout '{t}'"),
            CompileErr::CannotUnary(op, t1) => format!("cannot apply '{op}' to {t1}"),
            CompileErr::CannotBinary(op, t1, t2) => format!("cannot apply '{op}' to {t1} and {t2}"),
            CompileErr::CannotCmp(op, t1, t2) => format!("cannot compare '{op}' between {t1} and {t2}"),
            CompileErr::CannotCast(t1, t2) => format!("cannot perform type cast from '{t1}' to {t2}"),
            CompileErr::StructIndexOOB(i) => format!("cannot index struct, does not have field {i}"),
            CompileErr::CannotDetermineMain => String::from("could not determine entry point"),
            CompileErr::LLVMErr(e) => format!("{e}"),
            CompileErr::Generic(_, t) => t.clone(),
        }
    }
}

/// This trait is implemented for values that can be traversed in order to 
/// create an LLVM representation or write values into the compiler.
pub trait TraverseIR<'ctx> {
    /// The value returned in traversing.
    type Return;

    /// This function describes how the value is written into LLVM.
    fn write_value(&self, compiler: &mut Compiler<'ctx>) -> Self::Return;
}
// TODO: pub
trait TraverseIRPtr<'ctx> {
    fn write_ptr(&self, compiler: &mut Compiler<'ctx>) -> CompileResult<'ctx, PointerValue<'ctx>>;
}

impl<'ctx> TraverseIR<'ctx> for plir::Program {
    type Return = CompileResult<'ctx, FunctionValue<'ctx>>;

    /// To create a program from a script, we must determine the given `main` endpoint.
    /// 
    /// This is how it is currently implemented:
    /// First, evaluate all of the function declarations.
    /// 
    /// We can then determine the program entry point:
    /// -  If there are any functions named main, that function is the program.
    /// -  Otherwise, all unhoisted statements are collected into a function (main).
    /// - If there is both a function named main and unhoisted statements present, this will error.
    fn write_value(&self, compiler: &mut Compiler<'ctx>) -> Self::Return {
        // split the functions from everything else:
        let mut main_fun = None;
        let mut fun_bodies = vec![];
        
        let hoist_pt = self.0.partition_point(plir::Stmt::hoisted);
        let (hoisted, rest) = self.0.split_at(hoist_pt);

        for stmt in hoisted {
            match stmt {
                plir::Stmt::FunDecl(dcl) => {
                    let fv = dcl.sig.write_value(compiler)?;
                    if dcl.sig.ident == "main" {
                        main_fun.replace(fv);
                    }
                    fun_bodies.push(dcl);
                },
                plir::Stmt::ExternFunDecl(dcl) => { compiler.import(dcl)?; },
                plir::Stmt::ClassDecl(cls) => cls.write_value(compiler)?,
                _ => unreachable!()
            }
        }
        
        // delay function resolution until everything hoisted has been evaluated
        for bodies in fun_bodies {
            bodies.write_value(compiler)?;
        }

        match (main_fun, rest) {
            (Some(f), []) => Ok(f),
            (Some(_), _)  => Err(CompileErr::CannotDetermineMain),
            (None, stmts) => {
                let main = compiler.module.add_function(
                    "main", 
                    compiler.ctx.void_type().fn_type(&[], false), 
                    None
                );
                
                let bb = compiler.ctx.append_basic_block(main, "main_body");
                compiler.builder.position_at_end(bb);
                for stmt in stmts {
                    stmt.write_value(compiler)?;
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
    type Return = CompileResult<'ctx, GonValue<'ctx>>;

    fn write_value(&self, compiler: &mut Compiler<'ctx>) -> <Self as TraverseIR<'ctx>>::Return {
        match self {
            plir::Stmt::Decl(d) => {
                let plir::Decl { ident, val, .. } = d;
                // TODO: support rt, mt, ty

                let val = val.write_value(compiler)?;
                compiler.alloca_and_store(ident, val)?;
                Ok(GonValue::Unit)
            },
            plir::Stmt::Return(me) => {
                match me {
                    Some(expr) => {
                        let e = expr.write_value(compiler)?;
                        compiler.build_return(e)
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
                d.sig.write_value(compiler)?;
                d.write_value(compiler)?;
                Ok(GonValue::Unit)
            },
            plir::Stmt::ExternFunDecl(fs) => {
                compiler.import(fs)?;
                Ok(GonValue::Unit)
            },
            plir::Stmt::Expr(e) => e.write_value(compiler),
            plir::Stmt::ClassDecl(c) => {
                c.write_value(compiler)?;
                Ok(GonValue::Unit)
            },
        }
    }
}

impl<'ctx> TraverseIR<'ctx> for plir::Expr {
    type Return = CompileResult<'ctx, GonValue<'ctx>>;

    fn write_value(&self, compiler: &mut Compiler<'ctx>) -> Self::Return {
        let plir::Expr { ty: expr_ty, expr } = self;
        let expr_layout = compiler.get_layout(expr_ty)?;

        match expr {
            plir::ExprType::Ident(_) => self.write_ptr(compiler).and_then(|ptr| {
                let bv = compiler.builder.build_load(expr_layout, ptr, "");
                compiler.reconstruct(expr_ty, bv)
            }),
            plir::ExprType::Block(block) => {
                // wrap in block for clarity
                let fun = compiler.parent_fn();
                let orig_bb = compiler.get_insert_block();
                let expr_bb = compiler.ctx.append_basic_block(fun, "block");
                let exit_bb = compiler.ctx.append_basic_block(fun, "post_block");

                compiler.builder.position_at_end(orig_bb);
                compiler.branch_and_goto(expr_bb);
                let bval = compiler.write_block(block, ExitPointers::bare(exit_bb), |_, _| {})?;

                compiler.builder.position_at_end(exit_bb);
                Ok(bval)
            },
            plir::ExprType::Literal(literal) => literal.write_value(compiler),
            plir::ExprType::ListLiteral(_) => todo!(),
            plir::ExprType::SetLiteral(_) => todo!(),
            plir::ExprType::DictLiteral(_) => todo!(),
            plir::ExprType::ClassLiteral(t, entries) => {
                let layout = compiler.get_layout(t)?.into_struct_type();
                let entries: Vec<_> = entries.iter()
                    .map(|e| {
                        e.write_value(compiler)
                            .map(|gv| compiler.basic_value_of(gv))
                    })
                    .collect::<Result<_, _>>()?;

                compiler.create_struct_value(layout, &entries)
                    .and_then(|bv| compiler.reconstruct(expr_ty, bv))
            },
            plir::ExprType::Assign(target, expr) => {
                let val = expr.write_value(compiler)?;

                let ptr = match target {
                    plir::AsgUnit::Ident(ident) => compiler.get_ptr(ident)?,
                    plir::AsgUnit::Path(p)      => p.write_ptr(compiler)?,
                    plir::AsgUnit::Index(_)     => todo!(),
                };

                compiler.builder.build_store(ptr, compiler.basic_value_of(val));
                Ok(val)
            },
            plir::ExprType::Path(p) => p.write_value(compiler),
            plir::ExprType::UnaryOps { ops, expr } => {
                match ops.split_last() {
                    Some((&(tail_op, _), head)) => {
                        let first = compiler.apply_unary(&**expr, tail_op)?;
                        head.iter()
                            .try_rfold(first, |e, &(op, _)| compiler.raw_unary(e, op))
                            .and_then(|bv| compiler.reconstruct(expr_ty, bv))
                    },
                    None => expr.write_value(compiler),
                }
            },
            plir::ExprType::BinaryOp { op, left, right } => {
                compiler.apply_binary(&**left, *op, &**right)
                    .and_then(|bv| compiler.reconstruct(expr_ty, bv))
            },
            plir::ExprType::Comparison { left, rights } => {
                let fun = compiler.parent_fn();
                let mut lval = left.write_value(compiler)?;
                
                match rights.split_last() {
                    Some(((last_cmp, last_rexpr), head)) => {
                        let mut incoming = vec![];
                        let post_bb = compiler.ctx.append_basic_block(fun, "post_cmp");

                        for (cmp, rexpr) in head {
                            // eval comparison
                            let rval = rexpr.write_value(compiler)?;
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
                        let rval = last_rexpr.write_value(compiler)?;
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
                    let cmp_val = cmp.write_value(compiler)?;
                    let cmp_val = compiler.truth(cmp_val);
                        
                    // create blocks and branch
                    let mut then_bb = compiler.ctx.prepend_basic_block(merge_bb, "then");
                    let else_bb     = compiler.ctx.prepend_basic_block(merge_bb, "else");
            
                    compiler.builder.build_conditional_branch(cmp_val, then_bb, else_bb);

                    // build then block
                    compiler.builder.position_at_end(then_bb);
                    // write ir from the block
                    compiler.write_block(block, ExitPointers::bare(merge_bb), |compiler, result| {
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
                    // update else block
                    else_bb = compiler.builder.get_insert_block().unwrap();
                    incoming.push((result, else_bb));
                };
                match last {
                    Some(block) => { compiler.write_block(block, ExitPointers::bare(merge_bb), close)?; },
                    None => {
                        compiler.builder.build_unconditional_branch(merge_bb);
                        close(compiler, GonValue::Unit)
                    },
                };

                compiler.builder.position_at_end(merge_bb);

                let phi = compiler.builder.build_phi(expr_layout, "if_result");
                compiler.add_incoming_gv(phi, &incoming);
                
                compiler.reconstruct(expr_ty, phi.as_basic_value())
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
                let condval = condition.write_value(compiler)?;
                let cond = compiler.truth(condval);
                compiler.builder.build_conditional_branch(cond, loop_bb, exit_loop_bb);

                compiler.builder.position_at_end(loop_bb);
                compiler.write_block(block, ExitPointers::loopy(cond_bb, exit_loop_bb), |_, _| {})?; 

                compiler.builder.position_at_end(exit_loop_bb);
                Ok(compiler.new_bool(true)) // TODO

            },
            plir::ExprType::For { .. } => todo!(),
            plir::ExprType::Call { funct, params } => {
                let mut pvals = vec![];

                let fun_ident = match &funct.expr {
                    plir::ExprType::Ident(ident) => Cow::from(ident),
                    plir::ExprType::Path(plir::Path::Method(referent, met, _)) => {
                        let ty = referent.ty.ident().expect("expected referent type to have identifier");
                        
                        pvals.push(compiler.write_ref_value(referent)?);
                        Cow::from(format!("{ty}::{met}"))
                    },
                    _ => todo!("arbitrary expr calls")
                };

                let fun = compiler.module.get_function(&fun_ident)
                    .ok_or_else(|| CompileErr::UndefinedFun(fun_ident.into_owned()))?;

                let fun_ret = match &funct.ty {
                    plir::Type::Fun(plir::FunType(_, ret)) => &**ret,
                    _ => unreachable!()
                };
                
                for p in params {
                    pvals.push(compiler.write_ref_value(p)?);
                }
                let pvals: Vec<_> = pvals.into_iter()
                    .map(Into::into)
                    .collect();

                let call = compiler.builder.build_call(fun, &pvals, "call");
                match call.try_as_basic_value().left() {
                    Some(basic) => compiler.reconstruct(fun_ret, basic),
                    None => Ok(GonValue::Unit),
                }
            },
            plir::ExprType::Index(idx) => idx.write_value(compiler),
            plir::ExprType::Spread(_) => todo!(),
            plir::ExprType::Split(_, _) => todo!(),
            plir::ExprType::Cast(e) => {
                e.write_value(compiler)
                    .and_then(|val| compiler.cast(val, expr_ty))
            },
        }
    }
}
impl<'ctx> TraverseIRPtr<'ctx> for plir::Expr {
    fn write_ptr(&self, compiler: &mut Compiler<'ctx>) -> CompileResult<'ctx, PointerValue<'ctx>> {
        match &self.expr {
            plir::ExprType::Ident(ident) => compiler.get_ptr(ident),
            plir::ExprType::Path(p) => p.write_ptr(compiler),
            _ => {
                self.write_value(compiler)
                    .and_then(|value| compiler.alloca_and_store("", value))
            }
        }
    }
}
impl<'ctx> TraverseIR<'ctx> for Option<plir::Expr> {
    type Return = CompileResult<'ctx, GonValue<'ctx>>;

    fn write_value(&self, compiler: &mut Compiler<'ctx>) -> Self::Return {
        match self {
            Some(e) => e.write_value(compiler),
            None => Ok(GonValue::Unit),
        }
    }
}

impl<'ctx> TraverseIR<'ctx> for Literal {
    type Return = CompileResult<'ctx, GonValue<'ctx>>;

    fn write_value(&self, compiler: &mut Compiler<'ctx>) -> Self::Return {
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

impl<'ctx> TraverseIR<'ctx> for plir::Path {
    type Return = CompileResult<'ctx, GonValue<'ctx>>;

    fn write_value(&self, compiler: &mut Compiler<'ctx>) -> Self::Return {
        match self {
            plir::Path::Static(_, _) => todo!(),
            plir::Path::Struct(e, attrs) => {
                if let Some((_, last_ty)) = attrs.last() {
                    let el_ptr = self.write_ptr(compiler)?;
                    let el = compiler.builder.build_load(compiler.get_layout(last_ty)?, el_ptr, "path_load");
                    compiler.reconstruct(last_ty, el)
                } else {
                    e.write_value(compiler)
                }
            },
            plir::Path::Method(_, _, _) => panic!("Expected method to be resolved in call"),
        }
    }
}
impl<'ctx> TraverseIRPtr<'ctx> for plir::Path {
    fn write_ptr(&self, compiler: &mut Compiler<'ctx>) -> CompileResult<'ctx, PointerValue<'ctx>> {
        match self {
            plir::Path::Static(_, _) => todo!(),
            plir::Path::Struct(e, attrs) => {
                if !attrs.is_empty() {
                    let ptr = e.write_ptr(compiler)?;
                    let ty = compiler.get_layout(&e.ty)?;
                    let i32_ty = compiler.ctx.i32_type();

                    let mut indexes = vec![i32_ty.const_zero()];
                    let attr_idx = attrs.iter()
                        .map(|&(i, _)| i)
                        .map(|i| i32_ty.const_int(i as u64, false));
                    indexes.extend(attr_idx);

                    // SAFETY: After PLIR pass, this should be valid.
                    let el_ptr = unsafe {
                        compiler.builder.build_in_bounds_gep(ty, ptr, &indexes, "path_access")
                    };

                    Ok(el_ptr)
                } else {
                    e.write_ptr(compiler)
                }
            },
            plir::Path::Method(_, _, _) => panic!("Method does not have a pointer"),
        }
    }
}

impl<'ctx> TraverseIR<'ctx> for plir::Index {
    type Return = CompileResult<'ctx, GonValue<'ctx>>;

    fn write_value(&self, compiler: &mut Compiler<'ctx>) -> Self::Return {
        let Self { expr, index } = self;

        let expr = expr.write_value(compiler)?;
        let _index = index.write_value(compiler)?;

        // This should be type checked in PLIR:
        match expr {
            GonValue::Float(_)  => Err(CompileErr::Generic("type error", String::from("index wrong type (unreachable)"))),
            GonValue::Int(_)    => Err(CompileErr::Generic("type error", String::from("index wrong type (unreachable)"))),
            GonValue::Bool(_)   => Err(CompileErr::Generic("type error", String::from("index wrong type (unreachable)"))),
            GonValue::Unit      => Err(CompileErr::Generic("type error", String::from("index wrong type (unreachable)"))),
            GonValue::Struct(_) => {
                todo!()
                // // TODO: support unicode
                // let buf = compiler.builder.build_extract_value(s, 0, "buf").unwrap().into_pointer_value();
                // let len = compiler.builder.build_extract_value(s, 1, "len").unwrap().into_int_value();

                // let i8_type = compiler.ctx.i8_type();
                // let i64_type = compiler.ctx.i64_type();
                
                // // this should also be type checked in PLIR:
                // let idx = compiler.basic_value_of(index).into_int_value();

                // // bounds check
                // let lower = compiler.raw_cmp(len.get_type().const_zero(), op::Cmp::Le, idx);
                // let upper = compiler.raw_cmp(idx, op::Cmp::Lt, len);
                
                // let bounds = compiler.builder.build_and(lower, upper, "bounds_check");
                
                // let bb = compiler.get_insert_block();
                // let safe = compiler.ctx.insert_basic_block_after(bb, "safe_idx");
                // let oob = compiler.ctx.insert_basic_block_after(safe, "oob_idx");
                // let exit = compiler.ctx.insert_basic_block_after(oob, "exit_idx");

                // compiler.builder.build_conditional_branch(bounds, safe, oob);
                
                // compiler.builder.position_at_end(safe);
                // let pos = unsafe {compiler.builder.build_gep(
                //     i8_type.array_type(0), 
                //     buf, 
                //     &[i64_type.const_zero(), idx],
                //     ""
                // ) };

                // let val = compiler.builder.build_load(i8_type, pos, "").into_int_value();
                // let val = compiler.builder.build_int_cast(val, i64_type, "");
                // compiler.builder.build_unconditional_branch(exit);
                
                // compiler.builder.position_at_end(oob);
                // compiler.branch_and_goto(exit);
                
                // let phi = compiler.builder.build_phi(i64_type, "");
                // phi.add_incoming(&[
                //     (&val, safe),
                //     (&i64_type.const_zero(), oob)
                // ]);

                // // TODO: make char
                // Ok(GonValue::Int(phi.as_basic_value().into_int_value()))
            },
        }
    }
}

impl<'ctx> TraverseIR<'ctx> for plir::FunSignature {
    type Return = CompileResult<'ctx, FunctionValue<'ctx>>;

    fn write_value(&self, compiler: &mut Compiler<'ctx>) -> Self::Return {
        compiler.define_fun(self).map(|(fv, _)| fv)
    }
}

impl<'ctx> TraverseIR<'ctx> for plir::FunDecl {
    type Return = CompileResult<'ctx, FunctionValue<'ctx>>;

    fn write_value(&self, compiler: &mut Compiler<'ctx>) -> Self::Return {
        let plir::FunDecl { sig, block } = self;

        let fun = compiler.module.get_function(&sig.ident)
            .unwrap_or_else(|| panic!("function {} should be declared", sig.ident));

        let bb = compiler.ctx.append_basic_block(fun, "body");
        compiler.builder.position_at_end(bb);

        // store params
        for (param, val) in iter::zip(&sig.params, fun.get_param_iter()) {
            if let BasicValueEnum::PointerValue(ptr) = val {
                compiler.vars.insert(param.ident.clone(), ptr);
            } else {
                let gv = compiler.reconstruct(&param.ty, val)?;
                compiler.alloca_and_store(&param.ident, gv)?;
            }
        }

        // return nothing if the return value is Unit
        compiler.write_block(block, Default::default(), |_, _| {})?;
        
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

impl<'ctx> TraverseIR<'ctx> for plir::Class {
    type Return = CompileResult<'ctx, ()>;

    fn write_value(&self, compiler: &mut Compiler<'ctx>) -> Self::Return {
        let plir::Class { ident, fields } = self;

        let fields: Vec<_> = fields.values()
            .map(|fd| compiler.get_layout(&fd.ty))
            .collect::<Result<_, _>>()?;
        
        let struct_ty = compiler.ctx.opaque_struct_type(ident);
        struct_ty.set_body(&fields, false);
        compiler.define_type(ident, struct_ty);

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::test_utils::prelude::*;

    fn test_compile(t: Test) -> TestResult<()> {
        println!("=== compile {} ===", t.name);
        let plir = t.codegen()?;

        println!("{plir}");
        with_compiler(|c| {
            match c.compile(&plir) {
                Ok(_) => {
                    c.module.print_to_stderr();
                    Ok(())
                },
                Err(e) => t.wrap_test_result(Err(e)),
            }
        })
    }

    fn test_run(t: Test) -> TestResult<()> {
        println!("=== run {} ===", t.name);
        let plir = t.codegen()?;

        println!("{plir}");
        with_compiler(|c| {
            let result = c.compile(&plir)
                .and_then(|f| unsafe { c.jit_run::<()>(f) });
            
            t.wrap_test_result(result)
        })
    }

    load_tests!(CG_TESTS, "_test_files/plir_llvm/codegen.gon");
    load_tests!(EARLY_EXIT_TESTS, "_test_files/plir_llvm/early_exits.gon");
    load_tests!(TYPE_TESTS, "_test_files/plir_llvm/compiler_types.gon");

    #[test]
    fn basic_pass() -> TestResult<()> {
        CG_TESTS.pass_all(test_run, &[
            "basic_if", 
            "basic_while", 
            "basic_access", 
            "basic_arith_chain", 
            // "basic_pattern", TODO!: complete
            "basic_block", 
            "basic_extern",
            "basic_logic_cmp",
        ])
    }

    #[test]
    fn recursive_funs() -> TestResult<()> {
        CG_TESTS.pass_all(test_compile, &["fun_recursion_inf"])?;
        CG_TESTS.pass_all(test_run, &[
            "recursive_fib",
            "hoist_block"
        ])
    }

    #[test]
    #[ignore] // TODO: complete
    fn early_exit() -> TestResult<()> {
        EARLY_EXIT_TESTS.pass_all(test_run, &[
            "early_return",
            "return_void",
            "never_decl",
            "never_block",
            "never_if",
            "never_while",
            "while_ret_break"
        ])
    }


    #[test]
    fn type_test() -> TestResult<()> {
        TYPE_TESTS.pass_all(test_run, &[
            "class_chain",
            "initializer",
            "method_access",
            "decl_cast_check",
            "fun_cast_check",
            "type_res",
            "fun_call"
        ])
    }
}