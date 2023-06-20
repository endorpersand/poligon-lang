//! Converts the PLIR tree into LLVM bitcode.
//! 
//! This is done with the [`LLVMCodegen`] struct. The process
//! for using the codegen function:
//! 1. Obtain a PLIR syntax tree.
//! 2. Use [`LLVMCodegen::compile`] on the `plir::Program`.
//! 3. Obtain the module created by the program with [`LLVMCodegen::pop_module`].
//! 4. Repeat for all the programs you want to code generate.

mod value;
mod op_impl;

use std::borrow::Cow;
use std::collections::HashMap;
use std::iter;
use std::path::Path;

use inkwell::{OptimizationLevel, AddressSpace};
use inkwell::basic_block::BasicBlock;
use inkwell::context::Context;
use inkwell::module::{Module, Linkage};
use inkwell::support::LLVMString;
use inkwell::types::{BasicTypeEnum, PointerType, BasicType, FunctionType};
use inkwell::values::{FunctionValue, BasicValue, PointerValue, PhiValue, BasicValueEnum, InstructionValue, GlobalValue};

use crate::ast::{op, Literal};
use crate::err::GonErr;

pub use self::value::*;
use self::value::apply_bv;

/// Computes a const layout.
/// The layouts are defined in [`default_layouts`].
/// 
/// If an unimplemented layout is accessed, this panics.
macro_rules! layout {
    ($compiler:expr, $i:ident) => {
        $compiler.get_layout(&plir::ty!($crate::compiler::plir::Type::$i)).unwrap()
    };
    ($compiler:expr, $i:literal) => {
        $compiler.get_layout(&plir::ty!($i)).unwrap()
    };
}
pub(in crate::compiler) use layout;

macro_rules! params {
    ($($e:expr),*) => { &[$($e.into()),*] }
}
pub(in crate::compiler) use params;

/// A macro that makes the syntax for creating [`FunctionType`]s simpler.
macro_rules! fn_type {
    (($($e:expr),*) -> $r:expr) => {
        $r.fn_type(params![$($e),*], false)
    };
    ((~) -> $r:expr) => {
        $r.fn_type(params![], true)
    };
    (($($e:expr),+,~) -> $r:expr) => {
        $r.fn_type(params![$($e),+], true)
    };
}
pub(in crate::compiler) use fn_type;

use super::llvm::Builder2;
use super::llvm::types::{ReturnableType, BasicTypeEnumS};
use super::{plir, to_str};


/// Pointers to indicate where each exit statement type should send program flow.
/// Used in [`LLVMCodegen::write_block`].
#[derive(Default, Clone, Copy)]
pub struct ExitPointers<'ctx> {
    exit: Option<BasicBlock<'ctx>>,
    brk:  Option<BasicBlock<'ctx>>,
    cont: Option<BasicBlock<'ctx>>
}
impl<'ctx> ExitPointers<'ctx> {
    /// Indicates this block is a bare (or conditional) block.
    /// If this block hits an `exit` statement, it will exit through the provided
    /// block.
    pub fn bare(exit: BasicBlock<'ctx>) -> Self {
        Self { exit: Some(exit), brk: None, cont: None }
    }

    /// Indicates this block is a loop block.
    /// The specified blocks indicate where a `continue` or `break` statement 
    /// should exit out of.
    pub fn loopy(cont: BasicBlock<'ctx>, brk: BasicBlock<'ctx>) -> Self {
        Self { exit: Some(cont), brk: Some(brk), cont: Some(cont) }
    }
}

pub(super) struct TypeLayouts<V> {
    layouts: HashMap<plir::Type, V>
}
impl<V> TypeLayouts<V> {
    fn new() -> Self {
        TypeLayouts { layouts: HashMap::new() }
    }

    fn get_by_type(&self, ty: &plir::Type) -> Option<&V> {
        self.layouts.get(ty)
    }

    fn insert(&mut self, ty: plir::Type, v: V) {
        self.layouts.insert(ty, v);
    }
}

impl<'ctx> TypeLayouts<BasicTypeEnum<'ctx>> {
    fn with_builtins(ctx: &'ctx Context) -> Self {
        use plir::Type;
        
        let mut layouts = Self::new();

        for (id, layout) in [
            (Type::S_INT,   ctx.i64_type().into()),
            (Type::S_FLOAT, ctx.f64_type().into()),
            (Type::S_BOOL,  ctx.bool_type().into()),
            (Type::S_CHAR,  ctx.i32_type().into()),
            ("#ptr",        ctx.i8_type().ptr_type(Default::default()).into()),
            ("#byte",       ctx.i8_type().into()),
            (Type::S_VOID,  ctx.struct_type(&[], false).into()),
        ] {
            layouts.insert(plir::ty!(id), layout);
        }

        layouts
    }
}

/// Writes LLVM bytecode for the provided module into the provided file path.
pub(crate) fn module_to_ll(module: &Module, p: impl AsRef<Path>) -> std::io::Result<()> {
    use std::fs::File;
    use std::io::prelude::*;

    let mut file = File::create(p)?;
    file.write_all(module.print_to_string().to_bytes())
}

/// Writes LLVM bitcode for the provided module into the provided file path.
pub(crate) fn module_to_bc(module: &Module, p: impl AsRef<Path>) {
    module.write_bitcode_to_path(p.as_ref());
}

/// Executes a function as main (via JIT), and returns the error code.
/// 
/// # Safety
/// This is unsafe as it is a call to LLVM (not Rust).
pub(crate) unsafe fn module_jit_run<'ctx>(module: &Module<'ctx>, fun: FunctionValue<'ctx>) -> LLVMResult<std::ffi::c_int> {
    let jit = module.create_jit_execution_engine(OptimizationLevel::Default)
        .map_err(LLVMErr::LLVMErr)?;

    unsafe {
        Ok(jit.run_function_as_main(fun, &[]))
    }
}

/// This struct converts from PLIR to LLVM.
pub struct LLVMCodegen<'ctx> {
    pub(super) ctx: &'ctx Context,
    pub(super) builder: Builder2<'ctx>,
    pub(super) module: Module<'ctx>,
    pub(super) generic_modules: HashMap<String, Module<'ctx>>,
    pub(super) layouts: TypeLayouts<BasicTypeEnum<'ctx>>,
    pub(super) exit_pointers: Vec<ExitPointers<'ctx>>,

    pub(super) vars: HashMap<String, PointerValue<'ctx>>,
    pub(super) globals: HashMap<String, GlobalValue<'ctx>>,
    pub(super) fn_aliases: HashMap<plir::FunIdent, String>
}

impl<'ctx> LLVMCodegen<'ctx> {
    /// Create a new Compiler, using a [`Context`] from inkwell.
    pub fn new(ctx: &'ctx Context) -> Self {
        Self {
            ctx,
            builder: Builder2::new(ctx.create_builder()),
            module: ctx.create_module("eval"),
            generic_modules: Default::default(),
            layouts: TypeLayouts::with_builtins(ctx),
            exit_pointers: vec![],

            vars: HashMap::new(),
            globals: HashMap::new(),
            fn_aliases: HashMap::new()
        }
    }

    /// Executes a function as main (via JIT), and returns the error code.
    /// 
    /// # Safety
    /// This is unsafe as it is a call to LLVM (not Rust).
    pub unsafe fn jit_run(&self, fun: FunctionValue<'ctx>) -> LLVMResult<std::process::ExitCode> {
        module_jit_run(&self.module, fun).map(|t| std::process::ExitCode::from(t as u8))
    }

    /// Writes LLVM bytecode for the current module into the provided file path.
    pub fn to_ll(&self, p: impl AsRef<Path>) -> std::io::Result<()> {
        module_to_ll(&self.module, p)
    }

    /// Writes LLVM bitcode for the current module into the provided file path.
    pub fn to_bc(&self, p: impl AsRef<Path>) {
        module_to_bc(&self.module, p)
    }

    /// Compiles the given item, 
    /// registers any defined variables or functions to the compiler,
    /// and returns the value generated by the write.
    pub fn compile<T: TraverseIR<'ctx>>(&mut self, t: &T) -> T::Return {
        t.write_value(self)
    }

    /// Removes the module created by this codegen and clears all other variables.
    pub fn pop_module(&mut self) -> Module<'ctx> {
        // clear everything:
        self.builder.clear_insertion_position();
        self.layouts = TypeLayouts::with_builtins(self.ctx);
        self.exit_pointers.clear();
        self.vars.clear();
        self.globals.clear();

        // clear module:
        std::mem::replace(&mut self.module, self.ctx.create_module("eval"))
    }

    /// Registers extern funs and classes into the current module.
    pub fn load_declared_types(&mut self, dtypes: &super::DeclaredTypes) -> LLVMResult<()> {
        for class in dtypes.types.values() {
            self.compile(class)?;
        }
        
        for (ident, ty) in &dtypes.values {
            if let plir::Type::Fun(f) = ty {
                let sig = f.extern_fun_sig(ident.clone());
                self.compile(&sig)?;
            }
            // TODO: allow other types
        }

        Ok(())
    }

    /// Sets the filename of the current module.
    pub fn set_filename(&self, name: &str) {
        self.module.set_name(name)
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

    fn create_generic_module(&mut self, id: &str) {
        let mod_name = to_str!(self.module.get_name());

        self.generic_modules.entry(id.to_string())
            .or_insert_with(|| {
                self.ctx.create_module(&format!("{mod_name}::generic.{id}"))
            });
    }

    /// Create an alloca instruction that can store a value of a given [`plir::Type`].
    fn alloca(&mut self, ty: &plir::Type, ident: &str) -> LLVMResult<PointerValue<'ctx>>
    {
        let alloca = self.builder.build_alloca(self.get_layout(ty)?, ident);
        self.vars.insert(String::from(ident), alloca);
        Ok(alloca)
    }

    fn get_ptr(&mut self, ident: &str) -> Option<PointerValue<'ctx>> {
        self.vars.get(ident).copied()
    }
    fn get_ptr_or_err(&mut self, ident: &str) -> LLVMResult<PointerValue<'ctx>> {
        self.get_ptr(ident)
            .ok_or_else(|| LLVMErr::UndefinedVar(String::from(ident)))
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
    /// Exit pointers indicate where the block should branch when a statement is called.
    /// See [`ExitPointers`] for details as to how it can be defined.
    /// 
    /// ExitPointers stack. If an outer block is a loop, and the inner block is a bare block,
    /// `break` and `continue` will branch through the pointers of the outer block.
    /// 
    /// This returns the GonValue of the block and the block branched to
    /// (for `break`, `continue`, and `exit` statements).
    pub fn write_block(&mut self, block: &plir::Block, exits: ExitPointers<'ctx>) -> LLVMResult<(GonValue<'ctx>, Option<BasicBlock<'ctx>>)> {
        use plir::ProcStmt;

        self.exit_pointers.push(exits);
        let (mcvalue, mjump) = match block.1.split_last() {
            Some((tail, head)) => {
                for stmt in head {
                    stmt.write_value(self)?;
                }

                match tail {
                    ProcStmt::Exit(me) => {
                        let value = me.write_value(self)?;
                        (Some(value), self.get_exit_pointer(|ep| ep.exit))
                    },
                    ProcStmt::Break => (None, self.get_exit_pointer(|ep| ep.brk)),
                    ProcStmt::Continue => (None, self.get_exit_pointer(|ep| ep.cont)),
                    ProcStmt::Return(me) => {
                        let ty = match me {
                            Some(e) => Cow::Borrowed(&e.ty),
                            None    => Cow::Owned(plir::ty!(plir::Type::S_VOID))
                        };
                        let value = me.write_value(self)?;

                        self.build_typed_return(&ty, value);
                        (None, None)
                    }
                    ProcStmt::Throw(msg) => {
                        let _char = layout!(self, S_CHAR).into_int_type();
                        let _int  = layout!(self, S_INT).into_int_type();

                        let msg_ptr = unsafe {
                            self.builder.build_global_string(msg, "throw_msg")
                                .as_pointer_value()
                        };
                        let w_ptr = self.module.get_global("_write")
                            .unwrap_or_else(|| unsafe {
                                self.builder.build_global_string("w", "_write")}
                            )
                            .as_pointer_value();

                        let fputs = self.import_intrinsic("#fputs")?;
                        let fputwc = self.import_intrinsic("#fputwc")?;
                        let fdopen = self.import_intrinsic("#fdopen")?;
                        let exit = self.import_intrinsic("#exit")?;
                        
                        let stderr = self.builder.build_call(fdopen, params![_int.const_int(2, false), w_ptr], "stderr")
                            .try_as_basic_value()
                            .left()
                            .expect("stderr")
                            .into_pointer_value();
                        
                        self.builder.build_call(fputs, params![msg_ptr, stderr], "error_msg");
                        self.builder.build_call(fputwc, params![_char.const_int('\n' as _, false), stderr], "");
                        self.builder.build_call(exit, params![_int.const_int(1, false)], "");
                        self.builder.build_unreachable();
                        (None, None)
                    },
                    _ => unreachable!("block should have ended with exit statement")
                }
            },
            None => unreachable!("block had no statements")
        };
        self.exit_pointers.pop();

        if let Some(jump) = mjump {
            self.builder.build_unconditional_branch(jump);
        }

        Ok((mcvalue.unwrap_or(GonValue::Unit), mjump))
    }

    /// Creates an opaque pointer type.
    pub fn ptr_type(&self, addr: AddressSpace) -> PointerType<'ctx> {
        self.ctx.i64_type().ptr_type(addr)
    }

    /// Build a function return instruction using a GonValue.
    pub fn build_typed_return(&self, ty: &plir::Type, gv: GonValue<'ctx>) -> InstructionValue<'ctx> {
        use plir::TypeRef::Prim;
        use Cow::Borrowed;

        match (ty.downgrade(), gv) {
            (Prim(Borrowed(plir::Type::S_VOID)), _) => self.builder.build_return(None),
            (_, GonValue::Unit)                     => self.builder.build_return(None),
            (_, GonValue::Basic(t))                 => self.builder.build_return(Some(&t))
        }
    }

    /// Create a function value from the function's PLIR signature.
    fn define_fun(&mut self, sig: &plir::FunSignature) -> LLVMResult<(FunctionValue<'ctx>, FunctionType<'ctx>)> {
        let plir::FunSignature { private, ident, params, ret, varargs } = sig;

        let fun = match self.get_fn_by_plir_ident(ident) {
            Some(f) => f,
            None => {
                let arg_tys: Vec<_> = params.iter()
                    .map(|p| self.get_ref_layout(&p.ty).map(Into::into))
                    .collect::<Result<_, _>>()?;
        
                let fun_ty = self.get_layout_or_void(ret)?
                    .fn_type(&arg_tys, *varargs);
                let linkage = match private {
                    true  => Some(Linkage::Private),
                    false => None,
                };
                let fun = self.module.add_function(&ident.as_llvm_ident(), fun_ty, linkage);
                let fun_name = to_str!(fun.get_name())
                    .to_string();
                self.fn_aliases.insert(ident.clone(), fun_name);

                // set arguments names
                for (param, arg) in iter::zip(params, fun.get_param_iter()) {
                    apply_bv!(let v = arg => v.set_name(&param.ident));
                }

                fun
            }
        };

        Ok((fun, fun.get_type()))
    }

    /// Import a function using the provided PLIR function signature.
    fn import(&mut self, sig: &plir::FunSignature) -> LLVMResult<FunctionValue<'ctx>> {
        // TODO: type check?
        let intrinsic = self.import_intrinsic(&sig.ident.as_llvm_ident())?;

        let llvm_name = to_str!(intrinsic.get_name())
            .to_string();
        self.fn_aliases.insert(sig.ident.clone(), llvm_name);
        Ok(intrinsic)
    }

    fn get_fn_by_plir_ident(&self, plir_ident: &plir::FunIdent) -> Option<FunctionValue<'ctx>> {
        match self.fn_aliases.get(plir_ident) {
            Some(id) => self.module.get_function(id),
            None     => self.module.get_function(&plir_ident.as_llvm_ident()),
        }
    }

    /// Define a type for the compiler to track.
    fn define_type(&mut self, ty: plir::Type, layout: impl BasicType<'ctx>) {
        self.layouts.insert(ty, layout.as_basic_type_enum());
    }

    /// Get the LLVM layout of a given PLIR type.
    pub(in crate::compiler) fn get_layout(&self, ty: &plir::Type) -> LLVMResult<BasicTypeEnum<'ctx>> {
        use plir::TypeRef::*;
        use Cow::Borrowed;

        if let Generic(Borrowed("#ll_array"), Borrowed([t]), ()) = ty.downgrade() {
            Ok(self.get_layout(t)?.array_type(0).into())
        } else {
            self.layouts.get_by_type(ty)
                .copied()
                .ok_or_else(|| LLVMErr::UnresolvedType(ty.clone()))
        }
    }
    
    /// Get the BasicTypeEnum for this PLIR type (or void if is void).
    /// 
    /// This is useful for function types returning.
    fn get_layout_or_void(&self, ty: &plir::Type) -> LLVMResult<ReturnableType<'ctx>> {
        use plir::{TypeRef::*, Type};
        use Cow::Borrowed;

        if let Prim(Borrowed(Type::S_VOID)) = ty.downgrade() {
            Ok(self.ctx.void_type()).map(Into::into)
        } else {
            self.get_layout(ty).map(Into::into)
        }
    }

    /// Determines whether this type is copy-by-reference through functions.
    /// Errors if type does not have an LLVM representation.
    fn is_ref_layout(&self, ty: &plir::Type) -> LLVMResult<bool> {
        use plir::{Type, TypeRef::*};
        use Cow::Borrowed;

        let layout = self.get_layout(ty)?;
        
        let cond = match (ty.downgrade(), layout) {
            (Prim(Borrowed(Type::S_VOID)), _) => false,
            (_, BasicTypeEnum::StructType(_)) => true,
            _ => false
        };
        Ok(cond)
    }

    /// Get the LLVM layout of a given PLIR type, keeping track of copy-by-reference.
    /// If a type is copy-by-reference, this type is LLVM `ptr`, otherwise it is its normal value.
    fn get_ref_layout(&self, ty: &plir::Type) -> LLVMResult<BasicTypeEnum<'ctx>> {
        if self.is_ref_layout(ty)? {
            Ok(self.ptr_type(Default::default()).into())
        } else {
            self.get_layout(ty)
        }
    }

    /// Similar to [`Expr::write_ir`], except writing in a pointer if this value is copy-by-reference.
    fn write_ref_value(&mut self, e: &plir::Expr) -> LLVMResult<BasicValueEnum<'ctx>> {
        if self.is_ref_layout(&e.ty)? {
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
pub enum LLVMErr {
    /// Variable was not declared.
    UndefinedVar(String),
    /// Function was not declared.
    UndefinedFun(plir::FunIdent),
    /// Imported object does not exist.
    CannotImport(String),
    /// The function created was invalid.
    InvalidFun,
    /// The given PLIR type could not be resolved into a type in LLVM.
    UnresolvedType(plir::Type),
    /// The unary operator cannot be applied to this LLVM type.
    CannotUnary(op::Unary, BasicTypeEnumS),
    /// The binary operator cannot be applied between these two LLVM types.
    CannotBinary(op::Binary, BasicTypeEnumS, BasicTypeEnumS),
    /// These two LLVM types can't be compared using the given operation.
    CannotCmp(op::Cmp, BasicTypeEnumS, BasicTypeEnumS),
    /// Cannot perform a type cast from A to B
    CannotCast(plir::Type /* from */, plir::Type /* to */),
    /// Endpoint for LLVM (main function) could not be resolved.
    CannotDetermineMain,
    /// Endpoint for LLVM (main function) had invalid parameters.
    InvalidMain,
    /// An error occurred within LLVM.
    LLVMErr(LLVMString),
    /// Index out of bounds access on struct occurred
    StructIndexOOB(usize),
    /// Some error occurred that there isn't a defined enum variant for
    Generic(&'static str, String)
}
/// A [`Result`] type for operations in compilation to LLVM.
pub type LLVMResult<T> = Result<T, LLVMErr>;

impl GonErr for LLVMErr {
    fn err_name(&self) -> &'static str {
        match self {
            | LLVMErr::UndefinedVar(_)
            | LLVMErr::UndefinedFun(_)
            | LLVMErr::CannotImport(_)
            => "name error",

            | LLVMErr::InvalidFun
            | LLVMErr::CannotDetermineMain
            | LLVMErr::InvalidMain
            => "syntax error",

            LLVMErr::UnresolvedType(_)
            | LLVMErr::CannotUnary(_, _)
            | LLVMErr::CannotBinary(_, _, _)
            | LLVMErr::CannotCmp(_, _, _)
            | LLVMErr::CannotCast(_, _)
            | LLVMErr::StructIndexOOB(_)
            => "llvm type error",
            
            | LLVMErr::LLVMErr(_) 
            => "llvm error",

            | LLVMErr::Generic(t, _)
            => t,
        }
    }
}

impl std::fmt::Display for LLVMErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LLVMErr::UndefinedVar(name)       => write!(f, "could not find variable '{name}'"),
            LLVMErr::UndefinedFun(name)       => write!(f, "could not find function '{name}'"),
            LLVMErr::CannotImport(name)       => write!(f, "cannot import '{name}'"),
            LLVMErr::InvalidFun               => write!(f, "could not create function"),
            LLVMErr::UnresolvedType(t)        => write!(f, "missing type layout '{t}'"),
            LLVMErr::CannotUnary(op, t1)      => write!(f, "cannot apply '{op}' to {t1}"),
            LLVMErr::CannotBinary(op, t1, t2) => write!(f, "cannot apply '{op}' to {t1} and {t2}"),
            LLVMErr::CannotCmp(op, t1, t2)    => write!(f, "cannot compare '{op}' between {t1} and {t2}"),
            LLVMErr::CannotCast(t1, t2)       => write!(f, "cannot perform type cast from {t1} to {t2}"),
            LLVMErr::StructIndexOOB(i)        => write!(f, "cannot index struct, does not have field {i}"),
            LLVMErr::CannotDetermineMain      => write!(f, "could not determine entry point"),
            LLVMErr::InvalidMain              => write!(f, "expected main to be of type {}", plir::ty!(() -> plir::ty!(plir::Type::S_VOID))),
            LLVMErr::LLVMErr(e)               => write!(f, "{e}"),
            LLVMErr::Generic(_, t)            => write!(f, "{t}"),
        }
    }
}
impl std::error::Error for LLVMErr {}

/// This trait is implemented for values that can be traversed in order to 
/// create an LLVM representation or write values into the compiler.
pub trait TraverseIR<'ctx> {
    /// The value returned in traversing.
    type Return;

    /// This function describes how the value is written into LLVM.
    fn write_value(&self, compiler: &mut LLVMCodegen<'ctx>) -> Self::Return;
}
// TODO: pub
trait TraverseIRPtr<'ctx> {
    fn write_ptr(&self, compiler: &mut LLVMCodegen<'ctx>) -> LLVMResult<PointerValue<'ctx>>;
}

impl<'ctx> TraverseIR<'ctx> for plir::Program {
    type Return = LLVMResult<FunctionValue<'ctx>>;

    /// To create a program from a script, we must determine the given `main` endpoint.
    /// 
    /// This is how it is currently implemented:
    /// First, evaluate all of the function declarations.
    /// 
    /// We can then determine the program entry point:
    /// -  If there are any functions named main, that function is the program.
    /// -  Otherwise, all unhoisted statements are collected into a function (main).
    /// - If there is both a function named main and unhoisted statements present, this will error.
    fn write_value(&self, compiler: &mut LLVMCodegen<'ctx>) -> Self::Return {
        use plir::{HoistedStmt, FunIdent};

        let _void = compiler.ctx.void_type();
        let _i8 = compiler.ctx.i8_type();

        // split the functions from everything else:
        let mut main_fun = None;
        let mut fun_bodies = vec![];
        let mut globals = vec![];
        let plir::Program(hoisted, proc) = &self;

        for stmt in hoisted {
            match stmt {
                HoistedStmt::FunDecl(dcl) => {
                    let fv = dcl.sig.write_value(compiler)?;
                    match &dcl.sig.ident {
                        FunIdent::Simple(s) if s == "main" => {
                            main_fun.replace(fv);
                        },
                        _ => {}
                    }
                    fun_bodies.push(dcl);
                },
                HoistedStmt::ExternFunDecl(dcl) => {
                    compiler.import(dcl)?;
                },
                HoistedStmt::ClassDecl(cls) => {
                    cls.write_value(compiler)?;
                },
                HoistedStmt::IGlobal(id, value) => {
                    globals.push((id, value));
                },
            }
        }

        let (main, mstmts) = match (main_fun, proc.as_slice()) {
            (Some(f), []) => (f, None),
            (Some(_), _)  => Err(LLVMErr::CannotDetermineMain)?,
            (None, stmts) => {
                let main_fn = compiler.module.add_function(
                    "main", 
                    fn_type![() -> _void],
                    None
                );
                (main_fn, Some(stmts))
            }
        };
        
        let global_bb = compiler.ctx.append_basic_block(main, "globals");
        compiler.builder.position_at_end(global_bb);

        // load globals before loading fun bodies
        // globals have to be loaded within a function, so we're doing it in main
        for (id, value) in globals {
            let global = unsafe {
                compiler.builder.build_global_string(value, id)
            };
            compiler.globals.insert(id.to_string(), global);
        }

        // SAFETY: this basic block was just created
        // and therefore cannot have been referenced.
        unsafe { global_bb.delete().unwrap(); }

        // this is delayed until after all types are resolved
        for bodies in fun_bodies {
            bodies.write_value(compiler)?;
        }

        // resolve main if it hasn't been resolved yet:
        if let Some(stmts) = mstmts {
            let main_bb = compiler.ctx.append_basic_block(main, "main_body");
            compiler.builder.position_at_end(main_bb);
            for stmt in stmts {
                stmt.write_value(compiler)?;
            }
            compiler.builder.build_return(None);
        }

        // fix main
        // 1. currently, main is () -> void. we need it to be () -> #byte.
        // 2. a locale needs to be registered so that wchar_t functions properly

        main.as_global_value().set_name("main.inner");
        main.set_linkage(Linkage::Private);

        let inner_main = main;
        let main = compiler.module.add_function(
            "main", 
            _i8.fn_type(&[], false),
            None
        );

        let bb = compiler.ctx.append_basic_block(main, "body");
        compiler.builder.position_at_end(bb);
        
        // register default locale
        let setlocale = compiler.import_intrinsic("#setlocale")?;
        let _int = compiler.ctx.i64_type();
        let template = unsafe { compiler.builder.build_global_string("en_US.UTF-8\0", "locale")};
        compiler.builder.build_call(setlocale, params![_int.const_zero(), template.as_pointer_value()], "");

        // wrap inner main
        compiler.builder.build_call(inner_main, &[], "");
        compiler.builder.build_return(Some(&_i8.const_zero()));

        match compiler.module.verify() {
            Ok(_)  => Ok(main),
            Err(s) => Err(LLVMErr::LLVMErr(s)),
        }
    }
}

impl<'ctx> TraverseIR<'ctx> for plir::ProcStmt {
    type Return = LLVMResult<GonValue<'ctx>>;

    fn write_value(&self, compiler: &mut LLVMCodegen<'ctx>) -> <Self as TraverseIR<'ctx>>::Return {
        use plir::ProcStmt;

        match self {
            ProcStmt::Decl(d) => {
                let plir::Decl { ident, val, ty, .. } = d;
                // TODO: support rt, mt

                let ptr = compiler.alloca(ty, ident)?;
                let val = val.write_value(compiler)?;
                compiler.builder.build_store(ptr, compiler.basic_value_of(val));
                Ok(GonValue::Unit)
            },
            ProcStmt::Return(_) => unreachable!("return should be resolved at block level"),
            ProcStmt::Exit(_)   => unreachable!("exit should be resolved at block level"),
            ProcStmt::Throw(_)  => unreachable!("throw should be resolved at block level"),
            ProcStmt::Break     => unreachable!("break should be resolved at block level"),
            ProcStmt::Continue  => unreachable!("continue should be resolved at block level"),
            ProcStmt::Expr(e) => e.write_value(compiler),
        }
    }
}

impl<'ctx> TraverseIR<'ctx> for plir::Expr {
    type Return = LLVMResult<GonValue<'ctx>>;

    fn write_value(&self, compiler: &mut LLVMCodegen<'ctx>) -> Self::Return {
        let plir::Expr { ty: expr_ty, expr } = self;
        let expr_layout = compiler.get_layout(expr_ty)?;

        match expr {
            plir::ExprType::Ident(id) => {
                match compiler.get_ptr(id) {
                    Some(ptr) => {
                        let bv = compiler.builder.build_load(expr_layout, ptr, "");
                        bv.set_name(id);
                        Ok(GonValue::Basic(bv))
                    },
                    None => {
                        // check for global
                        let &global = compiler.globals.get(id)
                            .ok_or_else(|| LLVMErr::UndefinedVar(id.clone()))?;

                        Ok(GonValue::Basic(global.as_pointer_value().into()))
                    }
                }
            },
            plir::ExprType::Block(block) => {
                // wrap in block for clarity
                let fun = compiler.parent_fn();
                let orig_bb = compiler.get_insert_block();
                let expr_bb = compiler.ctx.append_basic_block(fun, "block");
                let exit_bb = compiler.ctx.append_basic_block(fun, "post_block");

                compiler.builder.position_at_end(orig_bb);
                compiler.builder.branch_and_goto(expr_bb);
                let (bval, _) = compiler.write_block(block, ExitPointers::bare(exit_bb))?;

                compiler.builder.position_at_end(exit_bb);
                Ok(bval)
            },
            plir::ExprType::Literal(literal) => literal.write_value(compiler),
            plir::ExprType::ListLiteral(exprs) => {
                let plir::TypeRef::Generic(Cow::Borrowed(plir::Type::S_LIST), Cow::Borrowed([t]), ()) = expr_ty.downgrade() else {
                    panic!("expected list literal to return list, but actually returned {expr_ty}")
                };

                let _int = layout!(compiler, S_INT).into_int_type();
                let arr_ty = compiler.get_layout(t)?.array_type(exprs.len() as _);
                let elements: Vec<_> = exprs.iter()
                .map(|e| {
                    e.write_value(compiler)
                        .map(|gv| compiler.basic_value_of(gv))
                })
                .collect::<Result<_, _>>()?;

                let arr = compiler.builder.create_agg_value(arr_ty, &elements)?;
                let alloca = compiler.builder.build_alloca(arr_ty, "");
                compiler.builder.build_store(alloca, arr);

                let id = plir::FunIdent::new_static(expr_ty, "from_raw");
                let list_from_raw = compiler.get_fn_by_plir_ident(&id)
                    .ok_or_else(|| LLVMErr::UndefinedFun(id))?;

                let lst = compiler.builder.build_call(list_from_raw, params![alloca, _int.const_int(exprs.len() as _, false)], "list_literal")
                    .try_as_basic_value()
                    .left()
                    .unwrap();

                Ok(GonValue::Basic(lst))
            },
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

                compiler.builder.create_agg_value(layout, &entries)
                    .map(|bv| GonValue::Basic(bv.into()))
            },
            plir::ExprType::Assign(target, expr) => {
                let val = expr.write_value(compiler)?;

                let ptr = match target {
                    plir::AsgUnit::Ident(ident) => compiler.get_ptr_or_err(ident)?,
                    plir::AsgUnit::Path(p)      => p.write_ptr(compiler)?,
                    plir::AsgUnit::Index(_)     => todo!(),
                    plir::AsgUnit::Deref(d)     => d.write_ptr(compiler)?,
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
                            .try_rfold(first, |e, &(op, _)| compiler.apply_unary(e, op))
                            .map(GonValue::Basic)
                    },
                    None => expr.write_value(compiler),
                }
            },
            plir::ExprType::BinaryOp { op, left, right } => {
                compiler.apply_binary(&**left, *op, &**right)
                    .map(GonValue::Basic)
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
                        compiler.builder.branch_and_goto(post_bb);

                        let phi = compiler.builder.build_phi(compiler.ctx.bool_type(), "cmp_result");
                        add_incoming(phi, &incoming);

                        Ok(GonValue::Basic(phi.as_basic_value()))
                    },
                    None => Ok(lval),
                }
            },
            plir::ExprType::Range { left, right, step } => {
                let left = compiler.write_ref_value(left)?;
                let right = compiler.write_ref_value(right)?;
                
                match step.as_deref() {
                    Some(_) => todo!("deal with step"),
                    None => {
                        let id = plir::FunIdent::new_static(expr_ty, "new");
                        let range_new = compiler.get_fn_by_plir_ident(&id)
                            .unwrap_or_else(|| panic!("missing function {id}"));
                        let call = compiler.builder.build_call(range_new, params![left, right], "")
                            .try_as_basic_value()
                            .left()
                            .unwrap();
                        Ok(GonValue::Basic(call))
                    },
                }
            },
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
                    let else_bb = compiler.ctx.prepend_basic_block(merge_bb, "else");
            
                    compiler.builder.build_conditional_branch(cmp_val, then_bb, else_bb);

                    // build then block
                    compiler.builder.position_at_end(then_bb);
                    // write ir from the block
                    let (result, out_bb) = compiler.write_block(block, ExitPointers::bare(merge_bb))?;
                    // add block to phi if branches to merge
                    if out_bb == Some(merge_bb) {
                        then_bb = compiler.get_insert_block();
                        incoming.push((result, then_bb));
                    }
                    prev_else.replace(else_bb);
                }

                // handle last
                let mut else_bb = prev_else.unwrap();

                // build else block
                compiler.builder.position_at_end(else_bb);

                let (result, out_bb) = match last {
                    Some(block) => compiler.write_block(block, ExitPointers::bare(merge_bb))?,
                    None => {
                        compiler.builder.build_unconditional_branch(merge_bb);
                        (GonValue::Unit, Some(merge_bb))
                    },
                };
                if out_bb == Some(merge_bb) {
                    else_bb = compiler.get_insert_block();
                    incoming.push((result, else_bb));
                }

                compiler.builder.position_at_end(merge_bb);

                let phi = compiler.builder.build_phi(expr_layout, "if_result");
                compiler.add_incoming_gv(phi, &incoming);
                
                Ok(GonValue::Basic(phi.as_basic_value()))
            },
            plir::ExprType::While { condition, block } => {
                let bb = compiler.get_insert_block();
                let fun = compiler.parent_fn();

                let cond_bb = compiler.ctx.append_basic_block(fun, "while_cond");
                let loop_bb = compiler.ctx.append_basic_block(fun, "while");
                let exit_loop_bb = compiler.ctx.append_basic_block(fun, "post_while");

                // end BB by going into loop
                compiler.builder.position_at_end(bb);
                compiler.builder.branch_and_goto(cond_bb);

                // if cond is true, go into the loop. otherwise, exit
                let condval = condition.write_value(compiler)?;
                let cond = compiler.truth(condval);
                compiler.builder.build_conditional_branch(cond, loop_bb, exit_loop_bb);

                compiler.builder.position_at_end(loop_bb);
                compiler.write_block(block, ExitPointers::loopy(cond_bb, exit_loop_bb))?; 

                compiler.builder.position_at_end(exit_loop_bb);
                Ok(GonValue::Unit)

            },
            plir::ExprType::For { ident, element_type, iterator, block } => {
                // FIXME: cleanup
                let bb = compiler.get_insert_block();
                let fun = compiler.parent_fn();

                let cond_bb = compiler.ctx.append_basic_block(fun, "for_cond");
                let loop_bb = compiler.ctx.append_basic_block(fun, "for");
                let exit_loop_bb = compiler.ctx.append_basic_block(fun, "post_for");
                
                // iteration stuff:
                let i_ptr = compiler.alloca(element_type, ident)?;
                let it_id = plir::FunIdent::new_static(&iterator.ty, "next");
                let it_next = compiler.get_fn_by_plir_ident(&it_id)
                    .unwrap_or_else(|| unimplemented!("nonexistent ::next should have been detected by PLIR"));
                let iterator = compiler.write_ref_value(iterator)?;

                // end BB by going into loop
                compiler.builder.position_at_end(bb);
                compiler.builder.branch_and_goto(cond_bb);

                let mvalue = compiler.builder.build_call(it_next, params![iterator], "")
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_struct_value();
                let present = compiler.builder.build_extract_value(mvalue, 0, "present")
                    .unwrap()
                    .into_int_value();
                compiler.builder.build_conditional_branch(present, loop_bb, exit_loop_bb);

                compiler.builder.position_at_end(loop_bb);
                let value_ptr = compiler.builder.build_extract_value(mvalue, 1, "")
                    .unwrap()
                    .into_pointer_value();
                let value = compiler.builder.build_load(compiler.get_layout(element_type)?, value_ptr, "");
                compiler.builder.build_store(i_ptr, value);
                compiler.write_block(block, ExitPointers::loopy(cond_bb, exit_loop_bb))?; 

                compiler.builder.position_at_end(exit_loop_bb);
                Ok(GonValue::Unit)
            },
            plir::ExprType::Call { funct, params } => {
                let mut pvals = vec![];

                let fun_ident = match &funct.expr {
                    plir::ExprType::Ident(ident) => plir::FunIdent::new_simple(ident),
                    plir::ExprType::Path(p) => match p {
                        plir::Path::Static(ty, met, _) => {
                            plir::FunIdent::new_static(ty, met)
                        },
                        plir::Path::Struct(_, _) => unreachable!("struct attr cannot be fun"),
                        plir::Path::Method(referent, met, _) => {
                            pvals.push(compiler.write_ref_value(referent)?);
                            plir::FunIdent::new_static(&referent.ty, met)
                        },
                    },
                    e => todo!("arbitrary expr calls: {e:?}")
                };

                let fun = compiler.get_fn_by_plir_ident(&fun_ident)
                    .ok_or_else(|| LLVMErr::UndefinedFun(fun_ident))?;

                for p in params {
                    pvals.push(compiler.write_ref_value(p)?);
                }
                let pvals: Vec<_> = pvals.into_iter()
                    .map(Into::into)
                    .collect();

                let call = compiler.builder.build_call(fun, &pvals, "");
                Ok(call.try_as_basic_value().left().into())
            },
            plir::ExprType::Index(idx) => idx.write_value(compiler),
            plir::ExprType::Spread(_) => todo!(),
            plir::ExprType::Split(_, _) => todo!(),
            plir::ExprType::Cast(e) => {
                let src_ty = &e.ty;
                let dest_ty = &expr_ty;
                e.write_value(compiler)
                    .and_then(|val| compiler.cast(val, src_ty, dest_ty))
            },
            plir::ExprType::Deref(d) => d.write_value(compiler),
            plir::ExprType::GEP(ty, ptr_expr, params) => {
                let params: Vec<_> = params.iter()
                    .map(|e| {
                        let value = e.write_value(compiler)?;
                        Ok(compiler.basic_value_of(value).into_int_value())
                    })
                    .collect::<Result<_, _>>()?;
                let layout = compiler.get_layout(ty)?;
                
                let ptr_gv = ptr_expr.write_value(compiler)?;
                let ptr = compiler.basic_value_of(ptr_gv)
                    .into_pointer_value();

                let ptr_name = to_str!(ptr.get_name());
                let gep_ptr = unsafe {
                    compiler.builder.build_gep(layout, ptr, &params, &format!("{ptr_name}.gep"))
                };

                Ok(GonValue::Basic(gep_ptr.into()))
            },
            plir::ExprType::Alloca(ty) => {
                let layout = compiler.get_layout(ty)?;

                let mut ty_ident = &*ty.llvm_ident();
                if let Some(id) = ty_ident.strip_prefix('#') {
                    ty_ident = id;
                };

                let ptr = compiler.builder.build_alloca(layout, &format!("alloca.{ty_ident}"));
                Ok(GonValue::Basic(ptr.into()))
            }
            plir::ExprType::SizeOf(ty) => {
                let layout = compiler.get_layout(ty)?;
                let size = match layout {
                    BasicTypeEnum::ArrayType(t)   => t.size_of().expect("expected array type size"),
                    BasicTypeEnum::FloatType(t)   => t.size_of(),
                    BasicTypeEnum::IntType(t)     => t.size_of(),
                    BasicTypeEnum::PointerType(t) => t.size_of(),
                    BasicTypeEnum::StructType(t)  => t.size_of().expect("expected struct type size"),
                    BasicTypeEnum::VectorType(t)  => t.size_of().expect("expected vector type size"),
                };
                let _int = layout!(compiler, S_INT).into_int_type();

                Ok(GonValue::Basic(size.into()))
            },
        }
    }
}
impl<'ctx> TraverseIRPtr<'ctx> for plir::Expr {
    fn write_ptr(&self, compiler: &mut LLVMCodegen<'ctx>) -> LLVMResult<PointerValue<'ctx>> {
        match &self.expr {
            plir::ExprType::Ident(ident) => compiler.get_ptr_or_err(ident),
            plir::ExprType::Path(p) => p.write_ptr(compiler),
            plir::ExprType::Deref(d) => d.write_ptr(compiler),
            _ => {
                self.write_value(compiler)
                    .and_then(|value| {
                        let ptr = compiler.alloca(&self.ty, "")?;
                        compiler.builder.build_store(ptr, compiler.basic_value_of(value));
                        Ok(ptr)
                    })
            }
        }
    }
}
impl<'ctx> TraverseIR<'ctx> for Option<plir::Expr> {
    type Return = LLVMResult<GonValue<'ctx>>;

    fn write_value(&self, compiler: &mut LLVMCodegen<'ctx>) -> Self::Return {
        match self {
            Some(e) => e.write_value(compiler),
            None => Ok(GonValue::Unit),
        }
    }
}

impl<'ctx> TraverseIR<'ctx> for Literal {
    type Return = LLVMResult<GonValue<'ctx>>;

    fn write_value(&self, compiler: &mut LLVMCodegen<'ctx>) -> Self::Return {
        let value = match *self {
            Literal::Int(i)     => compiler.new_int(i),
            Literal::Float(f)   => compiler.new_float(f),
            Literal::Char(c)    => compiler.new_char(c),
            Literal::Str(ref s) => compiler.new_str(s),
            Literal::Bool(b)    => compiler.new_bool(b),
        };

        Ok(value)
    }
}

impl<'ctx> TraverseIR<'ctx> for plir::Path {
    type Return = LLVMResult<GonValue<'ctx>>;

    fn write_value(&self, compiler: &mut LLVMCodegen<'ctx>) -> Self::Return {
        match self {
            plir::Path::Static(_, _, _) => todo!(),
            plir::Path::Struct(e, attrs) => {
                if let Some((_, last_ty)) = attrs.last() {
                    let el_ptr = self.write_ptr(compiler)?;
                    let el = compiler.builder.build_load(compiler.get_layout(last_ty)?, el_ptr, &format!("{}.load", el_ptr.get_name().to_string_lossy()));
                    
                    Ok(GonValue::Basic(el))
                } else {
                    e.write_value(compiler)
                }
            },
            plir::Path::Method(_, _, _) => panic!("Expected method to be resolved in call"),
        }
    }
}
impl<'ctx> TraverseIRPtr<'ctx> for plir::Path {
    fn write_ptr(&self, compiler: &mut LLVMCodegen<'ctx>) -> LLVMResult<PointerValue<'ctx>> {
        match self {
            plir::Path::Static(_, _, _) => todo!(),
            plir::Path::Struct(e, attrs) => {
                if !attrs.is_empty() {
                    let _i32 = compiler.ctx.i32_type();
                    
                    let ptr = e.write_ptr(compiler)?;
                    let ty = compiler.get_layout(&e.ty)?;

                    let usize_indexes: Vec<_> = attrs.iter()
                    .map(|&(i, _)| i)
                    .collect();

                    let mut gep_indexes = vec![_i32.const_zero()];
                    let iv_indexes = usize_indexes.iter()
                        .map(|&i| _i32.const_int(i as u64, false));
                    gep_indexes.extend(iv_indexes);

                    let mut ssa_name = ptr.get_name()
                        .to_string_lossy()
                        .into_owned();
                    for idx in usize_indexes {
                        ssa_name.push('.');
                        ssa_name += &idx.to_string();
                    }

                    // SAFETY: After PLIR pass, this should be valid.
                    let el_ptr = unsafe {
                        compiler.builder.build_in_bounds_gep(ty, ptr, &gep_indexes, &ssa_name)
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
    type Return = LLVMResult<GonValue<'ctx>>;

    fn write_value(&self, compiler: &mut LLVMCodegen<'ctx>) -> Self::Return {
        let Self { expr, index } = self;

        let _expr = expr.write_value(compiler)?;
        let _index = index.write_value(compiler)?;
        todo!()
    }
}

impl<'ctx> TraverseIR<'ctx> for plir::IDeref {
    type Return = LLVMResult<GonValue<'ctx>>;

    fn write_value(&self, compiler: &mut LLVMCodegen<'ctx>) -> Self::Return {
        let ptr = self.write_ptr(compiler)?;
        let layout = compiler.get_layout(&self.ty)?;

        let ptr_name = to_str!(ptr.get_name());
        let bv = compiler.builder.build_load(layout, ptr, &format!("{ptr_name}.deref"));
        Ok(GonValue::Basic(bv))
    }
}
impl<'ctx> TraverseIRPtr<'ctx> for plir::IDeref {
    fn write_ptr(&self, compiler: &mut LLVMCodegen<'ctx>) -> LLVMResult<PointerValue<'ctx>> {
        // expr should always be a ptr
        self.expr.write_value(compiler)
            .map(|gv| {
                compiler.basic_value_of(gv).into_pointer_value()
            })
    }
}

impl<'ctx> TraverseIR<'ctx> for plir::FunSignature {
    type Return = LLVMResult<FunctionValue<'ctx>>;

    fn write_value(&self, compiler: &mut LLVMCodegen<'ctx>) -> Self::Return {
        // TODO: remove when visibility is added
        compiler.import(self)
            .or_else(|e| match e {
                LLVMErr::CannotImport(_) => compiler.define_fun(self).map(|(fv, _)| fv),
                _ => Err(e)
            })
    }
}

impl<'ctx> TraverseIR<'ctx> for plir::FunDecl {
    type Return = LLVMResult<FunctionValue<'ctx>>;

    fn write_value(&self, compiler: &mut LLVMCodegen<'ctx>) -> Self::Return {
        let plir::FunDecl { sig, block } = self;

        let fun = compiler.get_fn_by_plir_ident(&sig.ident)
            .unwrap_or_else(|| panic!("function {} should be declared", sig.ident));

        let bb = compiler.ctx.append_basic_block(fun, "body");
        compiler.builder.position_at_end(bb);

        // store params
        for (param, val) in iter::zip(&sig.params, fun.get_param_iter()) {
            if compiler.is_ref_layout(&param.ty)? {
                let BasicValueEnum::PointerValue(ptr) = val else {
                    panic!("ref value should have had a ptr parameter but it had {val}")
                };
                compiler.vars.insert(param.ident.clone(), ptr);
            } else {
                let ptr = compiler.alloca(&param.ty, &param.ident)?;
                compiler.builder.build_store(ptr, val);
            }
        }

        // return nothing if the return value is Unit
        compiler.write_block(block, Default::default())?;
        
        if fun.verify(true) {
            Ok(fun)
        } else {
            println!();
            println!("=== the module ===");
            println!("{}", compiler.module.print_to_string().to_string());
            // SAFETY: Not used after.
            unsafe { fun.delete() }
            Err(LLVMErr::InvalidFun)
        }
    }
}

impl<'ctx> TraverseIR<'ctx> for plir::Class {
    type Return = LLVMResult<()>;

    fn write_value(&self, compiler: &mut LLVMCodegen<'ctx>) -> Self::Return {
        let plir::Class { ty, fields } = self;

        match ty {
            plir::TypeRef::Prim(_) => {
                let fields: Vec<_> = fields.values()
                    .map(|fd| compiler.get_layout(&fd.ty))
                    .collect::<Result<_, _>>()?;
                
                let struct_ty = compiler.ctx.opaque_struct_type(&ty.llvm_ident());
                struct_ty.set_body(&fields, false);

                compiler.define_type(ty.clone(), struct_ty);
            },

            plir::TypeRef::Generic(id, params, _) => {
                compiler.create_generic_module(id);
                let module = &compiler.generic_modules[&**id];

                let type_vars: HashMap<_, _> = params.iter()
                    .map(|t| {
                        let plir::TypeRef::TypeVar(_, name) = t else {
                            panic!("expected type var in generic class construction")
                        };

                        (&**name, compiler.ctx.opaque_struct_type(&t.llvm_ident()))
                    })
                    .collect();

                let fields: Vec<_> = fields.values()
                    .map(|fd| match &fd.ty {
                        plir::TypeRef::TypeVar(_, name) => {
                            type_vars.get(&**name)
                                .ok_or_else(|| panic!("type var should have existed"))
                                .map(|&st| st.into())
                        },
                        plir::TypeRef::Prim(_) => compiler.get_layout(&fd.ty),
                        plir::TypeRef::Generic(_, _, _) => compiler.get_layout(&fd.ty),
                        plir::TypeRef::Tuple(_, _) => compiler.get_layout(&fd.ty),
                        plir::TypeRef::Fun(_) => compiler.get_layout(&fd.ty),
                        plir::TypeRef::Unk(_) => panic!("should not have unknowns in LLVM codegen")
                    })
                    .collect::<Result<_, _>>()?;

                let struct_ty = compiler.ctx.opaque_struct_type(&ty.llvm_ident());
                struct_ty.set_body(&fields, false);

                // private function to assert existence of struct type
                let f = module.add_function(
                    "#assert_type_existence", 
                    compiler.ctx.void_type().fn_type(params![struct_ty], false), 
                    Some(Linkage::Private)
                );
                compiler.ctx.append_basic_block(f, "_empty");
            },

            _ => todo!("class resolution of structure {ty}")
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use inkwell::context::Context;

    use crate::compiler::Compiler;
    use crate::test_utils::prelude::*;

    fn test_compile(t: Test) -> TestResult<()> {
        println!("=== compile {} ===", t.header.name);
        let ctx = Context::create();
        let mut compiler = Compiler::new(&ctx, "eval")
            .map_err(|e| t.wrap_compile_err(e))?;

        match compiler.load_gon_str(t.source(), None) {
            Ok(_) => {
                compiler.module.print_to_stderr();
                Ok(())
            },
            Err(e) => {
                Err(t.wrap_compile_err(e))
            },
        }
    }

    fn test_run(t: Test) -> TestResult<()> {
        println!("=== run {} ===", t.header.name);
        let ctx = Context::create();
        let mut compiler = Compiler::new(&ctx, "eval")
            .map_err(|e| t.wrap_compile_err(e))?;

        compiler.load_gon_str(t.source(), None)
            .map_err(|e| t.wrap_compile_err(e))?;

        let result = unsafe { compiler.jit_run_raw() }
            .map_err(|e| t.wrap_compile_err(e))?;

        match result {
            0 => Ok(()),
            t => Err(TestErr::ExitWrong(t as u8))
        }
    }

    load_tests!("compiler",
        CG_TESTS = "_test_files/plir_llvm/codegen.gon"
        EARLY_EXIT_TESTS = "_test_files/plir_llvm/early_exits.gon"
        TYPE_TESTS = "_test_files/plir_llvm/compiler_types.gon"
    );

    #[test]
    fn basic_pass() -> TestResult<()> {
        CG_TESTS.pass_all(test_run, &[
            "basic_if", 
            "basic_while", 
            "basic_access", 
            "basic_arith_chain", 
            "basic_pattern", // TODO!: complete
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
            "class_operator_overloading",
            "method_access",
            "decl_cast_check",
            "fun_cast_check",
            "type_res",
            "fun_call"
        ])
    }
}