//! Converts the AST tree into an intermediate language 
//! (Poligon Language Intermediate Representation).
//! 
//! This reduces the complexity of the language to
//! makes it easier to later convert into LLVM.
//! 
//! As well as reducing the complexity of the language, this
//! module plays the role of static resolution 
//! (similar to the interpreter's [`crate::interpreter::semantic`]).
//! It does type resolution, variable resolution, break/continue/return checks, 
//! and simply verifies AST code is correct.
//! 
//! The main function that performs the conversion is [`plir_codegen`], 
//! which utilizes the [`PLIRCodegen`] struct.

mod op_impl;

use std::borrow::Cow;
use std::collections::HashMap;
use std::path::Path;
use std::rc::Rc;

use indexmap::IndexMap;

use crate::ast::{self, ReasgType, MutType};
use crate::compiler::internals::C_INTRINSICS_PLIR;
use crate::err::{GonErr, FullGonErr, full_gon_cast_impl, CursorRange};

pub(crate) use self::op_impl::{CastType, OpErr};

use super::plir::{self, Located};

/// Produce the PLIR tree from the AST tree.
pub fn plir_codegen(t: ast::Program) -> PLIRResult<plir::Program> {
    let mut cg = PLIRCodegen::new();
    cg.consume_program(t)?;
    cg.unwrap()
}

/// Errors that can occur during the PLIR creation process.
#[derive(Debug)]
pub enum PLIRErr {
    /// Cannot call `break` from this block.
    CannotBreak,
    /// Cannot call `continue` from this block.
    CannotContinue,
    /// Cannot call `return` from this block.
    CannotReturn,
    // TODO: split into Covariant, Invariant, and Contravariant
    /// Expected one type, but found a different type
    ExpectedType(plir::Type /* expected */, plir::Type /* found */),
    /// Could not determine the type of the expression
    CannotResolveType,
    /// Variable is not defined
    UndefinedVar(String),
    /// Type/class is not defined
    UndefinedType(String),
    /// Attribute doesn't exist on type
    UndefinedAttr(plir::Type, String),
    /// Tried to use . access on a method
    CannotAccessOnMethod,
    /// Tried to assign to a method
    CannotAssignToMethod,
    /// Cannot call given type
    CannotCall(plir::Type),
    /// Wrong number of parameters
    WrongArity(usize /* expected */, usize /* got */),
    /// Wrong number of type parameters
    WrongTypeArity(usize /* expected */, usize /* got */),
    /// Cannot spread here.
    CannotSpread,
    /// Cannot use class initializer syntax on this type.
    CannotInitialize(plir::Type),
    /// The field was not initialized on the type.
    UninitializedField(plir::Type, String),
    /// The field was unexpectedly initialized on the type.
    UnexpectedField(plir::Type, String),
    /// Operation between two types cannot be computed.
    OpErr(OpErr),
    /// Cannot deref this expression (because it is not a pointer)
    CannotDeref,
}

type FullPLIRErr = FullGonErr<PLIRErr>;
/// A [`Result`] type for operations in the PLIR tree creation process.
pub type PLIRResult<T> = Result<T, FullPLIRErr>;

impl From<OpErr> for PLIRErr {
    fn from(err: OpErr) -> Self {
        Self::OpErr(err)
    }
}
full_gon_cast_impl!(OpErr, PLIRErr);

impl GonErr for PLIRErr {
    fn err_name(&self) -> &'static str {
        match self {
            | PLIRErr::CannotBreak
            | PLIRErr::CannotContinue
            | PLIRErr::CannotReturn 
            | PLIRErr::CannotSpread
            => "syntax error",
            
            | PLIRErr::ExpectedType(_, _)
            | PLIRErr::CannotResolveType
            | PLIRErr::UndefinedType(_)
            | PLIRErr::CannotAccessOnMethod
            | PLIRErr::CannotAssignToMethod
            | PLIRErr::CannotCall(_)
            | PLIRErr::WrongArity(_, _)
            | PLIRErr::WrongTypeArity(_, _)
            | PLIRErr::CannotInitialize(_)
            | PLIRErr::CannotDeref
            => "type error",
            
            | PLIRErr::UndefinedVar(_)
            | PLIRErr::UndefinedAttr(_, _)
            => "name error",
            
            | PLIRErr::UninitializedField(_, _)
            | PLIRErr::UnexpectedField(_, _)
            => "value error",

            | PLIRErr::OpErr(e)
            => e.err_name(),
        }
    }

}

impl std::fmt::Display for PLIRErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PLIRErr::CannotBreak                  => write!(f, "cannot 'break' here"),
            PLIRErr::CannotContinue               => write!(f, "cannot 'continue' here"),
            PLIRErr::CannotReturn                 => write!(f, "cannot 'return' here"),
            PLIRErr::ExpectedType(e, g)           => write!(f, "expected type '{e}', but got '{g}'"),
            PLIRErr::CannotResolveType            => write!(f, "cannot determine type"),
            PLIRErr::UndefinedVar(name)           => write!(f, "could not find identifier '{name}'"),
            PLIRErr::UndefinedType(name)          => write!(f, "could not find type '{name}'"),
            PLIRErr::UndefinedAttr(t, name)       => write!(f, "could not find attribute '{name}' on '{t}'"),
            PLIRErr::CannotAccessOnMethod         => write!(f, "cannot access on method"),
            PLIRErr::CannotAssignToMethod         => write!(f, "cannot assign to method"),
            PLIRErr::CannotCall(t)                => write!(f, "cannot call value of type '{t}'"),
            PLIRErr::WrongArity(e, g)             => write!(f, "wrong number of parameters - expected {e}, got {g}"),
            PLIRErr::WrongTypeArity(e, g)         => write!(f, "wrong number of type parameters - expected {e}, got {g}"),
            PLIRErr::CannotSpread                 => write!(f, "cannot spread here"),
            PLIRErr::CannotInitialize(t)          => write!(f, "cannot use initializer syntax on type '{t}'"),
            PLIRErr::UninitializedField(t, field) => write!(f, "uninitialized field '{field}' on type '{t}'"),
            PLIRErr::UnexpectedField(t, field)    => write!(f, "field '{field}' is not present on type '{t}'"),
            PLIRErr::CannotDeref                  => write!(f, "only pointers can be dereferenced"),
            PLIRErr::OpErr(e)                     => e.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
enum BlockExit {
    /// This block exited by returning a value.
    Return(plir::Type),

    /// This block exited by `break`.
    Break,

    /// This block exited by `continue`.
    Continue,

    /// This block exited normally.
    Exit(plir::Type)
}

#[derive(PartialEq, Eq)]
enum BlockBehavior {
    /// This block is a function body.
    Function,

    /// This block is the body for a `while` or `for` loop.
    Loop,

    /// This block is on its own.
    Bare,

    /// This block is the body of an `if` statement.
    Conditional
}

enum BlockExitHandle {
    /// Continue running in the upper loop.
    /// 
    /// If a block exits here, it has a known value type.
    Continue(plir::Type),
    
    /// This is either an `break` or `continue`.
    /// 
    /// If a block exits here, there is no type.
    LoopExit,

    /// Propagate the exit to the upper loop.
    /// 
    /// The second parameter is `true` if this is a conditional exit (it does not ALWAYS occur).
    Propagate(BlockExit, bool /* conditional? */),
}

impl BlockBehavior {
    fn handle_exit(&self, exit: BlockExit) -> Result<BlockExitHandle, PLIRErr> {
        match self {
            BlockBehavior::Function => match exit {
                BlockExit::Return(t) => Ok(BlockExitHandle::Continue(t)),
                BlockExit::Break     => Err(PLIRErr::CannotBreak),
                BlockExit::Continue  => Err(PLIRErr::CannotContinue),
                BlockExit::Exit(t)   => Ok(BlockExitHandle::Continue(t)),
            },
            BlockBehavior::Loop => match exit {
                BlockExit::Return(_) => Ok(BlockExitHandle::Propagate(exit, false)),
                BlockExit::Break     => Ok(BlockExitHandle::LoopExit),
                BlockExit::Continue  => Ok(BlockExitHandle::LoopExit),
                BlockExit::Exit(t)   => Ok(BlockExitHandle::Continue(t)),
            },
            BlockBehavior::Bare => match exit {
                BlockExit::Return(_) => Ok(BlockExitHandle::Propagate(exit, false)),
                BlockExit::Break     => Ok(BlockExitHandle::Propagate(exit, false)),
                BlockExit::Continue  => Ok(BlockExitHandle::Propagate(exit, false)),
                BlockExit::Exit(t)   => Ok(BlockExitHandle::Continue(t)),
            },
            BlockBehavior::Conditional => match exit {
                BlockExit::Return(_) => Ok(BlockExitHandle::Propagate(exit, true)),
                BlockExit::Break     => Ok(BlockExitHandle::Propagate(exit, true)),
                BlockExit::Continue  => Ok(BlockExitHandle::Propagate(exit, true)),
                BlockExit::Exit(t)   => Ok(BlockExitHandle::Continue(t)),
            },
        }
    }
}

fn primitives(prims: &[&str]) -> HashMap<String, TypeData> {
    prims.iter()
        .map(|&s| (String::from(s), TypeData::primitive()))
        .collect()
}

#[derive(Debug)]
enum Unresolved {
    Class(ast::Class),
    ExternFun(ast::FunSignature),
    Fun(ast::FunDecl),
    FunBlock(plir::FunSignature, Located<Rc<ast::Block>>),
    Import(ast::StaticPath)
}

#[derive(Debug)]
struct InsertBlock {
    block: Vec<plir::ProcStmt>,
    last_stmt_loc: CursorRange, 

    /// All conditional exits.
    exits: Vec<Located<BlockExit>>,
    /// The *unconditional* exit.
    /// If this is present, this is the last statement of the block.
    /// If a conditional exit does not pass, this exit is how the block exits.
    final_exit: Option<Located<BlockExit>>,

    vars: HashMap<String, plir::Type>,
    aliases: HashMap<String, String>,
    types: HashMap<String, TypeData>,
    unresolved: HashMap<String, Unresolved>,

    /// If this is not None, then this block is expected to return the provided type.
    /// This can be used as context for some functions to more effectively assign
    /// a type.
    expected_ty: Option<plir::Type>
}

impl InsertBlock {
    fn new(block_range: CursorRange, expected_ty: Option<plir::Type>) -> Self {
        Self {
            block: vec![],
            last_stmt_loc: block_range,
            exits: vec![],
            final_exit: None,
            vars: HashMap::new(),
            aliases: HashMap::new(),
            types: HashMap::new(),
            unresolved: HashMap::new(),
            expected_ty
        }
    }

    fn top() -> Self {
        Self {
            block: vec![],
            last_stmt_loc: (0, 0) ..= (0, 0),
            exits: vec![],
            final_exit: None,
            vars: HashMap::new(),
            aliases: HashMap::new(),
            types: primitives(&[
                plir::Type::S_INT,
                plir::Type::S_FLOAT,
                plir::Type::S_BOOL,
                plir::Type::S_CHAR,
                plir::Type::S_VOID,
                "#ptr",
                "#byte"
            ]),
            unresolved: HashMap::new(),
            expected_ty: None
        }
    }

    fn last_stmt_type(&self) -> Cow<plir::Type> {
        use plir::{ProcStmt, Type, ty};

        #[inline]
        fn void_ty<'t>() -> Cow<'t, Type> {
            Cow::Owned(ty!(Type::S_VOID))
        }

        #[inline]
        fn never_ty<'t>() -> Cow<'t, Type> {
            Cow::Owned(ty!(Type::S_NEVER))
        }

        match self.block.last() {
            Some(ProcStmt::Decl(d)) => {
                let ty = &d.val.ty;

                if ty.is_never() {
                     Cow::Borrowed(ty)
                } else {
                    void_ty()
                }
            },
            Some(ProcStmt::Return(_)) => never_ty(),
            Some(ProcStmt::Break)     => never_ty(),
            Some(ProcStmt::Continue)  => never_ty(),
            Some(ProcStmt::Exit(_))   => never_ty(),
            Some(ProcStmt::Expr(e))   => Cow::Borrowed(&e.ty),
            None => void_ty(),
        }
    }

    /// Determine whether another statement can be pushed into the insert block.
    fn is_open(&self) -> bool {
        !self.last_stmt_type().is_never()
    }

    /// Push a singular statement into this insert block.
    /// 
    /// The return indicates whether or not another statement 
    /// can be pushed into the insert block (whether a final exit has been set).
    fn push_stmt(&mut self, stmt: Located<impl Into<plir::ProcStmt>>) -> bool {
        use plir::ProcStmt;

        let Located(stmt, stmt_range) = stmt;
        match stmt.into() {
            ProcStmt::Return(e) => self.push_return(e, stmt_range),
            ProcStmt::Break => self.push_break(stmt_range),
            ProcStmt::Continue => self.push_cont(stmt_range),
            st => {
                if self.is_open() {
                    self.block.push(st);
                    self.last_stmt_loc = stmt_range;
                }

                self.is_open()
            }
        }
    }

    /// Push a return statement into this insert block.
    /// 
    /// The return will be false, indicating another statement cannot
    /// be pushed into the insert block (as a final exit has been set).
    fn push_return(&mut self, me: Option<plir::Expr>, stmt_range: CursorRange) -> bool {
        if self.is_open() {
            let ty = match me {
                Some(ref e) => e.ty.clone(),
                None => plir::ty!(plir::Type::S_VOID),
            };
            self.block.push(plir::ProcStmt::Return(me));
            self.final_exit.replace(Located::new(BlockExit::Return(ty), stmt_range));
        }

        false
    }

    /// Push a break statement into this insert block.
    /// 
    /// The return will be false, indicating another statement cannot
    /// be pushed into the insert block (as a final exit has been set).
    fn push_break(&mut self, stmt_range: CursorRange) -> bool {
        if self.is_open() {
            self.block.push(plir::ProcStmt::Break);
            self.final_exit.replace(Located::new(BlockExit::Break, stmt_range));
        }

        false
    }

    /// Push a continue statement into this insert block.
    /// 
    /// The return will be false, indicating another statement cannot
    /// be pushed into the insert block (as a final exit has been set).
    fn push_cont(&mut self, stmt_range: CursorRange) -> bool {
        if self.is_open() {
            self.block.push(plir::ProcStmt::Continue);
            self.final_exit.replace(Located::new(BlockExit::Continue, stmt_range));
        }

        false
    }

    /// Insert an unresolved class/function into the insert block.
    fn insert_unresolved(&mut self, unresolved: Unresolved) {
        let k = match &unresolved {
            Unresolved::Class(c)        => &c.ident,
            Unresolved::ExternFun(fs)   => &fs.ident,
            Unresolved::Fun(fd)         => &fd.sig.ident,
            Unresolved::FunBlock(fs, _) => &fs.ident,
            Unresolved::Import(mp)      => &mp.attr,
        };

        self.unresolved.insert(k.clone(), unresolved);
    }

    /// Insert a class into the insert block's type register.
    fn insert_class(&mut self, cls: &plir::Class) {
        self.types.insert(cls.ident.clone(), TypeData::structural(cls.clone()));
    }

    /// Declares a variable within this insert block.
    fn declare(&mut self, ident: &str, ty: plir::Type) {
        self.vars.insert(String::from(ident), ty);
    }

    /// Aliases ident to new_ident for the given block.
    fn alias(&mut self, ident: &str, new_ident: &str) {
        self.aliases.insert(String::from(ident), String::from(new_ident));
    }
}

#[derive(Clone, PartialEq, Eq)]
struct Var {
    ident: String,
    ty: plir::Type,
    decl_range: CursorRange
}
impl Var {
    fn into_expr(self) -> plir::Expr {
        plir::Expr::new(self.ty, plir::ExprType::Ident(self.ident))
    }

    fn split(self, sp: plir::Split) -> PLIRResult<plir::Expr> {
        let t = self.ty.split(sp)
            .map_err(|e| e.at_range(self.decl_range))?;
        let e = plir::Expr::new(t, plir::ExprType::Split(self.ident, sp));
        Ok(e)
    }
}

#[derive(PartialEq, Eq, Hash, Debug)]
struct SigKey<'k> {
    ident: Cow<'k, str>,
    params: Cow<'k, [plir::Type]>
}

impl<'k> SigKey<'k> {
    fn new(ident: impl Into<Cow<'k, str>>, params: impl Into<Cow<'k, [plir::Type]>>) -> Self {
        Self { ident: ident.into(), params: params.into() }
    }
}

/// Value type that holds the data of a type.
/// 
/// This allows the PLIR codegen to replace methods with functions
/// and access type fields.
#[derive(Debug)]
struct TypeData {
    ty: TypeStructure,
    methods: HashMap<SigKey<'static>, String>
}

impl TypeData {
    /// Create a primitive type (a type whose fields are defined in LLVM instead of Poligon)
    pub fn primitive() -> Self {
        Self {
            ty: TypeStructure::Primitive,
            methods: Default::default()
        }
    }

    /// Create a structural type (a type whose fields are defined in Poligon)
    pub fn structural(cls: plir::Class) -> Self {
        Self {
            ty: TypeStructure::Class(cls),
            methods: Default::default()
        }
    }

    /// Get a method defined in the type.
    pub fn get_method<'a>(&'a self, ident: &'a str, /* params: &'a [plir::Type] */) -> Option<&'a str> {
        let k = SigKey::new(ident, vec![]);
        
        self.methods.get(&k).map(String::as_str)
    }

    /// Add a method to the type.
    pub fn insert_method(&mut self, ident: String, metref: String) {
        let k = SigKey::new(ident, vec![]);

        self.methods.insert(k, metref);
    }

    /// Get a field on the type (if present).
    pub fn get_field(&self, ident: &str) -> Option<(usize, &plir::Type)> {
        match &self.ty {
            TypeStructure::Primitive => None,
            TypeStructure::Class(cls) => cls.fields.get_full(ident).map(|(i, _, v)| (i, &v.ty)),
        }
    }

    fn fields(&self) -> Option<&indexmap::IndexMap<String, plir::FieldDecl>> {
        match &self.ty {
            TypeStructure::Primitive => None,
            TypeStructure::Class(cls) => Some(&cls.fields)
        }
    }
}

#[derive(Debug)]
enum TypeStructure {
    Primitive,
    Class(plir::Class)
}

/// A struct which holds the types declared by PLIR code generation.
#[derive(Default, Clone)]
pub struct DeclaredTypes {
    pub(super) types: IndexMap<String, plir::Class>,
    pub(super) values: IndexMap<String, plir::Type>
}

impl DeclaredTypes {
    /// Writes the declared types into a file.
    pub fn to_file(&self, p: impl AsRef<Path>) -> std::io::Result<()> {
        use std::fs::File;
        use std::io::prelude::*;

        let mut file = File::create(p)?;

        for class in self.types.values() {
            writeln!(file, "{class}")?;
        }
        for (ident, val_ty) in &self.values {
            if let plir::Type::Fun(f) = val_ty {
                writeln!(file, "{};", plir::HoistedStmt::ExternFunDecl(f.fun_signature(ident)))?
            }
            // TODO: don't ignore other types of decls
        }

        Ok(())
    }
}
impl std::ops::AddAssign for DeclaredTypes {
    fn add_assign(&mut self, rhs: Self) {
        self.types.extend(rhs.types);
        self.values.extend(rhs.values);
    }
}

#[derive(Default)]
struct Globals {
    stmts: Vec<plir::HoistedStmt>,
    declared: DeclaredTypes
}
impl Globals {
    fn push(&mut self, stmt: impl Into<plir::HoistedStmt>) {
        use plir::HoistedStmt;

        let stmt: HoistedStmt = stmt.into();
        let name = stmt.get_ident();
        match &stmt {
            HoistedStmt::FunDecl(f) => {
                self.declared.values.insert(name.to_owned(), plir::Type::Fun(f.sig.ty()));
            },
            HoistedStmt::ExternFunDecl(f) => {
                if self.declared.values.contains_key(name) { return; }
                self.declared.values.insert(name.to_owned(), plir::Type::Fun(f.ty()));
            },
            HoistedStmt::ClassDecl(c) => {
                self.declared.types.insert(name.to_owned(), c.clone());
            },
            HoistedStmt::IGlobal(_, _) => {
                self.declared.values.insert(name.to_owned(), plir::ty!("#ptr"));
            }
        }

        self.stmts.push(stmt);
    }
}

/// This struct does the actual conversion from AST to PLIR.
/// 
/// # Usage
/// To create a method similar to [`plir_codegen`], one can do so:
/// 
/// ```no_run
/// use poligon_lang::ast;
/// use poligon_lang::compiler::{plir, PLIRCodegen, PLIRResult};
/// 
/// # // Don't tell 'em, this is very literally how it's implemented
/// fn plir_codegen(program: ast::Program) -> PLIRResult<plir::Program> {
///     let mut cg = PLIRCodegen::new();
///     cg.consume_program(program)?;
///     cg.unwrap()
/// }
/// ```
/// 
/// It is expected that the struct is dropped after every use.
/// 
/// We can also access the declared types from a previous code generation instance
/// and load them into a new code generator:
/// 
/// ```no_run
/// use poligon_lang::ast;
/// use poligon_lang::compiler::{plir, PLIRCodegen};
/// 
/// let mut cg = PLIRCodegen::new();
/// /// -- DO SOME THINGS WITH cg -- ///
/// let declared_types = cg.declared_types();
/// 
/// let mut cg2 = PLIRCodegen::new_with_declared_types(declared_types);
/// // functions and classes from cg are accessible in cg2
pub struct PLIRCodegen {
    program: InsertBlock,
    globals: Globals,
    blocks: Vec<InsertBlock>,
    var_id: usize
}

impl PLIRCodegen {
    /// Creates a new instance of the PLIRCodegen.
    pub fn new() -> Self {
        Self::new_with_declared_types(Default::default())
    }

    /// Creates a new instance of the PLIRCodegen with the given types already declared.
    pub fn new_with_declared_types(declared: DeclaredTypes) -> Self {
        let mut top = InsertBlock::top();
        for (ident, cls) in &declared.types {
            top.types.insert(ident.clone(), TypeData::structural(cls.clone()));
        }
        for (ident, ty) in &declared.values {
            top.declare(ident, ty.clone());
        }

        Self { 
            program: top, 
            globals: Globals {
                stmts: vec![],
                declared
            },
            blocks: vec![],
            var_id: 0
        }
    }

    /// Takes out the generated [`plir::Program`] from this struct.
    pub fn unwrap(self) -> PLIRResult<plir::Program> {
        assert!(self.blocks.is_empty(),
            "insert block was opened but not properly closed"
        );

        let InsertBlock {block, mut exits, unresolved, .. } = self.program;
        debug_assert!(unresolved.is_empty(), "there was an unresolved item in block");

        match exits.pop() {
            None => Ok(plir::Program(self.globals.stmts, block)),
            Some(Located(exit, range)) => match exit {
                BlockExit::Break     => Err(PLIRErr::CannotBreak.at_range(range)),
                BlockExit::Continue  => Err(PLIRErr::CannotContinue.at_range(range)),
                BlockExit::Return(_) => Err(PLIRErr::CannotReturn.at_range(range)),
                BlockExit::Exit(_)   => Err(PLIRErr::CannotReturn.at_range(range)),
            }
        }
    }

    /// Gets all the types declared by this code generation.
    pub fn declared_types(&self) -> DeclaredTypes {
        self.globals.declared.clone()
    }

    fn push_block(&mut self, block_range: CursorRange, expected_ty: Option<plir::Type>) {
        self.blocks.push(InsertBlock::new(block_range, expected_ty))
    }
    fn pop_block(&mut self) -> Option<InsertBlock> {
        self.blocks.pop()
    }
    fn peek_block(&mut self) -> &mut InsertBlock {
        self.blocks.last_mut().unwrap_or(&mut self.program)
    }

    /// Declares a variable with a given type.
    fn declare(&mut self, ident: &str, ty: plir::Type) {
        self.peek_block().declare(ident, ty)
    }

    /// Accesses to ident are mapped to new_ident for the current block.
    fn alias(&mut self, ident: &str, new_ident: &str) {
        self.peek_block().alias(ident, new_ident);
    }

    /// If there is an unresolved structure present at the identifier, try to resolve it.
    fn resolve_ident<'a>(&mut self, ident: &'a str) -> PLIRResult<&'a str> {
        use std::collections::hash_map::Entry;

        // mi is 0 to len
        let mi = self.blocks.iter().rev()
            .chain(std::iter::once(&self.program))
            .enumerate()
            .find_map(|(i, ib)| ib.unresolved.contains_key(ident).then_some(i));

        if let Some(i) = mi {
            // repivot peek block to point to the unresolved item
            let storage = self.blocks.split_off(self.blocks.len() - i);

            let Entry::Occupied(entry) = self.peek_block().unresolved.entry(ident.to_string()) else {
                unreachable!()
            };

            match entry.get() {
                Unresolved::Class(_) => {
                    let Unresolved::Class(cls) = entry.remove() else { unreachable!() };
                    self.consume_cls(cls)?;
                },
                Unresolved::ExternFun(_) => {
                    let Unresolved::ExternFun(fs) = entry.remove() else { unreachable!() };
                    
                    let fs = self.consume_fun_sig(fs)?;
                    self.push_global(fs);
                },
                Unresolved::Fun(_) => {
                    let (k, Unresolved::Fun(fd)) = entry.remove_entry() else { unreachable!() };
                    let ast::FunDecl { sig, block } = fd;

                    let sig = self.consume_fun_sig(sig)?;
                    self.peek_block().unresolved.insert(k, Unresolved::FunBlock(sig, block));
                },
                Unresolved::FunBlock(_, _) => {},
                Unresolved::Import(_) => todo!(),
            }

            // revert peek block after
            self.blocks.extend(storage);
        }

        // HACK: check for intrinsic
        if let Some(intrinsic) = ident.strip_prefix('#') {
            if let Some(t) = C_INTRINSICS_PLIR.get(intrinsic) {
                // If this is an intrinsic,
                // register the intrinsic on the top level
                // if it hasn't been registered

                self.push_global(plir::FunSignature {
                    ident: intrinsic.to_string(),
                    params: t.params.iter().enumerate().map(|(i, t)| plir::Param {
                        rt: Default::default(),
                        mt: Default::default(),
                        ident: format!("arg{i}"),
                        ty: t.clone(),
                    }).collect(),
                    varargs: t.varargs,
                    ret: (*t.ret).clone(),
                });
                self.declare(intrinsic, plir::Type::Fun(t.clone()));
                self.alias(ident, intrinsic);
                
                Ok(intrinsic)
            } else {
                Ok(ident)
            }
        } else {
            Ok(ident)
        }
    }

    /// [`PLIRCodegen::resolve_ident`], but using a type parameter
    fn resolve_ty(&mut self, ty: &plir::Type) -> PLIRResult<()> {
        self.resolve_ident(&ty.ident()).map(|_| ())
    }

    /// Find a specific item, starting from the deepest scope and scaling out.
    fn find_scoped<'a, T>(&'a self, f: impl FnMut(&'a InsertBlock) -> Option<T>) -> Option<T> {
        self.blocks.iter().rev()
            .chain(std::iter::once(&self.program))
            .find_map(f)
    }

    fn get_var_type(&mut self, mut ident: &str, range: CursorRange) -> PLIRResult<&plir::Type> {
        ident = self.resolve_ident(ident)?;

        self.find_scoped(|ib| ib.vars.get(ident))
            .ok_or_else(|| PLIRErr::UndefinedVar(String::from(ident)).at_range(range))
    }

    /// Identifier needs to be resolved beforehand, via [`PLIRCodegen::resolve_ident`].
    fn get_var_type_opt(&mut self, ident: &str) -> Option<&plir::Type> {
        self.find_scoped(|ib| ib.vars.get(ident))
    }

    fn dealias<'a>(&'a self, ident: &'a str) -> &'a str {
        match self.find_scoped(|ib| ib.aliases.get(ident)) {
            Some(t) => t,
            None => ident,
        }
    }

    fn get_class(&mut self, ty: &plir::Type, range: CursorRange) -> PLIRResult<&TypeData> {
        self.resolve_ty(ty)?;

        match ty.as_ref() {
            plir::TypeRef::Prim(ident) => self.find_scoped(|ib| ib.types.get(ident))
                .ok_or_else(|| {
                    PLIRErr::UndefinedType(String::from(ident))
                        .at_range(range)
                }),
            
            // HACK ll_array
            plir::TypeRef::Generic("#ll_array", [t]) => {
                let ty_ident = ty.ident();

                if self.find_scoped(|ib| ib.types.get(&*ty_ident)).is_none() {
                    self.get_class(t, range)?;
                    self.program.types.insert(ty_ident.to_string(), TypeData::primitive());
                }

                Ok(&self.program.types[&*ty_ident])
            },
            plir::TypeRef::Generic("#ll_array", a) => {
                Err(PLIRErr::WrongTypeArity(1, a.len()).at_range(range))
            },

            s => todo!("{s}")
        }
    }

    fn tmp_var_name(&mut self, ident: &str) -> String {
        let string = format!("_{ident}_{}", self.var_id);
        self.var_id += 1;
        string
    }

    fn push_tmp_decl(&mut self, ident: &str, e: plir::Expr, decl_range: CursorRange) -> Var {
        let ident = self.tmp_var_name(ident);
        let ety = e.ty.clone();

        let decl = plir::Decl {
            rt: ReasgType::Const,
            mt: MutType::Immut,
            ident: ident.clone(),
            ty: ety.clone(),
            val: e,
        };

        self.peek_block().push_stmt(Located::new(decl, decl_range.clone()));

        Var {
            ident,
            ty: ety,
            decl_range
        }
    }

    fn push_global(&mut self, global: impl Into<plir::HoistedStmt>) {
        self.globals.push(global.into())
    }

    /// Consumes an AST program into PLIR and attaches it to the [`PLIRCodegen`].
    /// 
    /// It can be accessed again with [`PLIRCodegen::unwrap`].
    pub fn consume_program(&mut self, prog: ast::Program) -> PLIRResult<()> {
        self.consume_stmts(prog.0)
    }

    /// Consume an iterator of statements into the current insert block.
    /// 
    /// This function stops parsing statements early if an unconditional exit has been found.
    /// At this point, the insert block cannot accept any more statements.
    fn consume_stmts(&mut self, stmts: impl IntoIterator<Item=Located<ast::Stmt>>) -> PLIRResult<()> {
        let mut eager_stmts = vec![];
        for stmt in stmts {
            match stmt.0 {
                ast::Stmt::FunDecl(fd) => {
                    self.peek_block().insert_unresolved(Unresolved::Fun(fd));
                },
                ast::Stmt::ExternFunDecl(fs) => {
                    self.peek_block().insert_unresolved(Unresolved::ExternFun(fs));
                    }
                ast::Stmt::ClassDecl(cls) => {
                    self.peek_block().insert_unresolved(Unresolved::Class(cls));
                },
                ast::Stmt::Import(mp) => {
                    self.peek_block().insert_unresolved(Unresolved::Import(mp));
                },
                ast::Stmt::IGlobal(id, s) => {
                    self.program.declare(&id, plir::ty!("#ptr"));
                    self.push_global(plir::HoistedStmt::IGlobal(id, s));
                }
                _ => eager_stmts.push(stmt),
            }
        }

        let peek_ctx_type = self.peek_block().expected_ty.clone();
        let len = eager_stmts.len();
        for (i, stmt) in eager_stmts.into_iter().enumerate() {
            if !self.consume_eager_stmt(stmt, &peek_ctx_type, len - 1 - i)? {
                break;
            }
        }

        let mut unresolved = &mut self.peek_block().unresolved;
        while let Some(ident) = unresolved.keys().next() {
            match unresolved.remove(&ident.clone()).unwrap() {
                Unresolved::Class(c) => { self.consume_cls(c)?; },
                Unresolved::ExternFun(fs) => {
                    let fs = self.consume_fun_sig(fs)?;
                    self.push_global(fs);
                },
                Unresolved::Fun(fd) => {
                    let ast::FunDecl { sig, block } = fd;

                    let sig = self.consume_fun_sig(sig)?;
                    self.consume_fun_block(sig, block)?;
                },
                Unresolved::FunBlock(sig, block) => {
                    self.consume_fun_block(sig, block)?;
                },
                Unresolved::Import(_) => todo!(),
            }
            unresolved = &mut self.peek_block().unresolved;
        }

        Ok(())
    }

    /// Consume a statement into the current insert block.
    /// 
    /// This function returns whether or not the insert block accepts any more statements.
    fn consume_eager_stmt(&mut self, stmt: Located<ast::Stmt>, ctx_type: &Option<plir::Type>, index_til_end: usize) -> PLIRResult<bool> {
        let Located(stmt, range) = stmt;

        match stmt {
            ast::Stmt::Decl(d) => self.consume_decl(Located::new(d, range)),
            ast::Stmt::Return(me) => {
                let maybe_expr = match me {
                    Some(e) => Some(self.consume_expr(e, ctx_type.clone())?),
                    None => None,
                };
                Ok(self.peek_block().push_return(maybe_expr, range))
            },
            ast::Stmt::Break => {
                Ok(self.peek_block().push_break(range))
            },
            ast::Stmt::Continue => {
                Ok(self.peek_block().push_cont(range))
            },
            ast::Stmt::Expr(e) => {
                let ety = if index_til_end == 0 {
                    ctx_type.clone()
                } else {
                    None
                };

                let e = self.consume_located_expr(e, ety)?;
                Ok(self.peek_block().push_stmt(e))
            },
            ast::Stmt::FunDecl(_) => unimplemented!("fun decl should not be resolved eagerly"),
            ast::Stmt::ExternFunDecl(_) => unimplemented!("extern fun decl should not be resolved eagerly"),
            ast::Stmt::ClassDecl(_) => unimplemented!("class decl should not be resolved eagerly"),
            ast::Stmt::Import(_) => unimplemented!("import decl should not be resolved eagerly"),
            ast::Stmt::ImportIntrinsic => Ok(self.peek_block().is_open()), // no-op
            ast::Stmt::IGlobal(_, _) => unimplemented!("global decl should not be resolved eagerly"),
        }
    }

    fn consume_insert_block(
        &mut self, 
        block: InsertBlock, 
        btype: BlockBehavior
    ) -> PLIRResult<plir::Block> {
        let InsertBlock { 
            mut block, last_stmt_loc, 
            exits, final_exit, 
            vars: _, aliases: _, types: _, 
            unresolved, expected_ty
        } = block;
        debug_assert!(unresolved.is_empty(), "there was an unresolved item in block");

        use plir::{ProcStmt, ty};

        // fill the unconditional exit if necessary:
        let mut final_exit = final_exit.unwrap_or_else(|| {
            // we need to add an explicit exit statement.

            // if the stmt given is an Expr, we need to replace the Expr with an explicit `exit` stmt.
            // otherwise just append an `exit` stmt.
            let exit_expr = match block.pop() {
                Some(ProcStmt::Expr(e)) => Some(e),
                Some(stmt) => {
                    block.push(stmt);
                    None
                },
                None => None
            };

            let exit_ty = match &exit_expr {
                Some(e) => e.ty.clone(),
                None => ty!(plir::Type::S_VOID),
            };

            if btype == BlockBehavior::Function {
                block.push(ProcStmt::Return(exit_expr));
                Located::new(BlockExit::Return(exit_ty), last_stmt_loc)
            } else {
                block.push(ProcStmt::Exit(exit_expr));
                Located::new(BlockExit::Exit(exit_ty), last_stmt_loc)
            }
        });

        // Type check block:
        if let Some(exp_ty) = expected_ty {
            let Located(fexit, exit_range) = &mut final_exit;
            let exit_range = exit_range.clone();

            match fexit {
                | BlockExit::Return(exit_ty) 
                | BlockExit::Exit(exit_ty) 
                => if exit_ty.as_ref() != exp_ty.as_ref() {
                    let Some(ProcStmt::Return(me) | ProcStmt::Exit(me)) = block.last_mut() else {
                        unreachable!();
                    };

                    match me.take() {
                        Some(e) => match op_impl::apply_special_cast(e, &exp_ty, CastType::FunDecl) {
                            Ok(e) => {
                                // cast was successful so apply it
                                me.replace(e);
                                *exit_ty = exp_ty;
                            },
                            Err(_) => Err(PLIRErr::ExpectedType(exp_ty, exit_ty.clone()).at_range(exit_range))?,
                        },
                        // exit_ty is void, and we already checked that exp_ty is not void
                        None => Err(PLIRErr::ExpectedType(exp_ty, exit_ty.clone()).at_range(exit_range))?,
                    }
                },
                // what is done here?
                BlockExit::Break => {},
                BlockExit::Continue => {},
            }
        }

        // resolve block's types and handle the methods of exiting the block
        let mut type_branches = vec![];
        for Located(exit, exit_range) in exits {
            match btype.handle_exit(exit.clone())? {
                BlockExitHandle::Continue(ty) => type_branches.push(ty),
                BlockExitHandle::LoopExit => {},

                // second parameter does not matter here because
                // this is already conditional.
                // as such, just propagate as a conditional exit.
                BlockExitHandle::Propagate(exit, _) => {
                    self.peek_block().exits.push(Located::new(exit, exit_range));
                },
            }
        }

        let Located(final_exit, exit_range) = final_exit;
        match btype.handle_exit(final_exit)? {
            BlockExitHandle::Continue(ty) => type_branches.push(ty),
            BlockExitHandle::LoopExit => {},
            BlockExitHandle::Propagate(exit, conditional) => {
                let located_exit = Located::new(exit, exit_range);
                if conditional {
                    self.peek_block().exits.push(located_exit);
                } else {
                    self.peek_block().final_exit.replace(located_exit);
                }
            },
        }

        let bty = plir::Type::resolve_branches(&type_branches)
            .ok_or(PLIRErr::CannotResolveType)?;
        Ok(plir::Block(bty, block))
    }
    fn consume_tree_block(
        &mut self, 
        block: Located<ast::Block>, 
        btype: BlockBehavior, 
        expected_ty: Option<plir::Type>
    ) -> PLIRResult<plir::Block> {
        let Located(ast::Block(stmts), block_range) = block;

        self.push_block(block_range, expected_ty);
        // collect all the statements from this block
        self.consume_stmts(stmts)?;
        let insert_block = self.pop_block().unwrap();
        self.consume_insert_block(insert_block, btype)
    }

    fn unpack_pat<T, E>(
        &mut self, 
        pat: Located<ast::Pat<T>>, 
        expr: Located<plir::Expr>, 
        (extra, mut split_extra): (E, impl FnMut(&E, plir::Split) -> PLIRResult<E>),
        mut map: impl FnMut(&mut Self, Located<T>, Located<plir::Expr>, E) -> PLIRResult<()>,
        consume_var: bool,
        stmt_range: CursorRange
    ) -> PLIRResult<()> {
        self.unpack_pat_inner(pat, expr, (extra, &mut split_extra), &mut map, consume_var, stmt_range)
    }

    fn unpack_pat_inner<T, E>(
        &mut self, 
        pat: Located<ast::Pat<T>>, 
        expr: Located<plir::Expr>, 
        extra: (E, &mut impl FnMut(&E, plir::Split) -> PLIRResult<E>),
        map: &mut impl FnMut(&mut Self, Located<T>, Located<plir::Expr>, E) -> PLIRResult<()>,
        consume_var: bool,
        stmt_range: CursorRange
    ) -> PLIRResult<()> {
        fn create_splits<T>(pats: &[&ast::Pat<T>]) -> Vec<plir::Split> {
            let len = pats.len();
            match pats.iter().position(|pat| matches!(pat, ast::Pat::Spread(_))) {
                Some(pt) => {
                    let rpt = len - pt;
                    
                    let mut splits: Vec<_> = (0..pt).map(plir::Split::Left).collect();
                    splits.push(plir::Split::Middle(pt, rpt));
                    splits.extend((0..rpt).rev().map(plir::Split::Right));

                    splits
                },
                None => (0..len).map(plir::Split::Left).collect(),
            }
        }

        let Located(pat, range) = pat;
        match pat {
            ast::Pat::Unit(t) => map(self, Located::new(t, range), expr, extra.0),
            ast::Pat::Spread(spread) => match spread {
                Some(pat) => self.unpack_pat_inner(*pat, expr, extra, map, consume_var, stmt_range),
                None => Ok(()),
            },
            ast::Pat::List(pats) => {
                let var = expr.map(|e| self.push_tmp_decl("decl", e, stmt_range.clone()));
                let delocated: Vec<_> = pats.iter().map(|t| &t.0).collect();
                let (extra, split_extra) = extra;

                for (idx, pat) in std::iter::zip(create_splits(&delocated), pats) {
                    let rhs = var.clone().map(|v| v.split(idx))
                        .transpose_result()?;

                    let extr = split_extra(&extra, idx)?;
                    self.unpack_pat_inner(pat, rhs, (extr, split_extra), map, false, stmt_range.clone())?;
                }

                // On final statement, consume the var, and return its original value.
                if consume_var {
                    self.peek_block().push_stmt(Located::new(var.0.into_expr(), stmt_range));
                }
                Ok(())
            },
        }
    }

    /// Consume a declaration into the current insert block.
    /// 
    /// This function returns whether or not the insert block accepts any more statements.
    fn consume_decl(&mut self, decl: Located<ast::Decl>) -> PLIRResult<bool> {
        let Located(ast::Decl { rt, pat, ty, val }, decl_range) = decl;
        
        let ty = match ty {
            Some(t) => Some(self.consume_type(t)?),
            None => None,
        };
        let e = self.consume_located_expr(val, ty.clone())?;

        self.unpack_pat(pat, e, 
            ((rt, ty), |(rt, mty), idx| {
                Ok((*rt, match mty {
                    Some(t) => Some(t.split(idx).map_err(|e| {
                        e.at_range(decl_range.clone())
                    })?),
                    None => None,
                }))
            }),
            |this, unit, e, extra| {
                let Located(ast::DeclUnit(ident, mt), _) = unit;
                let Located(e, erange) = e;

                let (rt, ty) = extra;

                let ty = ty.unwrap_or_else(|| e.ty.clone());
                // Type check decl, casting if possible
                let e = match op_impl::apply_special_cast(e, &ty, CastType::Decl) {
                    Ok(e) => e,
                    Err(e) => return Err(PLIRErr::ExpectedType(ty, e.ty).at_range(erange)),
                };

                this.declare(&ident, ty.clone());
                let decl = plir::Decl { rt, mt, ident, ty, val: e };
                this.peek_block().push_stmt(Located::new(decl, decl_range.clone()));

                Ok(())
            },
            false,
            decl_range.clone()
        )?;

        Ok(self.peek_block().is_open())
    }

    /// Consume a function signature and convert it into a PLIR function signature.
    fn consume_fun_sig(&mut self, sig: ast::FunSignature) -> PLIRResult<plir::FunSignature> {
        let ast::FunSignature { ident, params, varargs, ret } = sig;
        
        let params: Vec<_> = params.into_iter()
            .map(|p| -> PLIRResult<_> {
                let ast::Param { rt, mt, ident, ty } = p;
                let ty = match ty {
                    Some(t) => self.consume_type(t)?,
                    None => plir::ty!(plir::Type::S_UNK),
                };

                Ok(plir::Param { rt, mt, ident, ty })
            })
            .collect::<Result<_, _>>()?;
        
        let param_tys = params.iter()
            .map(|p| &p.ty)
            .cloned()
            .collect();
    
        let ret = match ret {
            Some(t) => self.consume_type(t)?,
            None => plir::ty!(plir::Type::S_VOID),
        };

        // declare function before parsing block
        self.declare(&ident, 
            plir::Type::fun_type(param_tys, ret.clone(), varargs)
        );

        Ok(plir::FunSignature { ident, params, ret, varargs })
    }
    /// Consume a function declaration statement into the current insert block.
    /// 
    /// This function returns whether or not the insert block accepts any more statements.
    fn consume_fun_block(&mut self, sig: plir::FunSignature, block: Located<Rc<ast::Block>>) -> PLIRResult<bool> {
        let Located(block, block_range) = block;
        let old_block = Rc::try_unwrap(block)
            .expect("AST function declaration block was unexpectedly in use and cannot be consumed into PLIR.");
    
        let block = {
            self.push_block(block_range, Some(sig.ret.clone()));

            for plir::Param { ident, ty, .. } in sig.params.iter() {
                self.declare(ident, ty.clone());
            }
    
            // collect all the statements from this block
            self.consume_stmts(old_block.0)?;
    
            let insert_block = self.pop_block().unwrap();
            self.consume_insert_block(insert_block, BlockBehavior::Function)?
        };

        let fun_decl = plir::FunDecl { sig, block };
        
        self.push_global(fun_decl);
        Ok(self.peek_block().is_open())
    }

    fn consume_type(&mut self, ty: Located<ast::Type>) -> PLIRResult<plir::Type> {
        let Located(ty, range) = ty;
        let ty = plir::Type::from(ty);

        // See if class is initialized, and if so, return the plir Type
        self.get_class(&ty, range).map(|_| ty)
    }

    fn consume_cls(&mut self, cls: ast::Class) -> PLIRResult<bool> {
        let ast::Class { ident, fields, methods } = cls;

        let fields = fields.into_iter()
            .map(|ast::FieldDecl { rt, mt, ident, ty }| -> PLIRResult<_> {
                self.consume_type(ty).map(|ty| {
                    (ident, plir::FieldDecl { rt, mt, ty })
                })
            })
            .collect::<Result<_, _>>()?;
        
        let cls = plir::Class { ident, fields };
        
        let ib = self.peek_block();
        ib.insert_class(&cls);

        for method in methods {
            let ast::MethodDecl {
                sig: ast::MethodSignature { referent, is_static, name: method_name, mut params, ret }, 
                block 
            } = method;

            if !is_static {
                let this = referent.unwrap_or_else(|| String::from("#unused"));
                params.insert(0, ast::Param { 
                    rt: Default::default(), 
                    mt: Default::default(), 
                    ident: this, 
                    // since this is synthesized, there's no real cursor,
                    // so just assign an arbitary one
                    ty: Some(Located::new(ast::Type(cls.ident.clone(), vec![]), (0, 0) ..= (0, 0)))
                });
            } else {
                // TODO, use this ident
            };
            let metref = format!("{}::{method_name}", &cls.ident);

            let sig = ast::FunSignature { ident: metref.clone(), params, varargs: false, ret };
            let decl = ast::FunDecl { sig, block };

            ib.insert_unresolved(Unresolved::Fun(decl));

            if let Some(c) = ib.types.get_mut(&cls.ident) {
                c.insert_method(method_name, metref);
            }
        }

        self.push_global(cls);
        Ok(self.peek_block().is_open())
    }

    fn consume_expr(&mut self, value: Located<ast::Expr>, ctx_type: Option<plir::Type>) -> PLIRResult<plir::Expr> {
        let Located(expr, range) = value;
        
        match expr {
            ast::Expr::Ident(ident) => {
                let ty = self.get_var_type(&ident, range)?.clone();
                let ref_ident = self.dealias(&ident).to_owned();

                Ok(plir::Expr::new(
                    ty,
                    plir::ExprType::Ident(ref_ident)
                ))
            },
            ast::Expr::Block(b) => {
                let block = self.consume_tree_block(b, BlockBehavior::Bare, None)?;
                
                Ok(plir::Expr::new(block.0.clone(), plir::ExprType::Block(block)))
            },
            ast::Expr::Literal(literal) => {
                let ty = match literal {
                    ast::Literal::Int(_)   => plir::ty!(plir::Type::S_INT),
                    ast::Literal::Float(_) => plir::ty!(plir::Type::S_FLOAT),
                    ast::Literal::Char(_)  => plir::ty!(plir::Type::S_CHAR),
                    ast::Literal::Str(_)   => plir::ty!(plir::Type::S_STR),
                    ast::Literal::Bool(_)  => plir::ty!(plir::Type::S_BOOL)
                };

                // check type exists (for string, this may not happen if std isn't loaded)
                self.get_class(&ty, range)?;
                //

                Ok(plir::Expr::new(
                    ty, plir::ExprType::Literal(literal)
                ))
            },
            ast::Expr::ListLiteral(lst) => {
                let el_ty = match ctx_type.as_ref().map(plir::Type::as_ref) {
                    Some(plir::TypeRef::Generic(plir::Type::S_LIST, [t])) => Some(t.clone()),
                    None => None,
                    _ => Err(PLIRErr::CannotResolveType.at_range(range.clone()))?
                };

                let new_inner: Vec<_> = lst.into_iter()
                    .map(|e| self.consume_expr(e, el_ty.clone()))
                    .collect::<Result<_, _>>()?;

                let el_ty = plir::Type::resolve_collection_ty(new_inner.iter().map(|e| &e.ty))
                    .or(el_ty)
                    .ok_or_else(|| PLIRErr::CannotResolveType.at_range(range))?;

                Ok(plir::Expr::new(
                    plir::ty!(plir::Type::S_LIST, [el_ty]),
                    plir::ExprType::ListLiteral(new_inner)
                ))
            },
            ast::Expr::SetLiteral(set) => {
                let el_ty = match ctx_type.as_ref().map(plir::Type::as_ref) {
                    Some(plir::TypeRef::Generic(plir::Type::S_SET, [t])) => Some(t.clone()),
                    None => None,
                    _ => Err(PLIRErr::CannotResolveType.at_range(range.clone()))?
                };

                let new_inner: Vec<_> = set.into_iter()
                    .map(|e| self.consume_expr(e, el_ty.clone()))
                    .collect::<Result<_, _>>()?;

                let el_ty = plir::Type::resolve_collection_ty(new_inner.iter().map(|e| &e.ty))
                    .or(el_ty)
                    .ok_or_else(|| PLIRErr::CannotResolveType.at_range(range))?;

                Ok(plir::Expr::new(
                    plir::ty!(plir::Type::S_SET, [el_ty]),
                    plir::ExprType::SetLiteral(new_inner)
                ))
            },
            ast::Expr::DictLiteral(entries) => {
                let (kty, vty) = match ctx_type.as_ref().map(plir::Type::as_ref) {
                    Some(plir::TypeRef::Generic(plir::Type::S_DICT, [k, v])) => {
                        (Some(k.clone()), Some(v.clone()))
                    },
                    None => (None, None),
                    _ => Err(PLIRErr::CannotResolveType.at_range(range.clone()))?
                };

                let new_inner: Vec<_> = entries.into_iter()
                    .map(|(k, v)| Ok((self.consume_expr(k, kty.clone())?, self.consume_expr(v, vty.clone())?)))
                    .collect::<PLIRResult<_>>()?;

                let (key_tys, val_tys): (Vec<_>, Vec<_>) = new_inner.iter()
                    .map(|(k, v)| (&k.ty, &v.ty))
                    .unzip();
                let key_ty = plir::Type::resolve_collection_ty(key_tys)
                    .or(kty)
                    .ok_or_else(|| PLIRErr::CannotResolveType.at_range(range.clone()))?;
                let val_ty = plir::Type::resolve_collection_ty(val_tys)
                    .or(vty)
                    .ok_or_else(|| PLIRErr::CannotResolveType.at_range(range))?;

                Ok(plir::Expr::new(
                    plir::ty!(plir::Type::S_DICT, [key_ty, val_ty]),
                    plir::ExprType::DictLiteral(new_inner)
                ))
            },
            ast::Expr::ClassLiteral(ty, entries) => {
                let tyrange = ty.range();
                let ty = self.consume_type(ty)?;
                
                let cls_fields: IndexMap<_, _> = self.get_class(&ty, tyrange.clone())?
                    .fields()
                    .ok_or_else(|| {
                        PLIRErr::CannotInitialize(ty.clone()).at_range(tyrange.clone())
                    })?
                    .iter()
                    .map(|(k, fd)| (k.clone(), fd.ty.clone()))
                    .collect();
                
                let mut entries: HashMap<_, _> = entries.into_iter()
                    .map(|(Located(k, krange), v)| (k, (krange, v)))
                    .collect();
                
                let new_entries = cls_fields.into_iter()
                    .map(|(k, field_ty)| {
                        let (_, ast_expr) = entries.remove(&k)
                            .ok_or_else(|| {
                                PLIRErr::UninitializedField(ty.clone(), k)
                                    .at_range(tyrange.clone())
                            })?;
                        
                        let Located(field_expr, expr_range) = self.consume_located_expr(ast_expr, Some(field_ty.clone()))?;
                        
                        op_impl::apply_special_cast(field_expr, &field_ty, CastType::Decl)
                            .map_err(|e| PLIRErr::ExpectedType(field_ty, e.ty).at_range(expr_range))
                    })
                    .collect::<PLIRResult<_>>()?;
                
                if let Some((f, (krange, _))) = entries.into_iter().next() {
                    Err(PLIRErr::UnexpectedField(ty, f).at_range(krange))
                } else {
                    Ok(plir::Expr::new(
                        ty.clone(), 
                        plir::ExprType::ClassLiteral(ty, new_entries)
                    ))
                }
            },
            ast::Expr::Assign(pat, expr) => {
                let expr = self.consume_located_expr(*expr, ctx_type.clone())?;

                self.push_block(range.clone(), ctx_type);
                self.unpack_pat(pat, expr, ((), |_, _| Ok(())),
                    |this, unit, e, _| {
                        let Located(unit, range) = unit;
                        let unit = match unit {
                            ast::AsgUnit::Ident(ident) => plir::AsgUnit::Ident(ident),
                            ast::AsgUnit::Path(p) => {
                                let p = this.consume_path(Located::new(p, range.clone()))?;
                                if matches!(p, plir::Path::Method(..)) {
                                    Err(PLIRErr::CannotAssignToMethod.at_range(range.clone()))?
                                } else {
                                    plir::AsgUnit::Path(p)
                                }
                            },
                            ast::AsgUnit::Index(idx) => {
                                let (_, idx) = this.consume_index(Located::new(idx, range.clone()))?;
                                plir::AsgUnit::Index(idx)
                            },
                            ast::AsgUnit::Deref(d) => {
                                let deref = this.consume_deref(Located::new(d, range.clone()), e.ty.clone())?;
                                plir::AsgUnit::Deref(deref)
                            },
                        };
                        
                        let asg = plir::Expr::new(
                            e.ty.clone(),
                            plir::ExprType::Assign(unit, Box::new(e.0))
                        );

                        this.peek_block().push_stmt(Located::new(asg, range));
                        Ok(())
                    },
                    true,
                    range
                )?;
                
                let insert_block = self.pop_block().unwrap();
                self.consume_insert_block(insert_block, BlockBehavior::Bare)
                    .map(|b| plir::Expr::new(b.0.clone(), plir::ExprType::Block(b)))
            },
            ast::Expr::Path(p) => {
                self.consume_path(Located::new(p, range)).map(Into::into)
            },
            ast::Expr::StaticPath(sp) => {
                let ast::StaticPath { ty, attr } = sp;
                let ty = self.consume_type(ty)?;
                let ident = format!("{ty}::{attr}");
                Ok(plir::Path::Static(ty, attr, self.get_var_type(&ident, range)?.clone()))
                    .map(Into::into)
            },
            ast::Expr::UnaryOps { ops, expr } => {
                let e = self.consume_expr(*expr, None)?;
                
                ops.into_iter().rev()
                    .try_fold(e, |expr, op| self.apply_unary(expr, op, range.clone()))
            },
            ast::Expr::BinaryOp { op, left, right } => {
                let left = self.consume_expr(*left, None)?;
                let right = self.consume_expr(*right, None)?;
                self.apply_binary(op, left, right, range)
            },
            ast::Expr::Comparison { left, rights } => {
                let left = self.consume_expr_and_box(*left, None)?;
                let rights = rights.into_iter()
                    .map(|(op, right)| Ok((op, self.consume_expr(right, None)?)))
                    .collect::<PLIRResult<_>>()?;

                Ok(plir::Expr::new(
                    plir::ty!(plir::Type::S_BOOL),
                    plir::ExprType::Comparison { left, rights }
                ))
            },
            ast::Expr::Range { left, right, step } => {
                let left = self.consume_expr_and_box(*left, None)?;
                let right = self.consume_expr_and_box(*right, None)?;
                let step = match step {
                    Some(st) => Some(self.consume_expr_and_box(*st, None)?),
                    None => None,
                };

                let ty = plir::Type::resolve_collection_ty([&left.ty, &right.ty])
                    .ok_or(PLIRErr::CannotResolveType)?;

                Ok(plir::Expr::new(
                    plir::ty!(plir::Type::S_RANGE, [ty]),
                    plir::ExprType::Range { left, right, step }
                ))
            },
            ast::Expr::If { conditionals, last } => {
                let conditionals: Vec<_> = conditionals.into_iter()
                    .map(|(cond, block)| {
                        let c = self.consume_expr_truth(cond)?;
                        let b = self.consume_tree_block(block, BlockBehavior::Conditional, ctx_type.clone())?;
                        Ok((c, b))
                    })
                    .collect::<PLIRResult<_>>()?;
                
                let last = match last {
                    Some(blk) => Some(self.consume_tree_block(blk, BlockBehavior::Conditional, ctx_type)?),
                    None => None,
                };
                
                let _void = plir::ty!(plir::Type::S_VOID);
                let else_ty = last.as_ref()
                    .map(|b| &b.0)
                    .unwrap_or(&_void);

                let type_iter = conditionals.iter()
                    .map(|(_, block)| &block.0)
                    .chain(std::iter::once(else_ty));

                Ok(plir::Expr::new(
                    plir::Type::resolve_branches(type_iter).ok_or(PLIRErr::CannotResolveType)?,
                    plir::ExprType::If { conditionals, last }
                ))
            },
            ast::Expr::While { condition, block } => {
                let condition = self.consume_expr_truth(*condition)?;
                let block = self.consume_tree_block(block, BlockBehavior::Loop, None)?;

                Ok(plir::Expr::new(
                    plir::ty!(plir::Type::S_VOID), // TODO: plir::ty!(plir::Type::S_LIST, [block.0.clone()]),
                    plir::ExprType::While { condition: Box::new(condition), block }
                ))
            },
            ast::Expr::For { ident, iterator, block } => {
                let iterator = self.consume_expr_and_box(*iterator, None)?;
                let block = self.consume_tree_block(block, BlockBehavior::Loop, None)?;

                Ok(plir::Expr::new(
                    plir::ty!(plir::Type::S_VOID), // TODO: plir::ty!(plir::Type::S_LIST, [block.0.clone()]),
                    plir::ExprType::For { ident, iterator, block }
                ))
            },
            ast::Expr::Call { funct, params } => {
                let funct = match *funct {
                    // HACK: GEP
                    Located(ast::Expr::StaticPath(sp), path_range) if sp.attr == "#gep" => {
                        let tyrange = sp.ty.1.clone();
                        let ty = self.consume_type(sp.ty)?;

                        let can_gep = match ty.as_ref() {
                            plir::TypeRef::Generic("#ll_array", _) => true,
                            _ => matches!(self.get_class(&ty, tyrange)?.ty, TypeStructure::Class(_))
                        };

                        return if can_gep {
                            let _ptr = plir::ty!("#ptr");
                            let _int = plir::ty!(plir::Type::S_INT);

                            let mut piter = params.into_iter();
                            let ptr = piter.next()
                                .ok_or_else(|| PLIRErr::WrongArity(1, 0).at_range(range))
                                .and_then(|e| self.consume_located_expr(e, Some(_ptr.clone())))
                                .and_then(|le| {
                                    let Located(e, erange) = le;

                                    op_impl::apply_special_cast(e, &_ptr, CastType::Call)
                                    .map_err(|e| {
                                        PLIRErr::ExpectedType(_ptr.clone(), e.ty).at_range(erange)
                                    })
                                })
                                .map(Box::new)?;

                            let params = piter.map(|expr| {
                                let _int = plir::ty!(plir::Type::S_INT);
                                let Located(param, prange) = self.consume_located_expr(
                                    expr, Some(_int.clone())
                                )?;
                                
                                op_impl::apply_special_cast(param, &_int, CastType::Call)
                                    .map_err(|e| {
                                        PLIRErr::ExpectedType(_int.clone(), e.ty).at_range(prange)
                                    })
                            })
                            .collect::<Result<_, _>>()?;

                            Ok(plir::Expr::new(
                                plir::ty!("#ptr"),
                                plir::ExprType::GEP(ty, ptr, params)
                            ))
                        } else {
                            Err(PLIRErr::UndefinedAttr(ty, String::from("gep")).at_range(path_range))?
                        };
                    },
                    e => self.consume_located_expr(e, None)?
                };

                match &funct.ty {
                    plir::Type::Fun(plir::FunType { params: param_tys, ret: _, varargs }) => {
                        let bad_arity = if *varargs {
                            param_tys.len() > params.len()
                        } else {
                            param_tys.len() != params.len()
                        };

                        if bad_arity {
                            let err = PLIRErr::WrongArity(param_tys.len(), params.len()).at_range(range);
                            return Err(err);
                        }

                        let mut pty_iter = param_tys.iter();
                        let params = params.into_iter()
                            .map(|expr| {
                                let mpty = pty_iter.next();
                                let Located(param, prange) = self.consume_located_expr(expr, mpty.cloned())?;
                                
                                if let Some(pty) = mpty {
                                    op_impl::apply_special_cast(param, pty, CastType::Call)
                                        .map_err(|e| {
                                            PLIRErr::ExpectedType(pty.clone(), e.ty).at_range(prange)
                                        })
                                } else {
                                    Ok(param)
                                }
                            })
                            .collect::<Result<_, _>>()?;
                        
                        plir::Expr::call(funct, params)
                    },
                    t => Err(PLIRErr::CannotCall(t.clone()).at_range(funct.range()))
                }
            },
            ast::Expr::Index(idx) => {
                self.consume_index(Located::new(idx, range))
                    .map(|(ty, index)| plir::Expr::new(ty, plir::ExprType::Index(index)))
            },
            ast::Expr::Spread(_) => Err(PLIRErr::CannotSpread.at_range(range)),
            ast::Expr::Deref(d) => {
                let dty = ctx_type
                    .ok_or_else(|| PLIRErr::CannotResolveType.at_range(range.clone()))?;

                self.consume_deref(Located::new(d, range), dty)
                    .map(|deref| plir::Expr::new(deref.ty.clone(), plir::ExprType::Deref(deref)))
            },
        }
    }

    fn consume_located_expr(&mut self, expr: Located<ast::Expr>, ctx_type: Option<plir::Type>) -> PLIRResult<Located<plir::Expr>> {
        let range = expr.range();
        self.consume_expr(expr, ctx_type)
            .map(|e| Located::new(e, range))
    }
    fn consume_expr_and_box(&mut self, expr: Located<ast::Expr>, ctx_type: Option<plir::Type>) -> PLIRResult<Box<plir::Expr>> {
        self.consume_expr(expr, ctx_type).map(Box::new)
    }
    fn consume_expr_truth(&mut self, expr: Located<ast::Expr>) -> PLIRResult<plir::Expr> {
        self.consume_expr(expr, None)
            .map(|e| op_impl::apply_cast(e, &plir::ty!(plir::Type::S_BOOL)).unwrap())
    }

    fn consume_path(&mut self, p: Located<ast::Path>) -> PLIRResult<plir::Path> {
        let Located(ast::Path { obj, attrs }, expr_range) = p;

        let obj = self.consume_expr_and_box(*obj, None)?;
        let mut path = plir::Path::Struct(obj, vec![]);

        for attr in attrs {
            let top_ty = path.ty();
            let cls = self.get_class(&top_ty, expr_range.clone())?;

            if let Some(metref) = cls.get_method(&attr) {
                if matches!(path, plir::Path::Method(..)) {
                    Err(PLIRErr::CannotAccessOnMethod.at_range(expr_range.clone()))?;
                } else {
                    let metref = metref.to_string();
                    let mut fun_ty: plir::FunType = self.get_var_type(&metref, expr_range.clone())?
                        .clone()
                        .try_into()?;
                    fun_ty.pop_front();

                    path = plir::Path::Method(
                        Box::new(path.into()), 
                        attr.clone(), 
                        fun_ty
                    );
                }
            } else {
                let field = cls.get_field(&attr)
                    .map(|(i, t)| (i, t.clone()))
                    .ok_or_else(|| {
                        PLIRErr::UndefinedAttr(top_ty.into_owned(), attr.clone())
                            .at_range(expr_range.clone())
                    })?;
    
                path.add_struct_seg(field)
                    .map_err(|_| PLIRErr::CannotAccessOnMethod.at_range(expr_range.clone()))?;
            }
        }

        Ok(path)
    }

    fn consume_index(&mut self, idx: Located<ast::Index>) -> PLIRResult<(plir::Type, plir::Index)> {
        // Type signature is needed for assignment.

        let Located(ast::Index { expr, index }, expr_range) = idx;

        let expr = self.consume_located_expr(*expr, None)?;
        let index = self.consume_expr(*index, None)?;
        
        op_impl::apply_index(expr, index, expr_range)
    }

    fn consume_deref(&mut self, d: Located<ast::IDeref>, ty: plir::Type) -> PLIRResult<plir::IDeref> {
        let Located(ast::IDeref(e), _) = d;
        let Located(expr, expr_range) = self.consume_located_expr(*e, None)?;

        if let plir::TypeRef::Prim("#ptr") = expr.ty.as_ref() {
            Ok(plir::IDeref { expr: Box::new(expr), ty })
        } else {
            Err(PLIRErr::CannotDeref.at_range(expr_range))
        }
    }
}

impl Default for PLIRCodegen {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use inkwell::context::Context;

    use crate::compiler::Compiler;
    use crate::test_utils::prelude::*;

    load_tests!("codegen", 
        TESTS = "_test_files/plir_llvm/codegen.gon"
        EARLY_EXIT_TESTS = "_test_files/plir_llvm/early_exits.gon"
        TYPE_TESTS = "_test_files/plir_llvm/compiler_types.gon"
    );

    fn cg_test(test: Test) -> TestResult<()> {
        let ctx = Context::create();
        let mut compiler = Compiler::new(&ctx, "eval")
            .map_err(|e| test.wrap_compile_err(e))?;
        let compile_result = compiler.generate_plir(test.source())
            .map_err(|e| test.wrap_compile_err(e));

        println!("=== {} {} ===",  test.header.name, if compile_result.is_ok() { "PASS" } else { "FAIL" });

        match &compile_result {
            Ok((plir, _))  => println!("{plir}"),
            Err(e) => println!("{e:?}\n"),
        }
        
        compile_result.map(|_| ())
    }

    #[test]
    fn basic_pass() -> TestResult<()> {
        TESTS.pass_all(cg_test, &[
            "basic_if", 
            "basic_while", 
            "basic_access", 
            "basic_arith_chain", 
            "basic_pattern", 
            "basic_block", 
            "basic_extern",
            "basic_logic_cmp",
        ])
    }

    #[test]
    fn early_exit() -> TestResult<()> {
        EARLY_EXIT_TESTS.pass_all(cg_test, &[
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
    fn recursive_funs() -> TestResult<()> {
        TESTS.pass_all(cg_test, &[
            "fun_recursion_inf",
            "recursive_fib",
            "hoist_block"
        ])?;
        TESTS.fail_all(cg_test, &[
            "hoist_block_fail"
        ])
    }

    #[test]
    fn type_test() -> TestResult<()> {
        TYPE_TESTS.pass_all(cg_test, &[
            "class_chain",
            "initializer",
            "class_operator_overloading",
            "method_access",
            "decl_cast_check",
            "fun_cast_check",
            "type_res",
            "fun_call"
        ])?;

        TYPE_TESTS.fail_all(cg_test, &[
            "class_operator_overloading_fail",
            "decl_cast_check_fail_1",
            "decl_cast_check_fail_2",
            "decl_cast_check_fail_3",
            "decl_cast_check_fail_4",
            "decl_cast_check_fail_5",
            "fun_cast_check_fail_1",
            "fun_cast_check_fail_2",
            "fun_cast_check_fail_3",
            "fun_cast_check_fail_4",
            "type_res_fail_1",
            "type_res_fail_2",
            "type_res_fail_3",
            "type_res_fail_4",
            "fun_call_fail_1",
            "fun_call_fail_2",
            "fun_call_fail_3"
        ])
    }
}