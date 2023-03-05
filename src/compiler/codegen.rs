//! Converts the AST tree into an intermediate language 
//! (Poligon Language Intermediate Representation).
//! 
//! This makes it simpler to later convert into LLVM.
//! 
//! The main function that performs the conversion is [`codegen`], 
//! which utilizes the [`CodeGenerator`] struct.

mod op_impl;

use std::borrow::Cow;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::{self, ReasgType, MutType};
use crate::err::{GonErr, FullGonErr, full_gon_cast_impl, CursorRange};

pub(crate) use self::op_impl::{CastType, OpErr};

use super::plir::{self, Located};

/// Produce the PLIR tree from the AST tree.
pub fn codegen(t: ast::Program) -> PLIRResult<plir::Program> {
    let mut cg = CodeGenerator::new();
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
    /// Cannot spread here.
    CannotSpread,
    /// Cannot use class initializer syntax on this type.
    CannotInitialize(plir::Type),
    /// The field was not initialized on the type.
    UninitializedField(plir::Type, String),
    /// The field was unexpectedly initialized on the type.
    UnexpectedField(plir::Type, String),
    /// Operation between two types cannot be computed.
    OpErr(OpErr)
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
            | PLIRErr::CannotInitialize(_)
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

    fn message(&self) -> String {
        match self {
            PLIRErr::CannotBreak => String::from("cannot 'break' here"),
            PLIRErr::CannotContinue => String::from("cannot 'continue' here"),
            PLIRErr::CannotReturn => String::from("cannot 'return' here"),
            PLIRErr::ExpectedType(e, f) => format!("expected type '{e}', but got '{f}'"),
            PLIRErr::CannotResolveType => String::from("cannot determine type"),
            PLIRErr::UndefinedVar(name) => format!("could not find identifier '{name}'"),
            PLIRErr::UndefinedType(name) => format!("could not find type '{name}'"),
            PLIRErr::UndefinedAttr(t, name) => format!("could not find attribute '{name}' on '{t}'"),
            PLIRErr::CannotAccessOnMethod => String::from("cannot access on method"),
            PLIRErr::CannotAssignToMethod => String::from("cannot assign to method"),
            PLIRErr::CannotCall(t) => format!("cannot call value of type '{t}'"),
            PLIRErr::WrongArity(e, g) => format!("wrong number of parameters - expected {e}, got {g}"),
            PLIRErr::CannotSpread => String::from("cannot spread here"),
            PLIRErr::CannotInitialize(t) => format!("cannot use initializer syntax on type '{t}'"),
            PLIRErr::UninitializedField(t, f) => format!("uninitialized field '{f}' on type '{t}'"),
            PLIRErr::UnexpectedField(t, f) => format!("field '{f}' is not present on type '{t}'"),
            PLIRErr::OpErr(e) => e.message(),
        }
    }
}

#[derive(Debug)]
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
    fn handle_exit(&self, exit: BlockExit) -> PLIRResult<BlockExitHandle> {
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
    FunBlock(plir::FunSignature, Rc<ast::Block>)
}

#[derive(Debug)]
struct InsertBlock {
    block: Vec<plir::ProcStmt>,

    /// All conditional exits.
    exits: Vec<BlockExit>,
    /// The *unconditional* exit.
    /// If this is present, this is the last statement of the block.
    /// If a conditional exit does not pass, this exit is how the block exits.
    final_exit: Option<BlockExit>,

    vars: HashMap<String, plir::Type>,
    types: HashMap<String, TypeData>,
    unresolved: HashMap<String, Unresolved>,
}

impl InsertBlock {
    fn new() -> Self {
        Self {
            block: vec![],
            exits: vec![],
            final_exit: None,
            vars: HashMap::new(),
            types: HashMap::new(),
            unresolved: HashMap::new()
        }
    }

    fn top() -> Self {
        Self {
            block: vec![],
            exits: vec![],
            final_exit: None,
            vars: HashMap::new(),
            types: primitives(&[
                plir::Type::S_INT,
                plir::Type::S_FLOAT,
                plir::Type::S_BOOL,
                plir::Type::S_CHAR,
                plir::Type::S_STR,
            ]),
            unresolved: HashMap::new()
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
            Some(ProcStmt::Return(_))        => never_ty(),
            Some(ProcStmt::Break)            => never_ty(),
            Some(ProcStmt::Continue)         => never_ty(),
            Some(ProcStmt::Exit(_))          => never_ty(),
            Some(ProcStmt::Expr(e))          => Cow::Borrowed(&e.ty),
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
    fn push_stmt(&mut self, stmt: impl Into<plir::ProcStmt>) -> bool {
        use plir::ProcStmt;
        match stmt.into() {
            ProcStmt::Return(e) => self.push_return(e),
            ProcStmt::Break => self.push_break(),
            ProcStmt::Continue => self.push_cont(),
            st => {
                if self.is_open() {
                    self.block.push(st)
                }

                self.is_open()
            }
        }
    }

    /// Push a return statement into this insert block.
    /// 
    /// The return will be false, indicating another statement cannot
    /// be pushed into the insert block (as a final exit has been set).
    fn push_return(&mut self, me: Option<plir::Expr>) -> bool {
        if self.is_open() {
            let ty = match me {
                Some(ref e) => e.ty.clone(),
                None => plir::ty!(plir::Type::S_VOID),
            };
            self.block.push(plir::ProcStmt::Return(me));
            self.final_exit.replace(BlockExit::Return(ty));
        }

        false
    }

    /// Push a break statement into this insert block.
    /// 
    /// The return will be false, indicating another statement cannot
    /// be pushed into the insert block (as a final exit has been set).
    fn push_break(&mut self) -> bool {
        if self.is_open() {
            self.block.push(plir::ProcStmt::Break);
            self.final_exit.replace(BlockExit::Break);
        }

        false
    }

    /// Push a continue statement into this insert block.
    /// 
    /// The return will be false, indicating another statement cannot
    /// be pushed into the insert block (as a final exit has been set).
    fn push_cont(&mut self) -> bool {
        if self.is_open() {
            self.block.push(plir::ProcStmt::Continue);
            self.final_exit.replace(BlockExit::Continue);
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
        };

        self.unresolved.insert(k.clone(), unresolved);
    }

    /// Insert a class into the insert block's type register.
    fn insert_class(&mut self, cls: &plir::Class) {
        self.types.insert(cls.ident.clone(), TypeData::structural(cls.clone()));
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

/// This struct does the actual conversion from AST to PLIR.
pub struct CodeGenerator {
    program: InsertBlock,
    globals: Vec<plir::HoistedStmt>,
    blocks: Vec<InsertBlock>,
    var_id: usize,

    // steps: HashMap<*const plir::Expr, usize>
}

impl CodeGenerator {
    /// Creates a new instance of the CodeGenerator.
    pub fn new() -> Self {
        Self { 
            program: InsertBlock::top(), 
            globals: vec![],
            blocks: vec![],
            var_id: 0
        }
    }

    /// Takes out the generated [`plir::Program`] from this struct.
    pub fn unwrap(self) -> PLIRResult<plir::Program> {
        assert!(self.blocks.is_empty(),
            "insert block was opened but not properly closed"
        );

        let InsertBlock {block, exits, unresolved, .. } = self.program;
        debug_assert!(unresolved.is_empty(), "there was an unresolved item in block");

        match exits.last() {
            None => Ok(plir::Program(self.globals, block)),
            Some(BlockExit::Break)     => Err(PLIRErr::CannotBreak),
            Some(BlockExit::Continue)  => Err(PLIRErr::CannotContinue),
            Some(BlockExit::Return(_)) => Err(PLIRErr::CannotReturn),
            Some(BlockExit::Exit(_))   => Err(PLIRErr::CannotReturn),
        }
    }

    fn push_block(&mut self) {
        self.blocks.push(InsertBlock::new())
    }
    fn pop_block(&mut self) -> Option<InsertBlock> {
        self.blocks.pop()
    }
    fn peek_block(&mut self) -> &mut InsertBlock {
        self.blocks.last_mut().unwrap_or(&mut self.program)
    }

    /// Declares a variable with a given type.
    fn declare(&mut self, ident: &str, ty: plir::Type) {
        self.peek_block().vars.insert(String::from(ident), ty);
    }

    /// If there is an unresolved structure present at the identifier, try to resolve it.
    fn resolve_ident(&mut self, ident: &str) -> PLIRResult<()> {
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
            }

            // revert peek block after
            self.blocks.extend(storage);
        }

        Ok(())
    }

    /// [`CodeGenerator::resolve_ident`], but using a type parameter
    fn resolve_ty(&mut self, ty: &plir::Type) -> PLIRResult<()> {
        self.resolve_ident(&ty.ident())
    }

    /// Find a specific item, starting from the deepest scope and scaling out.
    fn find_scoped<'a, T>(&'a self, f: impl FnMut(&'a InsertBlock) -> Option<T>) -> Option<T> {
        self.blocks.iter().rev()
            .chain(std::iter::once(&self.program))
            .find_map(f)
    }

    fn get_var_type(&mut self, ident: &str, range: CursorRange) -> PLIRResult<&plir::Type> {
        self.resolve_ident(ident)?;

        self.find_scoped(|ib| ib.vars.get(ident))
            .ok_or_else(|| PLIRErr::UndefinedVar(String::from(ident)).at_range(range))
    }

    /// Identifier needs to be resolved beforehand, via [`CodeGenerator::resolve_ident`].
    fn get_var_type_opt(&mut self, ident: &str) -> Option<&plir::Type> {
        self.find_scoped(|ib| ib.vars.get(ident))
    }

    fn get_class(&mut self, ty: &plir::Type, range: CursorRange) -> PLIRResult<&TypeData> {
        self.resolve_ty(ty)?;

        match ty {
            plir::Type::Prim(ident) => self.find_scoped(|ib| ib.types.get(ident))
                .ok_or_else(|| {
                    PLIRErr::UndefinedType(String::from(ident))
                        .at_range(range)
                }),
            _ => todo!()
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

        self.peek_block().push_stmt(decl);

        Var {
            ident,
            ty: ety,
            decl_range
        }
    }

    fn push_global(&mut self, global: impl Into<plir::HoistedStmt>) {
        self.globals.push(global.into())
    }

    /// Convert a program into PLIR, and attach it to the CodeGenerator.
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
            match stmt {
                Located(ast::Stmt::FunDecl(fd), _) => {
                    self.peek_block().insert_unresolved(Unresolved::Fun(fd));
                },
                Located(ast::Stmt::ExternFunDecl(fs), _) => {
                    self.peek_block().insert_unresolved(Unresolved::ExternFun(fs));
                    }
                Located(ast::Stmt::ClassDecl(cls), _) => {
                    self.peek_block().insert_unresolved(Unresolved::Class(cls));
                }
                s => eager_stmts.push(s),
            }
        }

        for stmt in eager_stmts {
            if !self.consume_eager_stmt(stmt)? {
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
            }
            unresolved = &mut self.peek_block().unresolved;
        }

        Ok(())
    }

    /// Consume a statement into the current insert block.
    /// 
    /// This function returns whether or not the insert block accepts any more statements.
    fn consume_eager_stmt(&mut self, stmt: Located<ast::Stmt>) -> PLIRResult<bool> {
        let Located(stmt, range) = stmt;

        match stmt {
            ast::Stmt::Decl(d) => self.consume_decl(Located::new(d, range)),
            ast::Stmt::Return(me) => {
                let maybe_expr = match me {
                    Some(e) => Some(self.consume_expr(e)?),
                    None => None,
                };
                Ok(self.peek_block().push_return(maybe_expr))
            },
            ast::Stmt::Break => {
                Ok(self.peek_block().push_break())
            },
            ast::Stmt::Continue => {
                Ok(self.peek_block().push_cont())
            },
            ast::Stmt::Expr(e) => {
                let e = self.consume_expr(e)?;
                Ok(self.peek_block().push_stmt(e))
            },
            ast::Stmt::FunDecl(_) => unimplemented!("fun decl should not be resolved eagerly"),
            ast::Stmt::ExternFunDecl(_) => unimplemented!("extern fun decl should not be resolved eagerly"),
            ast::Stmt::ClassDecl(_) => unimplemented!("class decl should not be resolved eagerly"),
        }
    }

    fn consume_insert_block(
        &mut self, 
        block: InsertBlock, 
        btype: BlockBehavior, 
        expected_ty: Option<plir::Type>
    ) -> PLIRResult<plir::Block> {
        let InsertBlock { 
            mut block, exits, final_exit, 
            vars: _, types: _, unresolved 
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
                BlockExit::Return(exit_ty)
            } else {
                block.push(ProcStmt::Exit(exit_expr));
                BlockExit::Exit(exit_ty)
            }
        });

        // Type check block:
        if let Some(exp_ty) = expected_ty {
            match &mut final_exit {
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
                            Err(_) => Err(PLIRErr::ExpectedType(exp_ty, exit_ty.clone()))?,
                        },
                        // exit_ty is void, and we already checked that exp_ty is not void
                        None => Err(PLIRErr::ExpectedType(exp_ty, exit_ty.clone()))?,
                    }
                },
                // what is done here?
                BlockExit::Break => {},
                BlockExit::Continue => {},
            }
        }

        // resolve block's types and handle the methods of exiting the block
        let mut type_branches = vec![];
        for exit in exits {
            match btype.handle_exit(exit)? {
                BlockExitHandle::Continue(ty) => type_branches.push(ty),
                BlockExitHandle::LoopExit => {},

                // second parameter does not matter here because
                // this is already conditional.
                // as such, just propagate as a conditional exit.
                BlockExitHandle::Propagate(exit, _) => {
                    self.peek_block().exits.push(exit);
                },
            }
        }
        match btype.handle_exit(final_exit)? {
            BlockExitHandle::Continue(ty) => type_branches.push(ty),
            BlockExitHandle::LoopExit => {},
            BlockExitHandle::Propagate(exit, conditional) => {
                if conditional {
                    self.peek_block().exits.push(exit);
                } else {
                    self.peek_block().final_exit.replace(exit);
                }
            },
        }

        let bty = plir::Type::resolve_branches(&type_branches)
            .ok_or(PLIRErr::CannotResolveType)?;
        Ok(plir::Block(bty, block))
    }
    fn consume_tree_block(
        &mut self, 
        block: ast::Block, 
        btype: BlockBehavior, 
        expected_ty: Option<plir::Type>
    ) -> PLIRResult<plir::Block> {
        self.push_block();
        // collect all the statements from this block
        self.consume_stmts(block.0)?;
        let insert_block = self.pop_block().unwrap();
        self.consume_insert_block(insert_block, btype, expected_ty)
    }

    fn unpack_pat<T, E>(
        &mut self, 
        pat: Located<ast::Pat<T>>, 
        expr: Located<plir::Expr>, 
        extra: E, 
        mut split_extra: impl FnMut(&E, plir::Split) -> PLIRResult<E>,
        mut map: impl FnMut(&mut Self, Located<T>, Located<plir::Expr>, E) -> PLIRResult<()>,
        consume_var: bool,
        stmt_range: CursorRange
    ) -> PLIRResult<()> {
        self.unpack_pat_inner(pat, expr, extra, &mut split_extra, &mut map, consume_var, stmt_range)
    }

    fn unpack_pat_inner<T, E>(
        &mut self, 
        pat: Located<ast::Pat<T>>, 
        expr: Located<plir::Expr>, 
        extra: E, 
        split_extra: &mut impl FnMut(&E, plir::Split) -> PLIRResult<E>,
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
            ast::Pat::Unit(t) => map(self, Located::new(t, range), expr, extra),
            ast::Pat::Spread(spread) => match spread {
                Some(pat) => self.unpack_pat_inner(*pat, expr, extra, split_extra, map, consume_var, stmt_range),
                None => Ok(()),
            },
            ast::Pat::List(pats) => {
                let var = expr.map(|e| self.push_tmp_decl("decl", e, stmt_range.clone()));

                let delocated: Vec<_> = pats.iter().map(|t| &t.0).collect();
                for (idx, pat) in std::iter::zip(create_splits(&delocated), pats) {
                    let rhs = var.map(|v| v.clone().split(idx))
                        .transpose_result()?;

                    let extr = split_extra(&extra, idx)?;
                    self.unpack_pat_inner(pat, rhs, extr, split_extra, map, false, stmt_range.clone())?;
                }

                if consume_var {
                    self.peek_block().push_stmt(var.into_expr());
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
        
        let e = self.consume_located_expr(val)?;
        let ty = match ty {
            Some(t) => Some(self.consume_type(t)?),
            None => None,
        };

        self.unpack_pat(pat, e, (rt, ty), 
            |(rt, mty), idx| {
                Ok((*rt, match mty {
                    Some(t) => Some(t.split(idx).map_err(|e| e.at_range(decl_range))?),
                    None => None,
                }))
            }, 
            |this, unit, e, extra| {
                let Located(ast::DeclUnit(ident, mt), urange) = unit;
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
                this.peek_block().push_stmt(decl);

                Ok(())
            },
            false,
            decl_range
        )?;

        Ok(self.peek_block().is_open())
    }

    /// Consume a function signature and convert it into a PLIR function signature.
    fn consume_fun_sig(&mut self, sig: ast::FunSignature) -> PLIRResult<plir::FunSignature> {
        let ast::FunSignature { ident, params, ret } = sig;
        
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
            plir::Type::fun_type(param_tys, ret.clone())
        );

        Ok(plir::FunSignature { ident, params, ret })
    }
    /// Consume a function declaration statement into the current insert block.
    /// 
    /// This function returns whether or not the insert block accepts any more statements.
    fn consume_fun_block(&mut self, sig: plir::FunSignature, block: Rc<ast::Block>) -> PLIRResult<bool> {
        let old_block = std::rc::Rc::try_unwrap(block)
            .expect("AST function declaration block was unexpectedly in use and cannot be consumed into PLIR.");
    
        let block = {
            self.push_block();

            for plir::Param { ident, ty, .. } in sig.params.iter() {
                self.declare(ident, ty.clone());
            }
    
            // collect all the statements from this block
            self.consume_stmts(old_block.0)?;
    
            let insert_block = self.pop_block().unwrap();
            self.consume_insert_block(insert_block, BlockBehavior::Function, Some(sig.ret.clone()))?
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
                    ty: Some(Located::new(ast::Type(cls.ident.clone(), vec![]), (0, 0) ..= (0, 0)))
                });
            } else {
                // TODO, use this ident
            };
            let metref = format!("{}::{method_name}", &cls.ident);

            let sig = ast::FunSignature { ident: metref.clone(), params, ret };
            let decl = ast::FunDecl { sig, block };

            ib.insert_unresolved(Unresolved::Fun(decl));

            if let Some(c) = ib.types.get_mut(&cls.ident) {
                c.insert_method(method_name, metref);
            }
        }

        self.push_global(cls);
        Ok(self.peek_block().is_open())
    }

    fn consume_expr(&mut self, value: Located<ast::Expr>) -> PLIRResult<plir::Expr> {
        let Located(expr, range) = value;
        
        match expr {
            ast::Expr::Ident(ident) => {
                Ok(plir::Expr::new(
                    self.get_var_type(&ident, range)?.clone(),
                    plir::ExprType::Ident(ident)
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

                Ok(plir::Expr::new(
                    ty, plir::ExprType::Literal(literal)
                ))
            },
            ast::Expr::ListLiteral(lst) => {
                let new_inner: Vec<_> = lst.into_iter()
                    .map(|e| self.consume_expr(e))
                    .collect::<Result<_, _>>()?;

                let elem_ty = plir::Type::resolve_collection_ty(new_inner.iter().map(|e| &e.ty))
                    .ok_or(PLIRErr::CannotResolveType)?;

                Ok(plir::Expr::new(
                    plir::ty!(plir::Type::S_LIST, [elem_ty]),
                    plir::ExprType::ListLiteral(new_inner)
                ))
            },
            ast::Expr::SetLiteral(set) => {
                let new_inner: Vec<_> = set.into_iter()
                    .map(|e| self.consume_expr(e))
                    .collect::<Result<_, _>>()?;

                let elem_ty = plir::Type::resolve_collection_ty(new_inner.iter().map(|e| &e.ty))
                    .ok_or(PLIRErr::CannotResolveType)?;

                Ok(plir::Expr::new(
                    plir::ty!(plir::Type::S_SET, [elem_ty]),
                    plir::ExprType::SetLiteral(new_inner)
                ))
            },
            ast::Expr::DictLiteral(entries) => {
                let new_inner: Vec<_> = entries.into_iter()
                    .map(|(k, v)| Ok((self.consume_expr(k)?, self.consume_expr(v)?)))
                    .collect::<PLIRResult<_>>()?;

                let (key_tys, val_tys): (Vec<_>, Vec<_>) = new_inner.iter()
                    .map(|(k, v)| (&k.ty, &v.ty))
                    .unzip();
                let key_ty = plir::Type::resolve_collection_ty(key_tys)
                    .ok_or(PLIRErr::CannotResolveType)?;
                let val_ty = plir::Type::resolve_collection_ty(val_tys)
                    .ok_or(PLIRErr::CannotResolveType)?;

                Ok(plir::Expr::new(
                    plir::ty!(plir::Type::S_DICT, [key_ty, val_ty]),
                    plir::ExprType::DictLiteral(new_inner)
                ))
            },
            ast::Expr::ClassLiteral(ty, entries) => {
                let tyrange = ty.range();
                let ty = self.consume_type(ty)?;

                let mut entries: HashMap<_, _> = entries.into_iter()
                    .map(|(Located(k, krange), v)| {
                        Ok((k, (krange, self.consume_located_expr(v)?)))
                    })
                    .collect::<PLIRResult<_>>()?;
                
                let cls = self.get_class(&ty, tyrange.clone())?;
                let fields = cls.fields()
                    .ok_or_else(|| {
                        PLIRErr::CannotInitialize(ty.clone()).at_range(tyrange.clone())
                    })?;
                
                let new_entries = fields.iter()
                    .map(|(k, fd)| {
                        let (_, Located(fexpr, erange)) = entries.remove(k)
                            .ok_or_else(|| {
                                PLIRErr::UninitializedField(ty.clone(), k.clone())
                                    .at_range(tyrange.clone())
                            })?;
                        
                        let fexpr = op_impl::apply_special_cast(fexpr, &fd.ty, CastType::Decl)
                            .map_err(|e| PLIRErr::ExpectedType(fd.ty.clone(), e.ty).at_range(erange))?;

                        Ok(fexpr)
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
                let expr = self.consume_located_expr(*expr)?;

                self.push_block();
                self.unpack_pat(pat, expr, (), |_, _| Ok(()),
                    |this, unit, e, _| {
                        let Located(unit, range) = unit;
                        let unit = match unit {
                            ast::AsgUnit::Ident(ident) => plir::AsgUnit::Ident(ident),
                            ast::AsgUnit::Path(p) => {
                                let p = this.consume_path(Located::new(p, range.clone()))?;
                                if matches!(p, plir::Path::Method(..)) {
                                    Err(PLIRErr::CannotAssignToMethod.at_range(range))?
                                } else {
                                    plir::AsgUnit::Path(p)
                                }
                            },
                            ast::AsgUnit::Index(idx) => {
                                let (_, idx) = this.consume_index(idx, range)?;
                                plir::AsgUnit::Index(idx)
                            },
                        };
                        
                        let asg = plir::Expr::new(
                            e.ty.clone(),
                            plir::ExprType::Assign(unit, Box::new(e.0))
                        );

                        this.peek_block().push_stmt(asg);
                        Ok(())
                    },
                    true,
                    range
                )?;
                
                let insert_block = self.pop_block().unwrap();
                self.consume_insert_block(insert_block, BlockBehavior::Bare, None)
                    .map(|b| plir::Expr::new(b.0.clone(), plir::ExprType::Block(b)))
            },
            ast::Expr::Path(p) => {
                self.consume_path(Located::new(p, range)).map(Into::into)
            },
            ast::Expr::StaticPath(ty, attr) => {
                let ty = self.consume_type(ty)?;
                let ident = format!("{ty}::{attr}");
                Ok(plir::Path::Static(ty, attr, self.get_var_type(&ident, range)?.clone()))
                    .map(Into::into)
            },
            ast::Expr::UnaryOps { ops, expr } => {
                let e = self.consume_expr(*expr)?;
                
                ops.into_iter().rev()
                    .try_fold(e, |expr, op| self.apply_unary(expr, op, range.clone()))
            },
            ast::Expr::BinaryOp { op, left, right } => {
                let left = self.consume_expr(*left)?;
                let right = self.consume_expr(*right)?;
                self.apply_binary(op, left, right, range)
            },
            ast::Expr::Comparison { left, rights } => {
                let left = self.consume_expr_and_box(*left)?;
                let rights = rights.into_iter()
                    .map(|(op, right)| Ok((op, self.consume_expr(right)?)))
                    .collect::<PLIRResult<_>>()?;

                Ok(plir::Expr::new(
                    plir::ty!(plir::Type::S_BOOL),
                    plir::ExprType::Comparison { left, rights }
                ))
            },
            ast::Expr::Range { left, right, step } => {
                let left = self.consume_expr_and_box(*left)?;
                let right = self.consume_expr_and_box(*right)?;
                let step = match step {
                    Some(st) => Some(self.consume_expr_and_box(*st)?),
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
                        let c = self.consume_expr(cond)?;
                        let b = self.consume_tree_block(block, BlockBehavior::Conditional, None)?;
                        Ok((c, b))
                    })
                    .collect::<PLIRResult<_>>()?;
                
                let last = match last {
                    Some(blk) => Some(self.consume_tree_block(blk, BlockBehavior::Conditional, None)?),
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
                let condition = self.consume_expr(*condition)?;
                let block = self.consume_tree_block(block, BlockBehavior::Loop, None)?;

                Ok(plir::Expr::new(
                    plir::ty!(plir::Type::S_VOID), // TODO: plir::ty!(plir::Type::S_LIST, [block.0.clone()]),
                    plir::ExprType::While { condition: Box::new(condition), block }
                ))
            },
            ast::Expr::For { ident, iterator, block } => {
                let iterator = self.consume_expr_and_box(*iterator)?;
                let block = self.consume_tree_block(block, BlockBehavior::Loop, None)?;

                Ok(plir::Expr::new(
                    plir::ty!(plir::Type::S_VOID), // TODO: plir::ty!(plir::Type::S_LIST, [block.0.clone()]),
                    plir::ExprType::For { ident, iterator, block }
                ))
            },
            ast::Expr::Call { funct, params } => {
                let funct = self.consume_located_expr(*funct)?;

                match &funct.ty {
                    plir::Type::Fun(plir::FunType(param_tys, _)) => {
                        if param_tys.len() != params.len() {
                            let err = PLIRErr::WrongArity(param_tys.len(), params.len()).at_range(range);
                            return Err(err);
                        }

                        let params = std::iter::zip(param_tys.iter(), params)
                            .map(|(pty, expr)| {
                                let Located(param, prange) = self.consume_located_expr(expr)?;
                                op_impl::apply_special_cast(param, pty, CastType::Call)
                                    .map_err(|e| {
                                        PLIRErr::ExpectedType(pty.clone(), e.ty).at_range(prange)
                                    })
                            })
                            .collect::<Result<_, _>>()?;

                        plir::Expr::call(funct, params)
                    },
                    t => Err(PLIRErr::CannotCall(t.clone()).at_range(funct.range()))
                }
            },
            ast::Expr::Index(idx) => {
                self.consume_index(idx, range)
                    .map(|(ty, index)| plir::Expr::new(ty, plir::ExprType::Index(index)))
            },
            ast::Expr::Spread(_) => Err(PLIRErr::CannotSpread.at_range(range)),
        }
    }

    fn consume_located_expr(&mut self, expr: Located<ast::Expr>) -> PLIRResult<Located<plir::Expr>> {
        let range = expr.range();
        self.consume_expr(expr)
            .map(|e| Located::new(e, range))
    }
    fn consume_expr_and_box(&mut self, expr: Located<ast::Expr>) -> PLIRResult<Box<plir::Expr>> {
        self.consume_expr(expr).map(Box::new)
    }

    fn consume_path(&mut self, p: Located<ast::Path>) -> PLIRResult<plir::Path> {
        let Located(ast::Path { obj, attrs }, expr_range) = p;

        let obj = self.consume_expr_and_box(*obj)?;
        let mut path = plir::Path::Struct(obj, vec![]);

        for attr in attrs {
            let top_ty = path.ty();
            let cls = self.get_class(&top_ty, expr_range.clone())?;

            if let Some(metref) = cls.get_method(&attr) {
                if matches!(path, plir::Path::Method(..)) {
                    Err(PLIRErr::CannotAccessOnMethod)?;
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
                    .ok_or_else(|| PLIRErr::UndefinedAttr(top_ty.into_owned(), attr.clone()))?;
    
                path.add_struct_seg(field)
                    .map_err(|_| PLIRErr::CannotAccessOnMethod)?;
            }
        }

        Ok(path)
    }
    fn consume_index(&mut self, idx: ast::Index, expr_range: CursorRange) -> PLIRResult<(plir::Type, plir::Index)> {
        // Type signature is needed for assignment.

        let ast::Index { expr, index } = idx;

        let expr = self.consume_located_expr(*expr)?;
        let index = self.consume_expr(*index)?;
        
        op_impl::apply_index(expr, index, expr_range)
    }
}

impl Default for CodeGenerator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use crate::test_utils::prelude::*;

    load_tests!("codegen", 
        TESTS = "_test_files/plir_llvm/codegen.gon"
        EARLY_EXIT_TESTS = "_test_files/plir_llvm/early_exits.gon"
        TYPE_TESTS = "_test_files/plir_llvm/compiler_types.gon"
    );

    fn cg_test(test: Test) -> TestResult<()> {
        let cg = test.codegen();
        println!("=== {} {} ===",  test.header.name, if cg.is_ok() { "PASS" } else { "FAIL" });

        match &cg {
            Ok(t)  => println!("{t}"),
            Err(e) => println!("{e:?}\n"),
        }
        
        cg.map(|_| ())
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