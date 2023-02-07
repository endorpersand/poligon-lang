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
use crate::err::GonErr;

pub(crate) use self::op_impl::{CastType, OpErr};

use super::plir;

/// Produce the PLIR tree from the AST tree.
pub fn codegen(t: ast::Program) -> PLIRResult<plir::Program> {
    let mut cg = CodeGenerator::new();
    cg.consume_program(t)?;
    cg.unwrap()
}

fn create_splits<T>(pats: &[ast::Pat<T>]) -> Vec<plir::Split> {
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
    /// Cannot spread here.
    CannotSpread,
    /// Operation between two types cannot be computed.
    OpErr(OpErr)
}
/// A [`Result`] type for operations in the PLIR tree creation process.
pub type PLIRResult<T> = Result<T, PLIRErr>;

impl From<OpErr> for PLIRErr {
    fn from(err: OpErr) -> Self {
        Self::OpErr(err)
    }
}

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
            => "type error",
            
            | PLIRErr::UndefinedVar(_)
            | PLIRErr::UndefinedAttr(_, _)
            => "name error",
            
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
            PLIRErr::UndefinedVar(name) => format!("could not find variable '{name}'"),
            PLIRErr::UndefinedType(name) => format!("could not find type '{name}'"),
            PLIRErr::UndefinedAttr(t, name) => format!("could not find attribute '{name}' on '{t}'"),
            PLIRErr::CannotAccessOnMethod => String::from("cannot access on method"),
            PLIRErr::CannotAssignToMethod => String::from("cannot assign to method"),
            PLIRErr::CannotCall(t) => format!("cannot call value of type '{t}'"),
            PLIRErr::CannotSpread => String::from("cannot spread here"),
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

fn primitives(prims: &[&str]) -> HashMap<String, Class> {
    prims.iter()
        .map(|&s| (String::from(s), Class::primitive()))
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
    init_block: Vec<plir::Stmt>,
    block: Vec<plir::Stmt>,

    /// All conditional exits.
    exits: Vec<BlockExit>,
    /// The *unconditional* exit.
    /// If this is present, this is the last statement of the block.
    /// If a conditional exit does not pass, this exit is how the block exits.
    final_exit: Option<BlockExit>,

    vars: HashMap<String, plir::Type>,
    classes: HashMap<String, Class>,
    unresolved: HashMap<String, Unresolved>,
}

impl InsertBlock {
    fn new() -> Self {
        Self {
            init_block: vec![],
            block: vec![],
            exits: vec![],
            final_exit: None,
            vars: HashMap::new(),
            classes: HashMap::new(),
            unresolved: HashMap::new()
        }
    }

    fn top() -> Self {
        Self {
            init_block: vec![],
            block: vec![],
            exits: vec![],
            final_exit: None,
            vars: HashMap::new(),
            classes: primitives(&[
                plir::Type::S_INT,
                plir::Type::S_FLOAT,
                plir::Type::S_BOOL,
                plir::Type::S_CHAR,
                plir::Type::S_STR,
            ]),
            unresolved: HashMap::new()
        }
    }
    /// Determine whether another statement can be pushed into the insert block.
    fn is_open(&self) -> bool {
        self.final_exit.is_none()
    }

    /// Push a singular statement into this insert block.
    /// 
    /// The return indicates whether or not another statement 
    /// can be pushed into the insert block (whether a final exit has been set).
    fn push_stmt(&mut self, stmt: plir::Stmt) -> bool {
        match stmt {
            plir::Stmt::Return(e) => self.push_return(e),
            plir::Stmt::Break => self.push_break(),
            plir::Stmt::Continue => self.push_cont(),
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
            self.block.push(plir::Stmt::Return(me));
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
            self.block.push(plir::Stmt::Break);
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
            self.block.push(plir::Stmt::Continue);
            self.final_exit.replace(BlockExit::Continue);
        }

        false
    }

    /// Push a singular statement into this insert block, at the front.
    /// 
    /// This should only be used for statements computed lazily (fun decl, classes, etc.)
    fn push_lazy_stmt(&mut self, stmt: plir::Stmt) {
        self.init_block.push(stmt)
    }
}

#[derive(Clone, PartialEq, Eq)]
struct Var {
    ident: String,
    ty: plir::Type
}
impl Var {
    fn into_expr(self) -> plir::Expr {
        plir::Expr::new(self.ty, plir::ExprType::Ident(self.ident))
    }

    fn split(self, sp: plir::Split) -> PLIRResult<plir::Expr> {
        let t = self.ty.split(sp)?;
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

#[derive(Debug)]
pub struct Class {
    ty: TypeData,
    methods: HashMap<SigKey<'static>, (String, plir::FunType)>
}

impl Class {
    pub fn primitive() -> Self {
        Self {
            ty: TypeData::Primitive,
            methods: Default::default()
        }
    }

    pub fn structural(fields: impl IntoIterator<Item=(String, plir::Type)>) -> Self {
        let fields = fields.into_iter()
            .enumerate()
            .map(|(i, (ident, ty))| (ident, (i, ty)))
            .collect();

        Self {
            ty: TypeData::Struct { fields },
            methods: Default::default()
        }
    }

    pub fn get_method<'a>(&'a self, ident: &'a str, /* params: &'a [plir::Type] */) -> Option<&'a (String, plir::FunType)> {
        let k = SigKey::new(ident, vec![]);
        
        self.methods.get(&k)
    }

    pub fn get_field(&self, ident: &str) -> Option<&TypeWithIndex> {
        match &self.ty {
            TypeData::Primitive => None,
            TypeData::Struct { fields } => fields.get(ident),
        }
    }
}

type TypeWithIndex = (usize, plir::Type);
#[derive(Clone, Debug)]
enum TypeData {
    Primitive,
    Struct {
        fields: HashMap<String, TypeWithIndex>
    }
}

/// This struct does the actual conversion from AST to PLIR.
pub struct CodeGenerator {
    program: InsertBlock,
    blocks: Vec<InsertBlock>,
    var_id: usize,

    // steps: HashMap<*const plir::Expr, usize>
}

impl CodeGenerator {
    /// Creates a new instance of the CodeGenerator.
    pub fn new() -> Self {
        Self { 
            program: InsertBlock::top(), 
            blocks: vec![],
            var_id: 0,
            // steps: HashMap::new()
        }
    }

    /// Takes out the generated [`plir::Program`] from this struct.
    pub fn unwrap(self) -> PLIRResult<plir::Program> {
        if !self.blocks.is_empty() {
            panic!("Could not create program. Insert block was opened but not properly closed.");
        }
        let InsertBlock {block, exits, mut init_block, .. } = self.program;

        init_block.extend(block);
        match exits.last() {
            None => Ok(plir::Program(init_block)),
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
                    self.peek_block().push_lazy_stmt(plir::Stmt::ExternFunDecl(fs));
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

    fn resolve<'a, T>(&'a self, f: impl FnMut(&'a InsertBlock) -> Option<T>) -> Option<T> {
        self.blocks.iter().rev()
            .chain(std::iter::once(&self.program))
            .find_map(f)
    }
    fn resolve_mut<'a, T>(&'a mut self, f: impl FnMut(&'a mut InsertBlock) -> Option<T>) -> Option<T> {
        self.blocks.iter_mut().rev()
            .chain(std::iter::once(&mut self.program))
            .find_map(f)
    }

    fn get_var_type(&self, ident: &str) -> PLIRResult<&plir::Type> {
        self.resolve(|ib| ib.vars.get(ident))
            .ok_or_else(|| PLIRErr::UndefinedVar(String::from(ident)))
    }
    fn get_class(&self, ty: &plir::Type) -> PLIRResult<&Class> {
        match ty {
            plir::Type::Prim(ident) => self.resolve(|ib| ib.classes.get(ident))
                .ok_or_else(|| PLIRErr::UndefinedType(String::from(ident))),
            _ => todo!()
        }
    }

    fn tmp_var_name(&mut self, ident: &str) -> String {
        let string = format!("_{ident}_{}", self.var_id);
        self.var_id += 1;
        string
    }

    fn push_tmp_decl(&mut self, ident: &str, e: plir::Expr) -> Var {
        let ident = self.tmp_var_name(ident);
        let ety = e.ty.clone();

        let decl = plir::Decl {
            rt: ReasgType::Const,
            mt: MutType::Immut,
            ident: ident.clone(),
            ty: ety.clone(),
            val: e,
        };

        self.peek_block().push_stmt(plir::Stmt::Decl(decl));

        Var {
            ident,
            ty: ety
        }
    }

    /// Convert a program into PLIR, and attach it to the CodeGenerator.
    pub fn consume_program(&mut self, prog: ast::Program) -> PLIRResult<()> {
        self.consume_stmts(prog.0)
    }

    /// Consume an iterator of statements into the current insert block.
    /// 
    /// This function stops parsing statements early if an unconditional exit has been found.
    /// At this point, the insert block cannot accept any more statements.
    fn consume_stmts(&mut self, stmts: Vec<ast::Stmt>) -> PLIRResult<()> {
        let mut eager_stmts = vec![];
        for stmt in stmts {
            match stmt {
                ast::Stmt::FunDecl(fd) => {
                    self.peek_block().unresolved
                        .insert(fd.sig.ident.clone(), Unresolved::Fun(fd));
                },
                ast::Stmt::ExternFunDecl(fs) => {
                    self.peek_block().unresolved
                        .insert(fs.ident.clone(), Unresolved::ExternFun(fs));
                    }
                ast::Stmt::ClassDecl(cls) => {
                    self.peek_block().unresolved
                        .insert(cls.ident.clone(), Unresolved::Class(cls));
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
                    self.peek_block().push_lazy_stmt(plir::Stmt::ExternFunDecl(fs));
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
    fn consume_eager_stmt(&mut self, stmt: ast::Stmt) -> PLIRResult<bool> {
        match stmt {
            ast::Stmt::Decl(d) => self.consume_decl(d),
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
                Ok(self.peek_block().push_stmt(plir::Stmt::Expr(e)))
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
            mut init_block, mut block, exits, final_exit, 
            vars: _, classes: _, unresolved: _ 
        } = block;
        
        init_block.extend(block);
        block = init_block;

        // fill the unconditional exit if necessary:
        let mut final_exit = final_exit.unwrap_or_else(|| {
            // we need to add an explicit exit statement.

            // if the stmt given is an Expr, we need to replace the Expr with an explicit `exit` stmt.
            // otherwise just append an `exit` stmt.
            let exit_expr = if let Some(plir::Stmt::Expr(_)) = block.last() {
                if let plir::Stmt::Expr(e) = block.pop().unwrap() {
                    Some(e)
                } else {
                    unreachable!()
                }
            } else {
                None
            };

            let exit_ty = match &exit_expr {
                Some(e) => e.ty.clone(),
                None => plir::ty!(plir::Type::S_VOID),
            };
            block.push(plir::Stmt::Exit(exit_expr));
            BlockExit::Exit(exit_ty)
        });

        // Type check block:
        if let Some(exp_ty) = expected_ty {
            match &mut final_exit {
                | BlockExit::Return(exit_ty) 
                | BlockExit::Exit(exit_ty) 
                => if exit_ty.as_ref() != exp_ty.as_ref() {
                    let Some(plir::Stmt::Return(me) | plir::Stmt::Exit(me)) = block.last_mut() else {
                        unreachable!();
                    };

                    match me.take() {
                        Some(e) => match op_impl::apply_special_cast(e, &exp_ty, CastType::Decl) {
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
        pat: ast::Pat<T>, 
        expr: plir::Expr, 
        extra: E, 
        mut split_extra: impl FnMut(&E, plir::Split) -> PLIRResult<E>,
        mut map: impl FnMut(&mut Self, T, plir::Expr, E) -> PLIRResult<()>,
        consume_var: bool
    ) -> PLIRResult<()> {
        self.unpack_pat_inner(pat, expr, extra, &mut split_extra, &mut map, consume_var)
    }

    fn unpack_pat_inner<T, E>(
        &mut self, 
        pat: ast::Pat<T>, 
        expr: plir::Expr, 
        extra: E, 
        split_extra: &mut impl FnMut(&E, plir::Split) -> PLIRResult<E>,
        map: &mut impl FnMut(&mut Self, T, plir::Expr, E) -> PLIRResult<()>,
        consume_var: bool
    ) -> PLIRResult<()> {
        match pat {
            ast::Pat::Unit(t) => map(self, t, expr, extra),
            ast::Pat::Spread(spread) => match spread {
                Some(pat) => self.unpack_pat_inner(*pat, expr, extra, split_extra, map, consume_var),
                None => Ok(()),
            },
            ast::Pat::List(pats) => {
                let var = self.push_tmp_decl("decl", expr);

                for (idx, pat) in std::iter::zip(create_splits(&pats), pats) {
                    let rhs = var.clone().split(idx)?;
                    let extr = split_extra(&extra, idx)?;
                    self.unpack_pat_inner(pat, rhs, extr, split_extra, map, false)?;
                }

                if consume_var {
                    self.peek_block().push_stmt(plir::Stmt::Expr(var.into_expr()));
                }
                Ok(())
            },
        }
    }

    /// Consume a declaration into the current insert block.
    /// 
    /// This function returns whether or not the insert block accepts any more statements.
    fn consume_decl(&mut self, decl: ast::Decl) -> PLIRResult<bool> {
        let ast::Decl { rt, pat, ty, val } = decl;

        let e = self.consume_expr(val)?;
        let ty = ty.map(plir::Type::from);

        self.unpack_pat(pat, e, (rt, ty), 
            |(rt, mty), idx| {
                Ok((*rt, match mty {
                    Some(t) => Some(t.split(idx)?),
                    None => None,
                }))
            }, 
            |this, unit, e, extra| {
                let ast::DeclUnit(ident, mt) = unit;
                let (rt, ty) = extra;

                let ty = ty.unwrap_or_else(|| e.ty.clone());
                // Type check decl, casting if possible
                let e = match op_impl::apply_special_cast(e, &ty, CastType::Decl) {
                    Ok(e) => e,
                    Err(e) => return Err(PLIRErr::ExpectedType(ty, e.ty)),
                };

                this.declare(&ident, ty.clone());
                let decl = plir::Decl { rt, mt, ident, ty, val: e };
                this.peek_block().push_stmt(plir::Stmt::Decl(decl));

                Ok(())
            },
            false
        )?;

        Ok(self.peek_block().is_open())
    }

    /// Consume a function signature and convert it into a PLIR function signature.
    fn consume_fun_sig(&mut self, sig: ast::FunSignature) -> PLIRResult<plir::FunSignature> {
        let ast::FunSignature { ident, params, ret } = sig;
        
        let (params, param_tys): (Vec<_>, Vec<_>) = params.into_iter()
        .map(|p| {
            let ast::Param { rt, mt, ident, ty } = p;
            let ty = ty.map_or(
                plir::ty!(plir::Type::S_UNK),
                plir::Type::from
            );

            plir::Param { rt, mt, ident, ty }
        })
        .map(|p| {
            let ty = p.ty.clone();
            (p, ty)
        })
        .unzip();
    
        let ret = ret.map_or(
            plir::ty!(plir::Type::S_VOID),
            plir::Type::from
        );

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
        
        self.peek_block().push_lazy_stmt(plir::Stmt::FunDecl(fun_decl));
        Ok(self.peek_block().is_open())
    }

    fn consume_cls(&mut self, cls: ast::Class) -> PLIRResult<bool> {
        let ast::Class { ident, fields, methods } = cls;

        let fields = fields.into_iter()
            .enumerate()
            .map(|(i, ast::types::FieldDecl { rt, mt, ident, ty })| {
                (ident, (i, plir::FieldDecl { rt, mt, ty: plir::Type::from(ty) }))
            })
            .collect();
        
        let ib = self.peek_block();
        for ast::types::MethodDecl { is_static, decl } in methods {
            ib.unresolved.insert(format!("{ident}::{}", &decl.sig.ident), Unresolved::Fun(decl));
        }
        let cls = plir::Class { ident, fields };
        
        ib.push_lazy_stmt(plir::Stmt::ClassDecl(cls));
        Ok(ib.is_open())
    }

    fn consume_expr(&mut self, expr: ast::Expr) -> PLIRResult<plir::Expr> {
        match expr {
            ast::Expr::Ident(ident) => {
                self.resolve_ident(&ident)?;
                Ok(plir::Expr::new(
                    self.get_var_type(&ident)?.clone(),
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
            ast::Expr::Assign(pat, expr) => {
                let expr = self.consume_expr(*expr)?;

                self.push_block();
                self.unpack_pat(pat, expr, (), |_, _| Ok(()),
                    |this, unit, e, _| {
                        let unit = match unit {
                            ast::AsgUnit::Ident(ident) => plir::AsgUnit::Ident(ident),
                            ast::AsgUnit::Path(p) => {
                                let p = this.consume_path(p)?;
                                if matches!(p, plir::Path::Method(..)) {
                                    plir::AsgUnit::Path(p)
                                } else {
                                    Err(PLIRErr::CannotAssignToMethod)?
                                }
                            },
                            ast::AsgUnit::Index(idx) => {
                                let (_, idx) = this.consume_index(idx)?;
                                plir::AsgUnit::Index(idx)
                            },
                        };
                        
                        let asg = plir::Expr::new(
                            e.ty.clone(),
                            plir::ExprType::Assign(unit, Box::new(e))
                        );

                        this.peek_block().push_stmt(plir::Stmt::Expr(asg));
                        Ok(())
                    },
                    true
                )?;
                
                let insert_block = self.pop_block().unwrap();
                self.consume_insert_block(insert_block, BlockBehavior::Bare, None)
                    .map(|b| plir::Expr::new(b.0.clone(), plir::ExprType::Block(b)))
            },
            ast::Expr::Path(p) => {
                self.consume_path(p).map(Into::into)
            },
            ast::Expr::UnaryOps { ops, expr } => {
                let e = self.consume_expr(*expr)?;
                
                ops.into_iter().rev()
                    .try_fold(e, op_impl::apply_unary)
            },
            ast::Expr::BinaryOp { op, left, right } => {
                op_impl::apply_binary(op,
                    self.consume_expr(*left)?,
                    self.consume_expr(*right)?,
                )
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
                    plir::ty!(plir::Type::S_LIST, [block.0.clone()]),
                    plir::ExprType::While { condition: Box::new(condition), block }
                ))
            },
            ast::Expr::For { ident, iterator, block } => {
                let iterator = self.consume_expr_and_box(*iterator)?;
                let block = self.consume_tree_block(block, BlockBehavior::Loop, None)?;

                Ok(plir::Expr::new(
                    plir::ty!(plir::Type::S_LIST, [block.0.clone()]),
                    plir::ExprType::For { ident, iterator, block }
                ))
            },
            ast::Expr::Call { funct, params } => {
                let funct = self.consume_expr_and_box(*funct)?;
                match &funct.ty {
                    plir::Type::Fun(plir::FunType(ptys, ret)) => {
                        let params = std::iter::zip(ptys.iter(), params)
                            .map(|(pty, expr)| {
                                let param = self.consume_expr(expr)?;
                                op_impl::apply_special_cast(param, pty, CastType::Call)
                                    .map_err(|e| PLIRErr::ExpectedType(pty.clone(), e.ty))
                            })
                            .collect::<Result<_, _>>()?;

                        Ok(plir::Expr::new(
                            (**ret).clone(), plir::ExprType::Call { funct, params }
                        ))
                    },
                    t => Err(PLIRErr::CannotCall(t.clone()))
                }
            },
            ast::Expr::Index(idx) => {
                self.consume_index(idx)
                    .map(|(ty, index)| plir::Expr::new(ty, plir::ExprType::Index(index)))
            },
            ast::Expr::Spread(_) => Err(PLIRErr::CannotSpread),
        }
    }

    fn consume_expr_and_box(&mut self, expr: ast::Expr) -> PLIRResult<Box<plir::Expr>> {
        self.consume_expr(expr).map(Box::new)
    }

    fn consume_path(&mut self, p: ast::Path) -> PLIRResult<plir::Path> {
        let ast::Path { obj, attrs } = p;
        let obj = self.consume_expr_and_box(*obj)?;
        let mut path = match attrs.first().unwrap().1 {
            true  => plir::Path::Static(obj, vec![]),
            false => plir::Path::Struct(obj, vec![]),
        };

        for (ident, st) in attrs {
            let top_ty = path.ty();
            let cls = self.get_class(top_ty)?;

            if st {
                // Only static access is thru types
                todo!()
            } else {
                match cls.get_method(&ident) {
                    Some((fname, ft)) => {
                        if matches!(path, plir::Path::Method(..)) {
                            Err(PLIRErr::CannotAccessOnMethod)?;
                        } else {
                            let mut ft = ft.clone();
                            ft.pop_front();

                            path = plir::Path::Method(
                                Box::new(path.into()), 
                                fname.clone(), 
                                plir::Type::Fun(ft.clone())
                            );
                        }
                    },
                    None => {
                        let field = cls.get_field(&ident)
                            .cloned()
                            .ok_or_else(|| PLIRErr::UndefinedAttr(top_ty.clone(), ident.clone()))?;
                        
                        path.add_struct_seg(field)
                            .map_err(|_| PLIRErr::CannotAccessOnMethod)?;
                    },
                }
            };
        }

        Ok(path)
    }
    fn consume_index(&mut self, idx: ast::Index) -> PLIRResult<(plir::Type, plir::Index)> {
        // Type signature is needed for assignment.

        let ast::Index { expr, index } = idx;

        let expr = self.consume_expr(*expr)?;
        let index = self.consume_expr(*index)?;
        
        op_impl::apply_index(expr, index)
    }
}

impl Default for CodeGenerator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer, parser};

    use super::*;

    fn assert_plir_pass(input: &str) {
        let lexed = lexer::tokenize(input).unwrap();
        let parsed = parser::parse(lexed).unwrap();
        println!("{}", codegen(parsed).unwrap())
    }

    #[test]
    fn get_display() {
        assert_plir_pass("if true {
            2;
        } else {
            3;
        }");

        assert_plir_pass("fun a(b: int, c: string) {
            let d = if 0 {
                return 1;
            } else {
                3;
            };

            4;
        }");

        assert_plir_pass("
            let [a, .., b] = [1, 2, 3, 4];
            let [a, ..b, c] = [1, 2, 3, 4];

            a = 4;
            [a, b, c, .., d, e, f] = [1, 2, 3, 4, 5, 6, 7, 8, 9];
        ");

        assert_plir_pass("
            fun print(a: int) {
                // :?
            }

            let vd = {
                let a = 1;
                {
                    print(a);
                }

                fun print(a: float) {
                    a * 2;
                }
            };
        ");
    }
}