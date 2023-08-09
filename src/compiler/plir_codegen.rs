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
mod ty_classes;
mod instrs;
mod walkers;

use std::borrow::Cow;
use std::collections::HashMap;
use std::path::Path;
use std::rc::Rc;

use indexmap::IndexMap;

use crate::ast::{self, ReasgType, MutType, Locatable};
use crate::compiler::dsds;
use crate::compiler::internals::C_INTRINSICS_PLIR;
use crate::compiler::plir::walk::WalkerMut;
use crate::err::{GonErr, FullGonErr, full_gon_cast_impl, CursorRange};

use self::instrs::{BlockBehavior, TerminalFrag, InstrBlock};
pub(crate) use self::op_impl::{CastFlags, OpErr};
use self::ty_classes::{TypeData, TypeDataView};

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
    /// Cannot iterate over this type
    CannotIterateType(plir::Type),
    /// Could not determine the type of the expression
    CannotResolveType,
    /// Variable (or attribute) is not defined
    UndefinedVarAttr(plir::FunIdent),
    /// Type/class is not defined
    UndefinedType(plir::Type),
    /// Parameter is not on this type
    UndefinedTypeParam(plir::Type, String),
    /// Type var needs to be resolved, but there are no relevant contexts
    ParamCannotBind(String, String),
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
    /// Identifier has multiple definitions
    DuplicateValueDefs(plir::FunIdent),
    /// Type has multiple definitions
    DuplicateTypeDefs(plir::Type),
    /// Type constraint failed
    TypeConstraintErr(ConstraintErr),
    /// Cannot resolve this monotype
    CannotResolveUnk(usize),
    /// Entry point could not be determined
    CannotDetermineMain,
    /// main() function has an incorrect type signature
    InvalidMain,
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
            | PLIRErr::CannotDetermineMain
            | PLIRErr::InvalidMain
            => "syntax error",
            
            | PLIRErr::CannotIterateType(_)
            | PLIRErr::CannotResolveType
            | PLIRErr::UndefinedType(_)
            | PLIRErr::UndefinedTypeParam(_, _)
            | PLIRErr::ParamCannotBind(_, _)
            | PLIRErr::CannotAccessOnMethod
            | PLIRErr::CannotAssignToMethod
            | PLIRErr::CannotCall(_)
            | PLIRErr::WrongArity(_, _)
            | PLIRErr::WrongTypeArity(_, _)
            | PLIRErr::CannotInitialize(_)
            | PLIRErr::CannotDeref
            | PLIRErr::TypeConstraintErr(_)
            | PLIRErr::CannotResolveUnk(_)
            => "type error",
            
            | PLIRErr::UndefinedVarAttr(_)
            | PLIRErr::DuplicateValueDefs(_)
            | PLIRErr::DuplicateTypeDefs(_)
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
        use plir::FunIdent;

        match self {
            PLIRErr::CannotBreak                  => write!(f, "cannot 'break' here"),
            PLIRErr::CannotContinue               => write!(f, "cannot 'continue' here"),
            PLIRErr::CannotReturn                 => write!(f, "cannot 'return' here"),
            PLIRErr::CannotIterateType(t)         => write!(f, "cannot iterate over type '{t}'"),
            PLIRErr::CannotResolveType            => write!(f, "cannot determine type"),
            PLIRErr::UndefinedVarAttr(id)         => match id {
                FunIdent::Simple(name)     => write!(f, "could not find identifier '{name}'"),
                FunIdent::Static(ty, attr) => write!(f, "could not find attribute '{attr}' on '{ty}'"),
            },
            PLIRErr::DuplicateValueDefs(t)        => write!(f, "{t} has multiple definitions"),
            PLIRErr::DuplicateTypeDefs(t)         => write!(f, "{t} has multiple definitions"),
            PLIRErr::UndefinedType(name)          => write!(f, "could not find type '{name}'"),
            PLIRErr::UndefinedTypeParam(name, p)  => write!(f, "could not find param '{p}' on '{name}'"),
            PLIRErr::ParamCannotBind(name, p)     => write!(f, "type parameter '{p}' on type '{name}' could not find sufficient context"),
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
            PLIRErr::TypeConstraintErr(e)         => {
                writeln!(f, "broken type constraint")?;
                e.fmt(f)
            },
            PLIRErr::CannotResolveUnk(idx)        => write!(f, "type ?{idx} could not be resolved"),
            PLIRErr::CannotDetermineMain          => write!(f, "modules can have a main function, or statements outside of functions, but not both"),
            PLIRErr::InvalidMain                  => write!(f, "expected main to be of type {}", plir::ty![() -> plir::ty![plir::Type::S_VOID]]),
            PLIRErr::OpErr(e)                     => e.fmt(f),
        }
    }
}
impl std::error::Error for PLIRErr {
    fn cause(&self) -> Option<&dyn std::error::Error> {
        match self {
            PLIRErr::OpErr(e) => Some(e),
            _ => None
        }
    }
}

fn primitives(prims: impl IntoIterator<Item=plir::Type>) -> HashMap<String, TypeData> {
    prims.into_iter()
        .map(|t| {
            let dat = TypeData::primitive(t.clone());
            (t.get_type_key().into_owned(), dat)
        })
        .collect()
}

#[derive(Debug)]
enum UnresolvedValue {
    ExternFun(PiFunSig),
    Fun(PiFunSig, Located<Rc<ast::Block>>),
    FunBlock(plir::FunSignature, Located<Rc<ast::Block>>),
    Import(ast::StaticPath)
}
#[derive(Debug)]
enum UnresolvedType {
    Class(ast::Class),
    #[allow(unused)]
    Import(ast::StaticPath)
}
trait Unresolved 
    where <Self::KeyRef as ToOwned>::Owned: std::hash::Hash + Eq
{
    type KeyRef: ToOwned + ?Sized;

    fn rez_key(&self) -> Cow<Self::KeyRef>;
    fn block_map(block: &mut InsertBlock) -> &mut IndexMap<<Self::KeyRef as ToOwned>::Owned, Self>
        where Self: Sized;
}

impl Unresolved for UnresolvedValue {
    type KeyRef = plir::FunIdent;

    fn rez_key(&self) -> Cow<plir::FunIdent> {
        match self {
            UnresolvedValue::ExternFun(pi)   => Cow::Borrowed(&pi.ident),
            UnresolvedValue::Fun(pi, _)      => Cow::Borrowed(&pi.ident),
            UnresolvedValue::FunBlock(fs, _) => Cow::Borrowed(&fs.ident),
            UnresolvedValue::Import(mp)      => Cow::Owned(plir::FunIdent::new_simple(&mp.attr)),
        }
    }

    fn block_map(block: &mut InsertBlock) -> &mut IndexMap<plir::FunIdent, Self> {
        &mut block.unres_values
    }
}
impl Unresolved for UnresolvedType {
    type KeyRef = str;

    fn rez_key(&self) -> Cow<str> {
        match self {
            UnresolvedType::Class(cls) => Cow::from(&cls.ident),
            UnresolvedType::Import(mp) => Cow::from(&mp.attr),
        }
    }

    fn block_map(block: &mut InsertBlock) -> &mut IndexMap<String, Self> {
        &mut block.unres_types
    }
}

#[derive(Debug)]
enum MaybeInit<U, I> {
    Uninit(U),
    Init(I)
}
type MaybeInitParam = MaybeInit<ast::Param, plir::Param>;
type MaybeInitType = MaybeInit<Located<ast::Type>, plir::Type>;

/// Partially initialized function signature.
#[derive(Debug)]
struct PiFunSig {
    /// The function identifier for the function.
    ident: plir::FunIdent,

    /// Identifiers for the type parameters of this function
    generics: Vec<String>,

    /// The list of parameters.
    /// Some of these parameters will already have been initialized,
    /// while others have already been initialized.
    params: Vec<MaybeInitParam>,

    /// Whether this function signature is varargs
    varargs: bool,

    /// The return type.
    /// This may or may not already have been initialized.
    ret: MaybeInitType
}

#[derive(Debug)]
struct GenericContext(String, HashMap<String, plir::Type>);

#[derive(Clone)]
struct ClassKey {
    block: *const InsertBlock,
    scope: usize,
    referent: plir::Type
}

#[derive(Debug)]
struct InsertBlock {
    instrs: InstrBlock,
    /// The range of the whole block, 
    /// only used to append a location to custom exits
    block_range: CursorRange,

    vars: HashMap<plir::FunIdent, plir::Type>,
    types: HashMap<String, TypeData>,
    type_aliases: HashMap<plir::Type, plir::Type>,

    unres_values: IndexMap<plir::FunIdent, UnresolvedValue>,
    unres_types: IndexMap<String, UnresolvedType>,

    generic_ctx: Option<GenericContext>,
    /// If this is not None, then this block is expected to return the provided type.
    /// This can be used as context for some functions to more effectively assign
    /// a type.
    expected_ty: Option<plir::Type>
}

impl InsertBlock {
    fn new(block_range: CursorRange, expected_ty: Option<plir::Type>) -> Self {
        Self {
            instrs: InstrBlock::new(),
            block_range,
            vars: HashMap::new(),
            types: HashMap::new(),
            type_aliases: HashMap::new(),
            unres_values: IndexMap::new(),
            unres_types:  IndexMap::new(),
            generic_ctx: None,
            expected_ty
        }
    }

    fn top() -> Self {
        use plir::{Type, ty};

        Self {
            instrs: InstrBlock::new(),
            block_range: (0, 0) ..= (0, 0),
            vars: HashMap::new(),
            types: primitives([
                ty!(Type::S_INT),
                ty!(Type::S_FLOAT),
                ty!(Type::S_BOOL),
                ty!(Type::S_CHAR),
                ty!(Type::S_VOID),
                ty!("#ptr"),
                ty!("#byte"),
            ]),
            type_aliases: HashMap::new(),
            unres_values: IndexMap::new(),
            unres_types: IndexMap::new(),
            generic_ctx: None,
            expected_ty: None
        }
    }

    /// Insert an unresolved class/function into the insert block.
    fn insert_unresolved<U>(&mut self, unresolved: U) 
        where U: Unresolved,
            <U::KeyRef as ToOwned>::Owned: std::hash::Hash + Eq
    {
        U::block_map(self)
            .insert(unresolved.rez_key().into_owned(), unresolved);
    }

    // ty parameter must be a class shape.
    fn insert_unresolved_method(&mut self, cls_ty: &plir::Type, method: ast::MethodDecl) {
        use MaybeInit::*;
        use Cow::Borrowed;
        use plir::TypeRef;

        let ast::MethodDecl {
            sig: ast::MethodSignature {
                referent, is_static, name: method_name, generics, params, ret
            }, 
            block 
        } = method;

        let mut preinit_params = vec![];
        if !is_static {
            let this = referent.unwrap_or_else(|| String::from("#unused"));
            let param = plir::Param {
                rt: Default::default(),
                mt: Default::default(),
                ident: this,
                ty: cls_ty.clone()
            };
            preinit_params.push(Init(param));
        };

        let try_init_ty = |mpty, fallback| {
            match mpty {
                None => Ok(fallback),
                Some(pty) => {
                    let Located(ast::Type(pty_id, pty_params), _) = &pty; 
                    
                    if pty_params.is_empty() {
                        // pty is a single String
                        if generics.contains(pty_id) {
                            todo!("fun generics, method {method_name} on {cls_ty} has a generic fun param")
                        } else if let TypeRef::Generic(cls_id, cls_params, ()) = cls_ty {
                            // check if pty_id is in the class's list of params
                            let pty_ref = TypeRef::TypeVar(Borrowed(cls_id), Borrowed(pty_id));
                            if cls_params.contains(&pty_ref) {
                                return Ok(pty_ref.upgrade());
                            }
                        }
                    }
                    
                    Err(pty)
                }
            }
        };

        preinit_params.extend({
            params.into_iter().map(|p| {
                let ast::Param { rt, mt, ident, ty: mpty } = p;
                match try_init_ty(mpty, plir::ty!(plir::Type::S_UNK)) {
                    Ok(plir_ty) => Init(plir::Param { rt, mt, ident, ty: plir_ty }),
                    Err(ast_ty) => Uninit(ast::Param { rt, mt, ident, ty: Some(ast_ty) }),
                }
            })
        });

        let metref = plir::FunIdent::new_static(cls_ty, &method_name);
        let ret = match try_init_ty(ret, plir::ty!(plir::Type::S_VOID)) {
            Ok(t)  => Init(t),
            Err(t) => Uninit(t),
        };
        
        let preinit = PiFunSig {
            ident: metref.clone(),
            generics,
            params: preinit_params,
            varargs: false,
            ret,
        };

        self.insert_unresolved(UnresolvedValue::Fun(preinit, block));

        if let Some(c) = self.types.get_mut(&*cls_ty.get_type_key()) {
            c.insert_method(cls_ty.generic_args().to_vec(), method_name, metref);
        }
    }

    /// Declares a variable in this insert block, 
    /// allowing it to be accessed in the enclosing insert block.
    fn declare<I>(&mut self, ident: &I, ty: plir::Type) 
        where I: plir::AsFunIdent + std::hash::Hash + ?Sized
    {
        self.vars.insert(ident.as_fun_ident().into_owned(), ty);
    }

    /// Declares a class in this insert block,
    /// allowing it to be accessed in the enclosing insert block.
    fn declare_cls(&mut self, cls: plir::Class) {
        self.types.insert(
            cls.ty.get_type_key().into_owned(),
            TypeData::structural(cls)
        );
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

/// A struct which holds the types declared by PLIR code generation.
#[derive(Default, Clone, Debug)]
pub struct DeclaredTypes {
    pub(super) types: IndexMap<plir::Type, plir::Class>,
    pub(super) values: IndexMap<plir::FunIdent, plir::Type>
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
                writeln!(file, "{};", plir::HoistedStmt::ExternFunDecl(f.extern_fun_sig(ident.clone())))?
            }
            // TODO: don't ignore other types of decls
        }

        Ok(())
    }

    fn push(&mut self, stmt: &plir::HoistedStmt) {
        use plir::HoistedStmt;

        match stmt {
            HoistedStmt::FunDecl(f) => {
                self.values.insert(f.sig.ident.clone(), f.sig.ty().into());
            },
            HoistedStmt::ExternFunDecl(f) => {
                self.values.insert(f.ident.clone(), f.ty().into());
            },
            HoistedStmt::ClassDecl(c) => {
                self.types.insert(c.ty.clone(), c.clone());
            },
            HoistedStmt::IGlobal(id, _) => {
                self.values.insert(plir::FunIdent::new_simple(id), plir::ty!("#ptr"));
            },
        }
    }
}
impl std::ops::AddAssign for DeclaredTypes {
    fn add_assign(&mut self, rhs: Self) {
        self.types.extend(rhs.types);
        self.values.extend(rhs.values);
    }
}

#[derive(PartialEq, Eq, Hash)]
enum GlobalKey {
    Value(plir::FunIdent),
    Type(plir::Type)
}

#[derive(Default)]
struct Globals {
    stmts: IndexMap<GlobalKey, plir::HoistedStmt>,
    declared: DeclaredTypes
}

impl Globals {
    /// Includes the hoisted statement into the global hoisted statement list.
    /// 
    /// This will also export the type/value into [`DeclaredTypes`].
    fn push(&mut self, stmt: impl Into<plir::HoistedStmt>) -> PLIRResult<()> {
        use plir::HoistedStmt;
        use indexmap::map::Entry;

        let stmt: HoistedStmt = stmt.into();

        let (key, should_export) = match &stmt {
            HoistedStmt::FunDecl(f) => {
                (GlobalKey::Value(f.sig.ident.clone()), !f.sig.private)
            },
            HoistedStmt::ExternFunDecl(f) => {
                (GlobalKey::Value(f.ident.clone()), !f.private)
            },
            HoistedStmt::ClassDecl(c) => {
                (GlobalKey::Type(c.ty.clone()), true)
            },
            HoistedStmt::IGlobal(id, _) => {
                let id = plir::FunIdent::new_simple(id);
                (GlobalKey::Value(id), true)
            }
        };

        match self.stmts.entry(key) {
            Entry::Occupied(e) => match (e.get(), &stmt) {
                // ignore in this case:
                (HoistedStmt::ExternFunDecl(l), HoistedStmt::ExternFunDecl(r)) if l == r => {},
                // throw in every other case:
                _ => match e.remove_entry().0 {
                    GlobalKey::Value(k) => Err(PLIRErr::DuplicateValueDefs(k))?,
                    GlobalKey::Type(k)  => Err(PLIRErr::DuplicateTypeDefs(k))?,
                }
            },
            Entry::Vacant(e) => {
                if should_export {
                    self.declared.push(&stmt);
                }
                e.insert(stmt);
            },
        }

        Ok(())
    }
}

#[derive(Debug)]
struct TypeResolver {
    /// A DSDS of monotypes.
    /// 
    /// The top root of a group should always contain a non-unknown.
    monos: dsds::UnionFind<plir::Type>,
    
    /// A Vec of the declared unknowns.
    /// 
    /// This indicates which range of characters represent the unknown type.
    unk_positions: Vec<CursorRange>
}
#[derive(Debug)]
enum ConstraintFail<T> {
    /// Two * types were set equal, but they are known to be inequal
    Mono(T, T),
    /// Two generic types (A<..>, B<..>) were set equal, but their identifier is not equal
    Generic(T, T),
    /// Two types were set equal, but their kinds aren't equal
    Shape(T, T)
}
/// Failed type constraint.
#[derive(Debug)]
pub struct ConstraintErr {
    error: Vec<ConstraintFail<plir::Type>>
}
impl From<ConstraintFail<plir::Type>> for ConstraintErr {
    fn from(value: ConstraintFail<plir::Type>) -> Self {
        ConstraintErr { error: vec![value] }
    }
}
impl ConstraintErr {
    fn caused(mut self, l: plir::Type, r: plir::Type) -> Self {
        self.error.push(ConstraintFail::Mono(l, r));
        self
    }
}
impl<T: std::fmt::Display> std::fmt::Display for ConstraintFail<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConstraintFail::Mono(a, b)    => write!(f, "type '{a}' != '{b}'"),
            ConstraintFail::Generic(a, b) => write!(f, "generic type of '{a}' does not match '{b}'"),
            ConstraintFail::Shape(a, b)   => write!(f, "kind of '{a}' does not match '{b}'"),
        }
    }
}
impl std::fmt::Display for ConstraintErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, e) in self.error.iter().rev().enumerate() {
            writeln!(f, "{:1$}{e}", "", i * 4)?;
        }
        Ok(())
    }
}

impl std::error::Error for ConstraintErr {}
struct CannotResolve(usize);
impl From<CannotResolve> for PLIRErr {
    fn from(value: CannotResolve) -> Self {
        PLIRErr::CannotResolveUnk(value.0)
    }
}
impl TypeResolver {
    fn new() -> TypeResolver {
        Self {
            monos: dsds::UnionFind::new(),
            unk_positions: vec![]
        }
    }

    fn new_unknown(&mut self, range: CursorRange) -> plir::Type {
        let unk = plir::Type::Unk(self.unk_positions.len());
        self.unk_positions.push(range);
        unk
    }
    fn normalize(&mut self, ty: plir::Type) -> plir::Type {
        match ty {
            plir::Type::Unk(_) => {
                let id = self.monos.make_set(ty);
                let root = self.monos.find(id);
                self.monos.get_idx(root).clone()
            },
            _ => ty
        }
    }
    /// Attempts to normalize, erroring if type is still unknown.
    fn normalize_or_err(&mut self, ty: plir::Type) -> Result<plir::Type, CannotResolve> {
        match self.normalize(ty) {
            plir::Type::Unk(idx) => Err(CannotResolve(idx)),
            t => Ok(t)
        }
    }
    fn deep_normalize(&mut self, mut ty: plir::Type) -> Result<plir::Type, CannotResolve> {
        use plir::{Type, FunType};

        fn has_unks(ty: &Type) -> bool {
            match ty {
                Type::Unk(_) => true,
                Type::TypeVar(_, _) => false,
                Type::Prim(_) => false,
                Type::Generic(_, t, ()) => t.iter().any(has_unks),
                Type::Tuple(t, ()) => t.iter().any(has_unks),
                Type::Fun(FunType { params, ret, varargs: _ }) => {
                    params.iter().any(has_unks) || has_unks(ret)
                },
            }
        }

        while has_unks(&ty) {
            ty = match ty {
                unk @ Type::Unk(_) => self.normalize_or_err(unk)?,
                tyvar @ Type::TypeVar(_, _) => return Ok(tyvar),
                prim @ Type::Prim(_) => return Ok(prim),
                Type::Generic(id, tys, ()) => {
                    let tys: Vec<_> = tys.iter().cloned()
                        .map(|ty| self.deep_normalize(ty))
                        .collect::<Result<_, _>>()?;
                
                    Type::new_generic(id, tys)
                },
                Type::Tuple(tys, ()) => {
                    let tys: Vec<_> = tys.iter().cloned()
                        .map(|ty| self.deep_normalize(ty))
                        .collect::<Result<_, _>>()?;

                    Type::new_tuple(tys)
                },
                Type::Fun(FunType { params, ret, varargs }) => {
                    let params: Vec<_> = params.iter().cloned()
                        .map(|p| self.deep_normalize(p))
                        .collect::<Result<_, _>>()?;

                    let ret = self.deep_normalize(*ret.into_owned())?;

                    Type::new_fun(params, ret, varargs)
                },
            }
        }

        Ok(ty)
    }
    fn add_constraint(&mut self, left: plir::Type, right: plir::Type) -> Result<(), ConstraintErr> {
        use plir::Type;
        use plir::FunType;

        let left = self.normalize(left);
        let right = self.normalize(right);

        match (&left, &right) {
            (Type::Unk(l), r) => { self.set_unk_ty(*l, r.clone()); },
            (r, Type::Unk(l)) => { self.set_unk_ty(*l, r.clone()); },
            (Type::Prim(_), Type::Prim(_)) => { 
                if left != right { Err(ConstraintFail::Mono(left, right))? }
            },
            (Type::TypeVar(_, _), Type::TypeVar(_, _)) => {
                if left != right { Err(ConstraintFail::Mono(left, right))? }
            },
            (Type::Generic(ai, ap, ()), Type::Generic(bi, bp, ())) => {
                if ai != bi { return Err(ConstraintFail::Generic(left, right))?; }
                if ap.len() != bp.len() { return Err(ConstraintFail::Shape(left, right))?; }

                for (a, b) in std::iter::zip(ap.iter().cloned(), bp.iter().cloned()) {
                    self.add_constraint(a, b)
                        .map_err(|e| e.caused(left.clone(), right.clone()))?;
                }
            },
            (Type::Tuple(ap, ()), Type::Tuple(bp, ())) => {
                if ap.len() != bp.len() { return Err(ConstraintFail::Shape(left, right))?; }
                
                for (a, b) in std::iter::zip(ap.iter().cloned(), bp.iter().cloned()) {
                    self.add_constraint(a, b)
                        .map_err(|e| e.caused(left.clone(), right.clone()))?;
                }
            },
            (Type::Fun(FunType { params: ap, ret: ar, varargs: av }), Type::Fun(FunType { params: bp, ret: br, varargs: bv })) => {
                if av != bv { return Err(ConstraintFail::Shape(left, right))?; }
                if ap.len() != bp.len() { return Err(ConstraintFail::Shape(left, right))?; }

                self.add_constraint(ar.upgrade(), br.upgrade())
                    .map_err(|e| e.caused(left.clone(), right.clone()))?;
                for (a, b) in std::iter::zip(ap.iter().cloned(), bp.iter().cloned()) {
                    self.add_constraint(a, b)
                        .map_err(|e| e.caused(left.clone(), right.clone()))?;
                }
            },
            _ => Err(ConstraintFail::Shape(left, right))?
        }

        Ok(())
    }

    /// Sets an unknown type variable equal to another type.
    fn set_unk_ty(&mut self, unk: usize, t: plir::Type) {
        use plir::Type;

        let left  = self.monos.make_set(Type::Unk(unk));
        let right = self.monos.make_set(t);

        self.monos.union_select(left, right, |l, r| {
            debug_assert!(
                matches!(l, Type::Unk(_)),
                "{l:?} should have been normalized before set_unk_ty call"
            );
            match r {
                Type::Unk(_) => dsds::Selector::Whatever,
                _ => dsds::Selector::Right,
            }
        });
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
    var_id: usize,
    resolver: TypeResolver
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
            top.types.insert(ident.get_type_key().into_owned(), TypeData::structural(cls.clone()));
        }
        for (ident, value_ty) in &declared.values {
            // register values in scope
            top.declare(ident, value_ty.clone());

            // register methods to cls data
            if let plir::FunIdent::Static(ty, attr) = ident {
                if let Some(cls) = top.types.get_mut(&*ty.get_type_key()) {
                    cls.insert_method(ty.generic_args().to_vec(), attr.clone(), ident.clone());
                }
            }
        }

        Self { 
            program: top, 
            globals: Globals {
                stmts: IndexMap::new(),
                declared
            },
            blocks: vec![],
            var_id: 0,
            resolver: TypeResolver::new()
        }
    }

    /// Takes out the generated [`plir::Program`] from this struct.
    pub fn unwrap(mut self) -> PLIRResult<plir::Program> {
        assert!(self.blocks.is_empty(),
            "insert block was opened but not properly closed"
        );
        debug_assert!(self.program.unres_values.is_empty(), 
            "there was an unresolved value in program: {}", 
            self.program.unres_values.keys().next().unwrap()
        );
        debug_assert!(self.program.unres_types.is_empty(),  
            "there was an unresolved type in program: {}", 
            self.program.unres_types.keys().next().unwrap()
        );
        
        {
            // remove terminals from top level:
            self.program.instrs.split_off_unpropagated_terminals(BlockBehavior::Top)?;

            // assert valid main conditions:
            let is_main_defined = match self.get_var_type("main")? {
                Some(plir::Type::Fun(plir::FunType { params, ret: _, varargs: false })) 
                    if params.is_empty() => true,
                Some(_) => Err(PLIRErr::InvalidMain)?,
                None => false,
            };
            
            let instrs = &mut self.program.instrs;
            if is_main_defined && !instrs.is_empty() {
                Err(PLIRErr::CannotDetermineMain.at_range(instrs[0].range()))?;
            }

            if !is_main_defined {
                // create main:
                if instrs.is_open() {
                    instrs.push({
                        plir::ProcStmt::Return(None)
                            .located_at((0, 0) ..= (0, 0))
                    });
                }
                let (stmts, _) = std::mem::take(instrs).unravel();
    
                let sig = plir::FunSignature {
                    private: false, 
                    ident: plir::FunIdent::new_simple("main"), 
                    params: vec![], 
                    varargs: false, 
                    ret: plir::ty![plir::Type::S_VOID]
                };
                let block = plir::Block(
                    plir::ty![plir::Type::S_VOID],
                    stmts.into_iter().map(|lstmt| lstmt.0).collect()
                );
                self.declare_fun(plir::FunDecl { sig, block })?;
            }
        }

        let hoisted_stmts = self.globals.stmts.into_values().collect();
        let mut program = plir::Program(hoisted_stmts, vec![]);
        
        let mut resolver = self.resolver;
        walkers::TypeApplier(&mut resolver).walk_program(&mut program)
            .map_err(|e @ CannotResolve(n)| {
                PLIRErr::from(e)
                    .at_range(resolver.unk_positions[n].clone())
            })?;
        
        Ok(program)
    }

    /// Gets all the types declared by this code generation.
    pub fn declared_types(&self) -> DeclaredTypes {
        self.globals.declared.clone()
    }

    fn push_block(&mut self, block_range: CursorRange, expected_ty: Option<plir::Type>) -> &mut InsertBlock {
        self.blocks.push(InsertBlock::new(block_range, expected_ty));
        self.blocks.last_mut().unwrap()
    }
    fn pop_block(&mut self) -> Option<InsertBlock> {
        self.blocks.pop()
    }
    fn peek_block(&mut self) -> &mut InsertBlock {
        self.blocks.last_mut().unwrap_or(&mut self.program)
    }

    /// Resolves an identifier during PLIR traversal.
    /// 
    /// When an identifier is found during PLIR traversal,
    /// it searches through the codegen's scopes 
    /// to find if this identifier is some function (or similar object). 
    /// If it finds such an object, it will try to resolve the object's
    /// type and add it to the list of globals.
    fn resolve_ident<I>(&mut self, ident: &I) -> PLIRResult<()> 
        where I: plir::AsFunIdent + std::hash::Hash + ?Sized
    {
        use indexmap::map::Entry;

        if let Some(idx) = self.find_scope_index(|ib| ib.unres_values.contains_key(&*ident.as_fun_ident())) {
            self.execute_upwards(idx, |this| -> PLIRResult<_> {
                let fid = ident.as_fun_ident().into_owned();
                let Entry::Occupied(entry) = this.peek_block().unres_values.entry(fid) else {
                    unreachable!()
                };
    
                match entry.get() {
                    UnresolvedValue::ExternFun(_) => {
                        let UnresolvedValue::ExternFun(pi) = entry.remove() else { unreachable!() };
                        
                        let fs = this.consume_fun_sig(pi)?;
                        this.push_global(fs)?;
                    },
                    UnresolvedValue::Fun(_, _) => {
                        let (k, UnresolvedValue::Fun(pi, block)) = entry.remove_entry() else { unreachable!() };
    
                        let sig = this.consume_fun_sig(pi)?;
                        this.peek_block().unres_values.insert(k, UnresolvedValue::FunBlock(sig, block));
                    },
                    UnresolvedValue::FunBlock(_, _) => {},
                    UnresolvedValue::Import(_) => todo!(),
                }

                Ok(())
            })?;
        }

        if let Some(t) = C_INTRINSICS_PLIR.get(&*ident.as_fun_ident().as_llvm_ident()) {
            // If this is an intrinsic,
            // register the intrinsic on the top level
            // if it hasn't been registered

            let mut fs = t.extern_fun_sig(ident.as_fun_ident().into_owned());
            fs.private = true;
            self.push_global(fs)?;

            self.peek_block().declare(ident, plir::Type::Fun(t.clone()));
        }

        Ok(())
    }

    /// Returns an iterator of the codegen's insert blocks from most inner to most outer.
    fn block_iter(&self) -> impl Iterator<Item = &InsertBlock> {
        self.blocks.iter().rev()
            .chain(std::iter::once(&self.program))
    }

    /// Find the insert block matching the predicate, starting from the deepest scope and scaling out.
    fn find_scope_index<'a>(&'a self, mut pred: impl FnMut(&'a InsertBlock) -> bool) -> Option<usize> {
        self.block_iter()
            .enumerate()
            .find(|(_, t)| pred(t))
            .map(|(i, _)| i)
    }

    /// Temporarily move into a higher scope for the runtime of the closure.
    fn execute_upwards<T>(&mut self, steps: usize, f: impl FnOnce(&mut PLIRCodegen) -> T) -> T {
        let storage = self.blocks.split_off(self.blocks.len() - steps);
        let result = f(self);
        self.blocks.extend(storage);

        result
    }

    /// Gets the type of the identifier, returning None if not present.
    /// 
    /// This function also tries to resolve the variable using [`PLIRCodegen::resolve_ident`].
    /// Any errors during function/class resolution will be propagated.
    fn get_var_type<I>(&mut self, ident: &I) -> PLIRResult<Option<plir::Type>> 
        where I: plir::AsFunIdent + std::hash::Hash + ?Sized
    {
        self.resolve_ident(ident)
            .map(|_| {
                let var = self.block_iter()
                    .find_map(|ib| ib.vars.get(&*ident.as_fun_ident()))
                    .cloned();

                var.map(|t| self.resolver.normalize(t))
            })
    }

    /// Gets the type of the identifier, raising an UndefinedVar error if not present.
    /// 
    /// This function also tries to resolve the variable using [`PLIRCodegen::resolve_ident`].
    /// Any errors during function/class resolution will be propagated.
    fn get_var_type_or_err<I>(&mut self, ident: &I, range: CursorRange) -> PLIRResult<plir::Type> 
        where I: plir::AsFunIdent + std::hash::Hash + ?Sized
    {
        self.get_var_type(ident)?
            .ok_or_else(|| {
                PLIRErr::UndefinedVarAttr(ident.as_fun_ident().into_owned()).at_range(range)
            })
    }

    fn get_method(&mut self, key: &ClassKey, attr: &str) -> PLIRResult<Option<(plir::FunIdent, plir::Type)>> {
        let var = 'var: {
            let Some(id) = self.get_class(key).get_method_ref(attr) else { break 'var None };
            let Some(var) = self.get_var_type(&id)? else { break 'var None };

            Some((id, self.get_class(key).attach_type_vars_to(var)))
        };

        Ok(var)
    }
    fn get_method_or_err(&mut self, key: &ClassKey, attr: &str, range: CursorRange) -> PLIRResult<(plir::FunIdent, plir::Type)> {
        self.get_method(key, attr)?
            .ok_or_else(|| {
                let cls = self.get_class(key);
                PLIRErr::UndefinedVarAttr(plir::FunIdent::new_static(&cls.view_type(), attr))
                    .at_range(range)
            })
    }

    // Resolves the given type key.
    fn resolve_type_key(&mut self, key: &str) -> PLIRResult<()> {
        use indexmap::map::Entry;

        // find if this key is present. if so, load unresolved type at the scope where it was declared
        if let Some(idx) = self.find_scope_index(|ib| ib.unres_types.contains_key(key)) {
            self.execute_upwards(idx, |this| -> PLIRResult<_> {
                let Entry::Occupied(entry) = this.peek_block().unres_types.entry(key.to_string()) else {
                    unreachable!()
                };
    
                match entry.get() {
                    UnresolvedType::Class(_) => {
                        let UnresolvedType::Class(cls) = entry.remove() else { unreachable!() };
                        this.consume_cls(cls)
                    },
                    UnresolvedType::Import(_) => todo!(),
                }
            })
        } else {
            Ok(())
        }
    }

    fn get_class_key(&mut self, lty: Located<&plir::Type>) -> PLIRResult<ClassKey> {
        use plir::TypeRef;

        // resolve non-concrete types:
        let range = lty.range();

        let mut lty = lty.map(|ty| {
            // shallow resolve
            match ty {
                TypeRef::Unk(_) => {
                    self.resolver.normalize_or_err(ty.upgrade())
                        .map_err(|e| PLIRErr::from(e).at_range(range.clone()))
                },
                TypeRef::TypeVar(tv_type, tv_param) => {
                    self.resolve_type_key(tv_type)?;

                    let ctx = self.block_iter()
                        .filter_map(|ib| ib.generic_ctx.as_ref())
                        .find(|ctx| &ctx.0 == tv_type)
                        .ok_or_else(|| {
                            PLIRErr::ParamCannotBind(tv_type.to_string(), tv_param.to_string())
                                .at_range(range.clone())
                        })?;
                    
                    let ty = ctx.1.get(&**tv_param).ok_or_else(|| {
                        let td = self.block_iter()
                            .find_map(|ib| ib.types.get(&**tv_type))
                            .unwrap_or_else(|| panic!("expected {tv_type} to be defined"));
                        
                        PLIRErr::UndefinedTypeParam(
                            td.type_shape().upgrade(),
                            tv_param.to_string()
                        ).at_range(range.clone())
                    })?;

                    Ok(ty.upgrade())
                },
                TypeRef::Prim(_) | TypeRef::Generic(_, _, _) | TypeRef::Tuple(_, _) | TypeRef::Fun(_) => Ok(ty.upgrade()),
            }
        }).transpose_result()?;

        // make sure right number of arguments are applied
        self.verify_type(lty.as_mut())?;
        self.resolve_type_key(&lty.get_type_key())?;

        let Located(ty, range) = lty;
        match ty.downgrade() {
            TypeRef::Unk(_) | TypeRef::TypeVar(_, _) => unreachable!("did not expect get_class for {ty}"),
            TypeRef::Prim(_) | TypeRef::Generic(_, _, _) => {
                let (scope, block) = self.block_iter().enumerate()
                    .find(|(_, ib)| ib.types.contains_key(&*ty.get_type_key()))
                    .ok_or_else(|| {
                        PLIRErr::UndefinedType(ty.clone())
                            .at_range(range.clone())
                    })?;

                Ok(ClassKey { block, scope, referent: ty })
            },
            TypeRef::Tuple(_, _) | TypeRef::Fun(_) => todo!("getting type data for {ty}"),
        }
    }

    fn get_class(&self, k: &ClassKey) -> TypeDataView {
        let ClassKey { block, scope, referent } = k;

        let ib = self.block_iter().nth(*scope)
            .unwrap_or_else(|| panic!("class key requested scope {scope}, but that scope does not exist"));

        assert_eq!(*block, ib, "block {scope} should have been {block:?}, but it was {:?}", ib as *const _);

        let key = &*referent.get_type_key();
        let td = ib.types.get(key)
            .unwrap_or_else(|| panic!("expected type {key} to be defined on block {block:?}"));

        td.type_view(referent.generic_args().to_vec())
    }

    fn get_generic_context(&self, generic: plir::TypeRef) -> GenericContext {
        let plir::TypeRef::Generic(id, params, ()) = generic else { panic!("Expected generic type in get_generic_context") };

        let td = self.block_iter()
            .find_map(|ib| ib.types.get(&*id))
            .unwrap_or_else(|| panic!("type '{id}' should have existed"));

        let plir::Type::Generic(_, _, ()) = &td.type_shape() else {
            panic!("type '{id}' should have been generic")
        };

        let map = std::iter::zip(td.generic_params(), params.iter())
            .map(|(k, v)| (k, v.upgrade()))
            .collect();

        GenericContext(id.to_string(), map)
    }

    /// Expects this expression to have a given type after casts are applied.
    fn expect_type(&mut self, le: Located<plir::Expr>, t: plir::Type, cf: CastFlags) -> PLIRResult<plir::Expr> {
        match self.apply_cast(le, &t, cf)? {
            // Cast was successful, so we know the return expression's type matches
            Ok(le) => Ok(le.0),
            Err(le) => match self.resolver.add_constraint(le.ty.clone(), t.clone()) {
                Ok(_) => Ok(le.0),
                Err(e) => Err(PLIRErr::TypeConstraintErr(e).at_range(le.1)),
            },
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

        self.peek_block().instrs.push(Located::new(decl, decl_range.clone()));

        Var {
            ident,
            ty: ety,
            decl_range
        }
    }

    fn push_global(&mut self, global: impl Into<plir::HoistedStmt>) -> PLIRResult<()> {
        self.globals.push(global)
    }

    /// Consumes an AST program into PLIR and attaches it to the [`PLIRCodegen`].
    /// 
    /// It can be accessed again with [`PLIRCodegen::unwrap`].
    pub fn consume_program(&mut self, prog: ast::Program) -> PLIRResult<()> {
        self.consume_stmts(prog.0)?;
        Ok(())
    }

    /// Consume an iterator of statements into the current insert block.
    /// 
    /// This function stops parsing statements early if an unconditional exit has been found.
    /// At this point, the insert block cannot accept any more statements.
    fn consume_stmts(&mut self, stmts: impl IntoIterator<Item=Located<ast::Stmt>>) -> PLIRResult<()> {
        fn partially_init_sig(sig: ast::FunSignature) -> PiFunSig {
            use MaybeInit::*;
            let ast::FunSignature { ident, generics, params, varargs, ret } = sig;

            PiFunSig { 
                ident: plir::FunIdent::Simple(ident), 
                generics, 
                params: params.into_iter()
                    .map(Uninit)
                    .collect(), 
                varargs, 
                ret: match ret {
                    Some(t) => Uninit(t),
                    None => Init(plir::ty!(plir::Type::S_VOID)),
                }
            }
        }
        let mut eager_stmts = vec![];
        for stmt in stmts {
            match stmt.0 {
                ast::Stmt::FunDecl(fd) => {
                    let ast::FunDecl { sig, block } = fd;
                    let pi = partially_init_sig(sig);
                    self.peek_block().insert_unresolved(UnresolvedValue::Fun(pi, block));
                },
                ast::Stmt::ExternFunDecl(fs) => {
                    let pi = partially_init_sig(fs);
                    self.peek_block().insert_unresolved(UnresolvedValue::ExternFun(pi));
                    }
                ast::Stmt::ClassDecl(cls) => {
                    self.peek_block().insert_unresolved(UnresolvedType::Class(cls));
                },
                ast::Stmt::Import(mp) => {
                    self.peek_block().insert_unresolved(UnresolvedValue::Import(mp));
                },
                ast::Stmt::IGlobal(id, s) => {
                    self.program.declare(&id, plir::ty!("#ptr"));
                    self.push_global(plir::HoistedStmt::IGlobal(id, s))?;
                },
                ast::Stmt::FitClassDecl(ty, methods) => {
                    let ty = self.consume_type(ty)?;
                    let block = self.peek_block();
                    for m in methods {
                        block.insert_unresolved_method(&ty, m);
                    }
                }
                _ => eager_stmts.push(stmt),
            }
        }

        let peek_ctx_type = self.peek_block().expected_ty.clone();
        let len = eager_stmts.len();
        for (i, stmt) in eager_stmts.into_iter().enumerate() {
            self.consume_proc_stmt(stmt, &peek_ctx_type, len - 1 - i)?;
            if !self.peek_block().instrs.is_open() {
                break;
            }
        }

        // resolve remaining unresolved types and values to check for
        // any compiler errors within this code

        let mut unres_types = &mut self.peek_block().unres_types;
        while let Some(ident) = unres_types.keys().next() {
            match unres_types.remove(&ident.clone()).unwrap() {
                UnresolvedType::Class(cls) => self.consume_cls(cls)?,
                UnresolvedType::Import(_) => todo!(),
            }
            unres_types = &mut self.peek_block().unres_types;
        }

        let mut unres_values = &mut self.peek_block().unres_values;
        while let Some(ident) = unres_values.keys().next() {
            match unres_values.remove(&ident.clone()).unwrap() {
                UnresolvedValue::ExternFun(pi) => {
                    let fs = self.consume_fun_sig(pi)?;
                    self.push_global(fs)?;
                },
                UnresolvedValue::Fun(pi, block) => {
                    let sig = self.consume_fun_sig(pi)?;
                    self.consume_fun_block(sig, block)?;
                },
                UnresolvedValue::FunBlock(sig, block) => {
                    self.consume_fun_block(sig, block)?;
                },
                UnresolvedValue::Import(_) => todo!(),
            }
            unres_values = &mut self.peek_block().unres_values;
        }

        Ok(())
    }

    /// Consume a statement into the current insert block.
    /// 
    /// This function returns whether or not the insert block accepts any more statements.
    fn consume_proc_stmt(&mut self, stmt: Located<ast::Stmt>, ctx_type: &Option<plir::Type>, index_til_end: usize) -> PLIRResult<()> {
        use plir::ProcStmt;

        let Located(stmt, range) = stmt;

        match stmt {
            ast::Stmt::Decl(d) => self.consume_decl(Located::new(d, range)),
            ast::Stmt::Return(me) => {
                let maybe_expr = match me {
                    Some(e) => Some(self.consume_expr(e, ctx_type.clone())?),
                    None => None,
                };
                self.peek_block().instrs.push(ProcStmt::Return(maybe_expr).located_at(range));
                Ok(())
            },
            ast::Stmt::Break => {
                self.peek_block().instrs.push(ProcStmt::Break.located_at(range));
                Ok(())
            },
            ast::Stmt::Continue => {
                self.peek_block().instrs.push(ProcStmt::Continue.located_at(range));
                Ok(())
            },
            ast::Stmt::Throw(msg) => {
                self.peek_block().instrs.push(ProcStmt::Throw(msg).located_at(range));
                Ok(())
            },
            ast::Stmt::Expr(e) => {
                let ety = ctx_type.clone()
                    .filter(|_| index_til_end == 0);

                let e = self.consume_located_expr(e, ety)?;
                self.peek_block().instrs.push(e);
                Ok(())
            },
            ast::Stmt::FunDecl(_) => unimplemented!("fun decl should not be resolved eagerly"),
            ast::Stmt::ExternFunDecl(_) => unimplemented!("extern fun decl should not be resolved eagerly"),
            ast::Stmt::ClassDecl(_) => unimplemented!("class decl should not be resolved eagerly"),
            ast::Stmt::Import(_) => unimplemented!("import decl should not be resolved eagerly"),
            ast::Stmt::ImportIntrinsic => Ok(()), // no-op
            ast::Stmt::IGlobal(_, _) => unimplemented!("global decl should not be resolved eagerly"),
            ast::Stmt::FitClassDecl(_, _) => unimplemented!("fit class decl should not be resolved eagerly"),
        }
    }

    fn consume_insert_block(
        &mut self, 
        block: InsertBlock, 
        btype: BlockBehavior
    ) -> PLIRResult<(plir::Block, TerminalFrag)> {
        let InsertBlock { 
            mut instrs, block_range, 
            vars: _, types: _, type_aliases: _,
            unres_values, unres_types, generic_ctx: _,
            expected_ty
        } = block;
        debug_assert!(unres_values.is_empty(), 
            "there was an unresolved value in block: {}", 
            unres_values.keys().next().unwrap()
        );
        debug_assert!(unres_types.is_empty(),  
            "there was an unresolved type in block: {}", 
            unres_types.keys().next().unwrap()
        );

        use plir::ProcStmt;

        // make sure this block ends:
        if instrs.is_open() {
            // we need to add an explicit exit statement.

            // if the stmt given is an Expr, we need to replace the Expr with an explicit `exit` stmt.
            // otherwise just append an `exit` stmt.
            let exit_range = match instrs.last() {
                Some(lstmt) => lstmt.range(),
                None => block_range,
            };
            let exit_mexpr = match instrs.last() {
                Some(Located(ProcStmt::Expr(_), _)) => {
                    let Some(Located(ProcStmt::Expr(e), _)) = instrs.pop() else { unreachable!() };
                    Some(e)
                },
                Some(_) => None,
                None => None,
            };

            let term_instr = if btype == BlockBehavior::Function {
                ProcStmt::Return(exit_mexpr)
            } else {
                ProcStmt::Exit(exit_mexpr)
            };
            instrs.push(term_instr.located_at(exit_range));
        }

        self.consume_instr_block(instrs, expected_ty, btype)
    }
    fn consume_tree_block(
        &mut self, 
        block: Located<ast::Block>, 
        btype: BlockBehavior, 
        expected_ty: Option<plir::Type>
    ) -> PLIRResult<(plir::Block, TerminalFrag)> {
        let Located(ast::Block(stmts), block_range) = block;

        self.push_block(block_range, expected_ty);
        // collect all the statements from this block
        self.consume_stmts(stmts)?;
        let insert_block = self.pop_block().unwrap();
        self.consume_insert_block(insert_block, btype)
    }
    fn consume_instr_block(
        &mut self, 
        mut instrs: InstrBlock, 
        expected_ty: Option<plir::Type>,
        btype: BlockBehavior
    ) -> PLIRResult<(plir::Block, TerminalFrag)> {
        use plir::ty;
        let unpropagated_addrs = instrs.split_off_unpropagated_terminals(btype)?;

        // Type check block:
        if let Some(exp_ty) = expected_ty {
            for laddr in &unpropagated_addrs {
                let Some(stmt) = instrs.get_mut(laddr) else {
                    panic!("no statement at {:?}", &**laddr)
                };

                match stmt {
                    | plir::ProcStmt::Return(me @ Some(_))
                    | plir::ProcStmt::Exit(me @ Some(_))
                    => {
                        let flags = match btype == BlockBehavior::Function {
                            true  => CastFlags::Decl | CastFlags::Void,
                            false => CastFlags::Implicit,
                        };

                        let e = me.take().unwrap();
                        let new_e = self.expect_type(e.located_at(laddr.range()), exp_ty.clone(), flags)?;
                        me.replace(new_e);
                    },

                    | plir::ProcStmt::Return(None)
                    | plir::ProcStmt::Exit(None)
                    | plir::ProcStmt::Break
                    | plir::ProcStmt::Continue => {
                        self.resolver.add_constraint(ty!(plir::Type::S_VOID), exp_ty.clone())
                            .map_err(|e| PLIRErr::TypeConstraintErr(e).at_range(laddr.range()))?;
                    },
                    plir::ProcStmt::Throw(_) => {},
                    
                    | plir::ProcStmt::Decl(_)
                    | plir::ProcStmt::Expr(_)
                    => panic!("not terminal at {:?}", &**laddr),
                }
            }
        }

        let (type_branches, exit_ranges): (Vec<_>, Vec<_>) = unpropagated_addrs.iter()
            .map(|Located(addr, range)| {
                let Some(stmt) = instrs.get(addr) else {
                    panic!("no statement at {addr:?}")
                };
    
                let ty = match stmt {
                    | plir::ProcStmt::Return(Some(e))
                    | plir::ProcStmt::Exit(Some(e))
                    => e.ty.clone(),
    
                    | plir::ProcStmt::Return(None)
                    | plir::ProcStmt::Exit(None)
                    | plir::ProcStmt::Break
                    | plir::ProcStmt::Continue
                    => ty![plir::Type::S_VOID],
    
                    plir::ProcStmt::Throw(_) => ty![plir::Type::S_NEVER],
    
                    | plir::ProcStmt::Decl(_)
                    | plir::ProcStmt::Expr(_)
                    => panic!("not terminal at {addr:?}"),
                };

                (ty, range.clone())
            })
            .unzip();

        let block_value_ty = plir::Type::resolve_branches(&type_branches)
            .ok_or_else(|| {
                let err = PLIRErr::CannotResolveType.at_unknown();
                exit_ranges.into_iter().fold(err, |e, range| e.and_at_range(range))
            })?;
        
        let (instrs, frag) = instrs.unravel();
        
        let stmts = instrs.into_iter()
            .map(|lstmt| lstmt.0)
            .collect();

        Ok((plir::Block(block_value_ty, stmts), frag))
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
                    self.peek_block().instrs.push(Located::new(var.0.into_expr(), stmt_range));
                }
                Ok(())
            },
        }
    }

    /// Consume a declaration into the current insert block.
    /// 
    /// This function returns whether or not the insert block accepts any more statements.
    fn consume_decl(&mut self, decl: Located<ast::Decl>) -> PLIRResult<()> {
        let Located(ast::Decl { rt, pat, ty, val }, decl_range) = decl;
        
        let ty = match ty {
            Some(t) => self.consume_type(t)?,
            None => self.resolver.new_unknown(pat.range()),
        };
        let e = self.consume_located_expr(val, Some(ty.clone()))?;

        self.unpack_pat(pat, e, 
            ((rt, ty), |(rt, ty), idx| {
                // TODO: add special rules for plir::Type::Unk
                let spl_ty = ty.split(idx).map_err(|e| {
                    e.at_range(decl_range.clone())
                })?;

                Ok((*rt, spl_ty))
            }),
            |this, unit, le, extra| {
                let Located(ast::DeclUnit(ident, mt), _) = unit;
                let (rt, ty) = extra;

                // Type check decl, casting if possible
                let e = this.expect_type(le, ty.clone(), CastFlags::Decl)?;

                this.peek_block().declare(&ident, ty.clone());
                let decl = plir::Decl { rt, mt, ident, ty, val: e };
                this.peek_block().instrs.push(Located::new(decl, decl_range.clone()));

                Ok(())
            },
            false,
            decl_range.clone()
        )?;

        Ok(())
    }

    /// Creates a global definition for the external function (allowing it to be generated in LLVM)
    /// and allows the external function to be accessed by the enclosing block.
    pub(super) fn declare_extern_fun(&mut self, fs: plir::FunSignature) -> PLIRResult<()> {
        self.peek_block().declare(&fs.ident, fs.ty().into());
        self.push_global(fs)
    }

    /// Creates a global definition for the function (allowing it to be generated in LLVM)
    /// and allows the function to be accessed by the enclosing block.
    pub(super) fn declare_fun(&mut self, f: plir::FunDecl) -> PLIRResult<()> {
        let ib = self.peek_block();

        // only declare if not already on there
        match ib.vars.get(&f.sig.ident) {
            Some(t) if t != &f.sig.ty() => {
                panic!("declared function {} overrides value of a different type", f.sig.ident)
            }
            None => {
                ib.declare(&f.sig.ident, f.sig.ty().into());
            }
            _ => {}
        }

        self.push_global(f)
    }

    /// Creates a global definition for the class (allowing it to be generated in LLVM)
    /// and allows a class to be accessed by the enclosing block.
    pub(super) fn declare_cls(&mut self, cls: plir::Class) -> PLIRResult<()> {
        self.peek_block().declare_cls(cls.clone());
        self.push_global(cls)
    }

    /// Consume a function signature and convert it into a PLIR function signature.
    fn consume_fun_sig(&mut self, pi: PiFunSig) -> PLIRResult<plir::FunSignature> {
        let PiFunSig { ident, generics, params, varargs, ret } = pi;
        debug_assert!(generics.is_empty(), "todo: generic funs");

        let gctx = if let plir::FunIdent::Static(ty @ plir::Type::Generic(_, _, ()), _) = &ident {
            Some(self.get_generic_context(ty.downgrade()))
        } else {
            None
        };

        // phantom block to encapsulate generic type context
        let ib = self.push_block((0, 0) ..= (0, 0), None);
        ib.generic_ctx = gctx;

        let params = params.into_iter()
            .map(|mp| -> PLIRResult<_> {
                match mp {
                    MaybeInit::Uninit(ast::Param { rt, mt, ident, ty }) => {
                        let ty = match ty {
                            Some(t) => self.consume_type(t)?,
                            None => plir::ty!(plir::Type::S_UNK),
                        };
    
                        Ok(plir::Param { rt, mt, ident, ty })
                    },
                    MaybeInit::Init(plir_par) => Ok(plir_par),
                }
            })
            .collect::<Result<_, _>>()?;
        
        let ret = match ret {
            MaybeInit::Uninit(ast_ty) => self.consume_type(ast_ty)?,
            MaybeInit::Init(plir_ty) => plir_ty,
        };

        self.pop_block();

        let fs = plir::FunSignature {
            /// Only globals should be accessible to external modules
            private: !self.blocks.is_empty(), 
            ident,
            params,
            ret,
            varargs
        };

        self.peek_block().declare(&fs.ident, fs.ty().into());
        Ok(fs)
    }

    /// Consume a function declaration statement into the current insert block.
    /// 
    /// This function returns whether or not the insert block accepts any more statements.
    fn consume_fun_block(&mut self, sig: plir::FunSignature, block: Located<Rc<ast::Block>>) -> PLIRResult<()> {
        let Located(block, block_range) = block;
        let old_block = Rc::try_unwrap(block)
            .unwrap_or_else(|ob| (*ob).clone());
    
        let block = {
            let gctx = if let plir::FunIdent::Static(ty @ plir::Type::Generic(_, _, ()), _) = &sig.ident {
                Some(self.get_generic_context(ty.downgrade()))
            } else {
                None
            };
            let ib = self.push_block(block_range, Some(sig.ret.clone()));
            ib.generic_ctx = gctx;

            for plir::Param { ident, ty, .. } in sig.params.iter() {
                self.peek_block().declare(ident, ty.clone());
            }
    
            // collect all the statements from this block
            self.consume_stmts(old_block.0)?;
    
            let insert_block = self.pop_block().unwrap();
            let (block, frag) = self.consume_insert_block(insert_block, BlockBehavior::Function)?;
            self.peek_block().instrs.add_fragment(frag);

            block
        };

        let fun_decl = plir::FunDecl { sig, block };
        self.declare_fun(fun_decl)
    }

    /// Verifies a [`plir::Type`] is valid (and represents a valid class), making changes to it if necessary.
    /// The new value for the given type should be a valid `plir::Type` and therefore does not need to be checked further.
    pub(super) fn verify_type(&mut self, lty: Located<&mut plir::Type>) -> PLIRResult<()> {
        use plir::TypeRef;
        use std::cmp::Ordering;

        if let Some(aliased_ty) = self.block_iter().find_map(|ib| ib.type_aliases.get(lty.0)) {
            *lty.0 = aliased_ty.clone();
        }

        let Located(ty, range) = lty;
        match ty {
            // no verification is done for these,
            // they do need to be resolved, however
            TypeRef::Unk(_) => Ok(()),
            TypeRef::TypeVar(_, _) => Ok(()),

            TypeRef::Prim(id) | TypeRef::Generic(id, _, _) => {
                // HACK: #ll_array
                if id == "#ll_array" && !self.program.types.contains_key("#ll_array") {
                    let array_type = plir::ty!("#ll_array", [
                        TypeRef::new_type_var("#ll_array", "E")
                    ]);
                    self.program.types.insert(
                        String::from("#ll_array"), 
                        TypeData::primitive(array_type)
                    );
                }

                let id = id.to_string();
                let param_len = self.block_iter().find_map(|ib| {
                    match ib.types.get(&id) {
                        Some(td) => Some(td.generic_params().len()),
                        None => match ib.unres_types.get(&id)? {
                            UnresolvedType::Class(cls) => Some(cls.generics.len()),
                            UnresolvedType::Import(_) => todo!(),
                        },
                    }
                }).ok_or_else(|| {
                    PLIRErr::UndefinedType(ty.clone())
                        .at_range(range.clone())
                })?;

                let arg_len = ty.generic_args().len();

                match param_len.cmp(&arg_len) {
                    Ordering::Less => {
                        Err(PLIRErr::WrongTypeArity(param_len, arg_len).at_range(range))?
                    },
                    Ordering::Equal => {},
                    Ordering::Greater => {
                        let mut params = ty.generic_args().to_vec();
                        params.resize_with(param_len, || self.resolver.new_unknown(range.clone()));
                        *ty = TypeRef::new_generic(id, params);
                    },
                }

                Ok(())
            },

            // TODO
            TypeRef::Tuple(_, _) => Ok(()),
            TypeRef::Fun(_) => Ok(()),
        }
    }

    /// Consumes a located [`ast::Class`] into a located concrete [`plir::Type`].
    fn consume_located_type(&mut self, ty: Located<ast::Type>) -> PLIRResult<Located<plir::Type>> {
        let Located(ast::Type(ty_ident, ty_params), range) = ty;
        
        let ty = {
            if ty_params.is_empty() {
                self.block_iter()
                    .filter_map(|ib| ib.generic_ctx.as_ref())
                    .find(|ctx| ctx.1.contains_key(&ty_ident))
                    .map_or_else(
                        || plir::Type::new_prim(ty_ident.to_string()), 
                        |ctx| plir::Type::new_type_var(ctx.0.to_string(), ty_ident.to_string())
                    )
            } else {
                let ty_params: Vec<_> = ty_params.into_iter()
                    .map(|t| self.consume_type(t))
                    .collect::<Result<_, _>>()?;
                
                plir::Type::new_generic(ty_ident, ty_params)
            }
        };

        let mut lty = Located::new(ty, range);
        self.verify_type(lty.as_mut())?;
        Ok(lty)
    }

    /// Consumes a located [`ast::Class`] into a concrete [`plir::Type`].
    fn consume_type(&mut self, ty: Located<ast::Type>) -> PLIRResult<plir::Type> {
        self.consume_located_type(ty)
            .map(|lty| lty.0)
    }

    fn consume_cls(&mut self, cls: ast::Class) -> PLIRResult<()> {
        let ast::Class { ident: cls_id, generics, fields, methods } = cls;

        let ib = self.push_block((0, 0) ..= (0, 0), None);
        ib.generic_ctx = Some(GenericContext(cls_id.to_string(), {
            generics.iter()
                .map(|p| {
                    (p.to_string(), plir::Type::new_type_var(cls_id.to_string(), p.to_string()))
                })
                .collect()
        }));

        let fields = fields.into_iter()
            .map(|ast::FieldDecl { rt, mt, ident: field_id, ty }| -> PLIRResult<_> {
                let ty = self.consume_type(ty)?;
                Ok((field_id, plir::Field { rt, mt, ty }))
            })
            .collect::<Result<_, _>>()?;
        
        self.pop_block();
        let params = generics.into_iter()
            .map(|p| plir::Type::new_type_var(cls_id.clone(), p));
        
        let cls = plir::Class { ty: plir::Type::new_generic(cls_id.clone(), params), fields };
        for m in methods {
            self.peek_block().insert_unresolved_method(&cls.ty, m);
        }
        self.declare_cls(cls)?;

        Ok(())
    }
    fn consume_expr(&mut self, value: Located<ast::Expr>, ctx_type: Option<plir::Type>) -> PLIRResult<plir::Expr> {
        let Located(expr, range) = value;
        
        match expr {
            ast::Expr::Ident(ident) => {
                let ty = self.get_var_type_or_err(&ident, range)?;

                Ok(plir::Expr::new(
                    ty,
                    plir::ExprType::Ident(ident)
                ))
            },
            ast::Expr::Block(b) => {
                let (block, frag) = self.consume_tree_block(b, BlockBehavior::Bare, None)?;
                self.peek_block().instrs.add_fragment(frag);

                Ok(plir::Expr::new(block.0.clone(), plir::ExprType::Block(block)))
            },
            ast::Expr::Literal(literal) => {
                let mut ty = match literal {
                    ast::Literal::Int(_)   => plir::ty!(plir::Type::S_INT),
                    ast::Literal::Float(_) => plir::ty!(plir::Type::S_FLOAT),
                    ast::Literal::Char(_)  => plir::ty!(plir::Type::S_CHAR),
                    ast::Literal::Str(_)   => plir::ty!(plir::Type::S_STR),
                    ast::Literal::Bool(_)  => plir::ty!(plir::Type::S_BOOL)
                };

                // check type exists (for string, this may not happen if std isn't loaded)
                self.verify_type(Located::new(&mut ty, range))?;
                //

                Ok(plir::Expr::new(
                    ty, plir::ExprType::Literal(literal)
                ))
            },
            ast::Expr::ListLiteral(lst) => {
                let el_ty = match ctx_type.as_ref().map(plir::Type::downgrade) {
                    Some(plir::TypeRef::Generic(Cow::Borrowed(plir::Type::S_LIST), Cow::Borrowed([t]), ())) => Some(t.clone()),
                    None => None,
                    _ => Err(PLIRErr::CannotResolveType.at_range(range.clone()))?
                };

                let new_inner: Vec<_> = lst.into_iter()
                    .map(|e| self.consume_expr(e, el_ty.clone()))
                    .collect::<Result<_, _>>()?;

                let el_ty = plir::Type::resolve_collection_ty(new_inner.iter().map(|e| &e.ty))
                    .or(el_ty)
                    .ok_or_else(|| PLIRErr::CannotResolveType.at_range(range.clone()))?;

                let mut coll_ty = plir::ty!(plir::Type::S_LIST, [el_ty]);
                self.verify_type(Located::new(&mut coll_ty, range))?;

                Ok(plir::Expr::new(
                    coll_ty,
                    plir::ExprType::ListLiteral(new_inner)
                ))
            },
            ast::Expr::SetLiteral(set) => {
                let el_ty = match ctx_type.as_ref().map(plir::Type::downgrade) {
                    Some(plir::TypeRef::Generic(Cow::Borrowed(plir::Type::S_SET), Cow::Borrowed([t]), ())) => Some(t.clone()),
                    None => None,
                    _ => Err(PLIRErr::CannotResolveType.at_range(range.clone()))?
                };

                let new_inner: Vec<_> = set.into_iter()
                    .map(|e| self.consume_expr(e, el_ty.clone()))
                    .collect::<Result<_, _>>()?;

                let el_ty = plir::Type::resolve_collection_ty(new_inner.iter().map(|e| &e.ty))
                    .or(el_ty)
                    .ok_or_else(|| PLIRErr::CannotResolveType.at_range(range.clone()))?;

                let mut coll_ty = plir::ty!(plir::Type::S_SET, [el_ty]);
                self.verify_type(Located::new(&mut coll_ty, range))?;
    
                Ok(plir::Expr::new(
                    coll_ty,
                    plir::ExprType::SetLiteral(new_inner)
                ))
            },
            ast::Expr::DictLiteral(entries) => {
                let (kty, vty) = match ctx_type.as_ref().map(plir::Type::downgrade) {
                    Some(plir::TypeRef::Generic(Cow::Borrowed(plir::Type::S_DICT), Cow::Borrowed([k, v]), ())) => {
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
                    .ok_or_else(|| PLIRErr::CannotResolveType.at_range(range.clone()))?;
                
                let mut coll_ty = plir::ty!(plir::Type::S_DICT, [key_ty, val_ty]);
                self.verify_type(Located::new(&mut coll_ty, range))?;

                Ok(plir::Expr::new(
                    coll_ty,
                    plir::ExprType::DictLiteral(new_inner)
                ))
            },
            ast::Expr::ClassLiteral(ty, entries) => {
                let lty = self.consume_located_type(ty)?;
                
                let cls_key = self.get_class_key(lty.as_ref())?;
                let cls_fields: IndexMap<_, _> = self.get_class(&cls_key).fields()
                    .ok_or_else(|| {
                        PLIRErr::CannotInitialize(lty.0.clone()).at_range(lty.range())
                    })?
                    .into_iter()
                    .map(|(k, fd)| (k, fd.ty))
                    .collect();
                
                let mut entries: HashMap<_, _> = entries.into_iter()
                    .map(|(Located(k, krange), v)| (k, (krange, v)))
                    .collect();
                
                let new_entries = cls_fields.into_iter()
                    .map(|(k, field_ty)| {
                        let (_, ast_expr) = entries.remove(&k)
                            .ok_or_else(|| {
                                PLIRErr::UninitializedField(lty.0.clone(), k)
                                    .at_range(lty.range())
                            })?;
                        
                        let lfield_expr = self.consume_located_expr(ast_expr, Some(field_ty.clone()))?;
                        
                        self.expect_type(lfield_expr, field_ty, CastFlags::Implicit)
                    })
                    .collect::<PLIRResult<_>>()?;
                
                if let Some((f, (krange, _))) = entries.into_iter().next() {
                    Err(PLIRErr::UnexpectedField(lty.0, f).at_range(krange))
                } else {
                    Ok(plir::Expr::new(
                        lty.0.clone(), 
                        plir::ExprType::ClassLiteral(lty.0, new_entries)
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

                        this.peek_block().instrs.push(Located::new(asg, range));
                        Ok(())
                    },
                    true,
                    range
                )?;
                
                let insert_block = self.pop_block().unwrap();
                let (block, frag) = self.consume_insert_block(insert_block, BlockBehavior::Bare)?;
                self.peek_block().instrs.add_fragment(frag);

                Ok(plir::Expr::new(block.0.clone(), plir::ExprType::Block(block)))
            },
            ast::Expr::Path(p) => {
                self.consume_path(Located::new(p, range)).map(Into::into)
            },
            ast::Expr::StaticPath(sp) => {
                let ast::StaticPath { ty, attr } = sp;

                let lty = self.consume_located_type(ty)?;

                let cls_key = self.get_class_key(lty.as_ref())?;
                let (_, met_type) = self.get_method_or_err(
                    &cls_key, &attr, range
                )?;
                
                Ok(plir::Path::Static(lty.0, attr, met_type).into())
            },
            ast::Expr::UnaryOps { ops, expr } => {
                let le = self.consume_located_expr(*expr, None)?;
                
                ops.into_iter().rev()
                    .try_fold(le, |expr, op| self.apply_unary(expr, op, range.clone()))
                    .map(|le| le.0)
            },
            ast::Expr::BinaryOp { op, left, right } => {
                let left = self.consume_located_expr(*left, None)?;
                let right = self.consume_located_expr(*right, None)?;
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
                    .ok_or_else(|| PLIRErr::CannotResolveType.at_range(range))?;

                Ok(plir::Expr::new(
                    plir::ty!(plir::Type::S_RANGE, [ty]),
                    plir::ExprType::Range { left, right, step }
                ))
            },
            ast::Expr::If { conditionals, last } => {
                let (conditionals, mut frags): (Vec<_>, Vec<_>) = conditionals.into_iter()
                    .map(|(cond, block)| {
                        let c = self.consume_expr_truth(cond)?;
                        let (b, frag) = self.consume_tree_block(block, BlockBehavior::Bare, ctx_type.clone())?;
                        Ok(((c, b), frag))
                    })
                    .collect::<PLIRResult<Vec<_>>>()?
                    .into_iter()
                    .unzip();
                
                let (last, frag) = match last {
                    Some(blk) => {
                        let (block, frag) = self.consume_tree_block(blk, BlockBehavior::Bare, ctx_type)?;
                        (Some(block), frag)
                    },
                    None => (None, TerminalFrag::new()),
                };
                frags.push(frag);
                self.peek_block().instrs.add_conditional_fragments(frags);

                let void_ = plir::ty!(plir::Type::S_VOID);
                let else_ty = last.as_ref()
                    .map(|b| &b.0)
                    .unwrap_or(&void_);

                let type_iter = conditionals.iter()
                    .map(|(_, block)| &block.0)
                    .chain(std::iter::once(else_ty));

                Ok(plir::Expr::new(
                    plir::Type::resolve_branches(type_iter)
                        .ok_or_else(|| PLIRErr::CannotResolveType.at_range(range))?,
                    plir::ExprType::If { conditionals, last }
                ))
            },
            ast::Expr::While { condition, block } => {
                let condition = self.consume_expr_truth(*condition)?;
                let (block, frag) = self.consume_tree_block(block, BlockBehavior::Loop, None)?;
                self.peek_block().instrs.add_fragment(frag);

                Ok(plir::Expr::new(
                    plir::ty!(plir::Type::S_VOID),
                    plir::ExprType::While { condition: Box::new(condition), block }
                ))
            },
            ast::Expr::For { ident, iterator, block } => {
                let itrange = iterator.range();
                let iterator = self.consume_expr_and_box(*iterator, None)?;

                // FIXME: cleanup
                let cls_key = self.get_class_key(Located::new(&iterator.ty, itrange.clone()))?;
                let (_, it_next_ty) = self.get_method_or_err(&cls_key, "next", itrange.clone())?;

                let element_type = match it_next_ty.downgrade() {
                    plir::TypeRef::Fun(plir::FunTypeRef {
                        params: Cow::Borrowed([a]),
                        ret,
                        varargs: false
                    }) if a == &iterator.ty => {
                        let plir::TypeRef::Generic(Cow::Borrowed("option"), Cow::Borrowed([rp]), ()) = &**ret else {
                            return Err(PLIRErr::CannotIterateType(iterator.ty.clone()).at_range(itrange))?
                        };
                        let idty = rp.clone();

                        // FIXME: put this inside block scope
                        self.peek_block().declare(&ident, idty.clone());

                        idty
                    }
                    _ => Err(PLIRErr::CannotIterateType(iterator.ty.clone()).at_range(itrange))?,
                };

                let (block, frag) = self.consume_tree_block(block, BlockBehavior::Loop, None)?;
                self.peek_block().instrs.add_fragment(frag);

                Ok(plir::Expr::new(
                    plir::ty!(plir::Type::S_VOID),
                    plir::ExprType::For { ident, element_type, iterator, block }
                ))
            },
            ast::Expr::Call { funct, params } => {
                let (largs, funct) = match *funct {
                    // HACK: GEP, alloca, size_of
                    Located(ast::Expr::StaticPath(sp), _) if sp.attr == "#gep" => {
                        let ty = self.consume_type(sp.ty)?;

                        let ptr_ = plir::ty!("#ptr");
                        let int_ = plir::ty!(plir::Type::S_INT);

                        let mut piter = params.into_iter();
                        let ptr = piter.next()
                            .ok_or_else(|| PLIRErr::WrongArity(1, 0).at_range(range))
                            .and_then(|e| self.consume_located_expr(e, Some(ptr_.clone())))
                            .and_then(|le| self.expect_type(le, ptr_, CastFlags::Decl))
                            .map(Box::new)?;

                        let params = piter.map(|expr| {
                            let lparam = self.consume_located_expr(
                                expr, Some(int_.clone())
                            )?;
                            
                            self.expect_type(lparam, int_.clone(), CastFlags::Decl)
                        })
                        .collect::<Result<_, _>>()?;

                        return Ok(plir::Expr::new(
                            plir::ty!("#ptr"),
                            plir::ExprType::GEP(ty, ptr, params)
                        ))
                    },
                    Located(ast::Expr::StaticPath(sp), _) if sp.attr == "#alloca" => {
                        return Ok(plir::Expr::new(
                            plir::ty!("#ptr"),
                            plir::ExprType::Alloca(self.consume_type(sp.ty)?)
                        ))
                    },
                    Located(ast::Expr::StaticPath(sp), _) if sp.attr == "size_of" => {
                        return Ok(plir::Expr::new(
                            plir::ty!(plir::Type::S_INT),
                            plir::ExprType::SizeOf(self.consume_type(sp.ty)?)
                        ))
                    }
                    e => {
                        let largs: Vec<_> = params.into_iter()
                            .map(|le| self.consume_located_expr(le, None))
                            .collect::<Result<_, _>>()?;

                        let arg_tys = largs.iter()
                            .map(|e| e.ty.clone());
                        let fun_ty = plir::Type::new_fun(
                            arg_tys, 
                            ctx_type.unwrap_or(plir::ty!(plir::Type::S_VOID)), 
                            false
                        );
                        (largs, self.consume_located_expr(e, Some(fun_ty))?)
                    }
                };

                match &funct.ty {
                    plir::Type::Fun(plir::FunType { params: param_tys, ret: _, varargs }) => {
                        let bad_arity = if *varargs {
                            param_tys.len() > largs.len()
                        } else {
                            param_tys.len() != largs.len()
                        };

                        if bad_arity {
                            let err = PLIRErr::WrongArity(param_tys.len(), largs.len()).at_range(range);
                            return Err(err);
                        }


                        let pty_iter = param_tys.iter();
                        let mut largs = largs.into_iter();
                        let mut args: Vec<_> = std::iter::zip(pty_iter, largs.by_ref())
                            .map(|(pty, larg)| self.expect_type(larg, pty.clone(), CastFlags::Decl))
                            .collect::<Result<_, _>>()?;
                        args.extend(largs.map(|la| la.0));

                        plir::Expr::call(funct, args)
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
        self.consume_located_expr(expr, None)
            .and_then(|le| self.expect_type(le, plir::ty!(plir::Type::S_BOOL), CastFlags::Truth))
    }

    fn consume_path(&mut self, p: Located<ast::Path>) -> PLIRResult<plir::Path> {
        let Located(ast::Path { obj, attrs }, expr_range) = p;

        let obj = self.consume_expr_and_box(*obj, None)?;
        let mut path = plir::Path::Struct(obj, vec![]);

        for attr in attrs {
            let top_ty = path.ty();

            let cls_key = self.get_class_key(Located::new(&top_ty, expr_range.clone()))?;
            let cls = self.get_class(&cls_key);

            if cls.has_method(&attr) {
                if matches!(path, plir::Path::Method(..)) {
                    Err(PLIRErr::CannotAccessOnMethod.at_range(expr_range.clone()))?;
                } else {
                    let (_, fun_ty) = self.get_method_or_err(&cls_key, &attr, expr_range.clone())?;
                    let mut fun_ty: plir::FunType = fun_ty.try_into()?;
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
                        let id = plir::FunIdent::new_static(&top_ty, &attr);
                        PLIRErr::UndefinedVarAttr(id).at_range(expr_range.clone())
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
        let index = self.consume_located_expr(*index, None)?;
        
        self.apply_index(expr, index, expr_range)
    }

    fn consume_deref(&mut self, d: Located<ast::IDeref>, ty: plir::Type) -> PLIRResult<plir::IDeref> {
        let Located(ast::IDeref(e), _) = d;
        let Located(expr, expr_range) = self.consume_located_expr(*e, None)?;

        if let plir::TypeRef::Prim(Cow::Borrowed("#ptr")) = expr.ty.downgrade() {
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
            // "fun_call_fail_3"
        ])
    }
}