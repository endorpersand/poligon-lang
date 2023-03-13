//! Static variable resolver for the interpreter runtime.
//! 
//! When executing a [`ast::Program`], the static resolver is executed first
//! to lexically scope variables, validate `return`, `break`, `continue`, etc.
//! 
//! Instead of being run via program flow, the resolver scans the code statically
//! (which enables lexical scope and other things).
//! 
//! The main item of this module is [`TraverseResolve`], the trait enabling semantic traversal.

use std::collections::{HashMap, HashSet};

use ast::Located;

use crate::err::{GonErr, FullGonErr};
use crate::ast;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum SubType {
    None,
    List,
    Pattern
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum BlockType {
    Function,
    Loop,
    Program
}

#[derive(Debug, PartialEq, Eq)]
struct Local {
    vars: HashSet<String>,
    block_type: Option<BlockType>,
    sub_types: Vec<SubType>
}

impl Local {
    fn new(bt: Option<BlockType>) -> Self {
        Self {
            vars: HashSet::new(),
            block_type: bt,
            sub_types: vec![],
        }
    }

    fn insert(&mut self, s: String) -> bool {
        self.vars.insert(s)
    }

    fn contains(&self, s: &str) -> bool {
        self.vars.contains(s)
    }
}

/// An error that occurs in the variable resolution process.
#[derive(Debug, PartialEq, Eq)]
pub enum ResolveErr {
    /// Cannot call `return` from this block.
    CannotReturn,
    /// Cannot call `break` from this block.
    CannotBreak,
    /// Cannot call `continue` from this block.
    CannotContinue,
    /// Cannot spread here.
    CannotSpread,
    /// Cannot spread nothing here.
    CannotSpreadNone,
    /// This feature isn't implemented in the interpreter.
    CompilerOnly(&'static str)
}

impl GonErr for ResolveErr {
    fn err_name(&self) -> &'static str {
        "syntax error"
    }

    fn message(&self) -> String {
        match self {
            ResolveErr::CannotReturn => String::from("cannot 'return' here"),
            ResolveErr::CannotBreak => String::from("cannot 'break' here"),
            ResolveErr::CannotContinue => String::from("cannot 'continue' here"),
            ResolveErr::CannotSpread => String::from("cannot spread here"),
            ResolveErr::CannotSpreadNone => String::from("cannot empty spread here"),
            ResolveErr::CompilerOnly(s) => format!("compiler-only feature - {s}"),
        }
    }
}

type FullResolveErr = FullGonErr<ResolveErr>;
/// A [`Result`] type for operations in the static resolution process.
pub type ResolveResult<T> = Result<T, FullResolveErr>;

/// A struct that holds the resolved variables during the static resolution process.
#[derive(Debug, PartialEq, Eq)]
pub struct ResolveState {
    /// For every expression with a defined variable reference, 
    /// this map indicates the number of blocks up where this variable is defined
    steps: HashMap<*const ast::Expr, usize>,
    /// The local scope settings
    locals: Vec<Local>,
    /// Global scope settings
    global_subs: Vec<SubType>,
}

impl ResolveState {
    /// Create a new ResolveState.
    pub fn new() -> Self {
        Self { steps: HashMap::new(), locals: vec![], global_subs: vec![] }
    }

    fn block_type(&self) -> BlockType {
        self.locals.iter()
            .rev()
            .find_map(|local| local.block_type)
            .unwrap_or(BlockType::Program)
    }

    fn top_sub(&self) -> &[SubType] {
        self.locals.last()
            .map_or(&self.global_subs, |local| &local.sub_types)
    }
    fn top_sub_mut(&mut self) -> &mut Vec<SubType> {
        self.locals.last_mut()
            .map_or(&mut self.global_subs, |local| &mut local.sub_types)
    }
    fn sub_type(&self) -> SubType {
        self.top_sub()
            .last()
            .copied()
            .unwrap_or(SubType::None)
    }

    fn scope<F, T>(&mut self, f: F) -> ResolveResult<T>
        where F: FnOnce(&mut Self) -> ResolveResult<T>
    {
        self.locals.push(Local::new(None));
        let t = f(self);
        self.locals.pop();
        
        t
    }

    fn typed_scope<F, T>(&mut self, ty: BlockType, f: F) -> ResolveResult<T>
        where F: FnOnce(&mut Self) -> ResolveResult<T>
    {
        self.locals.push(Local::new(Some(ty)));
        let t = f(self);
        self.locals.pop();

        t
    }

    fn with_sub<F, T>(&mut self, ty: SubType, f: F) -> ResolveResult<T>
        where F: FnOnce(&mut Self) -> ResolveResult<T>
    {
        self.top_sub_mut().push(ty);
        let t = f(self);
        self.top_sub_mut().pop();

        t
    }

    // fn partial_declare(&mut self, ident: &str) {
    //     self.vars.last_mut().unwrap()
    //         .insert(String::from(ident), false);
    // }

    fn declare(&mut self, ident: &str) {
        if let Some(locals) = self.locals.last_mut() {
            locals.insert(String::from(ident));
        }
    }

    fn resolve(&mut self, e: &ast::Expr, ident: &str) {
        // at depth 0, this means it is in our scope and we have to traverse 0 scopes
        // at depth 1, it is one scope above
        // etc.
        let scope_count = self.locals.iter().rev()
            .enumerate()
            .find(|(_, scope)| scope.contains(ident))
            .map(|(idx, _)| idx);
        
        if let Some(sc) = scope_count {
            self.steps.insert(e as _, sc);
        }
    }

    /// Wipe all defined variables and scope settings from this ResolveState.
    pub fn clear(&mut self) {
        self.steps.clear();
        self.locals.clear();
        self.global_subs.clear();
    }
    
    /// Statically traverse over a tree and add it to the resolve state.
    pub fn traverse<T: TraverseResolve>(&mut self, t: &T) -> ResolveResult<()> {
        t.traverse_rs(self)
    }

    /// If this expression has a variable, return the number of blocks
    /// up where this variable is defined.
    /// 
    /// Otherwise return None.
    pub fn get_steps(&self, t: &ast::Expr) -> Option<usize> {
        self.steps.get(&(t as *const _)).copied()
    }
}

impl Default for ResolveState {
    fn default() -> Self {
        Self::new()
    }
}

/// This trait is implemented for values that can be traversed in the runtime process.
/// 
/// A traversal can be initiated either with the [`TraverseResolve::traverse_rs`] method 
/// or [`ResolveState::traverse`].
pub trait TraverseResolve {
    /// Traverses through this node, adding whatever information from the node to the ResolveState.
    /// 
    /// This function is fallible, returning Err if it fails.
    fn traverse_rs(&self, map: &mut ResolveState) -> ResolveResult<()>;
}
trait TRsDependent {
    fn traverse_rs(&self, map: &mut ResolveState, e: &ast::Expr) -> ResolveResult<()>;
}

impl<T: TraverseResolve> TraverseResolve for [T] {
    fn traverse_rs(&self, map: &mut ResolveState) -> ResolveResult<()> {
        self.iter().try_for_each(|t| t.traverse_rs(map))
    }
}
impl<T: TraverseResolve> TraverseResolve for &T {
    fn traverse_rs(&self, map: &mut ResolveState) -> ResolveResult<()> {
        (*self).traverse_rs(map)
    }
}
impl<T: TraverseResolve> TraverseResolve for Option<T> {
    fn traverse_rs(&self, map: &mut ResolveState) -> ResolveResult<()> {
        if let Some(t) = self {
            t.traverse_rs(map)
        } else {
            Ok(())
        }
    }
}
impl<T: TRsDependent> TRsDependent for [T] {
    fn traverse_rs(&self, map: &mut ResolveState, e: &ast::Expr) -> ResolveResult<()> {
        self.iter().try_for_each(|t| t.traverse_rs(map, e))
    }
}

// As a program, we do not want to create another scope.
impl TraverseResolve for ast::Program {
    fn traverse_rs(&self, map: &mut ResolveState) -> ResolveResult<()> {
        self.0.traverse_rs(map)
    }
}

// The Block node's default traversal is to create a new scope without modifying the type.
// This may differ from an intended goal, and in that case, 
// the implementation for [T] should be used instead.
impl TraverseResolve for ast::Block {
    fn traverse_rs(&self, map: &mut ResolveState) -> ResolveResult<()> {
        map.scope(|map| {
            self.0.traverse_rs(map)
        })
    }
}

impl TraverseResolve for Located<ast::Stmt> {
    fn traverse_rs(&self, map: &mut ResolveState) -> ResolveResult<()> {
        let Located(stmt, range) = self;
        let range = range.clone();

        match stmt {
            ast::Stmt::Decl(d)   => d.traverse_rs(map),
            ast::Stmt::Return(e) => match map.block_type() {
                BlockType::Function => e.traverse_rs(map),
                _ => Err(ResolveErr::CannotReturn.at_range(range))?
            },
            ast::Stmt::Break => match map.block_type() {
                BlockType::Loop => Ok(()),
                _ => Err(ResolveErr::CannotBreak.at_range(range))?
            },
            ast::Stmt::Continue => match map.block_type() {
                BlockType::Loop => Ok(()),
                _ => Err(ResolveErr::CannotContinue.at_range(range))?
            },
            ast::Stmt::FunDecl(f) => f.traverse_rs(map),
            ast::Stmt::ExternFunDecl(_) => Err(ResolveErr::CompilerOnly("extern function declarations").at_range(range))?,
            ast::Stmt::Expr(e)   => e.traverse_rs(map),
            ast::Stmt::ClassDecl(_) => Err(ResolveErr::CompilerOnly("classes").at_range(range))?,
            ast::Stmt::Import(_) => Err(ResolveErr::CompilerOnly("importing").at_range(range))?,
            ast::Stmt::ImportIntrinsic => Err(ResolveErr::CompilerOnly("importing").at_range(range))?,
        }
    }
}

impl TraverseResolve for ast::Located<ast::Expr> {
    fn traverse_rs(&self, map: &mut ResolveState) -> ResolveResult<()> {
        let ast::Located(expr, range) = self;
        let range = range.clone();

        match expr {
            e @ ast::Expr::Ident(s) => {
                map.resolve(e, s);
                Ok(())
            },
            ast::Expr::Block(p) => p.traverse_rs(map),
            ast::Expr::Literal(_) => Ok(()),
            ast::Expr::ListLiteral(l) => {
                map.with_sub(SubType::List, |map| l.traverse_rs(map))
            },
            ast::Expr::SetLiteral(s) => s.traverse_rs(map),
            ast::Expr::DictLiteral(d) => {
                for (k, v) in d {
                    k.traverse_rs(map)?;
                    v.traverse_rs(map)?;
                }

                Ok(())
            },
            ast::Expr::ClassLiteral(_, _) => Err(ResolveErr::CompilerOnly("classes").at_range(range)),
            ast::Expr::Assign(lhs, rhs) => {
                rhs.traverse_rs(map)?;
                map.with_sub(SubType::Pattern, |map| lhs.traverse_rs(map, self))
            },
            ast::Expr::Path(p) => p.obj.traverse_rs(map),
            ast::Expr::StaticPath(_) => Ok(()),
            ast::Expr::UnaryOps { ops: _, expr } => expr.traverse_rs(map),
            ast::Expr::BinaryOp { op: _, left, right } => {
                left.traverse_rs(map)?;
                right.traverse_rs(map)
            },
            ast::Expr::Comparison { left, rights } => {
                left.traverse_rs(map)?;
                for (_, e) in rights {
                    e.traverse_rs(map)?;
                }
                Ok(())
            },
            ast::Expr::Range { left, right, step } => {
                left.traverse_rs(map)?;
                right.traverse_rs(map)?;
                if let Some(t) = step {
                    t.traverse_rs(map)?;
                }
                Ok(())
            },
            ast::Expr::If { conditionals, last } => {
                for (e, p) in conditionals {
                    e.traverse_rs(map)?;
                    p.traverse_rs(map)?;
                }
        
                last.as_deref().traverse_rs(map)
            },
            ast::Expr::While { condition, block } => {
                condition.traverse_rs(map)?;
                map.typed_scope(BlockType::Loop, |map| {
                    block.0.traverse_rs(map)
                })
            },
            ast::Expr::For { ident, iterator, block } => {
                iterator.traverse_rs(map)?;

                map.typed_scope(BlockType::Loop, |map| {
                    map.declare(ident);
                    block.0.traverse_rs(map)
                })
            },
            ast::Expr::Call { funct, params } => {
                funct.traverse_rs(map)?;
                params.traverse_rs(map)
            },
            ast::Expr::Index(idx) => idx.traverse_rs(map),
            ast::Expr::Spread(e) => match map.sub_type() {
                SubType::None => Err(ResolveErr::CannotSpread.at_range(range)),
                SubType::List => match e {
                    Some(inner) => inner.traverse_rs(map),
                    None => Err(ResolveErr::CannotSpreadNone.at_range(range)),
                },
                SubType::Pattern => match e {
                    Some(inner) => inner.traverse_rs(map),
                    None => Ok(()),
                },
            },
        }
    }
}

impl TraverseResolve for ast::Decl {
    fn traverse_rs(&self, map: &mut ResolveState) -> ResolveResult<()> {
        let ast::Decl { pat, val, .. } = self;

        pat.traverse_rs(map)?;
        val.traverse_rs(map)
    }
}

impl TraverseResolve for ast::FunDecl {
    fn traverse_rs(&self, map: &mut ResolveState) -> ResolveResult<()> {
        map.declare(&self.sig.ident);

        map.typed_scope(BlockType::Function, |map| {
            for p in &self.sig.params {
                map.declare(&p.ident);
            }
    
            self.block.0.traverse_rs(map)
        })
    }
}

impl TraverseResolve for ast::Index {
    fn traverse_rs(&self, map: &mut ResolveState) -> ResolveResult<()> {
        self.expr.traverse_rs(map)?;
        self.index.traverse_rs(map)
    }
}

impl<T: TRsDependent> TRsDependent for ast::Pat<T> {
    fn traverse_rs(&self, map: &mut ResolveState, e: &ast::Expr) -> ResolveResult<()> {
        match self {
            ast::Pat::Unit(u) => u.traverse_rs(map, e),
            ast::Pat::List(lst) => lst.iter().try_for_each(|t| t.traverse_rs(map, e)),
            ast::Pat::Spread(mp) => match mp {
                Some(p) => p.traverse_rs(map, e),
                None => Ok(()),
            },
        }
    }
}
impl<T: TraverseResolve> TraverseResolve for ast::Pat<T> {
    fn traverse_rs(&self, map: &mut ResolveState) -> ResolveResult<()> {
        match self {
            ast::Pat::Unit(u) => u.traverse_rs(map),
            ast::Pat::List(lst) => lst.iter().try_for_each(|t| t.traverse_rs(map)),
            ast::Pat::Spread(mp) => match mp {
                Some(p) => p.traverse_rs(map),
                None => Ok(()),
            },
        }
    }
}

impl TRsDependent for ast::AsgUnit {
    fn traverse_rs(&self, map: &mut ResolveState, e: &ast::Expr) -> ResolveResult<()> {
        match self {
            ast::AsgUnit::Ident(ident) => {
                map.resolve(e, ident);
                Ok(())
            },
            ast::AsgUnit::Path(p) => p.obj.traverse_rs(map),
            ast::AsgUnit::Index(idx) => idx.traverse_rs(map),
        }
    }
}
impl TraverseResolve for ast::DeclUnit {
    fn traverse_rs(&self, map: &mut ResolveState) -> ResolveResult<()> {
        map.declare(&self.0);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::interpreter::semantic::ResolveState;
    use crate::{ast::*, Interpreter};

    use super::ResolveResult;

    macro_rules! map {
        () => {
            HashMap::new()
        };
        ($($k:expr => $v:expr),+) => {
            {
                let mut m = HashMap::new();
                $(m.insert($k, $v);)+
                m
            }
        };
    }

    macro_rules! Block {
        ($e:ident) => { Located(Stmt::Expr(Located(Expr::Block(Located($e, _)), _)), _) }
    }
    #[test]
    fn nonexistent_var() -> ResolveResult<()> {
        let program = Interpreter::from_string("a;")
            .parse().unwrap();

        let mut state = ResolveState::new();
        state.traverse(&program)?;

        assert_eq!(&state.steps, &map!{});
        Ok(())
    }

    #[test]
    fn declare_in_scope() -> ResolveResult<()> {
        let program = Interpreter::from_string("
            const a = 0;
            a;
        ").parse().unwrap();

        let mut state = ResolveState::new();
        state.traverse(&program)?;

        assert_eq!(&state.steps, &map!{});

        let program = Interpreter::from_string("
            const a = 0;
            {
                a;
            }
        ").parse().unwrap();

        let mut state = ResolveState::new();
        state.traverse(&program)?;

        assert_eq!(&state.steps, &map!{});

        let program = Interpreter::from_string("
            {
                const a = 0;
                {
                    a;
                }
            }
        ").parse().unwrap();

        let Block!(block) = &program.0[0] else { unreachable!() };
        let Block!(block) = &block.0[1] else { unreachable!() };
        let Located(Stmt::Expr(e), _) = &block.0[0] else { unreachable!() };

        let mut state = ResolveState::new();
        state.traverse(&program)?;

        let steps = map! {
            &**e as _ => 1
        };

        assert_eq!(&state.steps, &steps);
        Ok(())
    }

    #[test]
    fn lex_scope_closure() -> ResolveResult<()> {
        let program = Interpreter::from_string("
        let a = \"global\";

        {
            fun showA() {
                print(a);
            }

            {
                showA();
                let a = \"block\";
                showA();
            }
        }
        ").parse().ok().unwrap();

        let mut state = ResolveState::new();
        state.traverse(&program)?;

        println!("{:?}", state);
        Ok(())
    }
}