use std::collections::{HashMap, HashSet};

use crate::tree;

use super::runtime;

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
#[derive(Debug, PartialEq, Eq)]
pub struct ResolveState {
    steps: HashMap<*const tree::Expr, usize>,
    locals: Vec<Local>,
    global_subs: Vec<SubType>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ResolveErr {
    CannotReturn,
    CannotBreak,
    CannotContinue,
    CannotSpread,
    CannotSpreadNone
}
pub type ResolveResult<T> = Result<T, ResolveErr>;

impl From<ResolveErr> for runtime::RuntimeErr {
    fn from(re: ResolveErr) -> Self {
        match re {
            ResolveErr::CannotReturn     => Self::CannotReturn,
            ResolveErr::CannotBreak      => Self::CannotBreak,
            ResolveErr::CannotContinue   => Self::CannotContinue,
            ResolveErr::CannotSpread     => Self::CannotSpread,
            ResolveErr::CannotSpreadNone => Self::CannotSpreadNone,
        }
    }
}

impl ResolveState {
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

    fn resolve(&mut self, e: &tree::Expr, ident: &str) {
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

    pub fn clear(&mut self) {
        self.steps.clear();
        self.locals.clear();
    }

    pub fn traverse_tree(&mut self, t: &tree::Program) -> ResolveResult<()> {
        t.traverse_rs(self)
    }

    pub fn get_steps(&self, t: &tree::Expr) -> Option<usize> {
        self.steps.get(&(t as *const _)).copied()
    }
}

impl Default for ResolveState {
    fn default() -> Self {
        Self::new()
    }
}

pub trait TraverseResolve {
    fn traverse_rs(&self, map: &mut ResolveState) -> ResolveResult<()>;
}
trait TRsDependent {
    fn traverse_rs(&self, map: &mut ResolveState, e: &tree::Expr) -> ResolveResult<()>;
}

impl<T: TraverseResolve> TraverseResolve for [T] {
    fn traverse_rs(&self, map: &mut ResolveState) -> ResolveResult<()> {
        for t in self {
            t.traverse_rs(map)?
        }

        Ok(())
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
    fn traverse_rs(&self, map: &mut ResolveState, e: &tree::Expr) -> ResolveResult<()> {
        for t in self {
            t.traverse_rs(map, e)?;
        }

        Ok(())
    }
}

// As a program, we do not want to create another scope.
impl TraverseResolve for tree::Program {
    fn traverse_rs(&self, map: &mut ResolveState) -> ResolveResult<()> {
        self.0.0.traverse_rs(map)
    }
}

// The Block node's default traversal is to create a new scope without modifying the type.
// This may differ from an intended goal, and in that case, 
// the implementation for [T] should be used instead.
impl TraverseResolve for tree::Block {
    fn traverse_rs(&self, map: &mut ResolveState) -> ResolveResult<()> {
        map.scope(|map| {
            self.0.traverse_rs(map)
        })
    }
}

impl TraverseResolve for tree::Stmt {
    fn traverse_rs(&self, map: &mut ResolveState) -> ResolveResult<()> {
        match self {
            tree::Stmt::Decl(d)   => d.traverse_rs(map),
            tree::Stmt::Return(e) => match map.block_type() {
                BlockType::Function => e.traverse_rs(map),
                _ => Err(ResolveErr::CannotReturn)
            },
            tree::Stmt::Break => match map.block_type() {
                BlockType::Loop => Ok(()),
                _ => Err(ResolveErr::CannotBreak)
            },
            tree::Stmt::Continue => match map.block_type() {
                BlockType::Loop => Ok(()),
                _ => Err(ResolveErr::CannotContinue)
            },
            tree::Stmt::FunDecl(f) => f.traverse_rs(map),
            tree::Stmt::Expr(e)   => e.traverse_rs(map),
        }
    }
}

impl TraverseResolve for tree::Expr {
    fn traverse_rs(&self, map: &mut ResolveState) -> ResolveResult<()> {
        match self {
            e @ tree::Expr::Ident(s) => {
                map.resolve(e, s);
                Ok(())
            },
            tree::Expr::Block(p) => p.traverse_rs(map),
            tree::Expr::Literal(_) => Ok(()),
            tree::Expr::ListLiteral(l) => {
                map.with_sub(SubType::List, |map| l.traverse_rs(map))
            },
            tree::Expr::SetLiteral(s) => s.traverse_rs(map),
            tree::Expr::DictLiteral(d) => {
                for (k, v) in d {
                    k.traverse_rs(map)?;
                    v.traverse_rs(map)?;
                }

                Ok(())
            },
            tree::Expr::Assign(lhs, rhs) => {
                rhs.traverse_rs(map)?;
                map.with_sub(SubType::Pattern, |map| lhs.traverse_rs(map, self))
            },
            tree::Expr::Path(p) => p.obj.traverse_rs(map),
            tree::Expr::UnaryOps { ops: _, expr } => expr.traverse_rs(map),
            tree::Expr::BinaryOp { op: _, left, right } => {
                left.traverse_rs(map)?;
                right.traverse_rs(map)
            },
            tree::Expr::Comparison { left, rights } => {
                left.traverse_rs(map)?;
                for (_, e) in rights {
                    e.traverse_rs(map)?;
                }
                Ok(())
            },
            tree::Expr::Range { left, right, step } => {
                left.traverse_rs(map)?;
                right.traverse_rs(map)?;
                if let Some(t) = step {
                    t.traverse_rs(map)?;
                }
                Ok(())
            },
            tree::Expr::If { conditionals, last } => {
                for (e, p) in conditionals {
                    e.traverse_rs(map)?;
                    p.traverse_rs(map)?;
                }
        
                last.traverse_rs(map)
            },
            tree::Expr::While { condition, block } => {
                condition.traverse_rs(map)?;
                map.typed_scope(BlockType::Loop, |map| {
                    block.0.traverse_rs(map)
                })
            },
            tree::Expr::For { ident, iterator, block } => {
                iterator.traverse_rs(map)?;

                map.typed_scope(BlockType::Loop, |map| {
                    map.declare(ident);
                    block.0.traverse_rs(map)
                })
            },
            tree::Expr::Call { funct, params } => {
                funct.traverse_rs(map)?;
                params.traverse_rs(map)
            },
            tree::Expr::Index(idx) => idx.traverse_rs(map),
            tree::Expr::Spread(e) => match map.sub_type() {
                SubType::None => Err(ResolveErr::CannotSpread),
                SubType::List => match e {
                    Some(inner) => inner.traverse_rs(map),
                    None => Err(ResolveErr::CannotSpreadNone),
                },
                SubType::Pattern => match e {
                    Some(inner) => inner.traverse_rs(map),
                    None => Ok(()),
                },
            },
        }
    }
}

impl TraverseResolve for tree::Decl {
    fn traverse_rs(&self, map: &mut ResolveState) -> ResolveResult<()> {
        let tree::Decl { pat, val, .. } = self;

        pat.traverse_rs(map)?;
        val.traverse_rs(map)
    }
}

impl TraverseResolve for tree::FunDecl {
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

impl TraverseResolve for tree::Index {
    fn traverse_rs(&self, map: &mut ResolveState) -> ResolveResult<()> {
        self.expr.traverse_rs(map)?;
        self.index.traverse_rs(map)
    }
}

impl<T: TRsDependent> TRsDependent for tree::Pat<T> {
    fn traverse_rs(&self, map: &mut ResolveState, e: &tree::Expr) -> ResolveResult<()> {
        match self {
            tree::Pat::Unit(u) => u.traverse_rs(map, e),
            tree::Pat::List(lst) => lst.traverse_rs(map, e),
            tree::Pat::Spread(mp) => match mp {
                Some(p) => p.traverse_rs(map, e),
                None => Ok(()),
            },
        }
    }
}
impl<T: TraverseResolve> TraverseResolve for tree::Pat<T> {
    fn traverse_rs(&self, map: &mut ResolveState) -> ResolveResult<()> {
        match self {
            tree::Pat::Unit(u) => u.traverse_rs(map),
            tree::Pat::List(lst) => lst.traverse_rs(map),
            tree::Pat::Spread(mp) => match mp {
                Some(p) => p.traverse_rs(map),
                None => Ok(()),
            },
        }
    }
}

impl TRsDependent for tree::AsgUnit {
    fn traverse_rs(&self, map: &mut ResolveState, e: &tree::Expr) -> ResolveResult<()> {
        match self {
            tree::AsgUnit::Ident(ident) => {
                map.resolve(e, ident);
                Ok(())
            },
            tree::AsgUnit::Path(p) => p.obj.traverse_rs(map),
            tree::AsgUnit::Index(idx) => idx.traverse_rs(map),
        }
    }
}
impl TraverseResolve for tree::DeclUnit {
    fn traverse_rs(&self, map: &mut ResolveState) -> ResolveResult<()> {
        map.declare(&self.0);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::interpreter::semantic::ResolveState;
    use crate::{tree::*, Interpreter};

    use super::ResolveResult;


    macro_rules! program {
        ($($e:expr),*) => {
            Program(Block(vec![$($e),*]))
        }
    }

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

    fn ident(s: &str) -> Expr {
        Expr::Ident(String::from(s))
    }

    #[test]
    fn nonexistent_var() -> ResolveResult<()> {
        let program = program![
            Stmt::Expr(ident("a"))
        ];

        let mut state = ResolveState::new();
        state.traverse_tree(&program)?;

        assert_eq!(&state.steps, &map!{});
        Ok(())
    }

    #[test]
    fn declare_in_scope() -> ResolveResult<()> {
        let program = program![
            Stmt::Decl(Decl {
                rt: ReasgType::Const, 
                pat: DeclPat::Unit(DeclUnit(String::from("a"), MutType::Immut)), 
                ty: None, 
                val: Expr::Literal(Literal::Int(0)),
            }),
            Stmt::Expr(ident("a"))
        ];

        let mut state = ResolveState::new();
        state.traverse_tree(&program)?;

        assert_eq!(&state.steps, &map!{});

        let program = program![
            Stmt::Decl(Decl {
                rt: ReasgType::Const, 
                pat: DeclPat::Unit(DeclUnit(String::from("a"), MutType::Immut)), 
                ty: None, 
                val: Expr::Literal(Literal::Int(0)),
            }),
            Stmt::Expr(Expr::Block(Block(vec![Stmt::Expr(ident("a"))])))
        ];

        let mut state = ResolveState::new();
        state.traverse_tree(&program)?;

        assert_eq!(&state.steps, &map!{});

        let program = program![
            Stmt::Expr(Expr::Block(Block(vec![
                Stmt::Decl(Decl {
                    rt: ReasgType::Const, 
                    pat: DeclPat::Unit(DeclUnit(String::from("a"), MutType::Immut)), 
                    ty: None, 
                    val: Expr::Literal(Literal::Int(0)),
                }),
                Stmt::Expr(Expr::Block(Block(vec![Stmt::Expr(ident("a"))])))
            ])))
        ];

        let a = if let Stmt::Expr(Expr::Block(prog)) = &program.0.0[0] {
            if let Stmt::Expr(Expr::Block(prog)) = &prog.0[1] { 
                if let Stmt::Expr(e) = &prog.0[0] { e } else { unreachable!() }
            } else { unreachable!() }
        } else { unreachable!() };

        let mut state = ResolveState::new();
        state.traverse_tree(&program)?;

        let steps = map! {
            a as _ => 1
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
        state.traverse_tree(&program)?;

        println!("{:?}", state);
        Ok(())
    }
}