use std::collections::{HashMap, HashSet};

use crate::tree;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum BlockType {
    Function,
    Loop,
    Program
}

#[derive(Debug, PartialEq, Eq)]
pub struct ResolveState {
    steps: HashMap<*const tree::Expr, usize>,
    locals: Vec<HashSet<String>>,
    block_type: BlockType
}

#[derive(Debug, PartialEq, Eq)]
pub enum ResolveErr {
    CannotReturn,
    CannotBreak,
    CannotContinue
}
pub type ResolveResult<T> = Result<T, ResolveErr>;

impl From<ResolveErr> for crate::runtime::RuntimeErr {
    fn from(re: ResolveErr) -> Self {
        match re {
            ResolveErr::CannotReturn   => Self::CannotReturn,
            ResolveErr::CannotBreak    => Self::CannotBreak,
            ResolveErr::CannotContinue => Self::CannotContinue,
        }
    }
}

impl ResolveState {
    pub fn new() -> Self {
        Self { steps: HashMap::new(), locals: vec![], block_type: BlockType::Program }
    }

    fn _open_scope(&mut self) {
        self.locals.push(HashSet::new());
    }

    fn _close_scope(&mut self) {
        self.locals.pop();
    }

    fn scope<F, T>(&mut self, f: F) -> ResolveResult<T>
        where F: FnOnce(&mut Self) -> ResolveResult<T>
    {
        self._open_scope();
        let t = f(self);
        self._close_scope();
        
        t
    }

    fn typed_scope<F, T>(&mut self, ty: BlockType, f: F) -> ResolveResult<T>
        where F: FnOnce(&mut Self) -> ResolveResult<T>
    {
        let orig;
        (orig, self.block_type) = (self.block_type, ty);

        let t = self.scope(f);

        self.block_type = orig;
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
        t.0.traverse_rs(self)
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

// The Program node's default traversal is to create a new scope without modifying the type.
// This may differ from an intended goal, and in that case, 
// the implementation for [T] should be used instead.
impl TraverseResolve for tree::Program {
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
            tree::Stmt::Return(e) => match map.block_type {
                BlockType::Function => e.traverse_rs(map),
                _ => Err(ResolveErr::CannotReturn)
            },
            tree::Stmt::Break => match map.block_type {
                BlockType::Loop => Ok(()),
                _ => Err(ResolveErr::CannotBreak)
            },
            tree::Stmt::Continue => match map.block_type {
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
            tree::Expr::ListLiteral(l) => l.traverse_rs(map),
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
                lhs.traverse_rs(map, self)
            },
            tree::Expr::Attr(_) => todo!(),
            tree::Expr::StaticAttr(_) => todo!(),
            tree::Expr::UnaryOps(op) => op.traverse_rs(map),
            tree::Expr::BinaryOp(op) => op.traverse_rs(map),
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
            tree::Expr::If(e) => e.traverse_rs(map),
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
        }
    }
}

impl TraverseResolve for tree::Decl {
    fn traverse_rs(&self, map: &mut ResolveState) -> ResolveResult<()> {
        let tree::Decl { ident, val, .. } = self;

        // map.partial_declare(ident);
        map.declare(ident);
        val.traverse_rs(map)
    }
}

impl TraverseResolve for tree::FunDecl {
    fn traverse_rs(&self, map: &mut ResolveState) -> ResolveResult<()> {
        map.declare(&self.ident);

        map.typed_scope(BlockType::Function, |map| {
            for p in &self.params {
                map.declare(&p.ident);
            }
    
            self.block.0.traverse_rs(map)
        })
    }
}

impl TraverseResolve for tree::UnaryOps {
    fn traverse_rs(&self, map: &mut ResolveState) -> ResolveResult<()> {
        self.expr.traverse_rs(map)
    }
}
impl TraverseResolve for tree::BinaryOp {
    fn traverse_rs(&self, map: &mut ResolveState) -> ResolveResult<()> {
        self.left.traverse_rs(map)?;
        self.right.traverse_rs(map)
    }
}

impl TraverseResolve for tree::If {
    fn traverse_rs(&self, map: &mut ResolveState) -> ResolveResult<()> {
        for (e, p) in &self.conditionals {
            e.traverse_rs(map)?;
            p.traverse_rs(map)?;
        }

        self.last.traverse_rs(map)
    }
}

impl TraverseResolve for tree::Index {
    fn traverse_rs(&self, map: &mut ResolveState) -> ResolveResult<()> {
        self.expr.traverse_rs(map)?;
        self.index.traverse_rs(map)
    }
}

impl TRsDependent for tree::AsgPat {
    fn traverse_rs(&self, map: &mut ResolveState, e: &tree::Expr) -> ResolveResult<()> {
        match self {
            tree::AsgPat::Unit(u) => u.traverse_rs(map, e),
            tree::AsgPat::List(lst) => lst.traverse_rs(map, e),
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
            tree::AsgUnit::Path(_, _) => todo!(),
            tree::AsgUnit::Index(idx) => idx.traverse_rs(map),
        }
    }
}
#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use crate::semantic::ResolveState;
    use crate::{tree::*, Interpreter};

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

    fn ident(s: &str) -> Expr {
        Expr::Ident(String::from(s))
    }

    #[test]
    fn nonexistent_var() -> ResolveResult<()> {
        let program = Program(vec![
            Stmt::Expr(ident("a"))
        ]);

        let mut state = ResolveState::new();
        state.traverse_tree(&program)?;

        assert_eq!(&state.steps, &map!{});
        Ok(())
    }

    #[test]
    fn declare_in_scope() -> ResolveResult<()> {
        let program = Program(vec![
            Stmt::Decl(Decl {
                rt: ReasgType::Const, 
                mt: MutType::Immut, 
                ident: String::from("a"), 
                ty: None, 
                val: Expr::Literal(Literal::Int(0)),
            }),
            Stmt::Expr(ident("a"))
        ]);

        let mut state = ResolveState::new();
        state.traverse_tree(&program)?;

        assert_eq!(&state.steps, &map!{});

        let program = Program(vec![
            Stmt::Decl(Decl {
                rt: ReasgType::Const, 
                mt: MutType::Immut, 
                ident: String::from("a"), 
                ty: None, 
                val: Expr::Literal(Literal::Int(0)),
            }),
            Stmt::Expr(Expr::Block(Program(vec![Stmt::Expr(ident("a"))])))
        ]);

        let mut state = ResolveState::new();
        state.traverse_tree(&program)?;

        assert_eq!(&state.steps, &map!{});

        let program = Program(vec![
            Stmt::Expr(Expr::Block(Program(vec![
                Stmt::Decl(Decl {
                    rt: ReasgType::Const, 
                    mt: MutType::Immut, 
                    ident: String::from("a"), 
                    ty: None, 
                    val: Expr::Literal(Literal::Int(0)),
                }),
                Stmt::Expr(Expr::Block(Program(vec![Stmt::Expr(ident("a"))])))
            ])))
        ]);

        let a = if let Stmt::Expr(Expr::Block(prog)) = &program.0[0] {
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