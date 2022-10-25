use std::collections::{HashMap, HashSet};

use by_address::ByAddress;

use crate::tree;

#[derive(Debug, PartialEq)]
pub struct ResolveState<'a> {
    steps: HashMap<ByAddress<&'a tree::Expr>, Option<usize>>,
    vars: Vec<HashSet<String>>
}

impl<'map> ResolveState<'map> {
    fn new() -> Self {
        Self { steps: HashMap::new(), vars: vec![HashSet::new()] }
    }

    fn open_scope(&mut self) {
        self.vars.push(HashSet::new());
    }

    fn close_scope(&mut self) {
        self.vars.pop();
    }

    // fn partial_declare(&mut self, ident: &str) -> () {
    //     self.vars.last_mut().unwrap()
    //         .insert(String::from(ident), false);
    // }

    fn declare(&mut self, ident: &str) -> () {
        self.vars.last_mut().unwrap()
            .insert(String::from(ident));
    }

    fn resolve<'a>(&mut self, e: &'a tree::Expr, ident: &str) -> ()
        where 'a: 'map
    {
        // at depth 0, this means it is in our scope and we have to traverse 0 scopes
        // at depth 1, it is one scope ahead
        // etc.
        let scope_count = self.vars.iter().rev()
            .enumerate()
            .find(|(_, scope)| scope.contains(ident))
            .map(|(idx, _)| idx);
        
        self.steps.insert(ByAddress(e), scope_count);
    }
}

impl Default for ResolveState<'_> {
    fn default() -> Self {
        Self::new()
    }
}

pub trait TraverseResolve {
    fn traverse_rs<'map>(&'map self, map: &mut ResolveState<'map>) -> ();
}
trait TRsDependent {
    fn traverse_rs<'map>(&'map self, map: &mut ResolveState<'map>, e: &'map tree::Expr) -> ();
}

impl<T: TraverseResolve> TraverseResolve for [T] {
    fn traverse_rs<'map>(&'map self, map: &mut ResolveState<'map>) -> () {
        for t in self {
            t.traverse_rs(map)
        }
    }
}
impl<T: TraverseResolve> TraverseResolve for Option<T> {
    fn traverse_rs<'map>(&'map self, map: &mut ResolveState<'map>) -> () {
        map.open_scope();
        if let Some(t) = self {
            t.traverse_rs(map)
        }
        map.close_scope();
    }
}
impl<T: TRsDependent> TRsDependent for [T] {
    fn traverse_rs<'map>(&'map self, map: &mut ResolveState<'map>, e: &'map tree::Expr) -> () {
        for t in self {
            t.traverse_rs(map, e)
        }
    }
}

impl TraverseResolve for tree::Program {
    fn traverse_rs<'map>(&'map self, map: &mut ResolveState<'map>) -> () {
        map.open_scope();
        self.0.traverse_rs(map);
        map.close_scope();
    }
}

impl TraverseResolve for tree::Stmt {
    fn traverse_rs<'map>(&'map self, map: &mut ResolveState<'map>) -> () {
        match self {
            tree::Stmt::Decl(d) => d.traverse_rs(map),
            tree::Stmt::Return(e) => e.traverse_rs(map),
            tree::Stmt::Break => (),
            tree::Stmt::Continue => (),
            tree::Stmt::FunDecl(f) => f.traverse_rs(map),
            tree::Stmt::Expr(e) => e.traverse_rs(map),
        }
    }
}

impl TraverseResolve for tree::Expr {
    fn traverse_rs<'map>(&'map self, map: &mut ResolveState<'map>) -> () {
        match self {
            e @ tree::Expr::Ident(s) => map.resolve(e, s),
            tree::Expr::Block(p) => p.traverse_rs(map),
            tree::Expr::Literal(_) => (),
            tree::Expr::ListLiteral(l) => l.traverse_rs(map),
            tree::Expr::SetLiteral(s) => s.traverse_rs(map),
            tree::Expr::DictLiteral(d) => {
                for (k, v) in d {
                    k.traverse_rs(map);
                    v.traverse_rs(map);
                }
            },
            tree::Expr::Assign(lhs, rhs) => {
                rhs.traverse_rs(map);
                lhs.traverse_rs(map, self);
            },
            tree::Expr::Attr(_) => todo!(),
            tree::Expr::StaticAttr(_) => todo!(),
            tree::Expr::UnaryOps(op) => op.traverse_rs(map),
            tree::Expr::BinaryOp(op) => op.traverse_rs(map),
            tree::Expr::Comparison { left, rights } => {
                left.traverse_rs(map);
                for (_, e) in rights {
                    e.traverse_rs(map);
                }
            },
            tree::Expr::Range { left, right, step } => {
                left.traverse_rs(map);
                right.traverse_rs(map);
                if let Some(t) = step {
                    t.traverse_rs(map);
                }
            },
            tree::Expr::If(e) => e.traverse_rs(map),
            tree::Expr::While { condition, block } => {
                condition.traverse_rs(map);
                block.traverse_rs(map);
            },
            tree::Expr::For { ident, iterator, block } => {
                iterator.traverse_rs(map);

                map.open_scope();
                map.declare(ident);
                block.traverse_rs(map);
                map.close_scope();
            },
            tree::Expr::Call { funct, params } => {
                funct.traverse_rs(map);
                params.traverse_rs(map);
            },
            tree::Expr::Index(idx) => idx.traverse_rs(map),
        }
    }
}

impl TraverseResolve for tree::Decl {
    fn traverse_rs<'map>(&'map self, map: &mut ResolveState<'map>) -> () {
        let tree::Decl { ident, val, .. } = self;

        // map.partial_declare(ident);
        map.declare(ident);
        val.traverse_rs(map);
    }
}

impl TraverseResolve for tree::FunDecl {
    fn traverse_rs<'map>(&'map self, map: &mut ResolveState<'map>) -> () {
        map.declare(&self.ident);

        map.open_scope();
        for p in &self.params {
            map.declare(&p.ident);
        }

        self.block.0.traverse_rs(map);

        map.close_scope();
    }
}

impl TraverseResolve for tree::UnaryOps {
    fn traverse_rs<'map>(&'map self, map: &mut ResolveState<'map>) -> () {
        self.expr.traverse_rs(map)
    }
}
impl TraverseResolve for tree::BinaryOp {
    fn traverse_rs<'map>(&'map self, map: &mut ResolveState<'map>) -> () {
        self.left.traverse_rs(map);
        self.right.traverse_rs(map)
    }
}

impl TraverseResolve for tree::If {
    fn traverse_rs<'map>(&'map self, map: &mut ResolveState<'map>) -> () {
        for (e, p) in &self.conditionals {
            e.traverse_rs(map);
            p.traverse_rs(map);
        }

        self.last.traverse_rs(map)
    }
}

impl TraverseResolve for tree::Index {
    fn traverse_rs<'map>(&'map self, map: &mut ResolveState<'map>) -> () {
        self.expr.traverse_rs(map);
        self.index.traverse_rs(map);
    }
}

impl TRsDependent for tree::AsgPat {
    fn traverse_rs<'map>(&'map self, map: &mut ResolveState<'map>, e: &'map tree::Expr) -> () {
        match self {
            tree::AsgPat::Unit(u) => u.traverse_rs(map, e),
            tree::AsgPat::List(lst) => lst.traverse_rs(map, e),
        }
    }
}

impl TRsDependent for tree::AsgUnit {
    fn traverse_rs<'map>(&'map self, map: &mut ResolveState<'map>, e: &'map tree::Expr) -> () {
        match self {
            tree::AsgUnit::Ident(ident) => map.resolve(e, ident),
            tree::AsgUnit::Path(_, _) => todo!(),
            tree::AsgUnit::Index(idx) => idx.traverse_rs(map),
        }
    }
}
#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use by_address::ByAddress;

    use crate::semantic::ResolveState;
    use crate::tree::*;

    use super::TraverseResolve;

    macro_rules! map {
        ($($k:expr => $v:expr),*) => {
            {
                let mut m = HashMap::new();
                $(m.insert($k, $v);)+
                m
            }
        }
    }

    fn ident(s: &str) -> Expr {
        Expr::Ident(String::from(s))
    }

    #[test]
    fn nonexistent_var() {
        let a = Program(vec![
            Stmt::Expr(ident("a"))
        ]);

        let mut state = ResolveState::new();
        a.traverse_rs(&mut state);

        let steps = if let Stmt::Expr(e) = &a.0[0] {
            map! {
                ByAddress(e) => None
            }
        } else {
            unreachable!();
        };

        assert_eq!(&state.steps, &steps);
    }

    #[test]
    fn declare_in_scope() {
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
        program.traverse_rs(&mut state);

        let a = if let Stmt::Expr(e) = &program.0[1] { e } else { unreachable!() };
        let steps = map! {
            ByAddress(a) => Some(0)
        };


        assert_eq!(&state.steps, &steps);

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

        let a = if let Stmt::Expr(Expr::Block(prog)) = &program.0[1] { 
            if let Stmt::Expr(e) = &prog.0[0] { e } else { unreachable!() }
        } else { unreachable!() };

        let mut state = ResolveState::new();
        program.traverse_rs(&mut state);

        let steps = map! {
            ByAddress(a) => Some(1)
        };

        assert_eq!(&state.steps, &steps);
    }
}