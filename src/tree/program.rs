use super::*;
use id_arena::{Arena, Id, DefaultArenaBehavior, ArenaBehavior};
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub struct Program {
    pub program: Block,
    arena: NodeArena
}
impl Program {
    pub fn new(program: Block, arena: NodeArena) -> Self {
        Self { program, arena }
    }

}

fn cast<T, U>(t: Id<T>) -> Id<U> {
    let idx = t.index();
    let arx = DefaultArenaBehavior::arena_id(t);

    DefaultArenaBehavior::new_id(arx, idx)
}

#[derive(Debug, PartialEq)]
pub struct NodeArena {
    arena: Arena<Node>,
    node_locs: HashMap<Id<Node>, std::ops::RangeInclusive<(usize, usize)>>
}
impl NodeArena {
    pub fn new() -> Self {
        Self {
            arena: Arena::new(),
            node_locs: HashMap::new()
        }
    }

    fn alloc<T>(&mut self, t: T) -> Id<Node> 
        where T: Into<Node>
    {
        self.arena.alloc(t.into())
    }

    pub fn insert<T>(&mut self, t: T, loc: std::ops::RangeInclusive<(usize, usize)>) -> Id<T>
        where T: Into<Node>
    {
        let id = self.alloc(t);
        self.node_locs.insert(id, loc);

        cast(id)
    }

    pub fn get<T>(&self, id: Id<T>) -> Option<&T>
        where T: Nodeable
    {
        self.arena.get(cast(id))
            .map(Nodeable::from_node)
    }
}

pub trait Nodeable: Into<Node> {
    fn from_node(node: &Node) -> &Self;
}

macro_rules! make_node {
    ($($e:ident),*) => {
        #[derive(Debug, PartialEq)]
        pub enum Node {
            $($e($e)),*
        }

        $(
            impl From<$e> for Node {
                fn from(n: $e) -> Self {
                    Self::$e(n)
                }
            }

            impl Nodeable for $e {
                fn from_node(node: &Node) -> &Self {
                    if let Node::$e(n) = node {
                        n
                    } else {
                        unreachable!()
                    }
                }
            }
        )*
    }
}

make_node! { Expr, Stmt }