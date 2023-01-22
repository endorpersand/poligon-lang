//! Values in the runtime.
//!
//! This module provides:
//! - [`Value`]: A value in runtime
//! - [`VArbType`], [`ValueType`]: Value types
//! - [`GonFun`]: A function in runtime

use std::cell::Ref;
use std::collections::HashSet;
use std::rc::Rc;
use std::ops::Deref;

use super::{RtResult, TypeErr, ValueErr};
use crate::ast::{self, op};

mod fun;
mod ty;
mod op_impl;
pub(super) mod refval;

pub use fun::*;
pub use refval::*;
pub use ty::*;

/// A list of [`Value`]s
pub type GonList = RefValue<Vec<Value>>;
type GonListPtr = *const std::cell::RefCell<Vec<Value>>;

/// A value in Poligon's runtime
#[derive(PartialEq, Clone, Debug)]
pub enum Value {
    /// An int value (`5`)
    /// 
    /// In this implementation, it is limited by Rust's `isize`
    Int(isize),

    /// An IEEE-754 double value (`0.3`)
    Float(f64),

    /// A character (`'a'`)
    Char(char),

    /// A string (`"abc"`)
    Str(String),

    /// A boolean (`true`, `false`)
    Bool(bool),

    /// A list of values
    List(GonList),

    /// `void`
    Unit,

    /// A function
    Fun(GonFun)
}

struct ListRepr {
    refs: Vec<HashSet<GonListPtr>>
}
impl ListRepr {
    fn new() -> Self {
        ListRepr { refs: vec![] }
    }
    
    fn contains(&self, t: &GonList) -> bool {
        self.refs.iter()
            .any(|s| s.contains(&Rc::as_ptr(&t.rc)))
    }

    fn repr(&mut self, t: &Value) -> String {
        // push scope:
        self.refs.push(HashSet::new());

        let result = match t {
            Value::List(l) => if self.contains(l) {
                String::from("[...]")
            } else {
                format!("[{}]", {
                    self.refs.last_mut().unwrap().insert(Rc::as_ptr(&l.rc));
        
                    let strs = l.borrow().iter()
                        .map(|t| self.repr(t))
                        .collect::<Vec<_>>();
                    
                    strs.join(", ")
                })
            },
            
            t => t.repr(),
        };

        // pop scope:
        self.refs.pop();
        
        result
    }
}

/// Utility to cast values onto float and compare them
fn float_cmp(a: impl TryInto<f64>, b: impl TryInto<f64>, o: op::Cmp) -> Option<bool> {
    if let (Ok(af), Ok(bf)) = (a.try_into(), b.try_into()) {
        Some(o.cmp(af, bf))
    } else {
        None
    }
}

struct ListValueIter<'a> {
    r: Option<Ref<'a, [Value]>>,
}

impl ListValueIter<'_> {
    fn new(r: Ref<impl Deref<Target=[Value]>>) -> ListValueIter<'_> {
        ListValueIter { r: Some(Ref::map(r, Deref::deref)) }
    }
}

impl<'a> Iterator for ListValueIter<'a> {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        let borrow = self.r.take()?;
        
        match *borrow {
            [] => None,
            [_, ..] => {
                let (head, tail) = Ref::map_split(borrow, |slice| {
                    (&slice[0], &slice[1..])
                });
                self.r.replace(tail);
                Some(head.clone())
            }
        }
    }
}
impl<'a> DoubleEndedIterator for ListValueIter<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        let borrow = self.r.take()?;
        
        match *borrow {
            [] => None,
            [.., _] => {
                let (tail, head) = Ref::map_split(
                    borrow, 
                    |slice| slice.split_last().unwrap()
                );
                self.r.replace(head);
                Some(tail.clone())
            }
        }
    }
}

impl Value {
    /// Truthiness of a value: when it is cast to bool, what truth value should it have?
    /// 
    /// Numerics: Non-zero => true
    /// Collections: Non-empty => true
    pub(super) fn truth(&self) -> bool {
        match self {
            Value::Int(v)   => v != &0,
            Value::Float(v) => v != &0.0,
            Value::Char(_)  => true,
            Value::Str(v)   => !v.is_empty(),
            Value::Bool(v)  => *v,
            Value::List(v)  => !v.borrow().is_empty(),
            Value::Unit     => false,
            Value::Fun(_)   => true,
        }
    }

    /// Test if the current value is an int/float (numeric) or not.
    pub fn is_numeric(&self) -> bool {
        matches!(self, Value::Int(_) | Value::Float(_))
    }

    /// Get the concrete type of the current value.
    pub fn ty(&self) -> ValueType {
        match self {
            Value::Int(_)   => ValueType::Int,
            Value::Float(_) => ValueType::Float,
            Value::Char(_)  => ValueType::Char,
            Value::Str(_)   => ValueType::Str,
            Value::Bool(_)  => ValueType::Bool,
            Value::List(_)  => ValueType::List(Box::new(VArbType::Unk)),
            Value::Unit     => ValueType::Unit,
            Value::Fun(f)   => ValueType::Fun(f.ty.clone()),
        }
    }

    /// If the current value can be interpreted as an iterator of values, convert it into an iterator.
    /// Otherwise, return `None`.
    pub(super) fn as_iterator<'a>(&'a self) -> Option<Box<dyn DoubleEndedIterator<Item=Value> + 'a>> {
        match self {
            Value::Str(s)   => Some(Box::new(s.chars().map(Value::Char))),
            Value::List(l)  => {
                let iter = ListValueIter::new(l.borrow());
                Some(Box::new(iter))
            },
            Value::Int(_)   => None,
            Value::Float(_) => None,
            Value::Char(_)  => None,
            Value::Bool(_)  => None,
            Value::Unit     => None,
            Value::Fun(_)   => None,
        }
    }

    /// If the current value can be interpreted as a float, convert it into a float.
    /// Otherwise, return `None`.
    pub fn as_float(&self) -> Option<f64> {
        match self {
            Value::Int(i) => Some(*i as _),
            Value::Float(f) => Some(*f),
            _ => None,
        }
    }

    /// Produce the Poligon code representation of this value.
    pub fn repr(&self) -> String {
        match self {
            Value::Int(i)   => i.to_string(),
            Value::Float(f) => f.to_string(),
            Value::Char(c)  => format!("{:?}", c), // TODO: link these to language representations
            Value::Str(s)   => format!("{:?}", s), // TODO: link these to language representations
            Value::Bool(b)  => b.to_string(),
            Value::List(_)  => ListRepr::new().repr(self),
            Value::Unit     => ValueType::Unit.to_string(),
            Value::Fun(f)   => match &f.ident {
                Some(n) => format!("<function {}>", n),
                None => String::from("<anonymous function>"),
            },
        }
    }

    /// Produce the string representation of this value.
    pub fn str(&self) -> String {
        match self {
            Value::Char(c) => c.to_string(),
            Value::Str(s)  => s.to_string(),
            v @ (
                | Value::Int(_) 
                | Value::Float(_) 
                | Value::Bool(_) 
                | Value::List(_) 
                | Value::Unit
                | Value::Fun(_)
            ) => v.repr(),
        }
    }

    /// Try to index this value.
    pub fn get_index(&self, idx: Value) -> RtResult<Value> {
        match self {
            // There is a more efficient method of indexing lists 
            // than just "conv to iter => get nth item":
            e @ Value::List(_) => if let Value::Int(signed_idx) = idx {
                let mi = usize::try_from(signed_idx).ok();
                let Value::List(lst) = e else { unreachable!( ) };

                match mi {
                    Some(i) => lst.borrow().get(i).map(Value::clone),
                    None => None,
                }.ok_or_else(|| ValueErr::IndexOutOfBounds.into())
            } else {
                Err(TypeErr::CannotIndexWith(e.ty(), idx.ty()))?
            },

            // Convert to iter => get nth item
            e => {
                let mut it = e.as_iterator()
                    .ok_or_else(|| TypeErr::CannotIndex(e.ty()))?;

                if let Value::Int(signed_idx) = idx {
                    let i = usize::try_from(signed_idx)
                        .map_err(|_| ValueErr::IndexOutOfBounds)?;
                    
                    it.nth(i)
                        .ok_or_else(|| ValueErr::IndexOutOfBounds.into())
                } else {
                    Err(TypeErr::CannotIndexWith(e.ty(), idx.ty()))?
                }
            }
        }
    }

    /// Try to set an index of this value.
    pub fn set_index(&mut self, idx: Value, nv: Value) -> RtResult<Value> {
        match self {
            e @ Value::List(_) => if let Value::Int(signed_idx) = idx {
                let mi = usize::try_from(signed_idx).ok();
                let Value::List(lst) = e else { unreachable!( ) };

                match mi {
                    Some(i) => {
                        let mut lst_ref = lst.try_borrow_mut()?;
                        
                        lst_ref[i] = nv;
                        lst_ref.get(i).map(Value::clone)
                    },
                    None => None,
                }.ok_or_else(|| ValueErr::IndexOutOfBounds.into())
            } else {
                Err(TypeErr::CannotIndexWith(e.ty(), idx.ty()))?
            },

            e => Err(TypeErr::CannotSetIndex(e.ty()))?
        }
    }

    /// Apply a unary operator to a computed value.
    pub fn apply_unary(self, o: op::Unary) -> super::RtResult<Value> {
        let ty = self.ty();
        match o {
            op::Unary::Plus   => if self.is_numeric() { Some(self) } else { None },
            op::Unary::Minus  => match self {
                Value::Int(e)   => Some(Value::Int(-e)),
                Value::Float(e) => Some(Value::Float(-e)),
                _ => None,
            },
            op::Unary::LogNot => Some(Value::Bool(!self.truth())),
            op::Unary::BitNot => if let Value::Int(e) = self { Some(Value::Int(!e)) } else { None },
        }.ok_or(TypeErr::CannotUnary(o, ty).into())
    }
    
    /// Apply a comparison operator between two computed values.
    pub fn apply_cmp(&self, o: op::Cmp, right: &Self) -> super::RtResult<bool> {
        match o {
            op::Cmp::Lt | op::Cmp::Gt | op::Cmp::Le | op::Cmp::Ge => {
                match (self, right) {
                    // integer/float cmp
                    (Value::Float(af), b) => float_cmp(*af, b, o),
                    (a, Value::Float(bf)) => float_cmp(a, *bf, o),
                    (Value::Int(a), Value::Int(b)) => Some(o.cmp(a, b)),
    
                    // str, char can be compared against each other but not anything else
                    (Value::Str(a), Value::Str(b)) => Some(o.cmp(a, b)),
                    (Value::Char(a), Value::Char(b)) => Some(o.cmp(a, b)),
                    
                    // booleans cannot be compared,
                    // and cross-type comparisons cannot occur
                    _ => None
                }
            },
            op::Cmp::Eq | op::Cmp::Ne => {
                match (self, right) {
                    // integer/float cmp
                    (Value::Float(af), b) => float_cmp(*af, b, o),
                    (a, Value::Float(bf)) => float_cmp(a, *bf, o),
                    (Value::Int(a), Value::Int(b)) => Some(o.cmp(a, b)),
    
                    // str, char can be compared against each other but not anything else
                    (Value::Str(a), Value::Str(b)) => Some(o.cmp(a, b)),
                    (Value::Char(a), Value::Char(b)) => Some(o.cmp(a, b)),
                    
                    // booleans
                    (Value::Bool(a), Value::Bool(b)) => Some(o.cmp(a, b)),
    
                    // and cross-type comparisons cannot occur
                    _ => None
                }
            },
        }.ok_or_else(|| TypeErr::CannotCmp(o, self.ty(), right.ty()).into())
    }

    /// Deep copies the value. For lists, this means that this value 
    /// is equal but does not have the same identity as the original.
    /// 
    /// This is different from `.clone`, which copies references
    /// and preserves identity.
    pub fn deep_clone(&self) -> Value {
        match self {
            Value::List(l) => Value::new_list(l.borrow().clone()),
            e @ (
                | Value::Int(_) 
                | Value::Float(_) 
                | Value::Char(_) 
                | Value::Str(_) 
                | Value::Bool(_) 
                | Value::Unit
                | Value::Fun(_)
            ) => e.clone(),
        }
    }

    /// Create a list value.
    pub fn new_list(l: Vec<Value>) -> Self {
        Value::List(RefValue::new(l, true))
    }

    /// Create a function value, using a function defined in Rust.
    pub fn new_rust_fn(name: Option<&str>, ty: FunType, fun: fn(Vec<Value>) -> RtResult<Value>) -> Self {
        let gf = GonFun {
            ident: name.map(ToString::to_string),
            ty,
            fun: GInternalFun::Rust(fun)
        };

        Value::Fun(gf)
    }

    /// Create a function value, using a functon defined in Poligon.
    pub fn new_gon_fn(name: Option<&str>, ty: FunType, params: Vec<String>, fun: Rc<ast::Block>, idx: usize) -> Self {
        let gf = GonFun {
            ident: name.map(ToString::to_string),
            ty,
            fun: GInternalFun::Poligon(params, fun, idx)
        };

        Value::Fun(gf)
    }
}

impl TryFrom<&Value> for f64 {
    type Error = ();

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        value.as_float().ok_or(())
    }
}

impl From<ast::Literal> for Value {
    fn from(literal: ast::Literal) -> Self {
        match literal {
            ast::Literal::Int(v)   => Value::Int(v),
            ast::Literal::Float(v) => Value::Float(v),
            ast::Literal::Char(v)  => Value::Char(v),
            ast::Literal::Str(v)   => Value::Str(v),
            ast::Literal::Bool(v)  => Value::Bool(v),
        }
    }
}