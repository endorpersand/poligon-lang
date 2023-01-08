use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::marker::PhantomData;
use std::ptr::NonNull;

use super::gstd;
use crate::tree::{ReasgType, MutType};
use super::value::Value;

/// Stores the variables in the current scope.
/// 
/// This also provides read/write access to variables in parent scopes.
#[derive(Debug)]
pub struct ScopeContext<'a, T> {
    scope: HashMap<String, T>,
    parent: Option<NonNull<ScopeContext<'a, T>>>,
    _ghost: PhantomData<&'a ()>,
    _idx: usize
}

pub type VarContext<'a> = ScopeContext<'a, Value>;

/// Iterates through a VarContext and its parents to provide all accessible HashMaps
struct Iter<'a, T> {
    next: Option<&'a ScopeContext<'a, T>>
}
impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a ScopeContext<'a, T>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next.map(|current| {
            let m_next_ptr = current.parent;

            // SAFETY: Since we're here, this context must be a child context,
            // so there must be a pointer to a parent context here.
            self.next = m_next_ptr.map(|next_ptr| unsafe { next_ptr.as_ref() });
            current
        })
    }
}

/// Mutable version of `VCtxIter`
struct IterMut<'a, T> {
    next: Option<NonNull<ScopeContext<'a, T>>>
}
impl<'a, T: 'a> Iterator for IterMut<'a, T> {
    type Item = &'a mut ScopeContext<'a, T>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next.take().map(|mut c_ptr| {
            // SAFETY: Pointer can only come from the parent chains,
            // so, there must be a pointer to some context here.
            let current = unsafe { c_ptr.as_mut() };

            self.next = current.parent;
            current
        })
    }
}

impl<T> ScopeContext<'_, T> {
    /// Create a new ScopeContext.
    #[allow(unused)]
    pub fn empty() -> Self {
        Self { 
            scope: HashMap::new(), 
            parent: None, 
            _ghost: PhantomData,
            _idx: 0
        }
    }

    /// Create a child scope. 
    /// While this child scope is in use, this scope cannot be used.
    pub fn child(&mut self) -> ScopeContext<T> {
        ScopeContext { 
            scope: HashMap::new(), 
            parent: NonNull::new(self),
            _ghost: PhantomData,
            _idx: self._idx + 1
        }
    }

    fn ancestors(&self) -> Iter<T> {
        Iter { next: Some(self) }
    }
    fn ancestors_mut(&mut self) -> IterMut<T> {
        IterMut { next: NonNull::new(self) }
    }

    /// Iterator providing an immutable reference to all HashMaps of variables
    fn hash_maps(&self) -> impl Iterator<Item = &HashMap<String, T>> {
        self.ancestors().map(|m| &m.scope)
    }
    /// Iterator providing a mutable reference to all HashMaps of variables
    fn hash_maps_mut(&mut self) -> impl Iterator<Item = &mut HashMap<String, T>> {
        self.ancestors_mut().map(|m| &mut m.scope)
    }
    /// Query a variable (or return `None` if it does not exist)
    #[allow(unused)]
    pub fn get(&self, ident: &str) -> Option<&T> {
        self.hash_maps()
            .find_map(|m| m.get(ident))
    }

    /// Declares a variable in the current scope
    pub fn declare(&mut self, ident: String, v: T) -> super::RtResult<&T> {
        self.declare_full(ident, v, ReasgType::Let, MutType::Mut)
    }

    /// Declares a variable in the current scope (with a reassignment type and mutability type)
    pub fn declare_full(
        &mut self, ident: String, v: T, _rt: ReasgType, _mt: MutType
    ) -> super::RtResult<&T> {
        let entry = self.scope.entry(ident.clone());

        // only allow declare if variable in the top level is not declared
        match entry {
            Entry::Occupied(_) => Err(super::NameErr::AlreadyDeclared(ident))?,
            Entry::Vacant(_) => Ok(entry.or_insert(v)),
        }
    }

    /// Set a variable (or error if it is not declared)
    #[allow(unused)]
    pub fn set(&mut self, ident: &str, v: T) -> super::RtResult<&T> {
        let maybe_m = self.hash_maps_mut()
            .find(|m| m.contains_key(ident));

        if let Some(m) = maybe_m {
            m.insert(String::from(ident), v);

            Ok(self.get(ident).unwrap())
        } else {
            Err(super::NameErr::NotDeclared(String::from(ident)))?
        }
    }

    pub fn get_indexed(&self, ident: &str, midx: Option<usize>) -> Option<&T> {
        let maybe_m = {
            let mut hms = self.hash_maps();

            match midx {
                Some(idx) => hms.nth(idx),
                None => hms.last()
            }
        };
        
        maybe_m.and_then(|m| m.get(ident))
    }

    pub fn set_indexed(&mut self, ident: &str, v: T, midx: Option<usize>) -> super::RtResult<T> 
        where T: Clone
    {
        let maybe_m = {
            let mut hms = self.hash_maps_mut();

            match midx {
                Some(idx) => hms.nth(idx),
                None => hms.last()
            }
        };

        let ident = String::from(ident);
        if let Some(m) = maybe_m {
            match m.entry(ident) {
                Entry::Occupied(mut ent) => {
                    ent.insert(v);
                    Ok(ent.get().clone())
                },
                Entry::Vacant(ent) => {
                    Err(super::NameErr::NotDeclared(ent.into_key()))?
                },
            }
        } else {
            Err(super::NameErr::NotDeclared(ident))?
        }
    }

    // HACK.
    
    pub fn idx(&self) -> usize {
        self._idx
    }

    pub fn goto_idx(&mut self, idx: usize) -> Option<&mut ScopeContext<T>> {
        self.ancestors_mut()
            .find(|ctx| ctx._idx == idx)
    }
}

impl VarContext<'_> {
    /// Create a new VarContext that includes the standard library.
    pub fn new_with_std() -> Self {
        Self { 
            scope: gstd::std_map(), 
            parent: None, 
            _ghost: PhantomData,
            _idx: 0
        }
    }

}
#[cfg(test)]
mod tests {
    use crate::interpreter::runtime::RtResult;
    use crate::interpreter::runtime::Value;

    use super::VarContext;

    #[test]
    fn scope_test() -> RtResult<()> {
        let mut a = VarContext::new_with_std();
        a.declare(String::from("a"), Value::Int(1))?;
        
        let mut b = a.child();
        b.declare(String::from("b"), Value::Int(2))?;
        
        let mut c = b.child();
        c.declare(String::from("c"), Value::Int(3))?;
        
        // err, b/c mutable borrow
        // b.set(String::from("c"), Value::Int(14));
        
        println!("{:?}", c.get("d"));
        println!("{:?}", b.get("b"));

        Ok(())
    }
}