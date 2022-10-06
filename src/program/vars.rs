use std::collections::HashMap;
use std::marker::PhantomData;
use std::ptr::NonNull;

use super::value::Value;

/// Stores the variables in the current scope.
/// 
/// This also provides read/write access to variables in parent scopes.
#[derive(Debug)]
pub struct VarContext<'a> {
    scope: HashMap<String, Value>,
    parent: Option<NonNull<VarContext<'a>>>,
    _ghost: PhantomData<&'a ()>
}

/// Iterates through a VarContext and its parents to provide all accessible HashMaps
struct VCtxIter<'a> {
    next: Option<&'a VarContext<'a>>
}
impl<'a> Iterator for VCtxIter<'a> {
    type Item = &'a HashMap<String, Value>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next.map(|current| {
            let m_next_ptr = current.parent;

            // SAFETY: Since we're here, this context must be a child context,
            // so there must be a pointer to a parent context here.
            self.next = m_next_ptr.map(|next_ptr| unsafe { next_ptr.as_ref() });
            &current.scope
        })
    }
}

/// Mutable version of `VCtxIter`
struct VCtxIterMut<'a> {
    next: Option<NonNull<VarContext<'a>>>
}
impl<'a> Iterator for VCtxIterMut<'a> {
    type Item = &'a mut HashMap<String, Value>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next.take().map(|mut c_ptr| {
            // SAFETY: Pointer can only come from the parent chains,
            // so, there must be a pointer to some context here.
            let current = unsafe { c_ptr.as_mut() };

            self.next = current.parent;
            &mut current.scope
        })
    }
}

impl VarContext<'_> {
    /// Create a new VarContext.
    pub fn new() -> Self {
        Self { 
            scope: HashMap::new(), 
            parent: None, 
            _ghost: PhantomData 
        }
    }

    /// Create a child scope. 
    /// While this child scope is in use, this scope cannot be used.
    pub fn child(&mut self) -> VarContext {
        VarContext { 
            scope: HashMap::new(), 
            parent: NonNull::new(self),
            _ghost: PhantomData
        }
    }

    /// Iterator providing an immutable reference to all HashMaps of variables
    fn hash_maps(&self) -> VCtxIter {
        VCtxIter { next: Some(self) }
    }
    /// Iterator providing a mutable reference to all HashMaps of variables
    fn hash_maps_mut(&mut self) -> VCtxIterMut {
        VCtxIterMut { next: NonNull::new(self) }
    }
    /// Query a variable (or return `None` if it does not exist)
    pub fn get(&self, ident: &str) -> Option<&Value> {
        self.hash_maps()
            .flat_map(|m| m.get(ident))
            .next()
    }
    /// Set a variable (and declare it if it does not exist)
    pub fn set(&mut self, ident: String, v: Value) {
        let maybe_map = self.hash_maps_mut()
            .filter(|m| m.contains_key(&ident))
            .next();

        if let Some(map) = maybe_map {
            map.insert(ident, v);
        } else {
            self.scope.insert(ident, v);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::program::value::Value;

    use super::VarContext;

    #[test]
    fn scope_test() {
        let mut a = VarContext::new();
        a.set(String::from("a"), Value::Int(1));
        
        let mut b = a.child();
        b.set(String::from("b"), Value::Int(2));
        
        let mut c = b.child();
        c.set(String::from("c"), Value::Int(3));
        
        // err, b/c mutable borrow
        // b.set(String::from("c"), Value::Int(14));
        
        println!("{:?}", c.get("d"));
        println!("{:?}", b.get("b"));
    }
}