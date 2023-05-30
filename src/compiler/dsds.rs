use std::error::Error;
use std::fmt::Formatter;
use std::hash::Hash;
use std::marker::PhantomData;

use indexmap::{IndexSet, Equivalent};

/// Internal implementation of [`UnionFind`].
/// This data structure only holds and manipulates indexes.
#[derive(Debug)]
struct UnionFindInner {
    parents: Vec<usize>,
    sizes: Vec<usize>
}

impl UnionFindInner {
    /// Create a new disjoint-set data structure.
    fn new() -> Self {
        Self {
            parents: vec![],
            sizes: vec![]
        }
    }

    /// Gets the current size of the data structure
    fn len(&self) -> usize {
        self.parents.len()
    }

    /// Add a new disjoint set to data structure.
    /// 
    /// This function returns the new element that was added.
    fn make_set(&mut self) -> usize {
        let id = self.parents.len();
        self.parents.push(id);
        self.sizes.push(1);
        id
    }

    /// Finds the root of this element's group.
    /// 
    /// This will panic if the element >= the `len` of this data structure 
    /// (i.e., if it is not present).
    fn find(&mut self, t: usize) -> usize {
        let mut current = t;
        let mut parent = self.parents[current];

        while current != parent {
            current = parent;
            parent = self.parents[parent];
            self.parents[current] = self.parents[parent];
        }

        current
    }
    
    /// Merge the groups of the two provided elements. 
    /// 
    /// The group with the greater size (number of elements in its tree) becomes the root.
    /// 
    /// This returns true if union was successful.
    /// 
    /// This will panic if either element >= the `len` of this data structure
    /// (i.e. if either are not present).
    fn union(&mut self, x: usize, y: usize) -> bool {
        self.union_select(x, y, |_, _, _| Selector::Whatever)
    }

    /// Merges the source group into the destination group of the two provided elements, 
    /// using the predicate to determine the root.
    /// 
    /// The predicate takes the two roots of the group and returns a [`Selector`] indicating which root becomes the root node
    /// of the entire group.
    /// * If the predicate returns Left or Right, that root becomes the root of the new group.
    /// * If the predicate returns Whatever, the root with the largest size becomes the root of the new group.
    /// 
    /// This returns true if union was successful.
    /// 
    /// This will panic if either element >= the `len` of this data structure
    /// (i.e. if either are not present).
    fn union_select(&mut self, x: usize, y: usize, f: impl FnOnce(&mut UnionFindInner, usize, usize) -> Selector) -> bool {
        let mut xroot = self.find(x);
        let mut yroot = self.find(y);

        if xroot != yroot {
            let (xsize, ysize) = (self.sizes[xroot], self.sizes[yroot]);
            let f = |this, xr, yr| match f(this, xr, yr) {
                Selector::Whatever => (xsize < ysize).into(),
                s => s
            };
            if let Selector::Right = f(self, xroot, yroot) {
                (xroot, yroot) = (yroot, xroot);
            }

            // merge y into x
            self.parents[yroot] = xroot;
            self.sizes[xroot] += self.sizes[yroot];
            true
        } else {
            false
        }
    }
}

/// Index type to obtain a reference to an item in the DSDS.
#[derive(Debug)]
pub struct Idx<T>(usize, PhantomData<T>);
impl<T> Clone for Idx<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T> Copy for Idx<T> {}
impl<T> PartialEq for Idx<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0 && self.1 == other.1
    }
}
impl<T> Eq for Idx<T> {}
impl<T> PartialOrd for Idx<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl<T> Ord for Idx<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0).then_with(|| self.1.cmp(&other.1))
    }
}
/// A disjoint-set data structure.
/// 
/// This structure stores a collection of disjoint sets, 
/// allowing for creating new sets, merging sets, 
/// and checking if two elements are of the same set.
#[derive(Debug)]
pub struct UnionFind<T: Hash + Eq> {
    arena: IndexSet<T>,
    inner: UnionFindInner
}

impl<T: Hash + Eq> UnionFind<T> {
    /// Creates a new [`UnionFind`].
    pub fn new() -> Self {
        Self { arena: IndexSet::new(), inner: UnionFindInner::new() }
    }

    /// Gets the number of elements in the set.
    #[allow(dead_code)]
    pub fn len(&self) -> usize {
        self.arena.len()
    }

    /// Add a new element to the set if it does not already exist.
    /// 
    /// This function returns the index of the set associated with the added node.
    pub fn make_set(&mut self, t: T) -> Idx<T> {
        let id = match self.arena.get_index_of(&t) {
            Some(i) => i,
            None => {
                self.arena.insert(t);
                self.inner.make_set()
            }
        };
        Idx(id, PhantomData)
    }

    /// Gets the value associated with this index.
    pub fn get_idx(&self, i: Idx<T>) -> &T {
        self.arena.get_index(i.0).unwrap()
    }
    /// Gets the index associated with this value (or none if not present in set).
    pub fn get_idx_of<Q>(&self, i: &Q) -> Option<Idx<T>>
        where Q: ?Sized + Hash + Equivalent<T>
    {
        self.arena.get_index_of(i)
            .map(|id| Idx(id, PhantomData))
    }

    /// Finds the root of this element's group.
    pub fn find(&mut self, t: Idx<T>) -> Idx<T>
    {
        Idx(self.inner.find(t.0), PhantomData)
    }

    /// Merge the groups of the two provided elements. 
    /// 
    /// The group with the greater size (number of elements in its tree) becomes the root.
    /// This returns true if union was successful.
    #[allow(dead_code)]
    pub fn union(&mut self, x: Idx<T>, y: Idx<T>) -> bool {
        self.inner.union(x.0, y.0)
    }
    
    /// Merges the source group into the destination group, using the predicate to determine the root.
    /// 
    /// The predicate takes both roots and returns a [`Selector`] indicating which root becomes the root node
    /// of the entire group.
    /// * If the predicate returns Left or Right, that root becomes the root of the new group.
    /// * If the predicate returns Whatever, the root with the largest size becomes the root of the new group.
    /// 
    /// This returns true if union was successful.
    pub fn union_select(&mut self, x: Idx<T>, y: Idx<T>, f: impl FnOnce(&T, &T) -> Selector) -> bool
    {
        self.inner.union_select(x.0, y.0, |_, xr, yr| {
            let xr = self.arena.get_index(xr).unwrap();
            let yr = self.arena.get_index(yr).unwrap();
            f(xr, yr)
        })
    }

    /// Merges the source group into the destination group, using the predicate to determine the root (or indicating a failure).
    /// 
    /// The predicate takes both roots and returns a bool indicating which root becomes the root node
    /// of the entire group.
    /// * If the predicate returns Some(false), the left root becomes the root of the new group.
    /// * If the predicate returns Some(true), the right root becomes the root of the new group.
    /// * If the predicate returns None, nothing changes in the groups and a [`UnionSelectFail`] error is thrown.
    pub fn try_union_select(&mut self, x: Idx<T>, y: Idx<T>, f: impl FnOnce(&T, &T) -> Option<Selector>) -> Result<bool, UnionSelectFail>
    {
        let xr_idx = self.find(x);
        let yr_idx = self.find(y);
        let xr = self.get_idx(xr_idx);
        let yr = self.get_idx(yr_idx);

        match f(xr, yr) {
            Some(which) => Ok(self.inner.union_select(xr_idx.0, yr_idx.0, |_, _, _| which)),
            None        => Err(UnionSelectFail(()))
        }
    }
}

/// A type which holds the possible options for [`UnionFind::union_select`] and related functions.
#[derive(Debug, PartialEq, Eq)]
pub enum Selector {
    Left, Right, Whatever
}
impl From<bool> for Selector {
    fn from(value: bool) -> Self {
        match value {
            false => Selector::Left,
            true  => Selector::Right,
        }
    }
}

/// Indicates that a value was accessed which is not contained within the set.
#[derive(Debug)]
pub struct UnionSelectFail(());
impl std::fmt::Display for UnionSelectFail {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "try_union_select predicate returned None")
    }
}
impl Error for UnionSelectFail {}