use std::cell::{RefCell, Ref, RefMut, BorrowMutError};
use std::rc::Rc;

use crate::err::GonErr;

/// This is a utility type used by Poligon's interpreter
/// to create values that are passed by reference.
/// 
/// These follow the same borrow rules as `RefCell`,
/// meaning a mutable reference cannot be held at the same
/// time as an immutable reference.
/// 
/// It is expected that a reference can only be created
/// for a quick operation and dropped when the operation
/// is complete.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct RefValue<T> {
    pub(crate) rc: Rc<RefCell<T>>,
    mutable: bool
}

/// An error occurred in the use of the RefValue type
#[derive(Debug)]
pub enum RvErr {
    /// A mutable borrow was attempted at the same time as another borrow
    BorrowMutConcur,
    /// A mutable borrow was attempted at the same time as an immutable borrow
    BorrowMutImmutable
}

impl GonErr for RvErr {
    fn err_name(&self) -> &'static str {
        "concurrency error"
    }

}
impl std::fmt::Display for RvErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RvErr::BorrowMutConcur    => write!(f, "cannot perform mutation while reference is held"),
            RvErr::BorrowMutImmutable => write!(f, "cannot perform mutation while reference is immutably held"),
        }
    }
}

impl From<BorrowMutError> for RvErr {
    fn from(_: BorrowMutError) -> Self {
        RvErr::BorrowMutConcur
    }
}

impl<T> RefValue<T> {
    /// Create a reference value from a structural value.
    /// 
    /// If this value is immutable, attempts to mutably borrow will cause an error.
    pub fn new(t: T, mutable: bool) -> Self {
        Self { rc: Rc::new(RefCell::new(t)), mutable }
    }

    /// Immutably borrow this value. 
    /// This is used to compute operations to the inner value.
    pub fn borrow(&self) -> Ref<'_, T> {
        self.rc.borrow()
    }

    /// Mutably borrow this value.
    /// This is used to mutate the inner value.
    /// 
    /// This reference should be dropped once the operation is complete.
    pub fn try_borrow_mut(&self) -> Result<RefMut<'_, T>, RvErr> {
        if self.mutable {
            Ok(self.rc.try_borrow_mut()?)
        } else {
            Err(RvErr::BorrowMutImmutable)
        }
    }

    /// Test if the values are equal by identity. 
    /// 
    /// For value equality, use [`RefValue::eq`].
    pub fn ref_eq(&self, other: &RefValue<T>) -> bool {
        Rc::ptr_eq(&self.rc, &other.rc)
    }

    /// Create a immutable reference clone to the value.
    /// 
    /// The cloned value cannot be mutably borrowed.
    pub fn clone_immut(&self) -> Self {
        Self { rc: Rc::clone(&self.rc), mutable: false }
    }

    /// Create a structural clone to the value.
    /// 
    /// The new value will *not* be equal by identity.
    pub fn clone_deep(&self) -> T
        where T: Clone
    {
        self.rc.borrow().clone()
    }

    /// Test if this value can be mutably borrowed.
    pub fn mutable(&self) -> bool {
        self.mutable
    }
}