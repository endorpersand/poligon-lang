use std::cell::{RefCell, Ref, RefMut, BorrowError, BorrowMutError};
use std::rc::Rc;

use crate::runtime::RuntimeErr;

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct RefValue<T> {
    rc: Rc<RefCell<T>>,
    mutable: bool
}

#[derive(Debug)]
pub enum RvErr {
    BorrowConcur,
    BorrowMutConcur,
    BorrowMutImmutable
}

impl From<BorrowError> for RvErr {
    fn from(_: BorrowError) -> Self {
        RvErr::BorrowConcur
    }
}
impl From<BorrowMutError> for RvErr {
    fn from(_: BorrowMutError) -> Self {
        RvErr::BorrowMutConcur
    }
}
impl From<RvErr> for RuntimeErr {
    fn from(e: RvErr) -> Self {
        RuntimeErr::RvErr(e)
    }
}

impl<T> RefValue<T> {
    pub fn new(t: T, mutable: bool) -> Self {
        Self { rc: Rc::new(RefCell::new(t)), mutable }
    }

    pub fn wrap(rc: Rc<RefCell<T>>, mutable: bool) -> Self {
        Self { rc, mutable }
    }

    pub fn try_borrow(&self) -> Result<Ref<'_, T>, RvErr> {
        Ok(self.rc.try_borrow()?)
    }

    pub fn try_borrow_mut(&self) -> Result<RefMut<'_, T>, RvErr> {
        if self.mutable {
            Ok(self.rc.try_borrow_mut()?)
        } else {
            Err(RvErr::BorrowMutImmutable)
        }
    }

    pub fn ref_eq(&self, other: &RefValue<T>) -> bool {
        Rc::ptr_eq(&self.rc, &other.rc)
    }

    pub fn clone_immut(&self) -> Self {
        Self { rc: Rc::clone(&self.rc), mutable: false }
    }

    pub fn mutable(&self) -> bool {
        self.mutable
    }
}