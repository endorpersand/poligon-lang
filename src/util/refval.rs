use std::cell::RefCell;
use std::rc::Rc;

pub type RefValue<T> = Rc<RefCell<T>>;

pub trait RefValueUtil<T> {
    fn wrap(t: T) -> Self;
}

impl<T> RefValueUtil<T> for Rc<RefCell<T>> {
    fn wrap(t: T) -> Self {
        Rc::new(RefCell::new(t))
    }
}