use crate::span::Span;

/// AST node with a known location.
#[derive(PartialEq, Eq, Clone)]
pub struct Located<T>(pub T, pub Span);

impl<T: std::fmt::Debug> std::fmt::Debug for Located<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Located(node, loc) = self;

        write!(f, "[{:?} ..= {:?}]", loc.start(), loc.end())?;
        write!(f, "{node:?}")
    }
}
impl<T> std::ops::Deref for Located<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<T> std::ops::DerefMut for Located<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

pub(crate) type LocatedBox<T> = Box<Located<T>>;

impl<T: PartialEq> PartialEq<T> for Located<T> {
    fn eq(&self, other: &T) -> bool {
        &self.0 == other
    }
}

impl<T> Located<T> {
    /// Create a new located node.
    pub fn new(t: T, loc: Span) -> Self {
        Self(t, loc)
    }

    /// Create a new boxed located node.
    pub fn boxed(t: T, loc: Span) -> LocatedBox<T> {
        Box::new(Located(t, loc))
    }

    /// Similar to [`Option::as_ref`].
    pub fn as_ref(&self) -> Located<&T> {
        Located::new(&self.0, self.range())
    }

    /// Similar to [`Option::as_mut`].
    pub fn as_mut(&mut self) -> Located<&mut T> {
        let range = self.range();
        Located::new(&mut self.0, range)
    }
    
    /// Map a Located with a given type to a Located of another type.
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Located<U> {
        let Located(node, loc) = self;
        Located(f(node), loc)
    }

    /// Gets this located node's range.
    pub fn range(&self) -> Span {
        self.1
    }

}

impl<T: transposable::LTranspose> Located<T> {
    /// Transposes various types wrapped in Located out.
    /// 
    /// - `Located<Option<T>> => Option<Located<T>>`
    /// - `Located<Result<T, E>> => Result<Located<T>, E>`
    /// - `Located<Box<T>> => LocatedBox<T>`
    pub fn transpose(self) -> T::Transposed {
        T::ltranspose(self)
    }
}

/// Helper trait that converts a Located call for a method call
pub trait Locatable {
    /// Add a range to this object
    fn located_at(self, range: Span) -> Located<Self> 
        where Self: Sized
    {
        Located::new(self, range)
    }
}
impl<T> Locatable for T {}

mod transposable {
    use super::Located;

    pub trait LTranspose: Sized {
        type Transposed;

        fn ltranspose(located: Located<Self>) -> Self::Transposed;
    }

    impl<T> LTranspose for Option<T> {
        type Transposed = Option<Located<T>>;

        fn ltranspose(located: Located<Self>) -> Self::Transposed {
            located.0.map(|v| Located::new(v, located.1))
        }
    }
    impl<T, E> LTranspose for Result<T, E> {
        type Transposed = Result<Located<T>, E>;

        fn ltranspose(located: Located<Self>) -> Self::Transposed {
            located.0.map(|v| Located::new(v, located.1))
        }
    }
    impl<T> LTranspose for Box<T> {
        type Transposed = Box<Located<T>>;

        fn ltranspose(located: Located<Self>) -> Self::Transposed {
            let Located(val, range) = located;
            Box::new(Located::new(*val, range))
        }
    }
}