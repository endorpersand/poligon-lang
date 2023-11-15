//! This module holds utilities useful for handling spans.
//! 
//! The main items here are the [`Span`] struct, which holds the span of characters,
//! and the [`Spanned`] trait, which indicates that a struct has a span.

use std::ops::{Add, AddAssign, Range};

/// Indicates a specific character in given code.
pub type Cursor = usize;

/// A struct holding a span of characters (inclusive on both sides).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    /// Creates a new span.
    pub fn new(r: Range<Cursor>) -> Span {
        Span {
            start: r.start,
            end: r.end
        }
    }
    /// Creates a new span of no characters.
    pub fn none() -> Span {
        Span::new(0..0)
    }
    /// Creates a new span of one character.
    pub fn one(c: Cursor) -> Span {
        Span::new(c..(c + 1))
    }

    /// The start of the span.
    pub fn start(&self) -> Cursor {
        self.start
    }

    /// The end of the span.
    pub fn end(&self) -> Cursor {
        self.end
    }

    /// Merges a span into another span.
    /// 
    /// Spans are contiguous, so merging spans will
    /// also collect all tokens between the two spans
    /// which may not have originally been part of the spans.
    pub fn append(self, span: Span) -> Span {
        let (left1, right1) = (self.start, self.end);
        let (left2, right2) = (span.start, span.end);

        let left = left1.min(left2);
        let right = right1.max(right2);

        Span::new(left .. right)
    }
}
impl std::ops::RangeBounds<Cursor> for Span {
    fn start_bound(&self) -> std::ops::Bound<&Cursor> {
        std::ops::Bound::Included(&self.start)
    }

    fn end_bound(&self) -> std::ops::Bound<&Cursor> {
        std::ops::Bound::Excluded(&self.end)
    }
}
impl From<Range<Cursor>> for Span {
    fn from(value: Range<Cursor>) -> Self {
        Span::new(value)
    }
}
impl Add for Span {
    type Output = Span;

    fn add(self, rhs: Self) -> Self::Output {
        self.append(rhs)
    }
}
impl AddAssign for Span {
    fn add_assign(&mut self, rhs: Self) {
        *self = (*self).append(rhs);
    }
}

/// Trait indicating that an item has a span.
pub trait Spanned {
    /// Gets the span of this item.
    fn span(&self) -> Span;
}
impl Spanned for Span {
    fn span(&self) -> Span {
        *self
    }
}
impl<S: Spanned> Spanned for &S {
    fn span(&self) -> Span {
        (*self).span()
    }
}