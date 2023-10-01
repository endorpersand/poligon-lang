use std::ops::{RangeInclusive, RangeFrom};

/// Indicates a specific character in given code.
pub type Cursor = (usize /* line */, usize /* character */);

/// Indicates a contiguous range of characters in given code.
#[deprecated(note="use Span")]
pub type CursorRange = RangeInclusive<Cursor>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Span {
    Closed(RangeInclusive<Cursor>),
    Open(RangeFrom<Cursor>)
}
impl Span {
    pub fn from_bounds(start: Cursor, end: Option<Cursor>) -> Span {
        match end {
            Some(end) => Span::from(start ..= end),
            None => Span::from(start ..),
        }
    }

    pub fn start(&self) -> Cursor {
        match self {
            Span::Closed(r) => *r.start(),
            Span::Open(r) => r.start,
        }
    }

    pub fn end(&self) -> Option<Cursor> {
        match self {
            Span::Closed(r) => Some(*r.end()),
            Span::Open(_) => None,
        }
    }
    /// Merges a span into another span.
    /// 
    /// Spans are contiguous, so merging spans will
    /// also collect all tokens between the two spans
    /// which may not have originally been part of the spans.
    pub fn append(&self, span: &Span) -> Span {
        let (left1, right1) = (self.start(), self.end());
        let (left2, right2) = (span.start(), span.end());

        let left = left1.min(left2);
        let right = right1.zip(right2).map(|(r1, r2)| r1.max(r2));

        Span::from_bounds(left, right)
    }
}
impl std::ops::RangeBounds<Cursor> for Span {
    fn start_bound(&self) -> std::ops::Bound<&Cursor> {
        match self {
            Span::Closed(r) => r.start_bound(),
            Span::Open(r) => r.start_bound(),
        }
    }

    fn end_bound(&self) -> std::ops::Bound<&Cursor> {
        match self {
            Span::Closed(r) => r.end_bound(),
            Span::Open(r) => r.end_bound(),
        }
    }
}
impl From<RangeInclusive<Cursor>> for Span {
    fn from(value: RangeInclusive<Cursor>) -> Self {
        Span::Closed(value)
    }
}
impl From<RangeFrom<Cursor>> for Span {
    fn from(value: RangeFrom<Cursor>) -> Self {
        Span::Open(value)
    }
}

pub trait Spanned {
    fn span(&self) -> &Span;
}
impl Spanned for Span {
    fn span(&self) -> &Span {
        self
    }
}