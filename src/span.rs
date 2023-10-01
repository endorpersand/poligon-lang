use std::ops::RangeInclusive;

/// Indicates a specific character in given code.
pub type Cursor = (usize /* line */, usize /* character */);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Span {
    start: Cursor,
    end: Cursor,
}

impl Span {
    pub fn new(r: RangeInclusive<Cursor>) -> Span {
        Span {
            start: *r.start(),
            end: *r.end()
        }
    }
    pub fn one(c: Cursor) -> Span {
        Span::new(c ..= c)
    }

    pub fn start(&self) -> Cursor {
        self.start
    }

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

        Span::new(left ..= right)
    }
}
impl std::ops::RangeBounds<Cursor> for Span {
    fn start_bound(&self) -> std::ops::Bound<&Cursor> {
        std::ops::Bound::Included(&self.start)
    }

    fn end_bound(&self) -> std::ops::Bound<&Cursor> {
        std::ops::Bound::Included(&self.end)
    }
}
impl From<RangeInclusive<Cursor>> for Span {
    fn from(value: RangeInclusive<Cursor>) -> Self {
        Span::new(value)
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