//! Underlying core behind the error printing for the compiler.
//! 
//! This module unifies the several different types of errors output by the other modules.
//! Other error types (e.g. [`LexErr`]) can implement the [`GonErr`] trait to keep track of the 
//! error type's name and message.
//! 
//! [`FullGonErr`] does much of the work to convert [`GonErr`]s into a printed error in terminal.
//! 
//! TODO!: code block
//! 
//! [`LexErr`]: crate::lexer::LexErr

use std::collections::BTreeSet;
use std::error::Error;
use std::fmt::Write as _;
use std::ops::{RangeFrom, RangeBounds, Bound};
use crate::span::{Cursor, Span};

/// These are one-indexed. The first character is (1, 1).
type CursorNos = (usize /* line no. */, usize /* char no. */);

/// Errors that can be output by the Poligon compiler.
/// 
/// This trait requires that the struct provides the name of the error type and the message of the error (in Display).
/// Implementing these enables functionality to designate *where* an error occurred and to produce 
/// a formatted error message.
pub trait GonErr: Error + Sized {
    /// The name of the error type (e.g. `syntax error`, `runtime error`)
    fn err_name(&self) -> &'static str;

    /// Designate that this error occurred at a specific position
    fn at(self, p: Cursor) -> FullGonErr<Self> {
        FullGonErr::new(self, vec![ErrPos::from_point(p)])
    }
    
    /// Designate that this error occurred at a few specific positions
    fn at_points(self, pts: &[Cursor]) -> FullGonErr<Self> {
        let pts = pts.iter()
            .map(|&p| ErrPos::from_point(p));
        FullGonErr::new(self, pts)
    }
    
    /// Designate that this error occurred within a range of positions
    fn at_range(self, range: impl RangeBounds<Cursor>) -> FullGonErr<Self> {
        FullGonErr::new(self, vec![ErrPos::from_range(range)])
    }

    /// Designate that this error occurred at an unknown position in the code
    fn at_unknown(self) -> FullGonErr<Self> {
        FullGonErr::new(self, vec![])
    }
}

impl<E: GonErr> GonErr for &E {
    fn err_name(&self) -> &'static str {
        (*self).err_name()
    }
}
impl<E: GonErr> From<E> for FullGonErr<E> {
    fn from(err: E) -> Self {
        err.at_unknown()
    }
}
impl GonErr for std::io::Error {
    fn err_name(&self) -> &'static str {
        "io error"
    }
}
/// An error that has an associated position.
/// 
/// This struct is used by the Poligon compiler to format and print compiler errors.
#[derive(PartialEq, Eq, Debug)]
pub struct FullGonErr<E: GonErr> {
    pub(crate) err: E,
    pos: BTreeSet<ErrPos>
}

impl<E: GonErr> FullGonErr<E> {
    fn new(e: E, positions: impl IntoIterator<Item=ErrPos>) -> Self {
        Self { err: e, pos: positions.into_iter().collect() }
    }

    fn short_msg_builder<'a>(&'a self, src: &'a str) -> Result<MessageBuilder<'a>, std::fmt::Error> {
        let mut builder = MessageBuilder::new(src);
        
        let mut it = self.pos.iter();
        if let Some(e0) = it.next() {
            builder.add_position(e0)?;

            for e in it {
                builder.write(", ")?;
                builder.add_position(e)?;
            }
        }

        if !builder.output.is_empty() { builder.write(" :: ")?; }

        builder.add_descriptor(&self.err)?;
        builder.write("\n")?;

        Ok(builder)
    }

    fn full_msg_builder<'a>(&'a self, src: &'a str) -> Result<MessageBuilder<'a>, std::fmt::Error> {
        let mut builder = self.short_msg_builder(src)?;
        builder.write("\n")?;

        for p in &self.pos {
            builder.add_err_label(p)?;
            builder.write("\n")?;
        }

        Ok(builder)
    }

    /// Get a String designating where the error occurred 
    /// and the message associated with the error.
    pub fn short_msg(&self, src: &str) -> String {
        self.short_msg_builder(src).unwrap().output
    }

    /// Get a String designating where the error occurred,
    /// the message associated with the error,
    /// and a pointer to what happened at the line to cause the error.
    pub fn full_msg(&self, src: &str) -> String {
        self.full_msg_builder(src).unwrap().output
    }

    /// Map the inner error to another error.
    pub fn map<F: GonErr>(self, f: impl FnOnce(E) -> F) -> FullGonErr<F> {
        FullGonErr {
            err: f(self.err),
            pos: self.pos
        }
    }

    /// Designate that this error also occurred at a specific position
    pub fn and_at(mut self, p: Cursor) -> Self {
        self.pos.insert(ErrPos::from_point(p));
        self
    }
    
    /// Designate that this error also occurred within a range of positions
    pub fn and_at_range(mut self, range: impl RangeBounds<Cursor>) -> Self {
        self.pos.insert(ErrPos::from_range(range));
        self
    }
}

impl<E: GonErr + PartialEq> PartialEq<E> for FullGonErr<E> {
    fn eq(&self, other: &E) -> bool {
        &self.err == other
    }
}
impl<E: GonErr> From<std::convert::Infallible> for FullGonErr<E> {
    fn from(value: std::convert::Infallible) -> Self {
        match value {}
    }
}

macro_rules! impl_from_err {
    ($($t:ty => $u:ty$(: $id:ident => $e:block)?),*) => {
        $(
            $(
                impl From<$t> for $u {
                    fn from($id: $t) -> Self { $e }
                }
            )?
            impl From<$crate::err::FullGonErr<$t>> for $crate::err::FullGonErr<$u> {
                fn from(err: $crate::err::FullGonErr<$t>) -> Self {
                    err.map(<$u>::from)
                }
            }
        )*
    };
}
pub(crate) use impl_from_err;

#[derive(PartialEq, Eq, Debug)]
enum ErrPos {
    /// Error occurred at a specific character
    Point(Cursor),

    /// Error occurred at a range of points (start inclusive, end exclusive)
    Range(Span),

    /// Error occurred at an range of points (start inclusive, all the way to the end)
    RangeFrom(RangeFrom<Cursor>)
}

impl ErrPos {
    pub fn from_point(p: Cursor) -> Self {
        Self::Point(p)
    }

    pub fn from_range(range: impl RangeBounds<Cursor>) -> Self {
        let start = match range.start_bound() {
            Bound::Included(&p) => p,
            Bound::Excluded(&p) => p + 1,
            Bound::Unbounded => 0,
        };
        
        let end = match range.end_bound() {
            Bound::Included(&p) => Some(p + 1),
            Bound::Excluded(&p) => Some(p),
            Bound::Unbounded    => None,
        };

        match end {
            Some(e) if e == start + 1 => ErrPos::Point(start),
            Some(e) => ErrPos::Range(Span::from(start..e)),
            None => ErrPos::RangeFrom(start..)
        }
    }
}

impl PartialOrd for ErrPos {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for ErrPos {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use std::cmp::Ordering;

        fn key(pos: &ErrPos) -> (Cursor, Option<Cursor>) {
            match pos {
                ErrPos::Point(p)     => (*p,        Some(*p)),
                ErrPos::Range(r)     => (r.start(), Some(r.end())),
                ErrPos::RangeFrom(r) => (r.start,   None),
            }
        }

        let (lstart, lend) = key(self);
        let (rstart, rend) = key(other);
        
        // none last
        lstart.cmp(&rstart).then_with(|| match (lend, rend) {
            (Some(l), Some(r)) => l.cmp(&r),
            (Some(_), None)    => Ordering::Less,
            (None, Some(_))    => Ordering::Greater,
            (None, None)       => Ordering::Equal,
        })
    }
}

struct MessageBuilder<'s> {
    orig: &'s str,
    output: String
}

impl<'s> MessageBuilder<'s> {
    fn new(orig: &'s str) -> Self {
        Self {
            orig,
            output: String::new(),
        }
    }
    fn cursor_nos(&self, i: usize) -> CursorNos {
        let slice = &self.orig[..i];
        match slice.lines().count() {
            0 => (0, 0),
            lno => {
                let Some(line) = slice.lines().last() else {
                    unreachable!("str checked to be non-empty")
                };
                (lno, line.len() + 1)
            }
        }
    }

    fn add_position(&mut self, pos: &ErrPos) -> std::fmt::Result {
        match *pos {
            ErrPos::Point(i) => {
                let (lno, cno) = self.cursor_nos(i);
                write!(self.output, "{lno}:{cno}")
            },

            ErrPos::Range(ri) => {
                let (start_lno, start_cno) = self.cursor_nos(ri.start());
                let (end_lno, end_cno) = self.cursor_nos(ri.end());
                write!(self.output, "{start_lno}:{start_cno}-{end_lno}:{end_cno}")
            },
            
            ErrPos::RangeFrom(RangeFrom { start }) => {
                let (start_lno, start_cno) = self.cursor_nos(start);
                write!(self.output, "{start_lno}:{start_cno}-..")
            },
        }
    }

    fn add_descriptor(&mut self, error: &impl GonErr) -> std::fmt::Result {
        write!(self.output, "{name}: {desc}", 
            name = error.err_name(), 
            desc = error
        )
    }

    // This adds a \n.
    fn add_code_line(&mut self, lno: usize) -> Result<&str, std::fmt::Error> {
        let line = get_line_raw(self.orig, lno);
        writeln!(self.output, "{line}")?;
        Ok(line)
    }
    fn add_point_label(&mut self, p: usize) -> std::fmt::Result {
        let (lno, cno) = self.cursor_nos(p);
        self.add_code_line(lno)?;
        write!(
            self.output, "{ptr:>width$}",
            ptr = '^',
            width = cno
        )
    }
    fn add_range_ptr(&mut self, start_cno: usize, end_cno: usize) -> std::fmt::Result {
        write!(
            self.output, "{ptr:>space_width$}{ptr:~>underline_width$}",
            ptr = '^',
            space_width = start_cno,
            underline_width = end_cno - start_cno - 1
        )
    }
    fn add_range_labels(&mut self, r: &impl RangeBounds<Cursor>) -> std::fmt::Result {
        let start = match r.start_bound() {
            Bound::Included(&p) => p,
            Bound::Excluded(&p) => p + 1,
            Bound::Unbounded    => 0,
        };
        let end = match r.end_bound() {
            Bound::Included(&p) => p + 1,
            Bound::Excluded(&p) => p,
            Bound::Unbounded    => self.orig.len(),
        };
        let (start_lno, start_cno) = self.cursor_nos(start);
        let (end_lno, end_cno) = self.cursor_nos(end);

        match (start_lno == end_lno, end_cno.checked_sub(start_cno)) {
            (true, Some(0)) => Ok(()),
            (true, Some(1)) => self.add_point_label(start),
            (true, None) => panic!("end character precedes start character"),
            (true, _) => {
                // 3-9
                // 0 1 2 3 4 5 6 7 8 9
                // _ _ _ ^ ~ ~ ~ ~ ~ ^ _ _ _
                self.add_code_line(start_lno)?;
                self.add_range_ptr(start_cno, end_cno)
            },
            (false, _) => {
                let start_line = self.add_code_line(start_lno)?;
                let start_len = start_line.len();
                self.add_range_ptr(start_cno, start_len)?;
                writeln!(self.output)?;

                self.add_code_line(end_lno)?;
                self.add_range_ptr(0, end_cno)
            }
        }
    }
    fn add_err_label(&mut self, e: &ErrPos) -> std::fmt::Result {
        match e {
            ErrPos::Point(p) => self.add_point_label(*p),
            ErrPos::Range(r) => self.add_range_labels(r),
            ErrPos::RangeFrom(r) => self.add_range_labels(r),
        }
    }
    fn write(&mut self, c: &str) -> std::fmt::Result {
        self.output.write_str(c)
    }
}
/// Get line from original text.
/// 
/// Panics if line number is not in the string.
fn get_line_raw(txt: &str, lno: usize) -> &str {
    txt.lines()
        .nth(lno - 1)
        .unwrap_or_else(|| panic!("Expected line {lno} to exist"))
}
