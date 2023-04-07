//! Underlying core behind the error printing for the interpreter.
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
use std::fmt::Display;
use std::ops::{RangeInclusive, RangeFrom, RangeBounds, Bound};

/// Errors that can be printed by the Poligon interpreter.
/// 
/// This trait requires that the struct provides the name of the error type and the message of the error (in Display).
/// Implementing these enables functionality to designate *where* an error occurred and to produce 
/// a formatted error message.
pub trait GonErr: Display + Sized {
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

/// An error that has an associated position.
/// 
/// This struct is used by the Poligon interpreter to format and print interpreter errors.
#[derive(PartialEq, Eq, Debug)]
pub struct FullGonErr<E: GonErr> {
    pub(crate) err: E,
    pos: BTreeSet<ErrPos>
}

/// Indicates a specific character in given code.
pub type Cursor = (usize /* line */, usize /* character */);

/// Indicates a contiguous range of characters in given code.
pub type CursorRange = RangeInclusive<Cursor>;

#[derive(PartialEq, Eq, Debug)]
enum ErrPos {
    /// Error occurred at a specific point
    Point(Cursor),

    /// Error occurred at an inclusive range of points
    Range(CursorRange),

    /// Error occurred at an range of points, going to the end
    RangeFrom(RangeFrom<Cursor>)
}

impl ErrPos {
    pub fn from_point(p: Cursor) -> Self {
        Self::Point(p)
    }

    pub fn from_range(range: impl RangeBounds<Cursor>) -> Self {
        let start = match range.start_bound() {
            Bound::Included(p) | Bound::Excluded(p) => *p,
            Bound::Unbounded => (0, 0),
        };
        
        match range.end_bound() {
            Bound::Included(p) | Bound::Excluded(p) => {
                if p == &start {
                    ErrPos::Point(start)
                } else {
                    ErrPos::Range(start..=(*p))
                }
            },
            Bound::Unbounded => ErrPos::RangeFrom(start..),
        }
    }

    fn position(&self) -> String {
        match self {
            ErrPos::Point((lno, cno)) => format!("{}:{}", lno + 1, cno + 1),

            ErrPos::Range(ri) => {
                let (start_lno, start_cno) = ri.start();
                let (end_lno, end_cno) = ri.end();
                format!("{}:{}-{}:{}", start_lno + 1, start_cno + 1, end_lno + 1, end_cno + 1)
            },
            
            ErrPos::RangeFrom(RangeFrom { start }) => {
                let (start_lno, start_cno) = start;
                format!("{}:{}-..", start_lno + 1, start_cno + 1)
            },
        }
    }

    fn display_pointer(&self, src: &str) -> Vec<String> {
        match self {
            ErrPos::Point(p)     => ptr_point(src, *p).into(),
            ErrPos::Range(r)     => ptrs_range(src, r),
            ErrPos::RangeFrom(r) => ptrs_range(src, r),
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
                ErrPos::Point(p)     => (*p,         Some(*p)),
                ErrPos::Range(r)     => (*r.start(), Some(*r.end())),
                ErrPos::RangeFrom(r) => (r.start,    None),
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
/// Get line from original text.
/// 
/// Panics if line number is not in the string.
fn get_line(orig_txt: &str, lno: usize) -> String {
    orig_txt.lines().nth(lno)
        .unwrap_or_else(|| panic!("Expected line {} to exist", lno))
        .into()
}

fn ptr_point(orig_txt: &str, (lno, cno): Cursor) -> [String; 2] {
    let code = get_line(orig_txt, lno);
    let ptr = " ".repeat(cno) + "^";

    [code, ptr]
}
fn ptrs_range(orig_txt: &str, r: &impl RangeBounds<Cursor>) -> Vec<String> {
    let (start_lno, start_cno) = match r.start_bound() {
        Bound::Included(p) | Bound::Excluded(p) => *p,
        Bound::Unbounded => (0, 0),
    };
    let (end_lno, end_cno) = match r.end_bound() {
        Bound::Included(p) | Bound::Excluded(p) => *p,
        Bound::Unbounded => {
            let lcount = orig_txt.lines().count();
            if lcount == 0 { // if lcount is 0, there are no lines
                (0, 0)
            } else {
                let ccount = orig_txt.lines().last().unwrap().len();
                (lcount - 1, ccount.saturating_sub(1))
            }
        },
    };

    if (start_lno, start_cno) == (end_lno, end_cno) {
        ptr_point(orig_txt, (start_lno, start_cno)).into()
    } else if start_lno == end_lno {
        let code = get_line(orig_txt, start_lno);

        if end_cno < start_cno { panic!("end character precedes start character"); }

        // 3-9
        // 0 1 2 3 4 5 6 7 8 9
        // _ _ _ ^ ~ ~ ~ ~ ~ ^ _ _ _

        let ptrs =
            " ".repeat(start_cno)
            + &"~".repeat(end_cno - start_cno + 1);

            vec![code, ptrs]
    } else {
        let mut lines = vec![];
        // after start pointer, ~ until you get to the end of the line
        let start_code = get_line(orig_txt, start_lno);
        let start_len = start_code.len();
    
        let start_ptr = 
            " ".repeat(start_cno) 
            + "^"
            + &"~".repeat(start_len - start_cno - 1);
    
        lines.push(start_code);
        lines.push(start_ptr);
    
        // ~ before the end pointer
        let end_code = get_line(orig_txt, end_lno);
    
        let end_ptr = "~".repeat(end_cno) + "^";
    
        lines.push(end_code);
        lines.push(end_ptr);

        lines
    }
}

impl<E: GonErr> FullGonErr<E> {
    fn new(e: E, positions: impl IntoIterator<Item=ErrPos>) -> Self {
        Self { err: e, pos: positions.into_iter().collect() }
    }

    /// Get a String designating where the error occurred 
    /// and the message associated with the error.
    pub fn short_msg(&self) -> String {
        let line_fmt = self.pos.iter()
            .map(ErrPos::position)
            .collect::<Vec<_>>()
            .join(", ");

        if !line_fmt.trim().is_empty() {
            format!("{} :: {}: {}", line_fmt.trim(), self.err.err_name(), self.err)
        } else {
            format!("{}: {}", self.err.err_name(), self.err)
        }
    }

    /// Get a String designating where the error occurred,
    /// the message associated with the error,
    /// and a pointer to what happened at the line to cause the error.
    pub fn full_msg(&self, src: &str) -> String {
        let mut lines = vec![self.short_msg(), String::new()];
        
        for p in &self.pos {
            lines.extend(p.display_pointer(src));
        }

        lines.join("\n")
    }

    /// Map the inner error to another error.
    pub fn map<F: GonErr>(self, f: impl FnOnce(E) -> F) -> FullGonErr<F> {
        FullGonErr {
            err: f(self.err),
            pos: self.pos
        }
    }

    /// Cast the inner error to another error.
    pub fn cast_err<F: GonErr + From<E>>(self) -> FullGonErr<F> {
        self.map(F::from)
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

macro_rules! full_gon_cast_impl {
    ($t:ty, $u:ty) => {
        impl From<$crate::err::FullGonErr<$t>> for $crate::err::FullGonErr<$u> {
            fn from(err: $crate::err::FullGonErr<$t>) -> Self {
                err.cast_err()
            }
        }
    }
}
pub(crate) use full_gon_cast_impl;