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

use std::collections::HashMap;
use std::ops::{RangeInclusive, RangeFrom, RangeBounds, Bound};

/// Errors that can be printed by the Poligon interpreter.
/// 
/// This trait requires that the struct provides the name of the error type and the message of the error.
/// 
/// This trait provides functionality to designate *where* an error occurred.
pub trait GonErr {
    /// The name of the error type (e.g. `syntax error`, `runtime error`)
    fn err_name(&self) -> &'static str;
    /// The description of why the error occurred (e.g. `division by zero`)
    fn message(&self) -> String;

    /// Designate that this error occurred at a specific position
    fn at(self, p: Point) -> FullGonErr<Self> 
        where Self: Sized
    {
        FullGonErr::new(self, ErrPos::from_point(p))
    }
    
    /// Designate that this error occurred at a few specific positions
    fn at_points(self, pts: &[Point]) -> FullGonErr<Self> 
        where Self: Sized
    {
        FullGonErr::new(self, ErrPos::from_points(pts))
    }
    
    /// Designate that this error occurred within a range of positions
    fn at_range(self, range: impl RangeBounds<Point>) -> FullGonErr<Self> 
        where Self: Sized
    {
        FullGonErr::new(self, ErrPos::from_range(range))
    }


}

// TODO: this impl is only here to be used for debug purposes.
// it should be deleted once everything has been implemented.
impl<E: GonErr> From<E> for FullGonErr<E> {
    fn from(err: E) -> Self {
        err.at((0, 0))
    }
}

/// An error that has an associated position.
/// 
/// This struct is used by the Poligon interpreter to format and print interpreter errors.
#[derive(PartialEq, Eq, Debug)]
pub struct FullGonErr<E: GonErr> {
    pub(crate) err: E,
    position: ErrPos
}

type Point = (usize /* line */, usize /* line */);

#[derive(PartialEq, Eq, Debug)]
enum ErrPos {
    Point(Point),
    Points(Vec<Point>),
    Range(RangeInclusive<Point>),
    RangeFrom(RangeFrom<Point>)
}

impl ErrPos {
    pub fn from_point(p: Point) -> Self {
        Self::Point(p)
    }
    pub fn from_points(pts: &[Point]) -> Self {
        if let [pt] = pts {
            Self::Point(*pt)
        } else {
            let mut vec: Vec<_> = pts.into();
            vec.sort();

            Self::Points(vec)
        }
    }
    pub fn from_range(range: impl RangeBounds<Point>) -> Self {
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
}
/// Get line from original text.
/// 
/// Panics if line number is not in the string.
fn get_line(orig_txt: &str, lno: usize) -> String {
    orig_txt.lines().nth(lno)
        .unwrap_or_else(|| panic!("Expected line {} to exist", lno))
        .into()
}

fn ptr_point(orig_txt: &str, (lno, cno): Point) -> [String; 2] {
    let code = get_line(orig_txt, lno);
    let ptr = " ".repeat(cno) + "^";

    [code, ptr]
}
fn ptrs_range(orig_txt: &str, r: &impl RangeBounds<Point>) -> Vec<String> {
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
    fn new(e: E, err_pos: ErrPos) -> Self {
        Self { err: e, position: err_pos }
    }

    /// Get a String designating where the error occurred 
    /// and the message associated with the error.
    pub fn short_msg(&self) -> String {
        let line_fmt = match &self.position {
            ErrPos::Point((lno, cno)) => format!("{}:{}", lno, cno),

            ErrPos::Points(pts) => pts.iter()
                .map(|(lno, cno)| format!("{}:{}", lno, cno))
                .collect::<Vec<_>>()
                .join(", "),

            ErrPos::Range(ri) => {
                let (start_lno, start_cno) = ri.start();
                let (end_lno, end_cno) = ri.end();
                format!("{}:{}-{}:{}", start_lno, start_cno, end_lno, end_cno)
            },
            
            ErrPos::RangeFrom(RangeFrom { start }) => {
                let (start_lno, start_cno) = start;
                format!("{}:{}-..", start_lno, start_cno)
            },
        };

        if !line_fmt.is_empty() {
            format!("{} :: {}: {}", line_fmt, self.err.err_name(), self.err.message())
        } else {
            format!("{}: {}", self.err.err_name(), self.err.message())
        }
    }

    /// Get a String designating where the error occurred,
    /// the message associated with the error,
    /// and a pointer to what happened at the line to cause the error.
    pub fn full_msg(&self, orig_txt: &str) -> String {
        let mut lines = vec![self.short_msg(), String::new()];
        
        match &self.position {
            ErrPos::Point(p) => {
                lines.extend(ptr_point(orig_txt, *p));
            },
            ErrPos::Points(pts) => {
                let mut lmap: HashMap<_, Vec<_>> = HashMap::new();

                for (lno, cno) in pts {
                    lmap.entry(*lno).or_default().push(*cno);
                }

                let mut linemaps: Vec<_> = lmap.into_iter()
                    .collect();
                linemaps.sort();

                for (lno, cnos) in linemaps {
                    let code = get_line(orig_txt, lno);
                    lines.push(code);

                    let mut ptrs = vec![' '; *cnos.iter().max().unwrap() + 1];
                    for i in cnos {
                        ptrs[i] = '^';
                    }
                    lines.push(ptrs.into_iter().collect());
                }
            },
            ErrPos::Range(r) => lines.extend(ptrs_range(orig_txt, r)),
            ErrPos::RangeFrom(r) => lines.extend(ptrs_range(orig_txt, r)),
        }

        lines.join("\n")
    }
}