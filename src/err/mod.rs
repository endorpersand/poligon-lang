use std::collections::HashMap;
use std::ops::{RangeInclusive, RangeFrom, RangeBounds, Bound};

pub trait GonErr {
    fn err_name(&self) -> &'static str;
    fn message(&self) -> String;

    fn at(self, p: Point) -> FullGonErr<Self> 
        where Self: Sized
    {
        FullGonErr { err: self, position: ErrPos::Point(p) }
    }
    
    fn at_points(self, pts: &[Point]) -> FullGonErr<Self> 
        where Self: Sized
    {
        if pts.len() == 1 {
            self.at(pts[0])
        } else {
            let mut vec: Vec<_> = pts.into();
            vec.sort();
            FullGonErr { err: self, position: ErrPos::Points(vec) }
        }
    }
    
    fn at_range(self, range: impl RangeBounds<Point>) -> FullGonErr<Self> 
        where Self: Sized
    {
        let start = match range.start_bound() {
            Bound::Included(p) | Bound::Excluded(p) => *p,
            Bound::Unbounded => (0, 0),
        };
        
        let position = match range.end_bound() {
            Bound::Included(p) | Bound::Excluded(p) => ErrPos::Range(start..=(*p)),
            Bound::Unbounded => ErrPos::RangeFrom(start..),
        };

        FullGonErr { err: self, position }
    }


}

// TODO: this impl is only here to be used for debug purposes.
// it should be deleted once everything has been implemented.
pub struct DebugErr<E: std::fmt::Debug>(E);
impl<E: std::fmt::Debug> GonErr for DebugErr<E> {
    fn err_name(&self) -> &'static str {
        "error"
    }

    fn message(&self) -> String {
        format!("{:?}", self.0)
    }
}
impl<E: std::fmt::Debug> From<E> for FullGonErr<DebugErr<E>> {
    fn from(err: E) -> Self {
        DebugErr(err).at((0, 0))
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct FullGonErr<E: GonErr> {
    err: E,
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

/// Get line from original text.
/// 
/// Panics if line number is not in the string.
fn get_line(orig_txt: &str, lno: usize) -> String {
    orig_txt.lines().nth(lno)
        .expect(&format!("Expected line {} to exist", lno))
        .into()
}

fn ptr_point(orig_txt: &str, (lno, cno): Point) -> [String; 2] {
    let code = get_line(orig_txt, lno);
    let ptr = " ".repeat(cno) + "^";

    return [code, ptr];
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
            " ".repeat(start_lno)
            + "^"
            + &"~".repeat(end_lno - start_lno - 1)
            + "^";

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
    pub fn short_msg(&self) -> String {
        let (lno, cno) = *match &self.position {
            ErrPos::Point(p) => p,
            ErrPos::Points(pts) => pts.get(0).unwrap_or(&(0, 0)),
            ErrPos::Range(ri) => ri.start(),
            ErrPos::RangeFrom(RangeFrom { start }) => start,
        };

        format!("{}:{} :: {}: {}", lno, cno, self.err.err_name(), self.err.message())
    }

    pub fn full_msg(&self, orig_txt: &str) -> String {
        let mut lines = vec![self.short_msg()];
        
        match &self.position {
            ErrPos::Point(p) => {
                lines.extend(ptr_point(orig_txt, *p));
            },
            ErrPos::Points(pts) => {
                let mut lmap = HashMap::new();

                for (lno, cno) in pts {
                    lmap.entry(*lno).or_insert(vec![]).push(*cno);
                }

                for (lno, cnos) in lmap {
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