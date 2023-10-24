use std::fmt::{Display, Formatter};

mod ast;
mod plir;

trait StmtLike: Display {
    fn ends_with_block(&self) -> bool;
}

fn fmt_stmt_list<S: StmtLike>(f: &mut Formatter<'_>, stmts: &[S]) -> std::fmt::Result {
    for stmt in stmts {
        write!(f, "{stmt}")?;

        if !stmt.ends_with_block() {
            write!(f, ";")?;
        }
    
        writeln!(f)?;
    }
    
    Ok(())
}

fn fmt_list<D: Display>(f: &mut Formatter<'_>, elems: &[D]) -> std::fmt::Result {
    if let Some((tail, head)) = elems.split_last() {
        for el in head {
            write!(f, "{el}, ")?;
        }

        write!(f, "{tail}")
    } else {
        Ok(())
    }
}

fn fmt_mapped_list<T, F>(f: &mut Formatter<'_>, elems: &[T], map: F) -> std::fmt::Result 
    where F: Fn(&mut Formatter<'_>, &T) -> std::fmt::Result
{
    if let Some((tail, head)) = elems.split_last() {
        for el in head {
            map(f, el)?;
            write!(f, ", ")?;
        }

        map(f, tail)
    } else {
        Ok(())
    }
}

struct StmtsDisplay<'b, S>(&'b [S]);
impl<S: StmtLike> Display for StmtsDisplay<'_, S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        fmt_stmt_list(f, self.0)
    }
}

struct BlockDisplay<'b, S>(&'b [S]);
impl<S: StmtLike> Display for BlockDisplay<'_, S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.0.is_empty() {
            write!(f, "{{}}")
        } else {
            writeln!(f, "{{")?;

            let buf = StmtsDisplay(self.0).to_string();
            for line in buf.lines() {
                writeln!(f, "{:4}{line}", "")?;
            }

            write!(f, "}}")
        }
    }
}