use std::fmt::Display;

use crate::ast::{ReasgType, MutType};

use super::*;

fn fmt_stmt_list(f: &mut std::fmt::Formatter<'_>, stmts: &[Stmt]) -> std::fmt::Result {
    for stmt in stmts {
        write!(f, "{stmt}")?;

        if !stmt.ends_with_block() {
            write!(f, ";")?;
        }
        writeln!(f)?;
    }
    
    Ok(())
}

fn fmt_list<D: Display>(f: &mut std::fmt::Formatter<'_>, elems: &[D]) -> std::fmt::Result {
    if let Some((tail, head)) = elems.split_last() {
        for el in head {
            write!(f, "{el}, ")?;
        }

        write!(f, "{tail}")
    } else {
        Ok(())
    }
}
fn fmt_mapped_list<T, D: Display, F>(f: &mut std::fmt::Formatter<'_>, elems: &[T], map: F) -> std::fmt::Result 
    where F: Fn(&T) -> D
{
    if let Some((tail, head)) = elems.split_last() {
        for el in head {
            write!(f, "{}, ", map(el))?;
        }

        write!(f, "{}", map(tail))
    } else {
        Ok(())
    }
}

struct BlockDisplay<'b>(&'b [Stmt]);
impl Display for BlockDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt_stmt_list(f, self.0)
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt_stmt_list(f, &self.0)
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Decl(d) => write!(f, "{d}"),
            Stmt::Return(me) => match me {
                Some(e) => write!(f, "return {e}"),
                None => write!(f, "return"),
            },
            Stmt::Break => write!(f, "break"),
            Stmt::Continue => write!(f, "continue"),
            Stmt::Exit(me) => match me {
                Some(e) => write!(f, "exit {e}"),
                None => write!(f, "exit"),
            },
            Stmt::FunDecl(fd) => write!(f, "{fd}"),
            Stmt::ExternFunDecl(fs) => write!(f, "extern {fs}"),
            Stmt::Expr(e) => write!(f, "{e}"),
        }
    }
}

impl Display for Decl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Decl { rt, mt, ident, ty, val } = self;
        
        match rt {
            ReasgType::Let => write!(f, "let "),
            ReasgType::Const => write!(f, "const "),
        }?;
        match mt {
            MutType::Mut => write!(f, " mut ")?,
            MutType::Immut => {},
        };

        write!(f, "{ident}: {ty} = {val}")
    }
}

impl Display for FunSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FunSignature { ident, params, ret } = self;
        write!(f, "fun {ident}(")?;
        fmt_list(f, params)?;
        write!(f, ") -> {ret}")
    }
}

impl Display for FunDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FunDecl { sig, block } = self;
        write!(f, "{sig} {block}")
    }
}

impl Display for Param {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Param { rt, mt, ident, ty } = self;

        match rt {
            ReasgType::Let => {},
            ReasgType::Const => write!(f, "const ")?,
        }

        match mt {
            MutType::Mut => write!(f, "mut ")?,
            MutType::Immut => {},
        }

        write!(f, "{ident}: {ty}")
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Prim(ident) => write!(f, "{ident}"),
            Type::Generic(ident, params) => {
                write!(f, "{ident}")?;
                if !params.is_empty() {
                    write!(f, "<")?;
                    fmt_list(f, params)?;
                    write!(f, ">")?;
                }
                Ok(())
            },
            Type::Tuple(types) => {
                write!(f, "[")?;
                fmt_list(f, types)?;
                write!(f, "]")
            },
            Type::Fun(params, ret) => {
                write!(f, "(")?;
                fmt_list(f, params)?;
                write!(f, ") -> {ret}")
            },
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Expr { ty, expr } = self;
        match expr {
            ExprType::Block(_) => write!(f, "{expr}"), // it is already included in Block
            _ => write!(f, "<{ty}>({expr})")
        }
    }
}
impl Display for ExprType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprType::Ident(id) => write!(f, "{id}"),
            ExprType::Block(b) => write!(f, "{b}"),
            ExprType::Literal(lt) => write!(f, "{lt}"),
            ExprType::ListLiteral(lt) => {
                write!(f, "[")?;
                fmt_list(f, lt)?;
                write!(f, "]")
            },
            ExprType::SetLiteral(lt)  => {
                write!(f, "set {{")?;
                fmt_list(f, lt)?;
                write!(f, "}}")
            },
            ExprType::DictLiteral(lt) => {
                write!(f, "dict {{")?;
                fmt_mapped_list(f, lt, |(a, b)| format!("{a}: {b}"))?;
                write!(f, "}}")
            },
            ExprType::Assign(asg, expr) => write!(f, "{asg} = {expr}"),
            ExprType::Path(p) => write!(f, "{p}"),
            ExprType::UnaryOps { ops, expr } => {
                for (op, ty) in ops {
                    write!(f, "<{ty}>({op}")?;
                }
                write!(f, "{expr}")?;
                for _ in 0..(ops.len()) {
                    write!(f, ")")?;
                }
                Ok(())
            },
            ExprType::BinaryOp { op, left, right } => {
                write!(f, "{left} {op} {right}")
            },
            ExprType::Comparison { left, rights } => {
                write!(f, "{left}")?;

                for (cmp, right) in rights {
                    write!(f, " {cmp} {right}")?;
                }

                Ok(())
            },
            ExprType::Range { left, right, step } => {
                write!(f, "{left}..{right}")?;
                match step {
                    Some(s) => write!(f, " step {s}"),
                    None => Ok(()),
                }
            },
            ExprType::If { conditionals, last } => {
                if let Some(((penult_cond, penult_block), rest)) = conditionals.split_last() {
                    for (cond, block) in rest {
                        write!(f, "if {cond} {block} else")?;
                    }
                    
                    write!(f, "if {penult_cond} {penult_block}")?;
                    match last {
                        Some(b) => write!(f, " else {b}"),
                        None => Ok(()),
                    }
                } else {
                    // fallback which should not occur
                    match last {
                        Some(b) => write!(f, "{b}"),
                        None => Ok(()),
                    }
                }
            },
            ExprType::While { condition, block } => write!(f, "while {condition} {block}"),
            ExprType::For { ident, iterator, block } => write!(f, "for {ident} in {iterator} {block}"),
            ExprType::Call { funct, params } => {
                write!(f, "{funct}(")?;
                fmt_list(f, params)?;
                write!(f, ")")
            },
            ExprType::Index(idx) => write!(f, "{idx}"),
            ExprType::Spread(me) => {
                write!(f, "..")?;
                match me {
                    Some(e) => write!(f, "{e}"),
                    None => Ok(()),
                }
            },
            ExprType::Split(ident, idx) => {
                write!(f, "{ident}~[")?;
                match idx {
                    Split::Left(left) => write!(f, "{left}"),
                    Split::Middle(left, right) => write!(f, "{left}..=-{right}"),
                    Split::Right(right) => write!(f, "-{right}"),
                }?;
                write!(f, "]")
            },
        }
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Block(ty, stmts) = self;

        write!(f, "<{ty}>")?;
        if stmts.is_empty() {
            write!(f, "{{}}")
        } else {
            writeln!(f, "{{")?;

            let buf = BlockDisplay(stmts).to_string();
            for line in buf.lines() {
                writeln!(f, "{:4}{line}", "")?;
            }

            write!(f, "}}")
        }
    }
}

impl Display for AsgUnit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AsgUnit::Ident(ident) => write!(f, "{ident}"),
            AsgUnit::Path(p) => write!(f, "{p}"),
            AsgUnit::Index(idx) => write!(f, "{idx}"),
        }
    }
}

impl Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Path { obj, attrs } = self;
        
        for (_, _, ty) in attrs {
            write!(f, "<{ty}>(")?;
        }
        write!(f, "{obj}")?;

        for (attr, st, _) in attrs {
            if *st {
                write!(f, "::")
            } else {
                write!(f, ".")
            }?;

            write!(f, "{attr})")?;
        }

        Ok(())
    }
}

impl Display for Index {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Index { expr, index } = self;

        write!(f, "{expr}[{index}]")
    }
}

#[cfg(test)]
mod tests {
    use crate::Interpreter;

    #[test]
    fn test_basic() {
        let ir = Interpreter::from_file("_test_files/fib.gon").unwrap();
        println!("{}", ir.parse().unwrap());
    }
}