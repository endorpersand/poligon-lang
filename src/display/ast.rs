use std::fmt::{Display, Formatter};

use crate::ast::*;

use super::*;

impl StmtLike for Stmt {
    fn ends_with_block(&self) -> bool {
        self.ends_with_block()
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        fmt_stmt_list(f, &self.0)
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Decl(d) => write!(f, "{d}"),
            Stmt::Return(me) => match me {
                Some(e) => write!(f, "return {e}"),
                None => write!(f, "return"),
            },
            Stmt::Break => write!(f, "break"),
            Stmt::Continue => write!(f, "continue"),
            Stmt::FunDecl(fd) => write!(f, "{fd}"),
            Stmt::ExternFunDecl(fs) => write!(f, "extern {fs}"),
            Stmt::Expr(e) => write!(f, "{e}"),
            Stmt::ClassDecl(s) => write!(f, "{s}"),
        }
    }
}

impl Display for Decl {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Decl { rt, pat, ty, val } = self;
        
        match rt {
            ReasgType::Let => write!(f, "let "),
            ReasgType::Const => write!(f, "const "),
        }?;
        write!(f, "{pat}")?;
        
        if let Some(t) = ty {
            write!(f, ": {t}")?;
        }

        write!(f, " = {val}")
    }
}

impl<T: Display> Display for Pat<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Pat::Unit(t) => write!(f, "{t}"),
            Pat::Spread(mp) => {
                write!(f, "..")?;
                match mp {
                    Some(pat) => write!(f, "{pat}"),
                    None => Ok(()),
                }
            },
            Pat::List(vec) => {
                write!(f, "[")?;
                fmt_list(f, vec)?;
                write!(f, "]")
            },
        }
    }
}

impl Display for DeclUnit {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let DeclUnit(ident, mt) = self;
        
        match mt {
            MutType::Mut => write!(f, "mut ")?,
            MutType::Immut => {},
        }

        write!(f, "{ident}")
    }
}

impl Display for FunSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let FunSignature { ident, params, ret } = self;

        write!(f, "fun {ident}(")?;
        fmt_list(f, params)?;
        write!(f, ") ")?;

        if let Some(retty) = ret {
            write!(f, "-> {retty}")?;
        };

        Ok(())
    }
}
impl Display for MethodSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let MethodSignature { referent, is_static, name, params, ret } = self;

        write!(f, "fun ")?;
        if let Some(ref_ident) = referent {
            write!(f, "{ref_ident}")?;
        }
        match is_static {
            true  => write!(f, "::"),
            false => write!(f, "."),
        }?;
        
        write!(f, "{name}(")?;
        fmt_list(f, params)?;
        write!(f, ") ")?;

        if let Some(retty) = ret {
            write!(f, "-> {retty}")?;
        };

        Ok(())
    }
}
impl Display for FunDecl {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let FunDecl { sig, block } = self;
        write!(f, "{sig} {block}")
    }
}

impl Display for Param {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Param { rt, mt, ident, ty } = self;

        match rt {
            ReasgType::Let => {},
            ReasgType::Const => write!(f, "const ")?,
        }

        match mt {
            MutType::Mut => write!(f, "mut ")?,
            MutType::Immut => {},
        }

        write!(f, "{ident}")?;

        match ty {
            Some(t) => write!(f, ": {t}"),
            None => Ok(())
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Type(ident, params) = self;
        
        write!(f, "{ident}")?;
        if !params.is_empty() {
            write!(f, "<")?;
            fmt_list(f, params)?;
            write!(f, ">")?;
        }

        Ok(())
    }
}

impl Display for Class {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Class { ident, fields, methods } = self;
        write!(f, "class {ident} {{")?;
        for field in fields {
            writeln!(f,"{field}")?;
        }
        writeln!(f)?;
        for method in methods {
            writeln!(f,"{method}")?;
        }
        write!(f, "}}")
    }
}
impl Display for FieldDecl {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let FieldDecl { rt, mt, ident, ty } = self;
        
        match rt {
            ReasgType::Let => write!(f, "let "),
            ReasgType::Const => write!(f, "const "),
        }?;
        match mt {
            MutType::Mut => write!(f, "mut ")?,
            MutType::Immut => {},
        }
        write!(f, "{ident}: {ty}")
    }
}
impl Display for MethodDecl {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let MethodDecl { sig, block } = self;
        write!(f, "{sig} {block}")
    }
}
impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Ident(id) => write!(f, "{id}"),
            Expr::Block(b) => write!(f, "{b}"),
            Expr::Literal(lt) => write!(f, "{lt}"),
            Expr::ListLiteral(lt) => {
                write!(f, "[")?;
                fmt_list(f, lt)?;
                write!(f, "]")
            },
            Expr::SetLiteral(lt)  => {
                write!(f, "set {{")?;
                fmt_list(f, lt)?;
                write!(f, "}}")
            },
            Expr::DictLiteral(lt) => {
                write!(f, "dict {{")?;
                fmt_mapped_list(f, lt, |(a, b)| format!("{a}: {b}"))?;
                write!(f, "}}")
            },
            Expr::ClassLiteral(name, lt) => {
                write!(f, "{name} #{{")?;
                fmt_mapped_list(f, lt, |(a, b)| format!("{a}: {b}"))?;
                write!(f, "}}")
            },
            Expr::Assign(asg, expr) => write!(f, "{asg} = {expr}"),
            Expr::Path(p) => write!(f, "{p}"),
            Expr::StaticPath(t, a) => write!(f, "{t}::{a}"),
            Expr::UnaryOps { ops, expr } => {
                for op in ops {
                    write!(f, "{op}")?;
                }
        
                write!(f, "{expr}")
            },
            Expr::BinaryOp { op, left, right } => {
                write!(f, "{left} {op} {right}")
            },
            Expr::Comparison { left, rights } => {
                write!(f, "{left}")?;

                for (cmp, right) in rights {
                    write!(f, " {cmp} {right}")?;
                }

                Ok(())
            },
            Expr::Range { left, right, step } => {
                write!(f, "{left}..{right}")?;
                match step {
                    Some(s) => write!(f, " step {s}"),
                    None => Ok(()),
                }
            },
            Expr::If { conditionals, last } => {
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
            Expr::While { condition, block } => write!(f, "while {condition} {block}"),
            Expr::For { ident, iterator, block } => write!(f, "for {ident} in {iterator} {block}"),
            Expr::Call { funct, params } => {
                write!(f, "{funct}(")?;
                fmt_list(f, params)?;
                write!(f, ")")
            },
            Expr::Index(idx) => write!(f, "{idx}"),
            Expr::Spread(me) => {
                write!(f, "..")?;
                match me {
                    Some(e) => write!(f, "{e}"),
                    None => Ok(()),
                }
            },
        }
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", BlockDisplay(&self.0))
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Int(v) => write!(f, "{v}"),
            Literal::Float(v) => write!(f, "{v}"),
            Literal::Char(v) => write!(f, "{v:?}"),
            Literal::Str(v) => write!(f, "{v:?}"),
            Literal::Bool(v) => write!(f, "{v}"),
        }
    }
}

impl Display for AsgUnit {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AsgUnit::Ident(ident) => write!(f, "{ident}"),
            AsgUnit::Path(p) => write!(f, "{p}"),
            AsgUnit::Index(idx) => write!(f, "{idx}"),
        }
    }
}

impl Display for Path {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Path { obj, attrs } = self;
        write!(f, "{obj}")?;

        for attr in attrs {
            write!(f, ".{attr}")?;
        }

        Ok(())
    }
}

impl Display for Index {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Index { expr, index } = self;

        write!(f, "{expr}[{index}]")
    }
}

impl<T: Display> Display for Located<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}