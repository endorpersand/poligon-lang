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
        fmt_stmt_list(f, &self.stmts)
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Decl(e) => e.fmt(f),
            Stmt::Return(e) => e.fmt(f),
            Stmt::Break(e) => e.fmt(f),
            Stmt::Continue(e) => e.fmt(f),
            Stmt::Throw(e) => e.fmt(f),
            Stmt::FunDecl(e) => e.fmt(f),
            Stmt::ExternFunDecl(e) => e.fmt(f),
            Stmt::Expr(e) => e.fmt(f),
            Stmt::Class(e) => e.fmt(f),
            Stmt::Import(e) => e.fmt(f),
            Stmt::ImportIntrinsic(e) => e.fmt(f),
            Stmt::IGlobal(e) => e.fmt(f),
            Stmt::FitClassDecl(e) => e.fmt(f),
        }
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.ident.fmt(f)
    }
}
impl Display for StrLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.literal)
    }
}

impl Display for Decl {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Decl { rt, pat, ty, val, span: _ } = self;
        
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
            Pat::Spread { inner, span: _ } => {
                write!(f, "..")?;
                match inner {
                    Some(pat) => write!(f, "{pat}"),
                    None => Ok(()),
                }
            },
            Pat::List { values, span: _ } => {
                write!(f, "[")?;
                fmt_list(f, values)?;
                write!(f, "]")
            },
        }
    }
}

impl Display for DeclUnit {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let DeclUnit { ident, mt, span: _ } = self;
        
        match mt {
            MutType::Mut => write!(f, "mut ")?,
            MutType::Immut => {},
        }

        write!(f, "{ident}")
    }
}

impl Display for Return {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(e) = &self.expr {
            write!(f, "return {e}")
        } else {
            write!(f, "return")
        }
    }
}
impl Display for Break {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "break")
    }
}
impl Display for Continue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "continue")
    }
}
impl Display for Throw {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "throw {}", self.message)
    }
}

impl Display for FunSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let FunSignature { ident, generics, params, varargs, ret, span: _ } = self;

        write!(f, "fun {ident}")?;
        if !generics.is_empty() {
            write!(f, "<")?;
            fmt_list(f, generics)?;
            write!(f, ">")?;
        }
        write!(f, "(")?;

        let mut pd: Vec<_> = params.iter()
        .map(|t| t as _)
        .collect();
        if *varargs { pd.push(&".." as _); }
        fmt_dyn_list(f, &pd)?;

        write!(f, ") ")?;

        if let Some(retty) = ret {
            write!(f, "-> {retty}")?;
        };

        Ok(())
    }
}
impl Display for MethodSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let MethodSignature { referent, is_static, name, generic_params, params, ret, span } = self;

        write!(f, "fun ")?;
        if let Some(ref_ident) = referent {
            write!(f, "{ref_ident}")?;
        }
        match is_static {
            true  => write!(f, "::"),
            false => write!(f, "."),
        }?;
        write!(f, "{name}")?;
        
        if !generic_params.is_empty() {
            write!(f, "<")?;
            fmt_list(f, generic_params)?;
            write!(f, ">")?;
        }
        write!(f, "(")?;
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
        let FunDecl { sig, block, span: _ } = self;
        write!(f, "{sig} {block}")
    }
}

impl Display for Param {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Param { rt, mt, ident, ty, span: _ } = self;

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
        let Type { ident, params, span: _ } = self;
        
        write!(f, "{ident}")?;
        if !params.is_empty() {
            write!(f, "<")?;
            fmt_list(f, params)?;
            write!(f, ">")?;
        }

        Ok(())
    }
}

impl Display for ExternFunDecl {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "extern {}", self.sig)
    }
}

impl Display for Class {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Class { ident, generic_params, fields, methods, span: _ } = self;
        write!(f, "class {ident}")?;
        if !generic_params.is_empty() {
            write!(f, "<")?;
            fmt_list(f, generic_params)?;
            write!(f, ">")?;
        }
        write!(f, " {{")?;
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
        let FieldDecl { rt, mt, ident, ty, span: _ } = self;
        
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
        let MethodDecl { sig, block, span: _ } = self;
        write!(f, "{sig} {block}")
    }
}

impl Display for Import {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "import {}", self.path)
    }
}
impl Display for ImportIntrinsic {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "import intrinsic")
    }
}

impl Display for IGlobal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "global {} = {:?}", self.ident, self.value)
    }
}

impl Display for FitClassDecl {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let FitClassDecl { ty, methods, span } = self;

        write!(f, "fit class {ty} {{")?;
        for method in methods {
            writeln!(f,"{method}")?;
        }
        write!(f, "}}")
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Ident(id) => write!(f, "{id}"),
            Expr::Block(b) => write!(f, "{b}"),
            Expr::Literal(lt) => write!(f, "{lt}"),
            Expr::ListLiteral { values, span: _ } => {
                write!(f, "[")?;
                fmt_list(f, values)?;
                write!(f, "]")
            },
            Expr::SetLiteral { values, span: _ } => {
                write!(f, "set {{")?;
                fmt_list(f, values)?;
                write!(f, "}}")
            },
            Expr::DictLiteral { entries, span: _ } => {
                write!(f, "dict {{")?;
                fmt_mapped_list(f, entries, |(a, b)| format!("{a}: {b}"))?;
                write!(f, "}}")
            },
            Expr::ClassLiteral { ty, entries, span: _ } => {
                write!(f, "{ty} #{{")?;
                fmt_mapped_list(f, entries, |(a, b)| format!("{a}: {b}"))?;
                write!(f, "}}")
            },
            Expr::Assign { target, value, span: _ } => write!(f, "{target} = {value}"),
            Expr::Path(p) => write!(f, "{p}"),
            Expr::StaticPath(sp) => write!(f, "{sp}"),
            Expr::UnaryOps { ops, expr, span: _ } => {
                for op in ops {
                    write!(f, "{op}")?;
                }
        
                write!(f, "{expr}")
            },
            Expr::BinaryOp { op, left, right, span: _ } => {
                write!(f, "{left} {op} {right}")
            },
            Expr::Comparison { left, rights, span: _ } => {
                write!(f, "{left}")?;

                for (cmp, right) in rights {
                    write!(f, " {cmp} {right}")?;
                }

                Ok(())
            },
            Expr::Range { left, right, step, span: _ } => {
                write!(f, "{left}..{right}")?;
                match step {
                    Some(s) => write!(f, " step {s}"),
                    None => Ok(()),
                }
            },
            Expr::If { conditionals, last, span: _ } => {
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
            Expr::While { condition, block, span: _ } => write!(f, "while {condition} {block}"),
            Expr::For { ident, iterator, block, span: _ } => write!(f, "for {ident} in {iterator} {block}"),
            Expr::Call { funct, args, span: _ } => {
                write!(f, "{funct}(")?;
                fmt_list(f, args)?;
                write!(f, ")")
            },
            Expr::Index(idx) => write!(f, "{idx}"),
            Expr::Spread { expr, span: _ } => {
                write!(f, "..")?;
                match expr {
                    Some(e) => write!(f, "{e}"),
                    None => Ok(()),
                }
            },
            Expr::Deref(d) => write!(f, "{d}"),
        }
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", BlockDisplay(&self.stmts))
    }
}

impl Display for LitKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LitKind::Int(v) => write!(f, "{v}"),
            LitKind::Float(v) => write!(f, "{v}"),
            LitKind::Char(v) => write!(f, "{v:?}"),
            LitKind::Str(v) => write!(f, "{v:?}"),
            LitKind::Bool(v) => write!(f, "{v}"),
        }
    }
}
impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

impl Display for AsgUnit {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AsgUnit::Ident(ident) => write!(f, "{ident}"),
            AsgUnit::Path(p) => write!(f, "{p}"),
            AsgUnit::Index(idx) => write!(f, "{idx}"),
            AsgUnit::Deref(d) => write!(f, "{d}"),
        }
    }
}

impl Display for Path {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Path { obj, attrs, span: _ } = self;
        write!(f, "{obj}")?;

        for attr in attrs {
            write!(f, ".{attr}")?;
        }

        Ok(())
    }
}

impl Display for StaticPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}::{}", self.ty, self.attr)
    }
}

impl Display for Index {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Index { expr, index, span: _ } = self;

        write!(f, "{expr}[{index}]")
    }
}

impl Display for IDeref {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "*{}", self.reference)
    }
}