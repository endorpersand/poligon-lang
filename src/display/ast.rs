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
        let MethodSignature { referent, is_static, name, generic_params, params, ret, span: _ } = self;

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
        let FitClassDecl { ty, methods, span: _ } = self;

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
            Expr::Ident(e) => e.fmt(f),
            Expr::Block(e) => e.fmt(f),
            Expr::Literal(e) => e.fmt(f),
            Expr::ListLiteral(e) => e.fmt(f),
            Expr::SetLiteral(e) => e.fmt(f),
            Expr::DictLiteral(e) => e.fmt(f),
            Expr::ClassLiteral(e) => e.fmt(f),
            Expr::Assign(e) => e.fmt(f),
            Expr::Path(e) => e.fmt(f),
            Expr::StaticPath(e) => e.fmt(f),
            Expr::UnaryOps(e) => e.fmt(f),
            Expr::BinaryOp(e) => e.fmt(f),
            Expr::Comparison(e) => e.fmt(f),
            Expr::Range(e) => e.fmt(f),
            Expr::If(e) => e.fmt(f),
            Expr::While(e) => e.fmt(f),
            Expr::For(e) => e.fmt(f),
            Expr::Call(e) => e.fmt(f),
            Expr::Index(e) => e.fmt(f),
            Expr::Spread(e) => e.fmt(f),
            Expr::IDeref(e) => e.fmt(f),
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
impl Display for ListLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Self { values, span: _ } = self;
        write!(f, "[")?;
        fmt_list(f, values)?;
        write!(f, "]")
    }
}
impl Display for SetLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Self { values, span: _ } = self;
        write!(f, "set {{")?;
        fmt_list(f, values)?;
        write!(f, "}}")
    }
}
impl Display for DictLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Self { entries, span: _ } = self;
        write!(f, "dict {{")?;
        fmt_mapped_list(f, entries, |(a, b)| format!("{a}: {b}"))?;
        write!(f, "}}")
    }
}
impl Display for ClassLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Self { ty, entries, span: _ } = self;
        write!(f, "{ty} #{{")?;
        fmt_mapped_list(f, entries, |(a, b)| format!("{a}: {b}"))?;
        write!(f, "}}")
    }
}
impl Display for Assign {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Self { target, value, span: _ } = self;
        write!(f, "{target} = {value}")
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

impl Display for UnaryOps {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Self { ops, expr, span: _ } = self;
        for op in ops {
            write!(f, "{op}")?;
        }

        write!(f, "{expr}")
    }
}
impl Display for BinaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Self { op, left, right, span: _ } = self;
        write!(f, "{left} {op} {right}")
    }
}
impl Display for Comparison {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Self { left, rights, span: _ } = self;
        write!(f, "{left}")?;
    
        for (cmp, right) in rights {
            write!(f, " {cmp} {right}")?;
        }
    
        Ok(())
    }
}
impl Display for Range {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Self { left, right, step, span: _ } = self;
        write!(f, "{left}..{right}")?;
        match step {
            Some(s) => write!(f, " step {s}"),
            None => Ok(()),
        }
    }
}
impl Display for If {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Self { conditionals, last, span: _ } = self;
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
    }
} 
impl Display for While {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Self { condition, block, span: _ } = self;
        write!(f, "while {condition} {block}")
    }
}
impl Display for For {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Self { ident, iterator, block, span: _ } = self;
        write!(f, "for {ident} in {iterator} {block}")
    }
}
impl Display for Call {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Self { funct, generic_args, args, span: _ } = self;
        write!(f, "{funct}")?;
        if !generic_args.is_empty() {
            write!(f, "[")?;
            fmt_list(f, generic_args)?;
            write!(f, "]")?;
        }
        write!(f, ")")?;
        fmt_list(f, args)?;
        write!(f, ")")
    }
}
impl Display for Index {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Index { expr, index, span: _ } = self;
        write!(f, "{expr}[{index}]")
    }
}
impl Display for Spread {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Self { expr, span: _ } = self;
        write!(f, "..")?;
        match expr {
            Some(e) => write!(f, "{e}"),
            None => Ok(()),
        }
    }
}
impl Display for IDeref {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "*{}", self.reference)
    }
}