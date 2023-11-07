use std::fmt::{Display, Formatter};

use crate::ast::{ReasgType, MutType};

use crate::compiler::plir::*;

use super::*;

impl StmtLike for ProcStmt {
    fn ends_with_block(&self) -> bool { self.ends_with_block() }
}

impl StmtLike for HoistedStmt {
    fn ends_with_block(&self) -> bool { self.ends_with_block() }
}

fn fmt_typed_block(f: &mut Formatter<'_>, b: &Block, omit_ty: bool) -> std::fmt::Result {
    if omit_ty {
        write!(f, "{}", BlockDisplay(&b.1))
    } else {
        write!(f, "{b}")
    }
}

fn write_ident(f: &mut Formatter<'_>, ident: &str) -> std::fmt::Result {
    if ident.chars().all(|t| t.is_alphanumeric() || t == '_') {
        ident.fmt(f)
    } else {
        write!(f, "{ident:?}")
    }
}
fn write_ty(f: &mut Formatter<'_>, ty: &plir::Type) -> std::fmt::Result {
    match ty {
        Type::Prim(ident) => write_ident(f, ident),
        _ => write!(f, "{ty}") // for now, no types require wrapping
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        fmt_stmt_list(f, &self.0)
    }
}

impl Display for HoistedStmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            HoistedStmt::FunDecl(fd) => write!(f, "{fd}"),
            HoistedStmt::ExternFunDecl(fs) => {
                write!(f, "extern ")?;
                fun_signature(f, fs, true)
            },
            HoistedStmt::ClassDecl(c) => write!(f, "{c}"),
            HoistedStmt::IGlobal(id, val) => {
                write!(f, "global ")?;
                write_ident(f, id)?;
                write!(f, " = {val:?}")
            },
        }
    }
}
impl Display for ProcStmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ProcStmt::Decl(d) => write!(f, "{d}"),
            ProcStmt::Return(me) => match me {
                Some(e) => write!(f, "return {e}"),
                None => write!(f, "return"),
            },
            ProcStmt::Break => write!(f, "break"),
            ProcStmt::Continue => write!(f, "continue"),
            ProcStmt::Throw(s) => write!(f, "throw {s:?}"),
            ProcStmt::Exit(me) => match me {
                Some(e) => write!(f, "exit {e}"),
                None => write!(f, "exit"),
            },
            ProcStmt::Expr(e) => write!(f, "{e}"),
        }
    }
}
impl Display for Class {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Class { ty, fields: field_map } = self;
        write!(f, "class ")?;
        write_ty(f, ty)?;
        write!(f, " ")?;
        
        if field_map.is_empty() {
            write!(f, "{{ }}")
        } else {
            writeln!(f, "{{")?;

            for (fname, fdcl) in field_map.iter().take(field_map.len() - 1) {
                writeln!(f, "{:4}{fname}: {fdcl},", "")?;
            }

            let (fname, fdcl) = field_map.last().unwrap();
            writeln!(f, "{:4}{fname}: {fdcl}", "")?;

            write!(f, "}}")
        }
    }
}

impl Display for Field {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Field { rt, mt, ty } = self;
        
        match rt {
            ReasgType::Let => {},
            ReasgType::Const => write!(f, "const ")?,
        };
        match mt {
            MutType::Mut => write!(f, "mut ")?,
            MutType::Immut => {},
        }
        write!(f, "{ty}")
    }
}

impl Display for Decl {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Decl { rt, mt, ident, ty, val } = self;
        
        match rt {
            ReasgType::Let => write!(f, "let "),
            ReasgType::Const => write!(f, "const "),
        }?;
        match mt {
            MutType::Mut => write!(f, " mut ")?,
            MutType::Immut => {},
        };

        write_ident(f, ident)?;
        write!(f, ": {ty} = ")?;
        match val {
            Expr { expr: ExprType::Block(b), .. } => fmt_typed_block(f, b, ty == &b.0),
            e => write!(f, "{e}"),
        }
    }
}

impl Display for FunIdent {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FunIdent::Simple(s) => write_ident(f, s),
            FunIdent::Static(t, attr) => {
                write_ty(f, t)?;
                write!(f, "::")?;
                write_ident(f, attr)
            },
        }
    }
}

fn fun_signature(f: &mut Formatter<'_>, fs: &FunSignature, external: bool) -> std::fmt::Result {
    let FunSignature { private, ident, params, ret, varargs } = fs;
    if !external && *private {
        write!(f, "priv ")?;
    }
    write!(f, "fun {ident}(")?;
    
    fmt_iter(f, {
        params.iter()
            .map::<&dyn Display, _>(|t| t)
            .chain(varargs.then_some(&".." as _))
    })?;
    
    write!(f, ") -> {ret}")
}

impl Display for FunSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        fun_signature(f, self, false)
    }
}

impl Display for FunDecl {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let FunDecl { sig, block } = self;

        write!(f, "{sig} ")?;
        fmt_typed_block(f, block, sig.ret == block.0)
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

        write_ident(f, ident)?;
        write!(f, ": {ty}")
    }
}

impl Display for TypeRef<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeRef::Unk(unk) => write!(f, "?{unk}"),
            TypeRef::TypeVar(ty, var) => {
                write!(f, "?")?;
                write_ident(f, ty)?;
                write!(f, "::{var}")
            },
            TypeRef::Prim(ident) => write!(f, "{ident}"),
            TypeRef::Generic(ident, params, ()) => {
                write!(f, "{ident}")?;
                if !params.is_empty() {
                    write!(f, "[")?;
                    fmt_iter(f, &**params)?;
                    write!(f, "]")?;
                }
                Ok(())
            },
            TypeRef::Tuple(types, ()) => {
                write!(f, "[")?;
                fmt_iter(f, &**types)?;
                write!(f, "]")
            },
            TypeRef::Fun(ft) => write!(f, "{ft}"),
        }
    }
}

impl Display for FunTypeRef<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let FunTypeRef { params, ret, varargs } = self;
        
        write!(f, "(")?;

        fmt_iter(f, {
            params.iter()
                .map::<&dyn Display, _>(|t| t)
                .chain(varargs.then_some(&".." as _))
        })?;

        write!(f, ") -> {ret}")
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Expr { ty, expr } = self;

        let indicated = matches!(expr,
            | ExprType::Block(_)
            | ExprType::ClassLiteral(_, _)
        );

        if indicated {
            write!(f, "{expr}")
        } else {
            write!(f, "<{ty}>({expr})")
        }
    }
}
impl Display for ExprType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprType::Ident(id) => write!(f, "{id}"),
            ExprType::Block(b) => write!(f, "{b}"),
            ExprType::Literal(lt) => write!(f, "{lt}"),
            ExprType::ListLiteral(lt) => {
                write!(f, "[")?;
                fmt_iter(f, lt)?;
                write!(f, "]")
            },
            ExprType::SetLiteral(lt)  => {
                write!(f, "set {{")?;
                fmt_iter(f, lt)?;
                write!(f, "}}")
            },
            ExprType::DictLiteral(lt) => {
                write!(f, "dict {{")?;
                fmt_mapped_list(f, lt, |f, (a, b)| write!(f, "{a}: {b}"))?;
                write!(f, "}}")
            },
            ExprType::ClassLiteral(ident, lt) => {
                write!(f, "{ident} #{{")?;
                fmt_iter(f, lt)?;
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
            ExprType::For { ident, element_type, iterator, block } => write!(f, "for <{element_type}>({ident}) in {iterator} {block}"),
            ExprType::Call { funct, params } => {
                write!(f, "{funct}(")?;
                fmt_iter(f, params)?;
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
            ExprType::Cast(e) => write!(f, "castfrom {e}"),
            ExprType::Deref(d) => write!(f,"{d}"),
            ExprType::GEP(ty, ptr, rest) => {
                write_ty(f, ty)?;
                write!(f, "::#gep(")?;
                write!(f, "{ptr}")?;
                
                if !rest.is_empty() {
                    write!(f, ", ")?;
                    fmt_iter(f, rest)?;
                }
                write!(f, ")")
            },
            ExprType::Alloca(ty) => {
                write_ty(f, ty)?;
                write!(f, "::#alloca()")
            },
            ExprType::SizeOf(ty) => {
                write_ty(f, ty)?;
                write!(f, "::size_of()")
            },
        }
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Block(ty, stmts) = self;

        write!(f, "<{ty}>")?;
        write!(f, "{}", BlockDisplay(stmts))
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
        match self {
            Path::Static(o, attr, _) => {
                write_ty(f, o)?;
                write!(f, "::")?;
                write_ident(f, attr)
            },
            Path::Struct(o, attrs) => match attrs.split_last() {
                Some(((last, _), rest)) => {
                    for (_, ty) in rest.iter().rev() {
                        write!(f, "<{ty}>(")?;
                    }
                    write!(f, "{o}")?;
                    for (attr, _) in rest {
                        write!(f, ".{attr})")?;
                    }
                    write!(f, ".{last}")
                },
                None => write!(f, "{o}"),
            },
            Path::Method(o, method, _) => {
                write!(f, "{o}.")?;
                write_ident(f, method)
            },
        }
    }
}

impl Display for Index {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Index { expr, index } = self;

        write!(f, "{expr}[{index}]")
    }
}

impl Display for IDeref {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let IDeref { expr, ty: _ } = self;

        write!(f, "*{}", expr.expr)
    }
}