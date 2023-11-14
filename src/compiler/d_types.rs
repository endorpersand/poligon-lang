use std::borrow::Cow;
use std::convert::Infallible;
use std::path::Path;

use indexmap::IndexMap;

use crate::ast::MutType;
use crate::compiler::plir::LtGradeable;
use crate::err::{FullGonErr, GonErr, impl_from_err};
use crate::lexer::token::{TTKind, Token};
use crate::parser::{Parseable, TokenPattern, ParseErr, Parser, TryParseable};
use crate::{token, delim};

use super::plir;


/// A struct which holds the types declared by PLIR code generation.
#[derive(Default, Clone, Debug)]
pub struct DeclaredTypes {
    pub(super) types: IndexMap<plir::Type, plir::Class>,
    pub(super) values: IndexMap<plir::FunIdent, plir::Type>
}

impl DeclaredTypes {
    /// Writes the declared types into a file.
    pub fn to_file(&self, p: impl AsRef<Path>) -> std::io::Result<()> {
        use std::fs::File;
        use std::io::prelude::*;

        let mut file = File::create(p)?;

        for class in self.types.values() {
            writeln!(file, "{class}")?;
        }
        for (ident, val_ty) in &self.values {
            if let plir::Type::Fun(f) = val_ty {
                writeln!(file, "{};", plir::HoistedStmt::ExternFunDecl(f.extern_fun_sig(ident.clone())))?
            }
            // TODO: don't ignore other types of decls
        }

        Ok(())
    }

    pub(super) fn push(&mut self, stmt: &plir::HoistedStmt) {
        use plir::HoistedStmt;

        match stmt {
            HoistedStmt::FunDecl(f) => {
                self.values.insert(f.sig.ident.clone(), f.sig.ty().into());
            },
            HoistedStmt::ExternFunDecl(f) => {
                self.values.insert(f.ident.clone(), f.ty().into());
            },
            HoistedStmt::ClassDecl(c) => {
                self.types.insert(c.ty.clone(), c.clone());
            },
            HoistedStmt::IGlobal(id, _) => {
                self.values.insert(plir::FunIdent::new_simple(id.to_string()), plir::ty!("#ptr"));
            },
        }
    }
}
impl std::ops::AddAssign for DeclaredTypes {
    fn add_assign(&mut self, rhs: Self) {
        self.types.extend(rhs.types);
        self.values.extend(rhs.values);
    }
}

/// An error that occurs in the parsing process for a declared types file.
#[derive(Debug, PartialEq, Eq)]
pub enum DParseErr {
    /// An error was raised by a parser primitive, as opposed to an declared types struct.
    CoreErr(ParseErr),

    /// The parser expected an identifier.
    ExpectedIdent,

    /// The parser expected a type expression (e.g. `list<str>`).
    ExpectedType,

    /// The parser expected a function parameter.
    ExpectedParam,
}
impl std::fmt::Display for DParseErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DParseErr::CoreErr(e)    => e.fmt(f),
            DParseErr::ExpectedIdent => write!(f, "expected identifier"),
            DParseErr::ExpectedType  => write!(f, "expected type expression"),
            DParseErr::ExpectedParam => write!(f, "expected param"),
        }
    }
}
impl std::error::Error for DParseErr {}
impl GonErr for DParseErr {
    fn err_name(&self) -> &'static str {
        "syntax error"
    }
}
impl_from_err! { ParseErr => DParseErr: err => { Self::CoreErr(err) } }

type FullDParseErr = FullGonErr<DParseErr>;

impl Parseable for DeclaredTypes {
    type Err = FullDParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        use TTKind::Token as Tk;
        let mut dtypes = Self::default();

        loop {
            match parser.peek() {
                Some(Tk(token![class]))  => {
                    let cls: plir::Class = parser.parse()?;
                    dtypes.types.insert(cls.ty.upgrade(), cls);
                },
                Some(Tk(token![extern])) => {
                    let sig = parser.parse::<ExternFunDecl>()?.0;
                    dtypes.values.insert(sig.ident.clone(), plir::Type::from(sig.ty()));
                },
                Some(Tk(token![;])) => {
                    parser.next();
                },
                Some(_) => {
                    const TOKENS: &[Token] = &[token![class], token![extern]];
                    return Err(parser.cursor.error(TOKENS.fail_err().into()))
                },
                None => break,
            }
        }

        Ok(dtypes)
    }
}
impl Parseable for plir::Class {
    type Err = FullDParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        parser.expect(token![class])?;
        let ty = parser.parse()?;

        let group = parser.expect(delim!["{}"])?;

        let mut content = Parser::new(group);
        let fields = content.parse_tuple(token![,])?
            .assert_closed(content, 
                || token![,].fail_err().into(),
                || DParseErr::ExpectedIdent
            )?
            .values()
            .map(|plir::Param { rt, mt, ident, ty }| (ident, plir::Field { rt, mt, ty }))
            .collect();

        Ok(plir::Class { ty, fields })
    }
}

struct Ident {
    ident: String
}
impl TryParseable for Ident {
    type Err = Infallible;

    fn try_read(parser: &mut Parser<'_>) -> Result<Option<Self>, Self::Err> {
        use TTKind::Token as Tk;

        let id = match *parser.peek_slice(2) {
            [Tk(token![#]), Tk(Token::Ident(s) | Token::Str(s))] => Some(String::from("#") + s),
            [Tk(Token::Ident(s) | Token::Str(s)), ..] => Some(s.to_string()),
            _ => None
        };
        
        Ok(id.map(|ident| Ident { ident }))
    }

}
impl Parseable for Ident {
    type Err = FullDParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        parser.try_parse()?
            .ok_or_else(|| parser.cursor.error(DParseErr::ExpectedIdent))
    }
}

impl TryParseable for plir::Type {
    type Err = FullDParseErr;

    fn try_read(parser: &mut Parser<'_>) -> Result<Option<Self>, Self::Err> {
        let Some(ident) = parser.try_parse::<Ident>()? else { return Ok(None) };
            
        let ty = if let Some(group) = parser.match_(delim!["[]"]) {
            let mut content = Parser::new(group);
                let values: Vec<_> = content.parse_tuple(token![,])?
                    .assert_closed(content, 
                        || token![,].fail_err().into(),
                        || DParseErr::ExpectedType
                    )?
                    .values()
                    .collect();

            plir::Type::Generic(ident.ident.into(), Cow::Owned(values), ())
        } else {
            plir::Type::Prim(ident.ident.into())
        };

        Ok(Some(ty))
    }
}
impl Parseable for plir::Type {
    type Err = FullDParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        parser.try_parse()?
            .ok_or_else(|| parser.cursor.error(DParseErr::ExpectedType))
    }
}

struct ExternFunDecl(plir::FunSignature);
impl Parseable for ExternFunDecl {
    type Err = FullDParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        parser.expect(token![extern])?;
        parser.expect(token![fun])?;

        let ident = parser.parse()?;

        let group = parser.expect(delim!["()"])?;
        let mut content = Parser::new(group);
        let tpl = content.parse_tuple(token![,])?;
        let varargs = tpl.ended_on_terminator() && parser.match_(token![..]).is_some();
        let params = tpl.assert_closed(content, 
            || token![,].fail_err().into(),
            || DParseErr::ExpectedParam
        )?
            .values()
            .collect();

        parser.expect(token![->])?;
        let ret = parser.parse()?;
        parser.expect(token![;])?;

        Ok(Self(plir::FunSignature { private: false, ident, params, varargs, ret }))
    }
}

impl TryParseable for plir::Param {
    type Err = FullDParseErr;

    fn try_read(parser: &mut Parser<'_>) -> Result<Option<Self>, Self::Err> {
        let maybe_rt = parser.try_parse()?;
        let maybe_mt = parser.match_(token![mut]).map(|_| MutType::Mut);
        let maybe_ident = parser.try_parse::<Ident>()?;

        // the param checked so far is fully empty and probably not an actual param:
        if maybe_rt.is_none() && maybe_mt.is_none() && maybe_ident.is_none() {
            return Ok(None);
        }

        let rt = maybe_rt.unwrap_or_default();
        let mt = maybe_mt.unwrap_or_default();
        let ident = maybe_ident
            .ok_or_else(|| parser.cursor.error(DParseErr::ExpectedIdent))?
            .ident;

        parser.expect(token![:])?;
        let ty = parser.parse()?;
        
        Ok(Some(plir::Param { rt, mt, ident, ty }))
    }
}
impl Parseable for plir::Param {
    type Err = FullDParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        parser.try_parse()?
            .ok_or_else(|| parser.cursor.error(DParseErr::ExpectedParam))
    }
}
impl Parseable for plir::FunIdent {
    type Err = FullDParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        use TTKind::{Token as Tk, Group as Gr};

        match *parser.peek_slice(4) {
            | [Tk(token![#]), Tk(Token::Ident(_) | Token::Str(_)), Gr(delim!["[]"]), Tk(token![::])] 
            | [Tk(Token::Ident(_) | Token::Str(_)), Gr(delim!["[]"]), Tk(token![::])] 
            => {
                let ty = parser.parse()?;
                parser.expect(token![::])?;
                let ident = parser.parse::<Ident>()?;

                Ok(plir::FunIdent::new_static(ty, ident.ident))
            },
            _ => {
                let ident = parser.parse::<Ident>()?;
                Ok(plir::FunIdent::new_simple(ident.ident))
            }
        }
    }
}