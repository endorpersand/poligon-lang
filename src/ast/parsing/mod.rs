use std::convert::Infallible;

use crate::err::{FullGonErr, impl_from_err, GonErr};
use crate::token::{TTKind, TokenTree, FullToken, Token};
use crate::parser::pat::MatchFn;
use crate::parser::{Parser, Parseable, TryParseable, TokenPattern, ParseErr};
use crate::span::Spanned;
use crate::{ast, token, delim};

use super::PatErr;

mod expr;

/// An error that occurs in the parsing process.
#[derive(Debug, PartialEq, Eq)]
pub enum AstParseErr {
    /// An error was raised by a parser primitive, as opposed to an AST struct.
    CoreErr(ParseErr),

    /// The parser expected an identifier.
    ExpectedIdent,

    /// The parser expected an expression here, but failed to match an expression.
    ExpectedExpr,

    /// The parser expected a literal here, but got something else.
    ExpectedLiteral,

    /// The string provided could not be parsed into a numeric value.
    CannotParseNumeric,

    /// The parser expected a block (e.g. `{ code; code; }`).
    ExpectedBlock,

    /// The parser expected a type expression (e.g. `list<str>`).
    ExpectedType,
    
    /// The parser expected an assignment or declaration pattern, but failed to match one.
    ExpectedPattern,

    /// The parser expected a function parameter.
    ExpectedParam,

    /// An error occurred in creating an assignment pattern.
    AsgPatErr(PatErr),

    /// Parser is not in intrinsic mode and therefore cannot use an intrinsic identifier
    NoIntrinsicIdents,

    /// Parser is not in intrinsic mode and therefore this statement cannot be defined
    NoIntrinsicStmts,
}

impl GonErr for AstParseErr {
    fn err_name(&self) -> &'static str {
        "syntax error"
    }
}

impl std::fmt::Display for AstParseErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AstParseErr::CoreErr(e)         => e.fmt(f),
            AstParseErr::ExpectedIdent      => write!(f, "expected identifier"),
            AstParseErr::ExpectedExpr       => write!(f, "expected expression"),
            AstParseErr::ExpectedLiteral    => write!(f, "expected literal"),
            AstParseErr::CannotParseNumeric => write!(f, "could not parse numeric"),
            AstParseErr::ExpectedBlock      => write!(f, "expected block"),
            AstParseErr::ExpectedType       => write!(f, "expected type expression"),
            AstParseErr::ExpectedPattern    => write!(f, "expected pattern"),
            AstParseErr::ExpectedParam      => write!(f, "expected param"),
            AstParseErr::NoIntrinsicIdents  => write!(f, "cannot use intrinsic identifier"),
            AstParseErr::NoIntrinsicStmts   => write!(f, "cannot use intrinsic statement"),
            AstParseErr::AsgPatErr(e)       => e.fmt(f),
        }
    }
}
impl std::error::Error for AstParseErr {
    fn cause(&self) -> Option<&dyn std::error::Error> {
        match self {
            AstParseErr::CoreErr(t)   => Some(t),
            AstParseErr::AsgPatErr(t) => Some(t),
            _ => None
        }
    }
}
impl_from_err! { 
    PatErr => AstParseErr: err => { Self::AsgPatErr(err) },
    ParseErr => AstParseErr: err => { Self::CoreErr(err) }
}

/// A [`Result`] type for operations in the parsing process.
pub type AstParseResult<T> = Result<T, FullAstParseErr>;
pub(crate) type FullAstParseErr = FullGonErr<AstParseErr>;

macro_rules! expected_tokens {
    ($($t:tt),*) => {
        AstParseErr::from(<[_]>::fail_err(&[$(token![$t]),*]))
    }
}

fn parse_stmts_closed(parser: &mut Parser<'_>) -> AstParseResult<Vec<ast::Stmt>> {
    use std::ops::ControlFlow::{self, Break, Continue};

    let mut stmts = vec![];
    /*
        SAMPLE:
        let a = 1;
        if a == 1 {}
        let b = 2;
        
        [let a = 1][;]
        [if a == 1 {}][no semi]
        [let b = 2][;]
        [NONE][no semi] <-- terminate
    */
    
    let mut cf: ControlFlow<()> = Continue(());
    while let Continue(()) = cf {
        // outside of REPL mode:
        // if statement exists, check for semicolon
        // - for statements with blocks, semi can be ignored
        // - semicolon MUST appear otherwise

        // in REPL mode:
        // same rules apply, however:
        // - semicolon may be omitted at the end of a block
        // - therefore, an omitted semicolon indicates the end of the block

        let (mstmt, flow) = {
            let out = {
                let stmt = parser.try_parse::<ast::Stmt>()?;
                let semi = parser.match_(token![;]);

                let stop_reading = match stmt.as_ref() {
                    // for block-terminating statements, semi isn't needed
                    // drop the semi if it exists, but don't do anything with it
                    Some(st) if st.ends_with_block() => false,

                    // in REPL mode, semicolon does not need to appear at the end of
                    // the last statement of a block
                    Some(_) if false => semi.is_none(), // TODO: parser.repl_mode

                    // outside of REPL mode, semicolon does need to appear.
                    Some(_) => match semi.is_some() {
                        true => false,
                        false => Err(parser.cursor.error(expected_tokens![;]))?,
                    },
                    
                    // end if there is no statement and no semi
                    None => semi.is_none()
                };
                
                (stmt, if stop_reading { Break(()) } else { Continue(()) })
            };

            AstParseResult::Ok(out)
        }?;
        
        if let Some(stmt) = mstmt {
            stmts.push(stmt);
        }
        cf = flow;
    }

    Ok(stmts)
}

impl Parseable for ast::Program {
    type Err = FullAstParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let (stmts, span) = parser.try_spanned(parse_stmts_closed)?;

        if parser.is_empty() {
            Ok(ast::Program { stmts, span })
        } else {
            // there are more tokens left that couldn't be parsed as a program.
            // we have an issue.
            Err(parser.cursor.error(expected_tokens![;]))
        }
    }
}

impl TryParseable for ast::Ident {
    type Err = Infallible;

    fn try_read(parser: &mut Parser<'_>) -> Result<Option<Self>, Self::Err> {
        use TTKind::Token as Tk;

        static IDENT_MATCH: MatchFn<fn(&TokenTree) -> Option<FullToken>, AstParseErr> = MatchFn::new_with_err(
            |tt| match tt {
                TokenTree::Token(ft) if matches!(ft.kind, Token::Ident(_)) => Some(ft.clone()),
                _ => None,
            },
            || AstParseErr::ExpectedIdent
        );

        let ident = match parser.peek_slice(2).as_ref() {
            [Tk(token![#]), Tk(Token::Ident(_))] => {
                let (ident, span) = parser.spanned(|parser| {
                    parser.expect(token![#]).unwrap(); // should be unreachable
                    let Token::Ident(ident) = parser.expect(IDENT_MATCH).unwrap().kind else {
                        unreachable!()
                    };

                    String::from("#") + &ident
                });

                Some(ast::Ident { ident, span })
            },
            [Tk(Token::Ident(_)), ..] => {
                let FullToken { kind: Token::Ident(ident), span } = parser.expect(IDENT_MATCH).unwrap() else {
                    unreachable!()
                };

                Some(ast::Ident { ident, span })
            },
            _ => None
        };

        Ok(ident)
    }
}
impl Parseable for ast::Ident {
    type Err = FullAstParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        parser.try_parse()?
            .ok_or(parser.cursor.error(AstParseErr::ExpectedIdent))
    }
}

impl TryParseable for ast::StrLiteral {
    type Err = Infallible;

    fn try_read(parser: &mut Parser<'_>) -> Result<Option<Self>, Self::Err> {
        let lit = parser.match_(MatchFn::new(|tt| {
            if let TokenTree::Token(FullToken { kind: Token::Str(literal), span }) = tt {
                let literal = literal.clone();
                let span = *span;

                Some(ast::StrLiteral { literal, span })
            } else {
                None
            }
        }));

        Ok(lit)
    }
}
impl Parseable for ast::StrLiteral {
    type Err = FullAstParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        parser.try_parse()?
            .ok_or(parser.cursor.error(AstParseErr::ExpectedLiteral))
    }
}

const REASG_TOKENS: [Token; 2] = [token![let], token![const]];
impl TryParseable for ast::ReasgType {
    type Err = Infallible;

    fn try_read(parser: &mut Parser<'_>) -> Result<Option<Self>, Self::Err> {

        let rt = match parser.match_(REASG_TOKENS).map(|t| t.kind) {
            Some(token![let]) => Some(ast::ReasgType::Let),
            Some(token![const]) => Some(ast::ReasgType::Const),
            _ => None
        };

        Ok(rt)
    }
}
impl Parseable for ast::ReasgType {
    type Err = FullGonErr<ParseErr>;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        parser.try_parse()?
            .ok_or_else(|| {
                parser.cursor.error(REASG_TOKENS.fail_err())
            })
    }
}

impl TryParseable for ast::Stmt {
    type Err = FullAstParseErr;

    fn try_read(parser: &mut Parser<'_>) -> Result<Option<Self>, Self::Err> {
        use TTKind::Token as Tk;

        let st = match parser.peek() {
            Some(tt) => match tt {
                Tk(token![let] | token![const]) => Some(ast::Stmt::Decl(parser.parse()?)),
                Tk(token![return])   => Some(ast::Stmt::Return(parser.parse()?)),
                Tk(token![break])    => Some(ast::Stmt::Break(parser.parse()?)),
                Tk(token![continue]) => Some(ast::Stmt::Continue(parser.parse()?)),
                Tk(token![throw])    => Some(ast::Stmt::Throw(parser.parse()?)),
                Tk(token![fun])      => Some(ast::Stmt::FunDecl(parser.parse()?)),
                Tk(token![extern])   => Some(ast::Stmt::ExternFunDecl(parser.parse()?)),
                Tk(token![class])    => Some(ast::Stmt::Class(parser.parse()?)),
                Tk(token![fit])      => Some(ast::Stmt::FitClassDecl(parser.parse()?)),
                Tk(token![import])   => {
                    match parser.peek_nth(1) {
                        Some(Tk(Token::Ident(id))) if id == "intrinsic" => Some(ast::Stmt::ImportIntrinsic(parser.parse()?)),
                        _ => Some(ast::Stmt::Import(parser.parse()?))
                    }
                }
                Tk(token![global])   => Some(ast::Stmt::IGlobal(parser.parse()?)),
                _                    => parser.try_parse()?.map(ast::Stmt::Expr),
            }
            _ => None
        };

        Ok(st)
    }
}

impl TryParseable for ast::Type {
    type Err = FullAstParseErr;

    fn try_read(parser: &mut Parser<'_>) -> Result<Option<Self>, Self::Err> {
        let (result, span) = parser.try_spanned(|parser| {
            let Some(ident) = parser.try_parse()? else { return AstParseResult::Ok(None) };

            let args = if let Some(group) = parser.match_(delim!["[]"]) {
                let mut content = Parser::new(group);
                
                content.parse_tuple(token![,])?
                    .assert_non_empty(|| content.cursor.error(AstParseErr::ExpectedType))?
                    .assert_closed(content,
                        || token![,].fail_err().into(),
                        || AstParseErr::ExpectedType
                    )?
                    .values()
                    .collect()
            } else {
                vec![]
            };

            Ok(Some((ident, args)))
        })?;

        Ok(result.map(|(ident, params)| ast::Type { ident, params, span }))
    }
}
impl Parseable for ast::Type {
    type Err = FullAstParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        parser.try_parse()?
            .ok_or_else(|| parser.cursor.error(AstParseErr::ExpectedType))
    }
}

impl Parseable for ast::Decl {
    type Err = FullAstParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let ((rt, pat, ty, val), span) = parser.try_spanned(|parser| {
            let rt = parser.parse()?;
            let pat = parser.parse()?;

            let ty = match parser.match_(token![:]).is_some() {
                true => Some(parser.parse()?),
                false => None
            };

            parser.expect(token![=])?;
            let expr = parser.parse()?;
            
            AstParseResult::Ok((rt, pat, ty, expr))
        })?;

        Ok(ast::Decl { rt, pat, ty, val, span })
    }
}
impl TryParseable for ast::DeclPat {
    type Err = FullAstParseErr;

    fn try_read(parser: &mut Parser<'_>) -> Result<Option<Self>, Self::Err> {
        let Some(peek) = parser.peek_tree() else {
            return Ok(None)
        };

        let pat = match peek {
            TokenTree::Group(group) if group.delimiter == delim!["[]"] => {
                let span = group.span();

                let mut content = Parser::new(parser.expect(delim!["[]"])?);
                let values = content.parse_tuple(token![,])?
                    .assert_closed(content,
                        || expected_tokens![,],
                        || AstParseErr::ExpectedPattern,
                    )?
                    .values()
                    .collect();
                
                ast::DeclPat::List { values, span }
            },
            TokenTree::Token(token) => match &token.kind {
                token![..] => {
                    let (item, span) = parser.try_spanned(|parser| {
                        parser.expect(token![..])?;
                        parser.try_parse()
                    })?;
    
                    ast::DeclPat::Spread { inner: item.map(Box::new), span }
                },
                token![mut] | token![#] | Token::Ident(_) => {
                    let ((mt, ident), span) = parser.try_spanned(|parser| {
                        let mt = match parser.match_(token![mut]).is_some() {
                            true  => ast::MutType::Mut,
                            false => ast::MutType::Immut
                        };
    
                        AstParseResult::Ok((mt, parser.parse()?))
                    })?;
    
                    ast::DeclPat::Unit(ast::DeclUnit { ident, mt, span })
                },
                _ => return Ok(None)
            }
            _ => return Ok(None)
        };

        Ok(Some(pat))
    }
}
impl Parseable for ast::DeclPat {
    type Err = FullAstParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        parser.try_parse()?
            .ok_or_else(|| parser.cursor.error(AstParseErr::ExpectedPattern))
    }
}

impl Parseable for ast::Return {
    type Err = FullAstParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let (me, span) = parser.try_spanned(|parser| {
            parser.expect(token![return])?;
            parser.try_parse()
        })?;

        Ok(Self { expr: me, span })
    }
}
impl Parseable for ast::Break {
    type Err = FullGonErr<ParseErr>;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        parser.try_spanned(|parser| parser.expect(token![break]))
            .map(|(_, span)| Self { span })
    }
}
impl Parseable for ast::Continue {
    type Err = FullGonErr<ParseErr>;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        parser.try_spanned(|parser| parser.expect(token![continue]))
            .map(|(_, span)| Self { span })
    }
}
impl Parseable for ast::Throw {
    type Err = FullAstParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let (message, span) = parser.try_spanned(|parser| {
            parser.expect(token![throw])?;
            parser.parse()
        })?;

        Ok(Self { message, span })
    }
}
impl TryParseable for ast::Param {
    type Err = FullAstParseErr;

    fn try_read(parser: &mut Parser<'_>) -> Result<Option<Self>, Self::Err> {
        let (result, span) = parser.try_spanned(|parser| {
            let maybe_rt = parser.try_parse()?;
            let maybe_mt = parser.match_(token![mut]).map(|_| ast::MutType::Mut);
            let maybe_ident = parser.try_parse()?;

            // the param checked so far is fully empty and probably not an actual param:
            if maybe_rt.is_none() && maybe_mt.is_none() && maybe_ident.is_none() {
                return AstParseResult::Ok(None);
            }

            let rt = maybe_rt.unwrap_or_default();
            let mt = maybe_mt.unwrap_or_default();
            let ident = maybe_ident.ok_or_else(|| parser.cursor.error(AstParseErr::ExpectedIdent))?;

            let ty = match parser.match_(token![:]) {
                Some(_) => Some(parser.parse()?),
                None    => None,
            };

            Ok(Some((rt, mt, ident, ty)))
        })?;
        
        Ok(result.map(|(rt, mt, ident, ty)| ast::Param { rt, mt, ident, ty, span }))
    }
}
impl Parseable for ast::Param {
    type Err = FullAstParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
            parser.try_parse()?
                .ok_or_else(|| parser.cursor.error(AstParseErr::ExpectedParam))
    }
}
impl Parseable for ast::FunSignature {
    type Err = FullAstParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let ((ident, generics, params, ret), span) = parser.try_spanned(|parser| {
            parser.expect(token![fun])?;
            let ident = parser.parse()?;

            let generics = if let Some(group) = parser.match_(delim!["[]"]) {
                let mut content = Parser::new(group);
                
                content.parse_tuple(token![,])?
                    .assert_non_empty(|| content.cursor.error(AstParseErr::ExpectedIdent))?
                    .assert_closed(content, 
                        || expected_tokens![,],
                        || AstParseErr::ExpectedParam,
                    )?
                    .values()
                    .collect()
            } else {
                vec![]
            };

            let mut content = Parser::new(parser.expect(delim!["()"])?);
            let params = content.parse_tuple(token![,])?
                .assert_closed(content,
                    || expected_tokens![,],
                    || AstParseErr::ExpectedParam,
                )?
                .values()
                .collect();

            let ret = match parser.match_(token![->]) {
                Some(_) => Some(parser.parse()?),
                None    => None,
            };

            AstParseResult::Ok((ident, generics, params, ret))
        })?;

        Ok(Self { ident, generics, params, varargs: false, ret, span })
    }
}
impl Parseable for ast::FunDecl {
    type Err = FullAstParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let ((sig, block), span) = parser.try_spanned(|parser| {
            let sig = parser.parse()?;
            let block = parser.parse()?;
            AstParseResult::Ok((sig, block))
        })?;

        Ok(Self { sig, block, span })
    }
}

// TODO: combine MethodSignature & FunSignature
impl Parseable for ast::MethodSignature {
    type Err = FullAstParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let ((referent, is_static, name, generic_params, params, ret), span) = parser.try_spanned(|parser| {
            parser.expect(token![fun])?;
            let referent = parser.try_parse()?;
            let is_static = match parser.expect([token![::], token![.]])?.kind {
                token![::] => true,
                token![.]  => false,
                _ => unreachable!()
            };
            let name = parser.parse()?;

            let generics = if let Some(group) = parser.match_(delim!["[]"]) {
                let mut content = Parser::new(group);
                
                content.parse_tuple(token![,])?
                    .assert_non_empty(|| content.cursor.error(AstParseErr::ExpectedIdent))?
                    .assert_closed(content, 
                        || expected_tokens![,],
                        || AstParseErr::ExpectedParam,
                    )?
                    .values()
                    .collect()
            } else {
                vec![]
            };

            let mut content = Parser::new(parser.expect(delim!["()"])?);
            let params = content.parse_tuple(token![,])?
                .assert_closed(content,
                    || expected_tokens![,],
                    || AstParseErr::ExpectedParam,
                )?
                .values()
                .collect();
            
            let ret = match parser.match_(token![->]) {
                Some(_) => Some(parser.parse()?),
                None    => None,
            };

            AstParseResult::Ok((referent, is_static, name, generics, params, ret))
        })?;

        Ok(Self { referent, is_static, name, generic_params, params, ret, span  })
    }
}
impl Parseable for ast::MethodDecl {
    type Err = FullAstParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let ((sig, block), span) = parser.try_spanned(|parser| {
            let sig = parser.parse()?;
            let block = parser.parse()?;
            AstParseResult::Ok((sig, block))
        })?;

        Ok(Self { sig, block, span })
    }
}
impl Parseable for ast::ExternFunDecl {
    type Err = FullAstParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let (sig, span) = parser.try_spanned(|parser| {
            parser.expect(token![extern])?;
            parser.parse()
        })?;

        Ok(Self { sig, span })
    }
}
impl TryParseable for ast::FieldDecl {
    type Err = FullAstParseErr;

    fn try_read(parser: &mut Parser<'_>) -> Result<Option<Self>, Self::Err> {
        let (result, span) = parser.try_spanned(|parser| {
            let maybe_rt = parser.try_parse()?;
            let maybe_mt = parser.match_(token![mut]).map(|_| ast::MutType::Mut);
            let maybe_ident = parser.try_parse()?;

            // the param checked so far is fully empty and probably not an actual param:
            if maybe_rt.is_none() && maybe_mt.is_none() && maybe_ident.is_none() {
                return AstParseResult::Ok(None);
            }

            let rt = maybe_rt.unwrap_or_default();
            let mt = maybe_mt.unwrap_or_default();
            let ident = maybe_ident.ok_or_else(|| parser.cursor.error(AstParseErr::ExpectedIdent))?;

            parser.expect(token![:])?;
            let ty = parser.parse()?;

            Ok(Some((rt, mt, ident, ty)))
        })?;
        
        Ok(result.map(|(rt, mt, ident, ty)| ast::FieldDecl { rt, mt, ident, ty, span }))
    }
}
impl Parseable for ast::FieldDecl {
    type Err = FullAstParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        parser.try_parse()?
            .ok_or_else(|| parser.cursor.error(AstParseErr::ExpectedIdent))
    }
}

impl Parseable for ast::Class {
    type Err = FullAstParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let ((ident, generic_params, fields, methods), span) = parser.try_spanned(|parser| {
            parser.expect(token![class])?;
            let ident = parser.parse()?;
            let generic_params = match parser.match_(delim!["[]"]) {
                Some(group) => {
                    let mut content = Parser::new(group);
                    content.parse_tuple(token![,])?
                        .assert_non_empty(|| content.cursor.error(AstParseErr::ExpectedIdent))?
                        .assert_closed(content,
                            || expected_tokens![,],
                            || AstParseErr::ExpectedIdent,
                        )?
                        .values()
                        .collect()
                },
                None => vec![]
            };

            let group = parser.expect(delim!["{}"])?;
            let mut block_parser = Parser::new(group);

            let fields = block_parser.parse_tuple(token![,])?
                .values()
                .collect();
            
            let mut methods = vec![];
            while let Some(TTKind::Token(token![fun])) = block_parser.peek() {
                methods.push(block_parser.parse()?);
            }

            block_parser.close_or_else(|| group.delimiter.right().fail_err())?;
            
            AstParseResult::Ok((ident, generic_params, fields, methods))
        })?;

        Ok(Self { ident, generic_params, fields, methods, span })
    }
}
impl Parseable for ast::Import {
    type Err = FullAstParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let (path, span) = parser.try_spanned(|parser| {
            parser.expect(token![import])?;
            parser.parse()
        })?;

        Ok(Self { path, span })
    }
}
impl Parseable for ast::ImportIntrinsic {
    type Err = FullAstParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let (_, span) = parser.try_spanned(|parser| {
            parser.expect(token![import])?;
            parser.expect( Token::Ident(String::from("intrinsic")) )
        })?;

        Ok(Self { span })
    }
}
impl Parseable for ast::IGlobal {
    type Err = FullAstParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let ((ident, value), span) = parser.try_spanned(|parser| {
            parser.expect(token![global])?;
            let ident = parser.parse()?;
            parser.expect(token![=])?;
            let value = parser.parse()?;

            AstParseResult::Ok((ident, value))
        })?;

        Ok(ast::IGlobal { ident, value, span })
    }
}
impl Parseable for ast::FitClassDecl {
    type Err = FullAstParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let ((ty, methods), span) = parser.try_spanned(|parser| {
            parser.expect(token![fit])?;
            parser.expect(token![class])?;
            let ty = parser.parse()?;
            
            let group = parser.expect(delim!["{}"])?;
            let mut content = Parser::new(group);

            let mut methods = vec![];
            while let Some(TTKind::Token(token![fun])) = content.peek() {
                methods.push(content.parse()?);
            }
            content.close_or_else(|| group.delimiter.right().fail_err())?;
            
            AstParseResult::Ok((ty, methods))
        })?;

        Ok(Self { ty, methods, span })
    }
}
impl Parseable for ast::Block {
    type Err = FullAstParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let group = parser.expect(delim!["{}"])?;

        let mut content = Parser::new(group);
        let stmts = parse_stmts_closed(&mut content)?;
        content.close_or_else(|| group.delimiter.right().fail_err())?;

        Ok(ast::Block { stmts, span: group.span() })
    }
}
impl Parseable for ast::StaticPath {
    type Err = FullAstParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let ((ty, attr), span) = parser.try_spanned(|parser| {
            let ty = parser.parse()?;
            parser.expect(token![::])?;
            let attr = parser.parse()?;

            AstParseResult::Ok((ty, attr))
        })?;

        Ok(Self { ty, attr, span })
    }
}

#[cfg(test)]
mod tests {
    use std::ops;

    use crate::err::{FullGonErr, GonErr};
    use crate::lexer::tokenize;
    use crate::ast::*;
    use crate::parser::parse;
    use crate::span::Cursor;

    use super::*;

    macro_rules! stmts {
        ($($e:expr),*) => {
            vec![$(Stmt::from($e)),*]
        }
    }
    
    macro_rules! binop {
        ($op:ident, $span:expr, $left:expr, $right:expr) => {
            Expr::from(BinaryOp {
                op: op::Binary::$op,
                left: Box::new(Expr::from($left)),
                right: Box::new(Expr::from($right)),
                span: Span::new($span)
            })
        };
    }

    fn ident(ident: &str, span: ops::Range<Cursor>) -> Ident {
        Ident {
            ident: ident.to_string(),
            span: Span::new(span),
        }
    }

    macro_rules! literal {
        ($ident:ident($e:expr), $span:expr) => {
            Expr::Literal(Literal {
                kind: LitKind::$ident($e),
                span: Span::new($span)
            })
        };
        ($l:literal, $span:expr) => {
            Expr::Literal(Literal {
                kind: LitKind::Str(String::from($l)),
                span: Span::new($span)
            })
        }
    }

    macro_rules! decl_unit {
        (mut $id:literal, $uspan:expr, $ispan:expr) => { decl_unit!(@ Mut   $id, $uspan, $ispan) };
        (    $id:literal, $uspan:expr, $ispan:expr) => { decl_unit!(@ Immut $id, $uspan, $ispan) };
        (mut $id:literal, $uspan:expr)              => { decl_unit!(@ Mut   $id, $uspan, $uspan) };
        (    $id:literal, $uspan:expr)              => { decl_unit!(@ Immut $id, $uspan, $uspan) };
        
        (@ $mt:ident $id:literal, $uspan:expr, $ispan:expr) => {
            DeclUnit {
                ident: ident($id, $ispan),
                mt: MutType::$mt,
                span: Span::new($uspan)
            }
        }
    }
    /// Unwrap the result (or print error if not possible).
    fn unwrap_fe<T>(result: Result<T, FullGonErr<impl GonErr>>, input: &str) -> T {
        match result {
            Ok(t) => t,
            Err(e) => panic!("{}", e.full_msg(input)),
        }
    }
    /// Lex and parse string.
    fn parse_str<P: Parseable<Err = FullAstParseErr>>(s: &str) -> AstParseResult<P> {
        let stream = unwrap_fe(tokenize(s), s);
        parse(&stream)
    }
    /// Assert that the string provided parses into the program.
    #[allow(unused)]
    fn assert_parse<P>(input: &str, r: P) 
        where P: Parseable<Err = FullAstParseErr> + PartialEq<P> + std::fmt::Debug
    {
        assert_eq!(unwrap_fe(parse_str::<P>(input), input), r)
    }
    /// Assert that the string provided errors with the given error when parsed.
    #[allow(unused)]
    fn assert_parse_fail<E>(input: &str, result: E) 
        where E: std::fmt::Debug,
            FullAstParseErr: PartialEq<E>
    {
        match parse_str::<Program>(input) {
            Ok(t)  => panic!("Lexing resulted in value: {t:?}"),
            Err(e) => assert_eq!(e, result)
        }
    }

    #[test]
    fn bin_op_test() {
        assert_parse("2 + 3", binop! {
            Add, 0..5,
            literal!(Int(2), 0..1),
            literal!(Int(3), 4..5)
        });

        assert_parse("2 + 3 * 4", binop! {
            Add, 0..9,
            literal!(Int(2), 0..1),
            binop! {
                Mul, 4..9,
                literal!(Int(3), 4..5),
                literal!(Int(4), 8..9)
            }
        });
    }

    #[test]
    fn block_test() {
        assert_parse("{}", Block {
            stmts: stmts![],
            span: Span::new(0..2)
        });

        assert_parse("{{}}", Block {
            stmts: stmts![
                Expr::from(Block {
                    stmts: stmts![],
                    span: Span::new(1..3)
                })
            ],
            span: Span::new(0..4)
        });
    }

    /// Tests if statements.
    #[test]
    fn if_else_test() {
        assert_parse("
            if true {
                // :)
            }
        ", If {
            conditionals: vec![
                (literal!(Bool(true), 16..20), Block { stmts: stmts![], span: Span::new(21..58)})
            ],
            last: None,
            span: Span::new(13..58)
        });
        
        assert_parse("
            if true {
                // :)
            } else {
                // :(
            }
        ", If { 
            conditionals: vec![
                (literal!(Bool(true), 16..20), Block { stmts: stmts![], span: Span::new(21..58)})
            ],
            last: Some(Block { stmts: stmts![], span: Span::new(64..101)}),
            span: Span::new(13..101)
        });

        assert_parse("
            if true {
                // :)
            } else if condition {
                // :|
            } else {
                // :(
            }
        ", If {
            conditionals: vec![
                (literal!(Bool(true), 16..20),           Block { stmts: stmts![], span: Span::new(21..58)}),
                (Expr::from(ident("condition", 67..76)), Block { stmts: stmts![], span: Span::new(77..114)})
            ],
            last: Some(Block { stmts: stmts![], span: Span::new(120..157) }),
            span: Span::new(13..157)
        });

        assert_parse("
            if true {
                // :)
            } else if condition {
                // :|
            } else if condition {
                // :|
            } else if condition {
                // :|
            } else if condition {
                // :|
            } else {
                // :(
            }
        ", If {
            conditionals: vec![
                (literal!(Bool(true), 16..20),             Block { stmts: stmts![], span: Span::new(21..58)}),
                (Expr::from(ident("condition", 67..76)),   Block { stmts: stmts![], span: Span::new(77..114)}),
                (Expr::from(ident("condition", 123..132)), Block { stmts: stmts![], span: Span::new(133..170)}),
                (Expr::from(ident("condition", 179..188)), Block { stmts: stmts![], span: Span::new(189..226)}),
                (Expr::from(ident("condition", 235..244)), Block { stmts: stmts![], span: Span::new(245..282)}),
            ],
            last: Some(Block { stmts: stmts![], span: Span::new(288..325)}),
            span: Span::new(13..325)
        });
    }

    /// Tests while and for loop as well as 
    /// declarations, function calls, conditionals, assignment, ranges, and literals.
    #[test]
    fn loop_test() {
        // barebones
        assert_parse("while true {}", While {
            condition: Box::new(literal!(Bool(true), 6..10)),
            block: Block {stmts: stmts![], span: Span::new(11..13)},
            span: Span::new(0..13)
        });

        assert_parse("for i in it {}", For {
            ident: Ident { ident: String::from("i"), span: Span::new(4..5) },
            iterator: Box::new(Expr::from(ident("it", 9..11))),
            block: Block {stmts: stmts![], span: Span::new(12..14)},
            span: Span::new(0..14)
        });

        // full examples
        assert_parse("
            let i = 0;
            while i < 10 {
                print(i);
                i = i + 1;
            }
        ", 
        Program {
            stmts: stmts![
                Decl {
                    rt: ReasgType::Let, 
                    pat: Pat::Unit(decl_unit!("i", 17..18)),
                    ty: None, 
                    val: literal!(Int(0), 21..22),
                    span: Span::new(13..22)
                },
                Expr::from(While {
                    condition: Box::new(Expr::from(Comparison {
                        left: Box::new(Expr::from(ident("i", 42..43))), 
                        rights: vec![(op::Cmp::Lt, literal!(Int(10), 46..48))],
                        span: Span::new(42..48)
                    })),
                    block: Block {
                        stmts: stmts![
                            Expr::from(Call {
                                funct: Box::new(Expr::from(ident("print", 67..72))),
                                generic_args: vec![],
                                args: vec![Expr::from(ident("i", 73..74))],
                                span: Span::new(67..75)
                            }),
                            Expr::from(Assign {
                                target: AsgPat::Unit(AsgUnit::Ident(ident("i", 93..94))),
                                value: Box::new(binop! {
                                    Add, 97..102,
                                    ident("i", 97..98),
                                    literal!(Int(1), 101..102)
                                }),
                                span: Span::new(93..102)
                            })
                        ],
                        span: Span::new(49..117)
                    },
                    span: Span::new(36..117)
                })
            ],
            span: Span::new(13..117)
        });

        assert_parse("for i in 1..10 { print(i); }", For {
            ident: ident("i", 4..5),
            iterator: Box::new(Expr::from(Range {
                left: Box::new(literal!(Int(1), 9..10)), 
                right: Box::new(literal!(Int(10), 12..14)), 
                step: None,
                span: Span::new(9..14)
            })),
            block: Block {
                stmts: stmts![
                    Expr::from(Call {
                        funct: Box::new(Expr::from(ident("print", 17..22))),
                        generic_args: vec![],
                        args: vec![Expr::from(ident("i", 23..24))],
                        span: Span::new(17..25)
                    })
                ],
                span: Span::new(15..28)
            },
            span: Span::new(0..28)
        });
    }

    #[test]
    fn semicolon_test() {
        assert_parse_fail("2 2", expected_tokens![;]);

        assert_parse("if cond {}", Program {
            stmts: stmts![
                Expr::from(If {
                    conditionals: vec![
                        (Expr::from(ident("cond", 3..7)), Block { stmts: stmts![], span: Span::new(8..10)})
                    ],
                    last: None,
                    span: Span::new(0..10)
                })
            ],
            span: Span::new(0..10)
        });
        assert_parse("if cond {};", Program {
            stmts: stmts![
                Expr::from(If {
                    conditionals: vec![
                        (Expr::from(ident("cond", 3..7)), Block { stmts: stmts![], span: Span::new(8..10)})
                    ],
                    last: None,
                    span: Span::new(0..10)
                })
            ],
            span: Span::new(0..11)
        });

        assert_parse("
            let a = 1;
            let b = 2;
            let c = 3;
            if cond {
                let d = 5;
            }
        ", Program {
            stmts: stmts![
                Decl { 
                    rt: ReasgType::Let, 
                    pat: Pat::Unit(decl_unit!("a", 17..18)), 
                    ty: None, 
                    val: literal!(Int(1), 21..22),
                    span: Span::new(13..22)
                },
                Decl { 
                    rt: ReasgType::Let, 
                    pat: Pat::Unit(decl_unit!("b", 40..41)), 
                    ty: None, 
                    val: literal!(Int(2), 44..45),
                    span: Span::new(36..45)
                },
                Decl { 
                    rt: ReasgType::Let, 
                    pat: Pat::Unit(decl_unit!("c", 63..64)), 
                    ty: None, 
                    val: literal!(Int(3), 67..68),
                    span: Span::new(59..68)
                },
                Expr::from(If {
                    conditionals: vec![(
                        Expr::from(ident("cond", 85..89)),
                        Block {
                            stmts: stmts![
                                Decl { 
                                    rt: ReasgType::Let, 
                                    pat: Pat::Unit(decl_unit!("d", 112..113)),
                                    ty: None, 
                                    val: literal!(Int(5), 116..117),
                                    span: Span::new(108..117)
                                }
                            ],
                            span: Span::new(90..132)
                        }
                    )],
                    last: None,
                    span: Span::new(82..132)
                })
            ],
            span: Span::new(13..132)
        });
    }

    #[test]
    fn type_test() {
        assert_parse(
            "int",
            Type {
                ident:  ident("int", 0..3),
                params: vec![],
                span:   Span::new(0..3),
            }
        );

        assert_parse(
            "dict[K, V]",
            Type {
                ident: ident("dict", 0..4),
                params: vec![
                    Type {
                        ident: ident("K", 5..6), 
                        params: vec![],
                        span: Span::new(5..6)
                    },
                    Type {
                        ident: ident("V", 8..9), 
                        params: vec![],
                        span: Span::new(8..9)
                    },
                ],
                span: Span::new(0..10),
            }
        );

        assert_parse(
            "dict[list[list[int]], str]",
            Type {
                ident: ident("dict", 0..4),
                params: vec![
                    Type {
                        ident: ident("list", 5..9),
                        params: vec![
                            Type {
                                ident: ident("list", 10..14),
                                params: vec![
                                    Type {
                                        ident: ident("int", 15..18),
                                        params: vec![],
                                        span: Span::new(15..18)
                                    }
                                ],
                                span: Span::new(10..19)
                            }
                        ],
                        span: Span::new(5..20)
                    },
                    Type {
                        ident: ident("str", 22..25),
                        params: vec![],
                        span: Span::new(22..25)
                    }
                ],
                span: Span::new(0..26)
            }
        );
    }

    #[test]
    fn unary_ops_test() {
        assert_parse("+3", Expr::from(UnaryOps {
            ops: vec![token![+].try_into().unwrap()],
            expr: Box::new(literal!(Int(3), 1..2)),
            span: Span::new(0..2),
        }));
        
        assert_parse("+++++++3", Expr::from(UnaryOps {
            ops: [token![+].try_into().unwrap()].repeat(7),
            expr: Box::new(literal!(Int(3), 7..8)),
            span: Span::new(0..8),
        }));

        assert_parse("+-+-+-+-3", Expr::from(UnaryOps {
            ops: [token![+].try_into().unwrap(), token![-].try_into().unwrap()].repeat(4),
            expr: Box::new(literal!(Int(3), 8..9)),
            span: Span::new(0..9),
        }));

        assert_parse("!+-+-+-+-3", Expr::from(UnaryOps {
            ops: vec![
                    token![!].try_into().unwrap(), 
                    token![+].try_into().unwrap(), 
                    token![-].try_into().unwrap(), 
                    token![+].try_into().unwrap(), 
                    token![-].try_into().unwrap(), 
                    token![+].try_into().unwrap(), 
                    token![-].try_into().unwrap(), 
                    token![+].try_into().unwrap(), 
                    token![-].try_into().unwrap()
                ],
            expr: Box::new(literal!(Int(3), 9..10)),
            span: Span::new(0..10),
        }));

        assert_parse("+(+2)", Expr::from(UnaryOps {
            ops: [token![+].try_into().unwrap()].repeat(2),
            expr: Box::new(literal!(Int(2), 3..4)),
            span: Span::new(0..5),
        }));
    }

    #[test]
    fn decl_test() {
        assert_parse("
            let a       = 0;
            let mut b   = 1;
            const c     = 2;
            const mut d = 3;
        ", Program {
            stmts: stmts![
                Decl {
                    rt: ReasgType::Let,
                    pat: Pat::Unit(decl_unit!("a", 17..18)),
                    ty: None,
                    val: literal!(Int(0), 27..28),
                    span: Span::new(13..28)
                },
                Decl { 
                    rt: ReasgType::Let, 
                    pat: Pat::Unit(decl_unit!(mut "b", 46..51, 50..51)),
                    ty: None,
                    val: literal!(Int(1), 56..57),
                    span: Span::new(42..57)
                },
                Decl { 
                    rt: ReasgType::Const, 
                    pat: Pat::Unit(decl_unit!("c", 77..78)),
                    ty: None,
                    val: literal!(Int(2), 85..86),
                    span: Span::new(71..86)
                },
                Decl { 
                    rt: ReasgType::Const, 
                    pat: Pat::Unit(decl_unit!(mut "d", 106..111, 110..111)),
                    ty: None,
                    val: literal!(Int(3), 114..115),
                    span: Span::new(100..115)
                }
            ],
            span: Span::new(13..116),
        });
    }
}