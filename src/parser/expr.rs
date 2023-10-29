use crate::{ast::*, delim};
use crate::lexer::token::{Token, FullToken, TTKind};
use crate::span::Spanned;
use crate::token;

use super::{Parseable, FullParseErr, Parser, ParseErr, ParseResult, TokenPattern, Entry};

impl Parseable for Option<Expr> {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        parser.try_parse::<Expr15>()
            .map(|m_expr| m_expr.map(Into::into))
    }
}
impl Parseable for Expr {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        parser.try_parse()?
            .ok_or_else(|| parser.cursor.error(ParseErr::ExpectedExpr))
    }
}

macro_rules! parse_expr_enums {
    ($($i:ident: {$($v:ident),*}),*) => {
        $(
            enum $i {
                $($v($v)),*
            }

            impl From<$i> for $crate::ast::Expr {
                fn from(value: $i) -> Self {
                    match value {
                        $(
                            $i::$v(e) => Self::from(e)
                        ),*
                    }
                }
            }

            impl Parseable for $i {
                type Err = FullParseErr;
            
                fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
                    parser.try_parse()?
                        .ok_or_else(|| parser.cursor.error(ParseErr::ExpectedExpr))
                }
            }
        )*
    }
}

// 1.  Unit expressions (ident, block, parenthesized expressions, literals, if statements, loops)
// 2.  Calling, indexing, paths
// 3.  Deref
// 4.  Other unary operators (+, -, !, ~)
// 5.  Mult., division, mod (*, /, %)
// 6.  Add, sub (+, -)
// 7.  Shifts (<<, >>)
// 8.  Bitwise and (&)
// 9.  Bitwise xor (^)
// 10. Bitwise or (|)
// 11. Range (a..b)
// 12. Spread (..a)
// 13. Comparisons (<, >, <=, >=, ==, !=)
// 14. Logical and (&&)
// 15. Logical or (||)
// 16. Assignment

parse_expr_enums! {
    Identoid: { Ident, SetLiteral, DictLiteral, ClassLiteral},
    Expr0:    { Identoid, Block, Literal, ListLiteral, If, While, For, Expr },
    Expr1:    { Expr0, Path, StaticPath, Call, Index },
    Expr2:    { Expr1, IDeref },
    Expr3:    { Expr2, UnaryOps },
    Expr4:    { Expr3, BinaryOp },
    Expr5:    { Expr4, BinaryOp },
    Expr6:    { Expr5, BinaryOp },
    Expr7:    { Expr6, BinaryOp },
    Expr8:    { Expr7, BinaryOp },
    Expr9:    { Expr8, BinaryOp },
    Expr10:   { Expr9, Range },
    Expr11:   { Expr10, Spread },
    Expr12:   { Expr11, Comparison },
    Expr13:   { Expr12, BinaryOp },
    Expr14:   { Expr13, BinaryOp },
    Expr15:   { Expr14, Assign }
}

impl Parseable for Option<Expr15> {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let mut lhs = vec![];
        let Some(mut rhs) = parser.try_parse::<Expr14>()? else { return ParseResult::Ok(None) };

        while let Some(eq) = parser.match_(token![=]) {
            let new_rhs = parser.parse::<Expr14>()?;
            
            let new_lhs_expr: Expr = std::mem::replace(&mut rhs, new_rhs).into();
            let new_lhs = AsgPat::try_from(new_lhs_expr)
                .map_err(crate::err::FullGonErr::cast_err)?;
            lhs.push((new_lhs, eq));
        }

        let expr = if let Some((far_pat, far_eq)) = lhs.pop() {
            let rhs = Expr::from(rhs);
            let far_span = far_pat.span() + far_eq.span() + rhs.span();
            let far_asg = Assign {
                target: far_pat,
                value: Box::new(rhs),
                span: far_span,
            };

            let folded_asg = lhs.into_iter().rfold(far_asg, |acc, (pat, eq)| {
                let span = pat.span() + eq.span() + acc.span();
                Assign {
                    target: pat,
                    value: Box::new(Expr::Assign(acc)),
                    span,
                }
            });

            Expr15::Assign(folded_asg)
        } else {
            Expr15::Expr14(rhs)

        };

        Ok(Some(expr))
    }
}

macro_rules! left_assoc_ops {
    ($($E:ident: $EM1:ident, $o:expr),*) => {
        $(
            impl Parseable for Option<$E> {
                type Err = FullParseErr;
            
                fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
                    let Some(mut lhs) = parser.try_parse::<$EM1>()?.map($E::$EM1) else {
                        return ParseResult::Ok(None)
                    };
                    
                    while let Some(op) = parser.match_($o) {
                        let new_lhs = Expr::from(lhs);
                        let new_rhs = Expr::from(parser.parse::<$EM1>()?);
                        let span = new_lhs.span() + op.span() + new_rhs.span();
                        
                        lhs = $E::BinaryOp(BinaryOp {
                            op:    op.kind.try_into().unwrap(),
                            left:  Box::new(new_lhs),
                            right: Box::new(new_rhs),
                            span,
                        })
                    }
            
                    Ok(Some(lhs))
                }
            }
        )*
    }
}

left_assoc_ops! {
    Expr14: Expr13, token![||],
    Expr13: Expr12, token![&&],
    Expr9:  Expr8,  token![|],
    Expr8:  Expr7,  token![^],
    Expr7:  Expr6,  token![&],
    Expr6:  Expr5,  [token![<<], token![>>]],
    Expr5:  Expr4,  [token![+], token![-]],
    Expr4:  Expr3,  [token![*], token![/], token![%]]
}
impl Parseable for Option<Expr12> {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        const CMP_OPS: &[Token] = &[
            token![<=], token![<],
            token![==], token![!=],
            token![>=], token![>],
        ];

        let Some(lhs) = parser.try_parse::<Expr11>()? else { return Ok(None) };
        let mut rights = vec![];
        let mut span = None;

        while let Some(op) = parser.match_(CMP_OPS) {
            let rhs: Expr   = parser.parse::<Expr11>()?.into();

            let spanref = span.get_or_insert(op.span());
            *spanref += op.span();
            *spanref += rhs.span();

            let op: op::Cmp = op.kind.try_into().unwrap();

            rights.push((op, rhs));
        }

        if !rights.is_empty() {
            let left = Box::new(Expr::from(lhs));
            let span = left.span() + span.unwrap();
            
            Ok(Some(Expr12::Comparison(Comparison { left, rights, span })))
        } else {
            Ok(Some(Expr12::Expr11(lhs)))
        }
    }
}
impl Parseable for Option<Expr11> {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let m_expr = if let Some(TTKind::Token(token![..])) = parser.peek() {
            Some(Expr11::Spread(parser.parse()?))
        } else {
            parser.try_parse()?
                .map(Expr11::Expr10)
        };

        Ok(m_expr)
    }
}
impl Parseable for Spread {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let (e, span) = parser.try_spanned(|parser| {
            parser.expect(token![..])?;
            parser.try_parse()
        })?;

        Ok(Self { expr: e.map(Box::new), span })
    }
}

impl Parseable for Option<Expr10> {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let Some(lhs) = parser.try_parse::<Expr9>()? else { return Ok(None) };
        if let Some(op) = parser.match_(token![..]) {
            let lhs = Expr::from(lhs);
            let rhs = Expr::from(parser.parse::<Expr9>()?);

            let mut span = lhs.span() + op.span() + rhs.span();
            let step = match parser.match_(token![step]) {
                Some(t) => {
                    span += t.span();
                    Some(parser.parse::<Expr9>()?.into())
                },
                None => None,
            };

            Ok(Some(Expr10::Range(Range {
                left:  Box::new(lhs),
                right: Box::new(rhs),
                step:  step.map(Box::new),
                span,
            })))
        } else {
            Ok(Some(Expr10::Expr9(lhs)))
        }
    }
}

const UNARY_OPS: &[Token] = &[ token![+], token![-], token![~], token![!] ];
impl Parseable for Option<Expr3> {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let m_expr = match parser.peek() {
            Some(TTKind::Token(tok)) if UNARY_OPS.contains(tok) => {
                Some(Expr3::UnaryOps(parser.parse()?))
            },
            _ => {
                parser.try_parse()?
                    .map(Expr3::Expr2)
            }
        };

        Ok(m_expr)
    }
}
impl Parseable for UnaryOps {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let ((mut ops, inner), span) = parser.try_spanned(|parser| {
            let mut ops = vec![];
            while let Some(op) = parser.match_(UNARY_OPS) {
                ops.push(op.kind.try_into().unwrap());
            }

            if ops.is_empty() {
                return Err(parser.cursor.error(ParseErr::ExpectedTokens(UNARY_OPS.to_vec())));
            }
            let inner = parser.parse::<Expr2>()?;

            ParseResult::Ok((ops, inner))
        })?;

        let expr = Expr::from(inner);
        if let Expr::UnaryOps(uops) = expr {
            let Self { ops: inner_ops, expr, span: _ } = uops;
            ops.extend(inner_ops);

            Ok(Self { ops, expr, span })
        } else {
            Ok(Self { ops, expr: Box::new(expr), span })
        }
    }
}

impl Parseable for Option<Expr2> {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let m_expr = if let Some(TTKind::Token(token![*])) = parser.peek() {
            Some(Expr2::IDeref(parser.parse()?))
        } else {
            parser.try_parse()?
                .map(Expr2::Expr1)
        };

        Ok(m_expr)
    }
}
impl Parseable for IDeref {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let mut deref_spans = vec![];
        while let Some(op) = parser.match_(UNARY_OPS) {
            deref_spans.push(op.span());
        }

        let Some(far_span) = deref_spans.pop() else {
            return Err(parser.cursor.error(token![*].fail_err()));
        };

        let far_deref = IDeref {
            reference: Box::new(parser.parse::<Expr1>()?.into()),
            span: far_span,
        };
        let deref = deref_spans.into_iter().rfold(far_deref, |d, op_span| {
            let span = op_span + d.span();
            IDeref {
                reference: Box::new(d.into()), span
            }
        });

        Ok(deref)
    }
}

impl Parseable for Option<Expr1> {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        use super::TTKind::{Token as Tk, Group as Gr};

        let mut base = match parser.peek_slice(4).as_ref() {
            // if a type-like structure appears before ::, try parsing a type
            | [Tk(token![#]), Tk(Token::Ident(_)), Gr(delim!["[]"]), Tk(token![::])]
            | [Tk(Token::Ident(_)), Gr(delim!["[]"]), Tk(token![::]), ..]
            | [Tk(token![#]), Tk(Token::Ident(_)), Tk(token![::])]
            | [Tk(Token::Ident(_)), Tk(token![::]), ..]
            => {
                let ((ty, attr), span) = parser.try_spanned(|parser| {
                    let ty = parser.parse()?;
                    parser.expect(token![::])?;
                    let attr = parser.parse()?;

                    ParseResult::Ok((ty, attr))
                })?;

                Expr1::StaticPath(StaticPath { ty, attr, span })
            },
            // else, parse as an Expr0
            _ => {
                let Some(e0) = parser.try_parse()? else { return Ok(None) };
                Expr1::Expr0(e0)
            }
        };

        loop {
            match parser.peek_slice(3).as_ref() {
                // path access
                | [Tk(token![.]), Tk(token![#]), Tk(Token::Ident(_))]
                | [Tk(token![.]), Tk(Token::Ident(_)), ..]
                => {
                    let (ident, attr_span) = parser.try_spanned(|parser| {
                        parser.expect(token![.])?;
                        parser.parse()
                    })?;

                    if let Expr1::Path(Path { attrs, span, .. }) = &mut base {
                        attrs.push(ident);
                        *span += attr_span;
                    } else {
                        let obj = Box::new(Expr::from(base));
                        let span = obj.span() + attr_span;

                        base = Expr1::Path(Path {
                            obj,
                            attrs: vec![ident],
                            span,
                        })
                    }
                },
    
                // function call
                | [Tk(token![.]), Gr(delim!["()"]), ..]
                | [Tk(token![.]), Gr(delim!["[]"]), Gr(delim!["()"])]
                | [Gr(delim!["()"]), ..]
                | [Gr(delim!["[]"]), Gr(delim!["()"]), ..]
                => {
                    let ((generic_args, args), call_span) = parser.try_spanned(|parser| {
                        parser.match_(token![.]);
                        
                        let generics = if let Some(generics_group) = parser.match_(delim!["[]"]) {
                            let mut generics_content = Parser::new(generics_group);
                            
                            generics_content.parse_tuple(token![,])?
                                .assert_non_empty(|| ParseErr::ExpectedType)?
                                .assert_closed(generics_content, 
                                    || token![,].fail_err(),
                                    || ParseErr::ExpectedType
                                )?
                                .values()
                                .collect()
                        } else {
                            vec![]
                        };

                        let args_group = parser.expect(delim!["()"])?;
                        let mut args_content = Parser::new(args_group);
                        let tup = args_content.parse_tuple(token![,])?
                            .assert_closed(args_content,
                                || token![,].fail_err(),
                                || ParseErr::ExpectedParam
                            )?
                            .values()
                            .collect();

                        ParseResult::Ok((generics, tup))
                    })?;

                    let funct = Box::new(Expr::from(base));
                    let span = funct.span() + call_span;
                    base = Expr1::Call(Call { funct, generic_args, args, span })
                },
    
                // index
                | [Gr(delim!["[]"]), ..]
                => {
                    let group = parser.expect(delim!["[]"])?;
                    let mut content = Parser::new(group);
                    let index = content.parse()?;
                    content.close()?;

                    let expr = Box::new(Expr::from(base));
                    let index = Box::new(index);
                    let span = expr.span() + group.span();
                    base = Expr1::Index(Index { expr, index, span });
                },
                _ => break
            };
        }

        Ok(Some(base))
    }
}
impl Parseable for Option<Expr0> {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let Some(tt) = parser.peek() else { return Ok(None) };

        let expr = match tt {
            TTKind::Group(delimiter) => match delimiter {
                delim!["{}"] => Expr0::Block(parser.parse()?),
                delim!["[]"] => Expr0::ListLiteral(parser.parse()?),
                delim!["()"] => {
                    let group = parser.expect(delim!["()"])?;
                    let mut content = Parser::new(group);
                    let e = content.parse()?;
                    content.close()?;

                    Expr0::Expr(e)
                }
            },
            TTKind::Token(token) => match token {
                token![#] | Token::Ident(_) => Expr0::Identoid(parser.parse()?),
                Token::Numeric(_) | Token::Str(_) | Token::Char(_) | token![true] | token![false] => Expr0::Literal(parser.parse()?),
                token![if]    => Expr0::If(parser.parse()?),
                token![while] => Expr0::While(parser.parse()?),
                token![for]   => Expr0::For(parser.parse()?),
                _ => { return Ok(None) }
            }
        };
            // Ident, Block, Literal, ListLiteral, *SetLiteral, *DictLiteral, *ClassLiteral, If, While, For
        // };

        Ok(Some(expr))
    }
}
impl Parseable for Option<Identoid> {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        use super::TTKind::{Token as Tk, Group as Gr};

        let identoid = match parser.peek_slice(4).as_ref() {
            | [Tk(token![#]), Tk(Token::Ident(_)), Gr(delim!["[]"]), Tk(token![#])]
            | [Tk(token![#]), Tk(Token::Ident(_)), Tk(token![#]), ..]
            | [Tk(Token::Ident(_)), Gr(delim!["[]"]), Tk(token![#]), ..]
            | [Tk(Token::Ident(_)), Tk(token![#]), ..]
            => {
                Identoid::ClassLiteral(parser.parse()?)
            },
            _ => {
                let Some(ident) = parser.try_parse::<Ident>()? else { return Ok(None) };

                match (ident.ident.as_str(), parser.peek()) {
                    ("set",  Some(Gr(delim!["{}"]))) => {
                        let group = parser.expect(delim!["{}"])?;
                        let mut content = Parser::new(group);

                        let values = content.parse_tuple(token![,])?
                            .assert_closed(content, 
                                || ParseErr::ExpectedTokens(vec![token![,]]), 
                                || ParseErr::ExpectedExpr,
                            )?
                            .values()
                            .collect();

                        let span = ident.span + group.span();
                        Identoid::SetLiteral(SetLiteral { values, span })
                    }
                    ("dict", Some(Gr(delim!["{}"]))) => {
                        let group = parser.expect(delim!["{}"])?;
                        let mut content = Parser::new(group);

                        let entries = content.parse_tuple(token![,])?
                            .assert_closed(content,
                                || ParseErr::ExpectedTokens(vec![token![,]]),
                                || ParseErr::ExpectedExpr
                            )?
                            .values()
                            .map(|Entry { key, val, span: _ } | (key, val))
                            .collect();

                        let span = ident.span + group.span();
                        Identoid::DictLiteral(DictLiteral { entries, span })
                    }
                    _ => Identoid::Ident(ident)
                }
            }
        };

        Ok(Some(identoid))
    }
}
impl Parseable for ClassLiteral {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let ((ty, entries), span) = parser.try_spanned(|parser| {
            let ty = parser.parse()?;
            parser.expect(token![#])?;

            let group = parser.expect(delim!["{}"])?;
            let mut content = Parser::new(group);

            let entries = content.parse_tuple(token![,])?
                .assert_closed(content,
                    || ParseErr::ExpectedTokens(vec![token![,]]),
                    || ParseErr::ExpectedExpr
                )?
                .values()
                .map(|Entry { key, val, span: _ }| (key, val))
                .collect();

            ParseResult::Ok((ty, entries))
        })?;

        Ok(Self { ty, entries, span })
    }
}

impl Parseable for Literal {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let Some(FullToken { kind: tok_kind, span }) = parser.next() else {
            return Err(parser.cursor.error(ParseErr::ExpectedLiteral));
        };
        
        let lit_kind = match tok_kind {
            Token::Numeric(n) => {
                LitKind::from_numeric(&n)
                    .ok_or_else(|| parser.cursor.error(ParseErr::CannotParseNumeric))?
            }
            Token::Str(s)  => LitKind::Str(s),
            Token::Char(c) => LitKind::Char(c),
            token![true]   => LitKind::Bool(true),
            token![false]  => LitKind::Bool(false),
            _ => { return Err(parser.cursor.error(ParseErr::ExpectedLiteral)) }
        };
        Ok(Self { kind: lit_kind, span })
    }
}
impl Parseable for ListLiteral {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let group = parser.expect(delim!["[]"])?;
        let mut content = Parser::new(group);

        let values = content.parse_tuple(token![,])?
            .assert_closed(content, 
                || ParseErr::ExpectedTokens(vec![token![,]]),
                || ParseErr::ExpectedExpr
            )?
            .values()
            .collect();

        Ok(Self { values, span: group.span() })
    }
}
impl Parseable for If {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let if_span = parser.expect(token![if])?.span;

        let ((conditionals, last), rest_span) = parser.try_spanned(|parser| {
            let mut conditionals = vec![];
            let mut last = None;
    
            conditionals.push((parser.parse()?, parser.parse()?));
            while parser.match_(token![else]).is_some() {
                if parser.match_(token![if]).is_some() {
                    conditionals.push((parser.parse()?, parser.parse()?));
                } else {
                    last.replace(parser.parse()?);
                    break;
                }
            }

            ParseResult::Ok((conditionals, last))
        })?;

        let span = if_span + rest_span;
        Ok(If { conditionals, last, span })
    }
}
impl Parseable for While {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let ((condition, block), span) = parser.try_spanned(|parser| {
            parser.expect(token![while])?;
            let condition = parser.parse()?;
            let block = parser.parse()?;

            ParseResult::Ok((condition, block))
        })?;

        Ok(While {
            condition: Box::new(condition), 
            block, 
            span
        })
    }
}
impl Parseable for For {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let ((ident, iterator, block), span) = parser.try_spanned(|parser| {
            parser.expect(token![for])?;
            let ident = parser.parse()?;
            parser.expect(token![in])?;
            let iterator = parser.parse()?;
            let block = parser.parse()?;

            ParseResult::Ok((ident, iterator, block))
        })?;

        Ok(For {
            ident, 
            iterator: Box::new(iterator), 
            block,
            span 
        })
    }
}