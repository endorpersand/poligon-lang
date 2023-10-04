use crate::ast::*;
use crate::span::Spanned;
use crate::token;

use super::{Parseable, FullParseErr, Parser2, ParseErr, ParseResult};

impl Parseable for Option<Expr> {
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        parser.try_parse::<Expr15>()
            .map(|m_expr| m_expr.map(Into::into))
    }
}
impl Parseable for Expr {
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
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

            impl From<$i> for crate::ast::Expr {
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
            
                fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
                    todo!()
                }
            }
        )*
    }
}

parse_expr_enums! {
    Expr0:  { Ident, Block, Literal, ListLiteral, SetLiteral, DictLiteral, ClassLiteral, If, While, For },
    Expr1:  { Expr0, Path, StaticPath, Call, Index },
    Expr2:  { Expr1, IDeref },
    Expr3:  { Expr2, UnaryOps },
    Expr4:  { Expr3, BinaryOp },
    Expr5:  { Expr4, BinaryOp },
    Expr6:  { Expr5, BinaryOp },
    Expr7:  { Expr6, BinaryOp },
    Expr8:  { Expr7, BinaryOp },
    Expr9:  { Expr8, BinaryOp },
    Expr10: { Expr9, Range },
    Expr11: { Expr10, Spread },
    Expr12: { Expr11, Comparison },
    Expr13: { Expr12, BinaryOp },
    Expr14: { Expr13, BinaryOp },
    Expr15: { Expr14, Assign }
}

impl Parseable for Option<Expr15> {
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
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
            let far_span = far_pat.span().append(*far_eq.span()).append(*rhs.span());
            let far_asg = Assign {
                target: far_pat,
                value: Box::new(rhs),
                span: far_span,
            };

            let folded_asg = lhs.into_iter().rfold(far_asg, |acc, (pat, eq)| {
                let span = pat.span().append(*eq.span()).append(*acc.span());
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
impl Parseable for Option<Expr14> {
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        todo!()
    }
}