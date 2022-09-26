use std::collections::VecDeque;

use crate::lexer::token::{Token, token};

pub mod tree;

pub fn parse(tokens: impl IntoIterator<Item=Token>) -> Result<tree::Program, ParseErr> {
    Parser::new(tokens.into_iter().collect()).parse()
}

struct Parser {
    tokens: VecDeque<Token>
}

#[derive(Debug, PartialEq)]
pub enum ParseErr {
    ExpectedTokens(Vec<Token>),
    ExpectedIdent,
    ExpectedExpr,
    CannotParseNumeric,
    ExpectedBlock
}
type ParseResult<T> = Result<T, ParseErr>;

macro_rules! left_assoc_op {
    ($n:ident = $ds:ident (($($op:tt),+) $_:ident)*;) => {
        fn $n(&mut self) -> ParseResult<Option<tree::Expr>> {
            if let Some(mut e) = self.$ds()? {
                while let Some(op) = self.match_n(&[$(token![$op]),+]) {
                    e = tree::Expr::BinaryOp(tree::BinaryOp {
                        op,
                        left: Box::new(e),
                        right: self.$ds()?
                            .map(Box::new)
                            .ok_or(ParseErr::ExpectedExpr)?
                    });
                }

                Ok(Some(e))
            } else {
                Ok(None)
            }
        }
    };
    ($n:ident = $ds:ident ($op:tt $_:ident)*;) => {
        left_assoc_op!($n = $ds (($op) $_)*;);
    };
}

macro_rules! left_assoc_rules {
    ($($n:ident = $ds:ident (($($op:tt),+) $_:ident)*;)+) => {
        $(
            left_assoc_op!($n = $ds (($($op),+) $_)*;);
        )+
    };
    ($($n:ident = $ds:ident ($op:tt $_:ident)*;)+) => {
        $(
            left_assoc_op!($n = $ds (($op) $_)*;);
        )+
    };
}

impl Parser {
    fn new(tokens: VecDeque<Token>) -> Self {
        Self { tokens }
    }

    fn expect(&mut self, one_of: &[Token]) -> ParseResult<Token> {
        if let Some(t) = self.tokens.pop_front() {
            if one_of.contains(&t) {
                return Ok(t)
            }
        }
    
        Err(ParseErr::ExpectedTokens(one_of.into()))
    }

    fn expect1(&mut self, u: Token) -> ParseResult<()> {
        if let Some(t) = self.tokens.pop_front() {
            if t == u {
                return Ok(())
            }
        }
    
        Err(ParseErr::ExpectedTokens(vec![u]))
    }

    fn match_n(&mut self, one_of: &[Token]) -> Option<Token> {
        match self.tokens.get(0) {
            Some(t) if one_of.contains(t) => self.tokens.pop_front(),
            _ => None,
        }
    }

    fn match1(&mut self, u: Token) -> bool {
        match self.tokens.get(0) {
            Some(t) if t == &u => self.tokens.pop_front(),
            _ => None,
        }.is_some()
    }

    fn parse(mut self) -> ParseResult<tree::Program> {
        let mut program = vec![];

        // this consumes the entire token list
        while !self.tokens.is_empty() {
            // if statement exists, add it to program
            // if statement is nothing, ignore it
            let mst = self.match_stmt()?;
            if let Some(st) = mst { program.push(st); }

            self.expect1(Token::LineSep)?;
        }
        
        Ok(program)
    }

    fn expect_block(&mut self) -> ParseResult<tree::Program> {
        todo!()
    }

    fn match_stmt(&mut self) -> ParseResult<Option<tree::Stmt>> {
        let st = match self.tokens.get(0) {
            Some(token![let]) | Some(token![const]) => Some(self.expect_decl()?).map(tree::Stmt::Decl),
            Some(token![return])   => Some(self.expect_return()?),
            Some(token![break])    => Some(self.expect_break()?),
            Some(token![continue]) => Some(self.expect_cont()?),
            Some(token![fun])      => Some(self.expect_fun()?).map(tree::Stmt::FunDecl),
            Some(_)                => self.match_expr()?.map(tree::Stmt::Expr),
            None                   => None
        };

        Ok(st)
    }

    fn expect_decl(&mut self) -> ParseResult<tree::Decl> {
        let rt = match self.tokens.pop_front() {
            Some(token![let])   => tree::ReasnType::Let,
            Some(token![const]) => tree::ReasnType::Const,
            _ => unreachable!()
        };
        
        let mt = if self.match1(token![mut]) {
            tree::MutType::Mut
        } else {
            tree::MutType::Immut
        };

        let ident = self.expect_ident()?;

        let ty = if self.match1(token![:]) {
            Some(self.expect_type()?)
        } else {
            None
        };

        self.expect1(token![=])?;
        let expr = self.expect_expr()?;

        Ok(tree::Decl {
            rt,
            mt,
            var: ident,
            ty,
            val: expr
        })
    }
    fn expect_return(&mut self) -> ParseResult<tree::Stmt> {
        self.expect1(token![return])?;
        let e = self.expect_expr()?;

        Ok(tree::Stmt::Return(e))
    }
    fn expect_break(&mut self) -> ParseResult<tree::Stmt> {
        self.expect1(token![break])?;

        Ok(tree::Stmt::Break)
    }
    fn expect_cont(&mut self) -> ParseResult<tree::Stmt> {
        self.expect1(token![continue])?;

        Ok(tree::Stmt::Continue)
    }
    fn expect_fun(&mut self) -> ParseResult<tree::FunDecl> {
        self.expect1(token![fun])?;

        let ident = self.expect_ident();

        self.expect1(token!["("])?;
        todo!("(param),*");
        self.expect1(token![")"])?;

        let ret = if self.match1(token![->]) {
            Some(self.expect_type()?)
        } else {
            None
        };

        todo!()
    }

    fn expect_ident(&mut self) -> ParseResult<String> {
        if let Some(Token::Ident(s)) = self.tokens.pop_front() {
            Ok(s)
        } else {
            Err(ParseErr::ExpectedIdent)
        }
    }
    fn expect_type(&mut self) -> ParseResult<tree::Type> {
        todo!()
    }

    fn match_expr(&mut self) -> ParseResult<Option<tree::Expr>> {
        self.match_asg()
    }
    fn expect_expr(&mut self) -> ParseResult<tree::Expr> {
        self.match_expr()?.ok_or(ParseErr::ExpectedExpr)
    }

    fn match_asg(&mut self) -> ParseResult<Option<tree::Expr>> {
        // a = b = c = d = e = [expr]

        let mut vars = vec![];
        // TODO: asg ops
        while let Some(token![=]) = self.tokens.get(1) {
            vars.push(self.expect_ident()?);
            self.tokens.pop_front(); // this is a =
        }

        // no more =, so this must be logical or
        let expr = self.match_lor()?;
        
        if vars.is_empty() {
            // if vars is empty this is not an assignment expression
            Ok(expr)
        } else {
            let expr = expr.ok_or(ParseErr::ExpectedExpr)?;

            let asg = vars.into_iter()
                .rfold(expr, |e, v| tree::Expr::Assignment(v, Box::new(e)));
            
            Ok(Some(asg))
        }
    }

    // pattern follows for all left_assoc_rules!
    // fn match_lor(&mut self) -> ParseResult<Option<tree::Expr>> {
    //     if let Some(mut e) = self.match_land()? {
    //         while let Some(op) = self.match_n(token![||]) {
    //             e = tree::Expr::BinaryOp(tree::BinaryOp {
    //                 op,
    //                 left: Box::new(e),
    //                 right: self.match_land()?
    //                     .map(Box::new)
    //                     .ok_or(ParseErr::ExpectedExpr)?
    //             });
    //         }

    //         Ok(Some(e))
    //     } else {
    //         Ok(None)
    //     }
    // }

    left_assoc_rules! { 
        match_lor  = match_land  ( || match_land )*;
        match_land = match_cmp   ( && match_cmp  )*;
        // cmp
        // spread
        // range
        match_bor  = match_bxor  ( | match_bxor  )* ;
        match_bxor = match_band  ( ^ match_band  )* ;
        match_band = match_shift ( & match_shift )* ;
    }

    fn match_cmp(&mut self) -> ParseResult<Option<tree::Expr>> {
        let me = self.match_spread()?;

        if let Some(mut e) = me {
            let cmp_ops = [
                token![<], token![>], 
                token![<=], token![>=], 
                token![==], token![!=]
            ];
            // check if there's a comparison here
            if let Some(t) = self.match_n(&cmp_ops) {
                let rexpr = self.match_spread()?
                    .ok_or(ParseErr::ExpectedExpr)?;
                let right = (t, Box::new(rexpr));

                // check if there's a second comparison here
                let extra = if let Some(t) = self.match_n(&cmp_ops) {
                    let eexpr = self.match_spread()?
                    .ok_or(ParseErr::ExpectedExpr)?;
                    Some((t, Box::new(eexpr)))
                } else {
                    None
                };

                e = tree::Expr::Comparison { 
                    left: Box::new(e), 
                    right, 
                    extra 
                }
            }

            Ok(Some(e))
        } else {
            Ok(None)
        }
    }

    fn match_spread(&mut self) -> ParseResult<Option<tree::Expr>> {
        let is_spread = self.match1(token![..]);
        
        if let Some(mut e) = self.match_range()? {
            if is_spread {
                e = tree::Expr::UnaryOp(tree::UnaryOp { 
                    op: token![..], expr: Box::new(e)
                });
            }

            Ok(Some(e))
        } else {
            Ok(None)
        }
    }

    fn match_range(&mut self) -> ParseResult<Option<tree::Expr>> {
        if let Some(mut e) = self.match_bor()? {
            if self.match1(token![..]) {
                let right = self.match_bor()?.ok_or(ParseErr::ExpectedExpr)?;

                let step = if self.match1(token![step]) {
                    let sexpr = self.match_bor()?.ok_or(ParseErr::ExpectedExpr)?;
                    Some(sexpr)
                } else {
                    None
                };

                e = tree::Expr::Range {
                    left: Box::new(e), 
                    right: Box::new(right), 
                    step: step.map(Box::new)
                };
            }

            Ok(Some(e))
        } else {
            Ok(None)
        }
    }

    left_assoc_rules! {
        match_shift  = match_addsub ( ( << , >> ) match_addsub )* ;
        match_addsub = match_muldiv ( ( + , - ) match_muldiv )* ;
        match_muldiv = match_unary ( ( * , / , % ) match_unary )* ;
    }

    fn match_unary(&mut self) -> ParseResult<Option<tree::Expr>> {
        let unary_ops = [token![!], token![~], token![-], token![+]];

        let mut ops = vec![];
        while let Some(t) = self.match_n(&unary_ops) {
            ops.push(t);
        }

        let mut e = self.match_call()?.ok_or(ParseErr::ExpectedExpr)?;
        e = ops.into_iter()
            .rfold(e, |expr, op| tree::Expr::UnaryOp(tree::UnaryOp {
                op, expr: Box::new(expr)
            }));

        Ok(Some(e))
    }

    fn match_call(&mut self) -> ParseResult<Option<tree::Expr>> {
        if let Some(e) = self.match_path()? {
            if self.match1(token!["("]) {
                todo!("(expr),*");
                self.expect1(token![")"])?;
            }

            Ok(Some(e))
        } else {
            Ok(None)
        }
    }

    fn match_path(&mut self) -> ParseResult<Option<tree::Expr>> {
        if let Some(mut e) = self.match_unit()? {
            let mut segments = vec![];

            while let Some(t) = self.match_n(&[token![.], token![::]]) {
                segments.push((t, self.expect_ident()?));
            }

            e = segments.into_iter()
                .rfold(e, |expr, (t, ident)| match t {
                    token![.]  => tree::Expr::Attr(tree::Attr {
                        obj: Box::new(expr), attr: ident
                    }),
                    token![::] => tree::Expr::StaticAttr(tree::Attr {
                        obj: Box::new(expr), attr: ident
                    }),
                    _ => unreachable!()
                });

            Ok(Some(e))
        } else {
            Ok(None)
        }
    }

    fn match_unit(&mut self) -> ParseResult<Option<tree::Expr>> {
        if let Some(t) = self.tokens.get(0) {
            let unit = match t {
                Token::Ident(_)   => self.expect_ident().map(tree::Expr::Ident)?,
                Token::Numeric(_) | Token::Str(_) | Token::Char(_) => self.expect_literal()? ,
                token!["["]       => self.expect_list()?,
                token!["{"]       => self.expect_block().map(tree::Expr::Block)?,
                token![if]        => self.expect_if()?,
                token![while]     => self.expect_while()?,
                token![for]       => self.expect_for()?,
                token!["("] => {
                    self.expect1(token!["("])?;
                    let e = self.expect_expr()?;
                    self.expect1(token![")"])?;
            
                    e
                }
                _ => return Ok(None)
            };

            Ok(Some(unit))
        } else {
            Ok(None)
        }
    }

    fn expect_literal(&mut self) -> ParseResult<tree::Expr> {
        let lit = match self.tokens.pop_front() {
            Some(Token::Numeric(s)) => tree::Literal::from_numeric(&s)
                .ok_or(ParseErr::CannotParseNumeric)?,
            Some(Token::Str(s)) => tree::Literal::Str(s),
            Some(Token::Char(c)) => tree::Literal::Char(c),
            _ => unreachable!()
        };

        Ok(tree::Expr::Literal(lit))
    }
    fn expect_list(&mut self) -> ParseResult<tree::Expr> {
        self.expect1(token!["["])?;
        todo!("(expr),*");
        self.expect1(token!["]"])?;
    }
    fn expect_set(&mut self) -> ParseResult<tree::Expr> {
        todo!();
    }
    fn expect_dict(&mut self) -> ParseResult<tree::Expr> {
        todo!();
    }
    fn expect_if(&mut self) -> ParseResult<tree::Expr> {
        self.expect1(token![if])?;

        let mut pairs = vec![(self.expect_expr()?, self.expect_block()?)];
        let mut end = None;

        while self.match1(token![else]) {
            match self.tokens.get(0) {
                Some(&token![if]) => {
                    self.expect1(token![if])?;
                    pairs.push((self.expect_expr()?, self.expect_block()?));
                },
                Some(&token!["{"]) => {
                    let block = tree::Else::Block(self.expect_block()?);
                    end = Some(Box::new(block));
                    break;
                },
                _ => Err(ParseErr::ExpectedBlock)?
            }
        }

        let mut expr = {
            let (cond, blockt) = pairs.pop().unwrap();

            tree::If {
                condition: Box::new(cond),
                if_true: blockt,
                if_false: end
            }
        };
        expr = pairs.into_iter()
            .rfold(expr, |inner_if, (cond, blockt)| tree::If {
                condition: Box::new(cond),
                if_true: blockt,
                if_false: Some(Box::new(tree::Else::If(inner_if)))
            });
        
        Ok(tree::Expr::If(expr))
    }
    fn expect_while(&mut self) -> ParseResult<tree::Expr> {
        self.expect1(token![while])?;
        let condition = self.expect_expr()?;
        let block = self.expect_block()?;
        
        Ok(tree::Expr::While {
            condition: Box::new(condition), block
        })
    }
    fn expect_for(&mut self) -> ParseResult<tree::Expr> {
        self.expect1(token![for])?;
        let ident = self.expect_ident()?;
        self.expect1(token![in])?;
        let iterator = self.expect_expr()?;
        let block = self.expect_block()?;
        
        Ok(tree::Expr::For {
            ident, iterator: Box::new(iterator), block
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::token::token;
    use crate::lexer::tokenize;

    use super::{tree, parse, ParseErr};

    macro_rules! assert_parse {
        ($s:expr => $r:expr) => {
            assert_eq!(parse_str(&$s), Ok($r))
        }
    }

    fn parse_str(s: &str) -> Result<tree::Program, ParseErr> {
        parse(tokenize(s).unwrap())
    }

    #[test]
    fn expression_test() {
        assert_parse!("2 + 3;" => vec![
            tree::Stmt::Expr(tree::Expr::BinaryOp(tree::BinaryOp {
                op: token![+], 
                left: Box::new(tree::Expr::Literal(tree::Literal::Int(2))), 
                right: Box::new(tree::Expr::Literal(tree::Literal::Int(3)))
            }))
        ]);

        assert_parse!("2 + 3 * 4;" => vec![
            tree::Stmt::Expr(tree::Expr::BinaryOp(tree::BinaryOp {
                op: token![+], 
                left: Box::new(tree::Expr::Literal(tree::Literal::Int(2))), 
                right: Box::new(tree::Expr::BinaryOp(tree::BinaryOp {
                    op: token![*], 
                    left: Box::new(tree::Expr::Literal(tree::Literal::Int(3))), 
                    right: Box::new(tree::Expr::Literal(tree::Literal::Int(4)))
                }))
            }))
        ]);
    }

    #[test]
    fn if_else_test() {
        assert_parse!("if true {
            // :)
        }" => vec![
            tree::Stmt::Expr(tree::Expr::If(tree::If { 
                condition: Box::new(tree::Expr::Ident("true".to_string())), 
                if_true: vec![], 
                if_false: None
            }))
        ]);

        assert_parse!("if true {
            // :)
        } else {
            // :(
        }" => vec![
            tree::Stmt::Expr(tree::Expr::If(tree::If { 
                condition: Box::new(tree::Expr::Ident("true".to_string())), 
                if_true: vec![], 
                if_false: Some(Box::new(tree::Else::Block(vec![])))
            }))
        ]);

        assert_parse!("if true {
            // :)
        } else if condition {
            // :|
        } else {
            // :(
        }" => vec![
            tree::Stmt::Expr(tree::Expr::If(tree::If { 
                condition: Box::new(tree::Expr::Ident("true".to_string())), 
                if_true: vec![], 
                if_false: Some(Box::new(tree::Else::If(tree::If { 
                    condition: Box::new(tree::Expr::Ident("condition".to_string())), 
                    if_true: vec![], 
                    if_false: Some(Box::new(tree::Else::Block(vec![])))
                })))
            }))
        ]);

        assert_parse!("if true {
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
        }" => vec![
            tree::Stmt::Expr(tree::Expr::If(tree::If { 
                condition: Box::new(tree::Expr::Ident("true".to_string())), 
                if_true: vec![], 
                if_false: Some(Box::new(tree::Else::If(tree::If { 
                    condition: Box::new(tree::Expr::Ident("condition".to_string())), 
                    if_true: vec![], 
                    if_false: Some(Box::new(tree::Else::If(tree::If { 
                        condition: Box::new(tree::Expr::Ident("condition".to_string())), 
                        if_true: vec![], 
                        if_false: Some(Box::new(tree::Else::If(tree::If { 
                            condition: Box::new(tree::Expr::Ident("condition".to_string())), 
                            if_true: vec![], 
                            if_false: Some(Box::new(tree::Else::If(tree::If { 
                                condition: Box::new(tree::Expr::Ident("condition".to_string())), 
                                if_true: vec![], 
                                if_false: Some(Box::new(tree::Else::Block(vec![])))
                            })))
                        })))
                    })))
                })))
            }))
        ]);
    }
}