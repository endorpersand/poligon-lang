//! Converts sequences of tokens to a parseable program tree.
//! 
//! TODO! more doc

use std::collections::VecDeque;
use std::rc::Rc;

use crate::GonErr;
use crate::lexer::token::{Token, token};
use crate::tree::{self, PatErr};

pub fn parse(tokens: impl IntoIterator<Item=Token>) -> ParseResult<tree::Program> {
    Parser::new(tokens).parse()
}
pub fn parse_repl<T>(mut tokens: T) -> ParseResult<tree::Program> 
    where T: IntoIterator<Item=Token> + Extend<Token>
{
    tokens.extend(std::iter::once(token![;]));
    Parser::new(tokens).parse()
}
pub struct Parser {
    tokens: VecDeque<Token>
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseErr {
    ExpectedTokens(Vec<Token>),
    ExpectedIdent,
    ExpectedExpr,
    CannotParseNumeric,
    ExpectedBlock,
    ExpectedType,
    ExpectedPattern,
    AsgPatErr(PatErr)
}
impl GonErr for ParseErr {
    fn err_name(&self) -> &'static str {
        "syntax error"
    }

    fn message(&self) -> String {
        // TODO
        format!("{:?}", self)
    }
}
pub type ParseResult<T> = Result<T, ParseErr>;

macro_rules! left_assoc_op {
    ($n:ident = $ds:ident (($($op:tt),+) $_:ident)*;) => {
        fn $n(&mut self) -> ParseResult<Option<tree::Expr>> {
            if let Some(mut e) = self.$ds()? {
                while let Some(op) = self.match_n(&[$(token![$op]),+]) {
                    e = tree::Expr::BinaryOp(tree::BinaryOp {
                        op: op.try_into().unwrap(),
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
    fn new(tokens: impl IntoIterator<Item=Token>) -> Self {
        let tokens = tokens.into_iter()
            .filter(|t| !matches!(t, Token::Comment(_, _)))
            .collect();
        
        Self { tokens }
    }

    // General terminology:
    // "expect X": The next set of tokens must represent X, otherwise error.
    // "match X": If the next set of tokens represent X, consume those tokens. 
    //     Otherwise, do & return nothing.

    /// Expect that the next token is in the specified list of tokens.
    /// 
    /// Return the next token if it is a token in the list, 
    /// or error if the next token is not a token in the list.
    // fn expect(&mut self, one_of: &[Token]) -> ParseResult<Token> {
    //     if let Some(t) = self.tokens.pop_front() {
    //         if one_of.contains(&t) {
    //             return Ok(t)
    //         }
    //     }
    
    //     Err(ParseErr::ExpectedTokens(one_of.into()))
    // }

    /// Expect that the next token is in the specified token.
    /// 
    /// Error if the next token is not the specified token.
    fn expect1(&mut self, u: Token) -> ParseResult<()> {
        if let Some(t) = self.tokens.pop_front() {
            if t == u {
                return Ok(())
            }
        }
    
        Err(ParseErr::ExpectedTokens(vec![u]))
    }

    /// If the next token is in the specified list of tokens, 
    /// consume the token from input and return it.
    /// 
    /// Return None if it is not in the specified list of tokens.
    fn match_n(&mut self, one_of: &[Token]) -> Option<Token> {
        match self.tokens.get(0) {
            Some(t) if one_of.contains(t) => self.tokens.pop_front(),
            _ => None,
        }
    }

    /// Return whether the next token matches the specified token, 
    /// and consume the token from input if it does.
    fn match1(&mut self, u: Token) -> bool {
        match self.tokens.get(0) {
            Some(t) if t == &u => self.tokens.pop_front(),
            _ => None,
        }.is_some()
    }

    fn match_langle(&mut self) -> bool {
        // if the next token matches <, then done
        // also have to check for <<
        self.match1(token![<]) || {
            let is_match = matches!(self.tokens.get(0), Some(token![<<]));
            
            if is_match {
                self.tokens[0] = token![<];
            }

            is_match
        }
    }

    fn match_rangle(&mut self) -> bool {
        // if they match >, then done
        // also have to check for >>
        self.match1(token![>]) || {
            let is_match = matches!(self.tokens.get(0), Some(token![>>]));
            
            if is_match {
                self.tokens[0] = token![>];
            }

            is_match
        }
    }

    /// The parsing function.
    /// Takes a list of tokens and converts it into a parse tree.
    fn parse(mut self) -> ParseResult<tree::Program> {
        let program = self.expect_program()?;

        if self.tokens.is_empty() {
            Ok(program)
        } else {
            // there are more tokens left that couldn't be parsed as a program.
            // we have an issue.
            Err(ParseErr::ExpectedTokens(vec![token![;]]))
        }
    }

    /// Expect that the next tokens represent a program.
    /// 
    /// Return the program,
    /// or error if the tokens do not represent a program.
    fn expect_program(&mut self) -> ParseResult<tree::Program> {
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
        
        loop {
            let mst = self.match_stmt()?;

            // if statement exists, check if statement needs a semicolon and add to program list
            if let Some(st) = mst {
                match &st {
                    // it does not need a semi if it is a block, if, while, for
                    tree::Stmt::Expr(e) if matches!(e,
                        | tree::Expr::Block(_)
                        | tree::Expr::If(_)
                        | tree::Expr::While { condition: _, block: _ }
                        | tree::Expr::For { ident: _, iterator: _, block: _ }
                    ) => { self.match1(token![;]); },

                    // also for function declarations
                    tree::Stmt::FunDecl(_) => { self.match1(token![;]); },

                    // otherwise, it does
                    _ => self.expect1(token![;])?
                }

                stmts.push(st);
            } else if !self.match1(token![;]) {
                break; 
            }
        }

        Ok(tree::Program(stmts))
    }

    /// Expect that the next tokens represent a block.
    /// 
    /// Return the program encompassed by the block,
    /// or error if the tokens do not represent a block.
    fn expect_block(&mut self) -> ParseResult<tree::Program> {
        self.expect1(token!["{"])?;
        let p = self.expect_program()?;
        self.expect1(token!["}"])?;
        Ok(p)
    }

    /// Expect that the next tokens represent a statement.
    /// 
    /// Return the statement,
    /// or error if the tokens do not represent a statement.
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

    /// Expect that the next tokens represent a variable declaration.
    /// 
    /// Return the variable declaration,
    /// or error if the tokens do not represent a variable declaration.
    fn expect_decl(&mut self) -> ParseResult<tree::Decl> {
        let rt = match self.tokens.pop_front() {
            Some(token![let])   => tree::ReasgType::Let,
            Some(token![const]) => tree::ReasgType::Const,
            _ => unreachable!()
        };
        
        let pat = self.match_decl_pat()?
            .ok_or(ParseErr::ExpectedPattern)?;

        let ty = if self.match1(token![:]) {
            Some(self.expect_type()?)
        } else {
            None
        };

        self.expect1(token![=])?;
        let expr = self.expect_expr()?;

        Ok(tree::Decl {
            rt,
            pat,
            ty,
            val: expr
        })
    }

    fn match_decl_pat(&mut self) -> ParseResult<Option<tree::DeclPat>> {
        if let Some(t) = self.tokens.get(0) {
            let pat = match t {
                token!["["] => {
                    self.expect1(token!["["])?;
                    let tpl = self.expect_tuple_of(Parser::match_decl_pat)?;
                    self.expect1(token!["]"])?;

                    tree::DeclPat::List(tpl)
                },
                token![..] => {
                    self.expect1(token![..])?;
                    let item = self.match_decl_pat()?.map(Box::new);

                    tree::DeclPat::Spread(item)
                }
                token![mut] | Token::Ident(_) => {
                    let mt = if self.match1(token![mut]) {
                        tree::MutType::Mut
                    } else {
                        tree::MutType::Immut
                    };

                    let node = tree::DeclUnit::Ident(self.expect_ident()?, mt);
                    tree::DeclPat::Unit(node)
                },
                _ => return Ok(None)
            };

            Ok(Some(pat))
        } else {
            Ok(None)
        }
    }

    /// If the next tokens match a function parameter, 
    /// consume the tokens and return the parameter.
    /// 
    /// In the construction of a parameter, syntax errors are propagated.
    fn match_param(&mut self) -> ParseResult<Option<tree::Param>> {
        let mrt_token = self.match_n(&[token![let], token![const]]);
        let mut empty = mrt_token.is_none(); // if mrt is not empty, ensure empty is false
        
        let rt = match mrt_token {
            Some(token![let]) | None  => tree::ReasgType::Let,
            Some(token![const]) => tree::ReasgType::Const,
            _ => unreachable!()
        };
        
        let mt = if self.match1(token![mut]) {
            empty = false;
            tree::MutType::Mut
        } else {
            tree::MutType::Immut
        };

        // the param checked so far is fully empty and probably not an actual param:
        if empty && !matches!(self.tokens.get(0), Some(Token::Ident(_))) {
            return Ok(None);
        }

        let ident = self.expect_ident()?;
        let ty = if self.match1(token![:]) {
            Some(self.expect_type()?)
        } else {
            None
        };

        Ok(Some(tree::Param {
            rt,
            mt,
            ident,
            ty
        }))
    }

    /// Expect that the next tokens represent a return statement.
    /// 
    /// Return the statement,
    /// or error if the tokens do not represent a return statement.
    fn expect_return(&mut self) -> ParseResult<tree::Stmt> {
        self.expect1(token![return])?;
        let e = self.match_expr()?;

        Ok(tree::Stmt::Return(e))
    }
    /// Expect that the next tokens represent a break statement.
    /// 
    /// Return the statement,
    /// or error if the tokens do not represent a break statement.
    fn expect_break(&mut self) -> ParseResult<tree::Stmt> {
        self.expect1(token![break])?;

        Ok(tree::Stmt::Break)
    }
    /// Expect that the next tokens represent a continue statement.
    /// 
    /// Return the statement,
    /// or error if the tokens do not represent a continue statement.
    fn expect_cont(&mut self) -> ParseResult<tree::Stmt> {
        self.expect1(token![continue])?;

        Ok(tree::Stmt::Continue)
    }
    /// Expect that the next tokens represent a function declaration.
    /// 
    /// Return the function declaration,
    /// or error if the tokens do not represent a function declaration.
    fn expect_fun(&mut self) -> ParseResult<tree::FunDecl> {
        self.expect1(token![fun])?;

        let ident = self.expect_ident()?;

        self.expect1(token!["("])?;
        let params = self.expect_tuple_of(Parser::match_param)?;
        self.expect1(token![")"])?;

        let ret = if self.match1(token![->]) {
            Some(self.expect_type()?)
        } else {
            None
        };

        let block = self.expect_block()?;

        Ok(tree::FunDecl {
            ident,
            params,
            ret,
            block: Rc::new(block)
        })
    }

    /// Expect that the next token is an identifier token.
    /// 
    /// Return the identifier's String,
    /// or error if the token is not an identifier token.
    fn expect_ident(&mut self) -> ParseResult<String> {
        match self.tokens.pop_front() {
            Some(Token::Ident(s)) => Ok(s),
            _ => Err(ParseErr::ExpectedIdent)
        }
    }
    
    fn match_type(&mut self) -> ParseResult<Option<tree::Type>> {
        if matches!(self.tokens.get(0), Some(Token::Ident(_))) {
            let ident = self.expect_ident()?;

            let params = if self.match_langle() {
                let tpl = self.expect_tuple_of(Parser::match_type)?;
                
                if !self.match_rangle() { Err(ParseErr::ExpectedTokens(vec![token![>]]))? }

                if !tpl.is_empty() {
                    tpl
                } else {
                    // list<>
                    Err(ParseErr::ExpectedType)?
                }
            } else {
                vec![]
            };

            Ok(Some(tree::Type(ident, params)))
        } else {
            Ok(None)
        }
    }

    /// Expect that the next tokens represent a type expression.
    /// 
    /// Return the type expression,
    /// or error if the tokens do not represent a type expression.
    fn expect_type(&mut self) -> ParseResult<tree::Type> {
        self.match_type()?.ok_or(ParseErr::ExpectedType)
    }

    ///// EXPRESSION MATCHING
    /// Note that for expression matching,
    /// the labeled functions actually try to match an operation 
    /// OR any operation with a lower precedence.
    /// 
    /// f.e. match_addsub matches (+, -) but ALSO (*, /, %), and unary operations, etc.

    /// If the next tokens represent an expression, return the expression
    /// or return none if the tokens do not represent an expression.
    /// 
    /// Note that syntax errors are propagated through this function.
    fn match_expr(&mut self) -> ParseResult<Option<tree::Expr>> {
        self.match_asg()
    }
    /// Expect that the next tokens represent an expression
    /// 
    /// Return the expression,
    /// or error if the tokens do not represent a expression.
    fn expect_expr(&mut self) -> ParseResult<tree::Expr> {
        self.match_expr()?.ok_or(ParseErr::ExpectedExpr)
    }

    /// Match an assignment operation. (a = b)
    fn match_asg(&mut self) -> ParseResult<Option<tree::Expr>> {
        // a = b = c = d = e = [expr]

        let mut pats = vec![];
        let mut last = self.match_lor()?;
        // TODO: asg ops

        while self.match1(token![=]) {
            // if there was an equal sign, this expression must've been a pattern:
            let pat_expr = last.ok_or(ParseErr::ExpectedPattern)?;
            let pat = tree::AsgPat::try_from(pat_expr)
                .map_err(ParseErr::AsgPatErr)?;
            
            pats.push(pat);
            last = self.match_lor()?;
        }

        if !pats.is_empty() {
            let rhs = last.ok_or(ParseErr::ExpectedExpr)?;

            let asg = pats.into_iter()
                .rfold(rhs, |e, pat| {
                    tree::Expr::Assign(pat, Box::new(e))
                });
            
            Ok(Some(asg))
        } else {
            // if pats is empty this is not an assignment expression
            Ok(last)
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

    // This creates the matching function for:
    // logical OR, logical AND
    // bitwise OR, bitwise XOR, bitwise AND
    // These have a similar structure and don't need to be repeated several times.
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

    /// Match a comparison operation. (2 < 3, 2 < 3 < 4)
    fn match_cmp(&mut self) -> ParseResult<Option<tree::Expr>> {
        let me = self.match_spread()?;

        if let Some(mut e) = me {
            let cmp_ops = [
                token![<], token![>], 
                token![<=], token![>=], 
                token![==], token![!=]
            ];

            // check if there's a comparison here
            let mut rights = vec![];
            while let Some(t) = self.match_n(&cmp_ops) {
                let op = t.try_into().unwrap();
                let rexpr = self.match_spread()?
                    .ok_or(ParseErr::ExpectedExpr)?;
                
                rights.push((op, rexpr));

            }
            if !rights.is_empty() {
                e = tree::Expr::Comparison { 
                    left: Box::new(e), 
                    rights 
                }
            }

            Ok(Some(e))
        } else {
            Ok(None)
        }
    }

    // Match a spread operation. (..[1,2,3,4])
    fn match_spread(&mut self) -> ParseResult<Option<tree::Expr>> {
        let is_spread = self.match1(token![..]);
        let me = self.match_range()?;
        
        if is_spread {
            let boxed = me.map(Box::new);
            Ok(Some(tree::Expr::Spread(boxed)))
        } else {
            Ok(me)
        }
    }

    // Match a range operation. (1..5)
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

    // This creates the matching function for:
    // shifting (<<, >>)
    // addition/subtraction (+, -)
    // multiplication, division, modulo (+, -, %)
    left_assoc_rules! {
        match_shift  = match_addsub ( ( << , >> ) match_addsub )* ;
        match_addsub = match_muldiv ( ( + , - ) match_muldiv )* ;
        match_muldiv = match_unary ( ( * , / , % ) match_unary )* ;
    }

    /// Match a unary operation. (!expr, ~expr, -expr, +expr)
    fn match_unary(&mut self) -> ParseResult<Option<tree::Expr>> {
        let unary_ops = [token![!], token![~], token![-], token![+]];

        let mut ops = vec![];
        while let Some(t) = self.match_n(&unary_ops) {
            ops.push(t.try_into().unwrap());
        }

        let me = if ops.is_empty() {
            self.match_call_index()?
        } else {
            let e = self.match_call_index()?.ok_or(ParseErr::ExpectedExpr)?;

            Some(Parser::wrap_unary_op(ops, e))
        };

        Ok(me)
    }

    /// Helper function that constructs a "Unary Ops" node.
    /// 
    /// It takes the inner expression and acts as though the unary operators were applied to it.
    /// If the inner expression is a uops node, this also flattens the operators 
    /// (so that we don't have a unary operators node wrapping another one)
    fn wrap_unary_op(mut ops: Vec<tree::op::Unary>, inner: tree::Expr) -> tree::Expr {
        // flatten if unary ops inside
        let uops = if let tree::Expr::UnaryOps(uops) = inner {
            let tree::UnaryOps { ops: ops2, expr } = uops;

            ops.extend(ops2);
            tree::UnaryOps {
                ops, expr
            }
        } else {
            // wrap otherwise
            tree::UnaryOps {
                ops,
                expr: Box::new(inner)
            }
        };

        tree::Expr::UnaryOps(uops)
    }

    /// Match a function call OR index. (f(1, 2, 3, 4), a[1])
    fn match_call_index(&mut self) -> ParseResult<Option<tree::Expr>> {
        if let Some(mut e) = self.match_path()? {
            while let Some(delim) = self.match_n(&[token!["("], token!["["]]) {
                match delim {
                    token!["("] => {
                        let params = self.expect_tuple()?;
                        self.expect1(token![")"])?;

                        e = tree::Expr::Call {
                            funct: Box::new(e), 
                            params
                        };
                    },
                    token!["["] => {
                        let index = self.expect_expr()?;
                        self.expect1(token!["]"])?;

                        e = tree::Expr::Index(tree::Index {
                            expr: Box::new(e), 
                            index: Box::new(index)
                        });
                    },
                    _ => unreachable!()
                }
            }

            Ok(Some(e))
        } else {
            Ok(None)
        }
    }

    /// Match a path. (a.b.c::d::e::f.g.h.i)
    fn match_path(&mut self) -> ParseResult<Option<tree::Expr>> {
        if let Some(mut e) = self.match_unit()? {
            let mut attrs = vec![];

            while let Some(t) = self.match_n(&[token![.], token![::]]) {
                let static_attr = match t {
                    token![.]  => false,
                    token![::] => true,
                    _ => unreachable!()
                };

                attrs.push((self.expect_ident()?, static_attr));
            }

            if !attrs.is_empty() {
                e = tree::Expr::Path(tree::Path {
                    obj: Box::new(e),
                    attrs
                })
            }
            
            Ok(Some(e))
        } else {
            Ok(None)
        }
    }

    /// Match something with lower precedence than a path.
    fn match_unit(&mut self) -> ParseResult<Option<tree::Expr>> {
        if let Some(t) = self.tokens.get(0) {
            let unit = match t {
                Token::Ident(id) if id == "set" => self.expect_set()?,
                Token::Ident(id) if id == "dict" => self.expect_dict()?,
                Token::Ident(_)   => self.expect_ident().map(tree::Expr::Ident)?,
                Token::Numeric(_) | Token::Str(_) | Token::Char(_) | token![true] | token![false] => self.expect_literal()? ,
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
                },
                _ => return Ok(None)
            };

            Ok(Some(unit))
        } else {
            Ok(None)
        }
    }

    /// Expect that the next tokens represent values of type T separated by commas 
    /// (optionally with a terminating comma)
    /// 
    /// This function requires a function that represents the match function for type T
    fn expect_tuple_of<T, F>(&mut self, f: F) -> ParseResult<Vec<T>> 
        where F: Fn(&mut Self) -> ParseResult<Option<T>>
    {
        let mut exprs = vec![];
        // terminate when there's no more expression or when there's no more ,
        while let Some(e) = f(self)? {
            exprs.push(e);
            if !self.match1(token![,]) {
                break;
            }
        }

        Ok(exprs)
    }

    /// Expect that the next tokens represent expressions separated by commas
    fn expect_tuple(&mut self) -> ParseResult<Vec<tree::Expr>> 
    {
        self.expect_tuple_of(Parser::match_expr)
    }

    /// Expect a literal (numeric, str, char)
    fn expect_literal(&mut self) -> ParseResult<tree::Expr> {
        let lit = match self.tokens.pop_front().expect("unreachable") {
            Token::Numeric(s) => tree::Literal::from_numeric(&s)
                .ok_or(ParseErr::CannotParseNumeric)?,
            Token::Str(s) => tree::Literal::Str(s),
            Token::Char(c) => tree::Literal::Char(c),
            token![true] => tree::Literal::Bool(true),
            token![false] => tree::Literal::Bool(false),
            _ => unreachable!()
        };

        Ok(tree::Expr::Literal(lit))
    }

    /// Expect a list ([1, 2, 3, 4, 5])
    fn expect_list(&mut self) -> ParseResult<tree::Expr> {
        self.expect1(token!["["])?;
        let exprs = self.expect_tuple()?;
        self.expect1(token!["]"])?;
        
        Ok(tree::Expr::ListLiteral(exprs))
    }

    /// Expect a set (set {1, 2, 3, 4})
    fn expect_set(&mut self) -> ParseResult<tree::Expr> {
        self.expect1(Token::Ident("set".to_string()))?;

        let e = if self.match1(token!["{"]) {
            let exprs = self.expect_tuple()?;
            self.expect1(token!["}"])?;
            
            tree::Expr::SetLiteral(exprs)
        } else {
            tree::Expr::Ident("set".to_string())
        };

        Ok(e)
    }
    /// Expect a dict (dict {1: 2, 3: 4})
    fn expect_dict(&mut self) -> ParseResult<tree::Expr> {
        self.expect1(Token::Ident("dict".to_string()))?;

        let e = if self.match1(token!["{"]) {
            let entries = self.expect_tuple_of(Parser::match_entry)?;
            self.expect1(token!["}"])?;
            
            tree::Expr::DictLiteral(entries)
        } else {
            tree::Expr::Ident("dict".to_string())
        };

        Ok(e)
    }
    /// Match a dict entry (1: 2)
    fn match_entry(&mut self) -> ParseResult<Option<(tree::Expr, tree::Expr)>> {
        if let Some(k) = self.match_expr()? {
            self.expect1(token![:])?;
            let v = self.expect_expr()?;
            Ok(Some((k, v)))
        } else {
            Ok(None)
        }
    }

    /// Expect an if expression (if cond {}, if cond {} else cond {}, etc.)
    fn expect_if(&mut self) -> ParseResult<tree::Expr> {
        self.expect1(token![if])?;

        let mut conditionals = vec![(self.expect_expr()?, self.expect_block()?)];
        let mut last = None;

        while self.match1(token![else]) {
            match self.tokens.get(0) {
                Some(&token![if]) => {
                    self.expect1(token![if])?;
                    conditionals.push((self.expect_expr()?, self.expect_block()?));
                },
                Some(&token!["{"]) => {
                    let block = self.expect_block()?;
                    last.replace(block);
                    break;
                },
                _ => Err(ParseErr::ExpectedBlock)?
            }
        }

        let expr = tree::If {
            conditionals,
            last
        };
        
        Ok(tree::Expr::If(expr))
    }

    // Expect a while loop.
    fn expect_while(&mut self) -> ParseResult<tree::Expr> {
        self.expect1(token![while])?;
        let condition = self.expect_expr()?;
        let block = self.expect_block()?;
        
        Ok(tree::Expr::While {
            condition: Box::new(condition), block
        })
    }

    // Expect a for loop.
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
    use crate::tree::*;

    use super::{parse, ParseErr, Parser};

    macro_rules! assert_parse {
        ($s:expr => $r:expr) => {
            assert_eq!(parse_str(&$s), Ok(Program($r)))
        }
    }
    macro_rules! assert_parse_fail {
        ($s:expr => $r:expr) => {
            assert_eq!(parse_str(&$s), Err($r))
        }
    }

    fn parse_str(s: &str) -> Result<Program, ParseErr> {
        parse(tokenize(s).unwrap())
    }

    #[test]
    fn expression_test() {
        assert_parse!("2 + 3;" => vec![
            Stmt::Expr(Expr::BinaryOp(BinaryOp {
                op: op::Binary::Add, 
                left: Box::new(Expr::Literal(Literal::Int(2))), 
                right: Box::new(Expr::Literal(Literal::Int(3)))
            }))
        ]);

        assert_parse!("2 + 3 * 4;" => vec![
            Stmt::Expr(Expr::BinaryOp(BinaryOp {
                op: op::Binary::Add, 
                left: Box::new(Expr::Literal(Literal::Int(2))), 
                right: Box::new(Expr::BinaryOp(BinaryOp {
                    op: op::Binary::Mul, 
                    left: Box::new(Expr::Literal(Literal::Int(3))), 
                    right: Box::new(Expr::Literal(Literal::Int(4)))
                }))
            }))
        ]);

        assert_parse!("{}" => vec![
            Stmt::Expr(Expr::Block(Program(vec![])))
        ])
    }

    #[test]
    fn if_else_test() {
        assert_parse!("if true {
            // :)
        }" => vec![
            Stmt::Expr(Expr::If(If {
                conditionals: vec![
                    ((Expr::Literal(Literal::Bool(true)), Program(vec![])))
                ],
                last: None
            }))
        ]);

        assert_parse!("if true {
            // :)
        } else {
            // :(
        }" => vec![
            Stmt::Expr(Expr::If(If { 
                conditionals: vec![
                    (Expr::Literal(Literal::Bool(true)), Program(vec![]))
                ],
                last: Some(Program(vec![]))
            }))
        ]);

        assert_parse!("if true {
            // :)
        } else if condition {
            // :|
        } else {
            // :(
        }" => vec![
            Stmt::Expr(Expr::If(If { 
                conditionals: vec![
                    (Expr::Literal(Literal::Bool(true)), Program(vec![])),
                    (Expr::Ident("condition".to_string()), Program(vec![]))
                ],
                last: Some(Program(vec![]))
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
            Stmt::Expr(Expr::If(If { 
                conditionals: vec![
                    (Expr::Literal(Literal::Bool(true)), Program(vec![])),
                    (Expr::Ident("condition".to_string()), Program(vec![])),
                    (Expr::Ident("condition".to_string()), Program(vec![])),
                    (Expr::Ident("condition".to_string()), Program(vec![])),
                    (Expr::Ident("condition".to_string()), Program(vec![])),
                ],
                last: Some(Program(vec![]))
            }))
        ]);
    }

    #[test]
    fn semicolon_test() {
        assert_parse_fail!("2 2" => ParseErr::ExpectedTokens(vec![token![;]]));

        assert_parse!("if cond {}" => vec![Stmt::Expr(Expr::If(If {
            conditionals: vec![
                (Expr::Ident("cond".to_string()), Program(vec![]))
            ],
            last: None
        }))]);
        assert_parse!("if cond {};" => vec![Stmt::Expr(Expr::If(If {
            conditionals: vec![
                (Expr::Ident("cond".to_string()), Program(vec![]))
            ],
            last: None
        }))]);

        assert_parse!("
        let a = 0;
        let b = 1;
        let c = 2;
        if cond {
            let d = 3;
        }
        " => vec![
            Stmt::Decl(Decl { 
                rt: ReasgType::Let, 
                pat: DeclPat::Unit(DeclUnit::Ident(String::from("a"), MutType::Immut)), 
                ty: None, 
                val: Expr::Literal(Literal::Int(0))
            }),
            Stmt::Decl(Decl { 
                rt: ReasgType::Let, 
                pat: DeclPat::Unit(DeclUnit::Ident(String::from("b"), MutType::Immut)), 
                ty: None, 
                val: Expr::Literal(Literal::Int(1))
            }),
            Stmt::Decl(Decl { 
                rt: ReasgType::Let, 
                pat: DeclPat::Unit(DeclUnit::Ident(String::from("c"), MutType::Immut)), 
                ty: None, 
                val: Expr::Literal(Literal::Int(2))
            }),
            Stmt::Expr(Expr::If(If {
                conditionals: vec![(
                    Expr::Ident("cond".to_string()),
                    Program(vec![
                        Stmt::Decl(Decl { 
                            rt: ReasgType::Let, 
                            pat: DeclPat::Unit(DeclUnit::Ident(String::from("d"), MutType::Immut)), 
                            ty: None, 
                            val: Expr::Literal(Literal::Int(3))
                        })
                    ])
                )],
                last: None
            }))
        ])
    }

    #[test]
    fn type_test() {
        
        let tokens = tokenize("int").unwrap();
        assert_eq!(Parser::new(tokens).expect_type(), Ok(
            Type("int".to_string(), vec![])
        ));

        let tokens = tokenize("dict<a, b>").unwrap();

        assert_eq!(Parser::new(tokens).expect_type(), Ok(
            Type("dict".to_string(), vec![
                Type("a".to_string(), vec![]),
                Type("b".to_string(), vec![])
            ])
        ));

        let tokens = tokenize("dict<list<list<int>>, str>").unwrap();
        assert_eq!(Parser::new(tokens).expect_type(), Ok(
            Type("dict".to_string(), vec![
                Type("list".to_string(), vec![
                    Type("list".to_string(), vec![
                        Type("int".to_string(), vec![])
                    ])
                ]),
                Type("str".to_string(), vec![])
            ])
        ));
    }

    #[test]
    fn unary_ops_test() {
        assert_parse!("
        +3;
        " => vec![
            Stmt::Expr(Expr::UnaryOps(UnaryOps {
                ops: vec![token![+].try_into().unwrap()],
                expr: Box::new(Expr::Literal(Literal::Int(3)))
            }))
        ]);

        assert_parse!("
        +++++++3;
        " => vec![
            Stmt::Expr(Expr::UnaryOps(UnaryOps {
                ops: vec![token![+].try_into().unwrap()].repeat(7),
                expr: Box::new(Expr::Literal(Literal::Int(3)))
            }))
        ]);

        assert_parse!("
        +-+-+-+-3;
        " => vec![
            Stmt::Expr(Expr::UnaryOps(UnaryOps {
                ops: vec![token![+].try_into().unwrap(), token![-].try_into().unwrap()].repeat(4),
                expr: Box::new(Expr::Literal(Literal::Int(3)))
            }))
        ]);

        assert_parse!("
        !+-+-+-+-3;
        " => vec![
            Stmt::Expr(Expr::UnaryOps(UnaryOps {
                ops: vec![
                    token![!].try_into().unwrap(), 
                    token![+].try_into().unwrap(), 
                    token![-].try_into().unwrap(), 
                    token![+].try_into().unwrap(), 
                    token![-].try_into().unwrap(), 
                    token![+].try_into().unwrap(), 
                    token![-].try_into().unwrap(), 
                    token![+].try_into().unwrap(), 
                    token![-].try_into().unwrap()],
                expr: Box::new(Expr::Literal(Literal::Int(3)))
            }))
        ]);

        assert_parse!("+(+2);" => vec![
            Stmt::Expr(Expr::UnaryOps(UnaryOps {
                ops: vec![token![+].try_into().unwrap()].repeat(2),
                expr: Box::new(Expr::Literal(Literal::Int(2)))
            }))
        ])
    }
}