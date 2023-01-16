//! Converts sequences of tokens to an AST.
//! 
//! The parser is implemented as a recursive descent parser.
//! This parser has grammatical rules, which break down into smaller grammatical rules.
//! The string is assigned the top-most rule (`program`) and the individual units of
//! this rule are computed by recursive statements.
//! 
//! This module provides:
//! - [`parse`]: A function to parse [a list of lexed tokens][`crate::lexer`] into an AST.
//! - [`parse_repl`]: Similar to `parse`, but adjusted for [`Repl`][`crate::interpreter::Repl`] use.
//! - [`Parser`]: The struct that does all the parsing.

use lazy_static::lazy_static;

use std::collections::VecDeque;
use std::rc::Rc;

use crate::GonErr;
use crate::err::FullGonErr;
use crate::lexer::token::{Token, token, FullToken};
use crate::ast::{self, PatErr};

/// Parses a sequence of tokens to an isolated parseable program tree. 
/// This should be used for reading files.
/// 
/// The difference between this function and [`parse_repl`] is that
/// this function does not allow semicolons to be omitted at the end of blocks.
pub fn parse(tokens: impl IntoIterator<Item=FullToken>) -> ParseResult<ast::Program> {
    Parser::new(tokens, false).parse()
}
/// Parses a sequence of tokens to a parseable program tree.
/// This should be used for a REPL.
/// 
/// The difference between this function and [`parse`] is that
/// this function allows semicolons to be omitted at the end of blocks.
pub fn parse_repl(tokens: impl IntoIterator<Item=FullToken>) -> ParseResult<ast::Program> {
    Parser::new(tokens, true).parse()
}

/// A struct that does the conversion of tokens to a parseable program tree.
/// 
/// 
pub struct Parser {
    tokens: VecDeque<FullToken>,
    repl_mode: bool,
    eof: (usize, usize)
}

/// An error that occurs in the parsing process.
#[derive(Debug, PartialEq, Eq)]
pub enum ParseErr {
    /// The parser expected one of the tokens.
    ExpectedTokens(Vec<Token>),

    /// The parser expected an identifier.
    ExpectedIdent,

    /// The parser expected an expression here, but failed to match an expression.
    ExpectedExpr,

    /// The string provided could not be parsed into a numeric value.
    CannotParseNumeric,

    /// The parser expected a block (e.g. `{ code; code; }`).
    ExpectedBlock,

    /// The parser expected a type expression (e.g. `list<str>`).
    ExpectedType,
    
    /// The parser expected an assignment or declaration pattern, but failed to match one.
    ExpectedPattern,

    /// The parser expected a dictionary entry (e.g. `expr: expr`).
    ExpectedEntry,

    /// The parser expected a function parameter.
    ExpectedParam,

    /// An error occurred in creating an assignment pattern.
    AsgPatErr(PatErr)
}
impl GonErr for ParseErr {
    fn err_name(&self) -> &'static str {
        "syntax error"
    }

    fn message(&self) -> String {
        match self {
            ParseErr::ExpectedTokens(tokens) => if tokens.len() == 1 {
                format!("expected '{}'", tokens[0])
            } else {
                let tstr = tokens.iter()
                    .map(ToString::to_string)
                    .map(|s| format!("'{}'", s))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("expected one of {}", tstr)
            },
            ParseErr::ExpectedIdent      => String::from("expected identifier"),
            ParseErr::ExpectedExpr       => String::from("expected expression"),
            ParseErr::CannotParseNumeric => String::from("could not parse numeric"),
            ParseErr::ExpectedBlock      => String::from("expected block"),
            ParseErr::ExpectedType       => String::from("expected type expression"),
            ParseErr::ExpectedPattern    => String::from("expected pattern"),
            ParseErr::ExpectedEntry      => String::from("expected entry"),
            ParseErr::ExpectedParam      => String::from("expected param"),
            ParseErr::AsgPatErr(e) => match e {
                PatErr::InvalidAssignTarget => String::from("invalid assign target"),
                PatErr::CannotSpreadMultiple => String::from("cannot use spread pattern more than once"),
            },
        }
    }
}
/// A [`Result`] type for operations in the parsing process.
pub type ParseResult<T> = Result<T, FullParseErr>;
type FullParseErr = FullGonErr<ParseErr>;

macro_rules! left_assoc_op {
    ($n:ident = $ds:ident (($($op:tt),+) $_:ident)*;) => {
        fn $n(&mut self) -> ParseResult<Option<ast::Expr>> {
            if let Some(mut e) = self.$ds()? {
                while let Some(op) = self.match_n(&[$(token![$op]),+]) {
                    e = ast::Expr::BinaryOp {
                        op: op.tt.try_into().unwrap(),
                        left: Box::new(e),
                        right: self.$ds()?
                            .map(Box::new)
                            .ok_or(ParseErr::ExpectedExpr.at_range(self.peek_loc()))?
                    };
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

macro_rules! expected_tokens {
    ($($t:tt),*) => {
        ParseErr::ExpectedTokens(vec![$(token![$t])*,])
    }
}

/// Combine two ranges, such that the new range at least spans over the two provided ranges.
/// `l` should be left of `r`.
fn merge_ranges<T>(l: std::ops::RangeInclusive<T>, r: std::ops::RangeInclusive<T>) -> std::ops::RangeInclusive<T> {
    let (start, _) = l.into_inner();
    let (_, end) = r.into_inner();
    
    start ..= end
}

impl Parser {
    /// Create a new Parser to read a given set of tokens.
    /// 
    /// The `repl_mode` parameter indicates whether or not the final semicolon in blocks is necessary.
    /// See [`parse`] and [`parse_repl`] for more details.
    pub fn new(tokens: impl IntoIterator<Item=FullToken>, repl_mode: bool) -> Self {
        let mut tokens: VecDeque<_> = tokens.into_iter()
            .filter(|FullToken { tt, ..} | !matches!(tt, Token::Comment(_, _)))
            .collect();
        
        let eof = if let Some(FullToken { loc, ..}) = tokens.make_contiguous().last() {
            let &(lno, cno) = loc.end();
            (lno, cno + 1)
        } else {
            (0, 0)
        };

        Self { tokens, repl_mode, eof }
    }

    /// Consumes the parser and converts the tokens into an AST.
    pub fn parse(mut self) -> ParseResult<ast::Program> {
        let program = self.expect_stmts()?;

        if let Some(FullToken { loc, .. }) = self.tokens.get(0) {
            // there are more tokens left that couldn't be parsed as a program.
            // we have an issue.

            Err(expected_tokens![;].at_range(loc.clone()))
        } else {
            Ok(ast::Program(program))
        }
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
        if let Some(FullToken {tt: t, loc}) = self.tokens.pop_front() {
            if t == u {
                Ok(())
            } else {
                Err(ParseErr::ExpectedTokens(vec![u]).at_range(loc))
            }
        } else {
            Err(ParseErr::ExpectedTokens(vec![u]).at(self.eof))
        }
    }

    /// If the next token is in the specified list of tokens, 
    /// consume the token from input and return it.
    /// 
    /// Return None if it is not in the specified list of tokens.
    fn match_n(&mut self, one_of: &[Token]) -> Option<FullToken> {
        match self.peek_token() {
            Some(t) if one_of.contains(t) => self.tokens.pop_front(),
            _ => None,
        }
    }

    /// Return whether the next token matches the specified token, 
    /// and consume the token from input if it does.
    fn match1(&mut self, u: Token) -> bool {
        match self.peek_token() {
            Some(t) if t == &u => self.tokens.pop_front(),
            _ => None,
        }.is_some()
    }

    /// Match a left angle bracket in type expressions (`<`). 
    /// This can split [`<<`] into two tokens.
    fn match_langle(&mut self) -> bool {
        // if the next token matches <, then done
        // also have to check for <<
        self.match1(token![<]) || {
            let is_double = matches!(self.peek_token(), Some(token![<<]));
            
            if is_double {
                let FullToken { loc, .. } = &self.tokens[0];

                let &(slno, scno) = loc.start();
                let &end = loc.end();

                self.tokens[0] = FullToken::new(token![<], (slno, scno + 1)..=end);
            }

            is_double
        }
    }

    /// Match a right angle bracket in type expressions (`>`). 
    /// This can split [`>>`] into two tokens.
    fn match_rangle(&mut self) -> bool {
        // if they match >, then done
        // also have to check for >>
        self.match1(token![>]) || {
            let is_double = matches!(self.peek_token(), Some(token![>>]));
            
            if is_double {
                let FullToken { loc, .. } = &self.tokens[0];

                let &(slno, scno) = loc.start();
                let &end = loc.end();
                self.tokens[0] = FullToken::new(token![>], (slno, scno + 1)..=end);
            }

            is_double
        }
    }

    /// Look at the next token in the input if present.
    fn peek_token(&self) -> Option<&Token> {
        self.tokens.get(0).map(|FullToken {tt, ..}| tt)
    }

    /// Consume the next token in the input and return it if present.
    fn next_token(&mut self) -> Option<Token> {
        self.tokens.pop_front().map(|FullToken {tt, ..}| tt)
    }

    /// Look at the range of the next token in the input (or return EOF).
    fn peek_loc(&self) -> std::ops::RangeInclusive<(usize, usize)> {
        self.tokens.get(0)
            .map_or(
                self.eof..=self.eof,
                |FullToken {loc, ..}| loc.clone()
            )
    }
    
    /// Expect that the next tokens represent a vector of statements.
    /// 
    /// Return the block enclosing the vector of statements,
    /// or error if the tokens do not represent a vector of statements.
    fn expect_stmts(&mut self) -> ParseResult<ast::Block> {
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
            // outside of REPL mode:
            // if statement exists, check for semicolon
            // - for statements with blocks, semi can be ignored
            // - semicolon MUST appear otherwise

            // in REPL mode:
            // same rules apply, however:
            // - semicolon may be omitted at the end of a block
            // - therefore, an omitted semicolon indicates the end of the block

            if let Some(st) = self.match_stmt()? {
                let ends_with_block = st.ends_with_block();
                stmts.push(st);

                if ends_with_block {
                    // drop the semi if it exists, but don't do anything with it
                    self.match1(token![;]);
                } else if self.repl_mode {
                    // in REPL mode, semicolon does not need to appear at the end of a line
                    if !self.match1(token![;]) { break; }
                } else {
                    // outside of REPL mode, semicolon does need to appear.
                    self.expect1(token![;])?;
                }
            } else if !self.match1(token![;]) {
                break; 
            }
        }

        Ok(ast::Block(stmts))
    }

    /// Expect that the next tokens represent a block.
    /// 
    /// Return the block enclosing the vector of statements,
    /// or error if the tokens do not represent a block.
    fn expect_block(&mut self) -> ParseResult<ast::Block> {
        self.expect1(token!["{"])?;
        let p = self.expect_stmts()?;
        self.expect1(token!["}"])?;
        Ok(p)
    }

    /// Expect that the next tokens represent a statement.
    /// 
    /// Return the statement,
    /// or error if the tokens do not represent a statement.
    fn match_stmt(&mut self) -> ParseResult<Option<ast::Stmt>> {
        let st = match self.peek_token() {
            Some(token![let]) | Some(token![const]) => Some(self.expect_decl()?).map(ast::Stmt::Decl),
            Some(token![return])   => Some(self.expect_return()?),
            Some(token![break])    => Some(self.expect_break()?),
            Some(token![continue]) => Some(self.expect_cont()?),
            Some(token![fun])      => Some(self.expect_fun()?).map(ast::Stmt::FunDecl),
            Some(token![extern])   => {
                self.expect1(token![extern])?;
                Some(self.expect_fun_sig()?).map(ast::Stmt::ExternFunDecl)
            },
            Some(_)                => self.match_expr()?.map(ast::Stmt::Expr),
            None                   => None
        };

        Ok(st)
    }

    /// Expect that the next tokens represent a variable declaration.
    /// 
    /// Return the variable declaration,
    /// or error if the tokens do not represent a variable declaration.
    fn expect_decl(&mut self) -> ParseResult<ast::Decl> {
        let rt = match self.next_token() {
            Some(token![let])   => ast::ReasgType::Let,
            Some(token![const]) => ast::ReasgType::Const,
            _ => unreachable!()
        };
        
        let pat = self.match_decl_pat()?
            .ok_or_else(|| ParseErr::ExpectedPattern.at_range(self.peek_loc()))?;

        let ty = if self.match1(token![:]) {
            Some(self.expect_type()?)
        } else {
            None
        };

        self.expect1(token![=])?;
        let expr = self.expect_expr()?;

        Ok(ast::Decl {
            rt,
            pat,
            ty,
            val: expr
        })
    }

    fn match_decl_pat(&mut self) -> ParseResult<Option<ast::DeclPat>> {
        if let Some(t) = self.peek_token() {
            let pat = match t {
                token!["["] => {
                    self.expect1(token!["["])?;
                    let tpl = self.expect_closing_tuple_of(
                        Parser::match_decl_pat, 
                        token!["]"], 
                        ParseErr::ExpectedPattern
                    )?;

                    ast::DeclPat::List(tpl)
                },
                token![..] => {
                    self.expect1(token![..])?;
                    let item = self.match_decl_pat()?.map(Box::new);

                    ast::DeclPat::Spread(item)
                }
                token![mut] | Token::Ident(_) => {
                    let mt = if self.match1(token![mut]) {
                        ast::MutType::Mut
                    } else {
                        ast::MutType::Immut
                    };

                    let node = ast::DeclUnit(self.expect_ident()?, mt);
                    ast::DeclPat::Unit(node)
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
    fn match_param(&mut self) -> ParseResult<Option<ast::Param>> {
        let mrt_token = self.match_n(&[token![let], token![const]])
            .map(|t| t.tt);
        
        let mut empty = mrt_token.is_none(); // if mrt is not empty, ensure empty is false
        let rt = match mrt_token {
            Some(token![let]) | None => ast::ReasgType::Let,
            Some(token![const]) => ast::ReasgType::Const,
            _ => unreachable!()
        };
        
        let mt = if self.match1(token![mut]) {
            empty = false;
            ast::MutType::Mut
        } else {
            ast::MutType::Immut
        };

        // the param checked so far is fully empty and probably not an actual param:
        if empty && !matches!(self.peek_token(), Some(Token::Ident(_))) {
            return Ok(None);
        }

        let ident = self.expect_ident()?;
        let ty = if self.match1(token![:]) {
            Some(self.expect_type()?)
        } else {
            None
        };

        Ok(Some(ast::Param {
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
    fn expect_return(&mut self) -> ParseResult<ast::Stmt> {
        self.expect1(token![return])?;
        let e = self.match_expr()?;

        Ok(ast::Stmt::Return(e))
    }
    /// Expect that the next tokens represent a break statement.
    /// 
    /// Return the statement,
    /// or error if the tokens do not represent a break statement.
    fn expect_break(&mut self) -> ParseResult<ast::Stmt> {
        self.expect1(token![break])?;

        Ok(ast::Stmt::Break)
    }
    /// Expect that the next tokens represent a continue statement.
    /// 
    /// Return the statement,
    /// or error if the tokens do not represent a continue statement.
    fn expect_cont(&mut self) -> ParseResult<ast::Stmt> {
        self.expect1(token![continue])?;

        Ok(ast::Stmt::Continue)
    }

    /// Expect that the next tokens represent a function signature.
    /// 
    /// Return the function signature,
    /// or error if the tokens do not represent a function signature.
    fn expect_fun_sig(&mut self) -> ParseResult<ast::FunSignature> {
        self.expect1(token![fun])?;

        let ident = self.expect_ident()?;

        self.expect1(token!["("])?;
        let params = self.expect_closing_tuple_of(
            Parser::match_param,
            token![")"], 
            ParseErr::ExpectedParam
        )?;

        let ret = if self.match1(token![->]) {
            Some(self.expect_type()?)
        } else {
            None
        };

        Ok(ast::FunSignature { ident, params, ret })
    }

    /// Expect that the next tokens represent a function declaration.
    /// 
    /// Return the function declaration,
    /// or error if the tokens do not represent a function declaration.
    fn expect_fun(&mut self) -> ParseResult<ast::FunDecl> {
        let sig = self.expect_fun_sig()?;
        let block = self.expect_block()?;

        Ok(ast::FunDecl {
            sig,
            block: Rc::new(block)
        })
    }

    /// Expect that the next token is an identifier token.
    /// 
    /// Return the identifier's String,
    /// or error if the token is not an identifier token.
    fn expect_ident(&mut self) -> ParseResult<String> {
        match self.tokens.pop_front() {
            Some(FullToken { tt: Token::Ident(s), .. }) => Ok(s),
            Some(FullToken { loc, .. }) => Err(ParseErr::ExpectedIdent.at_range(loc)),
            None => Err(ParseErr::ExpectedIdent.at(self.eof))
        }
    }
    
    fn match_type(&mut self) -> ParseResult<Option<ast::Type>> {
        if matches!(self.peek_token(), Some(Token::Ident(_))) {
            let ident = self.expect_ident()?;

            let params = if self.match_langle() {
                let token_pos = self.peek_loc();

                let (tpl, comma_end) = self.expect_tuple_of(Parser::match_type)?;
                if !self.match_rangle() {
                    Err(if comma_end {
                        ParseErr::ExpectedType
                    } else {
                        expected_tokens![,]
                    }.at_range(self.peek_loc()))?
                }

                if !tpl.is_empty() {
                    tpl
                } else {
                    // list<>
                    //      ^
                    Err(ParseErr::ExpectedType.at_range(token_pos))?
                }
            } else {
                vec![]
            };

            Ok(Some(ast::Type(ident, params)))
        } else {
            Ok(None)
        }
    }

    /// Expect that the next tokens represent a type expression.
    /// 
    /// Return the type expression,
    /// or error if the tokens do not represent a type expression.
    fn expect_type(&mut self) -> ParseResult<ast::Type> {
        self.match_type()?
            .ok_or_else(|| ParseErr::ExpectedType.at_range(self.peek_loc()))
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
    fn match_expr(&mut self) -> ParseResult<Option<ast::Expr>> {
        self.match_asg()
    }
    /// Expect that the next tokens represent an expression
    /// 
    /// Return the expression,
    /// or error if the tokens do not represent a expression.
    fn expect_expr(&mut self) -> ParseResult<ast::Expr> {
        self.match_expr()?
            .ok_or_else(|| ParseErr::ExpectedExpr.at_range(self.peek_loc()))
    }

    /// Match an assignment operation. (a = b)
    fn match_asg(&mut self) -> ParseResult<Option<ast::Expr>> {
        // a = b = c = d = e = [expr]

        let mut pats = vec![];

        let mut pat_pos = self.peek_loc();
        let mut last = self.match_lor()?;
        // TODO: asg ops

        let mut eq_pos = self.peek_loc();
        while self.match1(token![=]) {
            // if there was an equal sign, this expression must've been a pattern:
            let pat_expr = last
                .ok_or_else(|| ParseErr::ExpectedPattern.at_range(eq_pos.clone()))?;
            let pat = match ast::AsgPat::try_from(pat_expr) {
                Ok(p) => p,
                Err(e) => {
                    Err(ParseErr::AsgPatErr(e)
                        .at_range(merge_ranges(pat_pos, eq_pos)))?
                },
            };
            
            pats.push(pat);

            pat_pos = self.peek_loc();
            last = self.match_lor()?;
            eq_pos = self.peek_loc();
        }

        if !pats.is_empty() {
            let rhs = last
                .ok_or_else(|| ParseErr::ExpectedExpr.at_range(self.peek_loc()))?;

            let asg = pats.into_iter()
                .rfold(rhs, |e, pat| {
                    ast::Expr::Assign(pat, Box::new(e))
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
        match_bor  = match_bxor  ( | match_bxor  )*;
        match_bxor = match_band  ( ^ match_band  )*;
        match_band = match_shift ( & match_shift )*;
    }

    /// Match a comparison operation. (2 < 3, 2 < 3 < 4)
    fn match_cmp(&mut self) -> ParseResult<Option<ast::Expr>> {
        let me = self.match_spread()?;

        if let Some(mut e) = me {
            lazy_static! {
                static ref CMP_OPS: [Token; 6] = [
                    token![<], token![>], 
                    token![<=], token![>=], 
                    token![==], token![!=]
                ];
            }

            // check if there's a comparison here
            let mut rights = vec![];
            while let Some(t) = self.match_n(&*CMP_OPS) {
                let op = t.tt.try_into().unwrap();
                let rexpr = self.match_spread()?
                    .ok_or_else(|| ParseErr::ExpectedExpr.at_range(self.peek_loc()))?;
                
                rights.push((op, rexpr));

            }
            if !rights.is_empty() {
                e = ast::Expr::Comparison { 
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
    fn match_spread(&mut self) -> ParseResult<Option<ast::Expr>> {
        let is_spread = self.match1(token![..]);
        let me = self.match_range()?;
        
        if is_spread {
            let boxed = me.map(Box::new);
            Ok(Some(ast::Expr::Spread(boxed)))
        } else {
            Ok(me)
        }
    }

    // Match a range operation. (1..5)
    fn match_range(&mut self) -> ParseResult<Option<ast::Expr>> {
        if let Some(mut e) = self.match_bor()? {
            if self.match1(token![..]) {
                let right = self.match_bor()?
                .ok_or_else(|| ParseErr::ExpectedExpr.at_range(self.peek_loc()))?;

                let step = if self.match1(token![step]) {
                    let sexpr = self.match_bor()?
                    .ok_or_else(|| ParseErr::ExpectedExpr.at_range(self.peek_loc()))?;
                    Some(sexpr)
                } else {
                    None
                };

                e = ast::Expr::Range {
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
    fn match_unary(&mut self) -> ParseResult<Option<ast::Expr>> {
        lazy_static! {
            static ref UNARY_OPS: [Token; 4] = [token![!], token![~], token![-], token![+]];
        }

        let mut ops = vec![];
        while let Some(t) = self.match_n(&*UNARY_OPS) {
            ops.push(t.tt.try_into().unwrap());
        }

        let me = if ops.is_empty() {
            self.match_call_index()?
        } else {
            let e = self.match_call_index()?
                .ok_or_else(|| ParseErr::ExpectedExpr.at_range(self.peek_loc()))?;

            Some(Parser::wrap_unary_op(ops, e))
        };

        Ok(me)
    }

    /// Helper function that constructs a "Unary Ops" node.
    /// 
    /// It takes the inner expression and acts as though the unary operators were applied to it.
    /// If the inner expression is a uops node, this also flattens the operators 
    /// (so that we don't have a unary operators node wrapping another one)
    fn wrap_unary_op(mut ops: Vec<ast::op::Unary>, inner: ast::Expr) -> ast::Expr {
        // flatten if unary ops inside
        if let ast::Expr::UnaryOps { ops: ops2, expr } = inner {
            ops.extend(ops2);
            ast::Expr::UnaryOps {
                ops, expr
            }
        } else {
            // wrap otherwise
            ast::Expr::UnaryOps {
                ops,
                expr: Box::new(inner)
            }
        }
    }

    /// Match a function call OR index. (f(1, 2, 3, 4), a[1])
    fn match_call_index(&mut self) -> ParseResult<Option<ast::Expr>> {
        if let Some(mut e) = self.match_path()? {
            while let Some(delim) = self.match_n(&[token!["("], token!["["]]) {
                match delim.tt {
                    token!["("] => {
                        let params = self.expect_closing_tuple(token![")"])?;

                        e = ast::Expr::Call {
                            funct: Box::new(e), 
                            params
                        };
                    },
                    token!["["] => {
                        let index = self.expect_expr()?;
                        self.expect1(token!["]"])?;

                        e = ast::Expr::Index(ast::Index {
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
    fn match_path(&mut self) -> ParseResult<Option<ast::Expr>> {
        if let Some(mut e) = self.match_unit()? {
            let mut attrs = vec![];

            while let Some(t) = self.match_n(&[token![.], token![::]]) {
                let static_attr = match t.tt {
                    token![.]  => false,
                    token![::] => true,
                    _ => unreachable!()
                };

                attrs.push((self.expect_ident()?, static_attr));
            }

            if !attrs.is_empty() {
                e = ast::Expr::Path(ast::Path {
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
    fn match_unit(&mut self) -> ParseResult<Option<ast::Expr>> {
        if let Some(t) = self.peek_token() {
            let unit = match t {
                Token::Ident(id) if id == "set" => self.expect_set()?,
                Token::Ident(id) if id == "dict" => self.expect_dict()?,
                Token::Ident(_)   => self.expect_ident().map(ast::Expr::Ident)?,
                Token::Numeric(_) | Token::Str(_) | Token::Char(_) | token![true] | token![false] => self.expect_literal()? ,
                token!["["]       => self.expect_list()?,
                token!["{"]       => self.expect_block().map(ast::Expr::Block)?,
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
    fn expect_tuple_of<T, F>(&mut self, f: F) -> ParseResult<(Vec<T>, bool /* ended in comma? */)> 
        where F: Fn(&mut Self) -> ParseResult<Option<T>>
    {
        let mut exprs = vec![];
        let mut comma_end = true;

        // terminate when there's no more expression or when there's no more ,
        while let Some(e) = f(self)? {
            exprs.push(e);

            if !self.match1(token![,]) {
                comma_end = false;
                break;
            }
        }

        Ok((exprs, comma_end))
    }

    /// Expect that the next tokens represent expressions separated by commas
    fn expect_closing_tuple(&mut self, close_with: Token) -> ParseResult<Vec<ast::Expr>> 
    {
        self.expect_closing_tuple_of(Parser::match_expr, close_with, ParseErr::ExpectedExpr)
    }

    /// Expect that the next tokens represent values of type T separated by commas 
    /// (optionally with a terminating comma)
    /// 
    /// This function requires a function that represents the **match** function for type T
    fn expect_closing_tuple_of<T, F>(
        &mut self, f: F, close_with: Token, or_else: ParseErr
    ) -> ParseResult<Vec<T>> 
        where F: Fn(&mut Self) -> ParseResult<Option<T>>
    {
        let (exprs, comma_end) = self.expect_tuple_of(f)?;

        // if the next token is not the close token,
        // then raise an error, because the tuple did not close properly
        if self.match1(close_with) {
            Ok(exprs)
        } else {
            let e = if comma_end {
                or_else
            } else {
                expected_tokens![,]
            };

            Err(e.at_range(self.peek_loc()))
        }
    }

    /// Expect a literal (numeric, str, char)
    fn expect_literal(&mut self) -> ParseResult<ast::Expr> {
        let FullToken { tt, loc } = self.tokens.pop_front().expect("unreachable");
        
        let lit = match tt {
            Token::Numeric(s) => ast::Literal::from_numeric(&s)
                .ok_or_else(|| ParseErr::CannotParseNumeric.at_range(loc))?,
            Token::Str(s)  => ast::Literal::Str(s),
            Token::Char(c) => ast::Literal::Char(c),
            token![true]   => ast::Literal::Bool(true),
            token![false]  => ast::Literal::Bool(false),
            _ => unreachable!()
        };

        Ok(ast::Expr::Literal(lit))
    }

    /// Expect a list ([1, 2, 3, 4, 5])
    fn expect_list(&mut self) -> ParseResult<ast::Expr> {
        self.expect1(token!["["])?;
        let exprs = self.expect_closing_tuple(token!["]"])?;
        
        Ok(ast::Expr::ListLiteral(exprs))
    }

    /// Expect a set (set {1, 2, 3, 4})
    fn expect_set(&mut self) -> ParseResult<ast::Expr> {
        self.expect1(Token::Ident("set".to_string()))?;

        let e = if self.match1(token!["{"]) {
            let exprs = self.expect_closing_tuple(token!["}"])?;
            
            ast::Expr::SetLiteral(exprs)
        } else {
            ast::Expr::Ident("set".to_string())
        };

        Ok(e)
    }
    /// Expect a dict (dict {1: 2, 3: 4})
    fn expect_dict(&mut self) -> ParseResult<ast::Expr> {
        self.expect1(Token::Ident("dict".to_string()))?;

        let e = if self.match1(token!["{"]) {
            let entries = self.expect_closing_tuple_of(
                Parser::match_entry,
                token!["}"], 
                ParseErr::ExpectedEntry
            )?;
            
            ast::Expr::DictLiteral(entries)
        } else {
            ast::Expr::Ident("dict".to_string())
        };

        Ok(e)
    }
    /// Match a dict entry (1: 2)
    fn match_entry(&mut self) -> ParseResult<Option<(ast::Expr, ast::Expr)>> {
        if let Some(k) = self.match_expr()? {
            self.expect1(token![:])?;
            let v = self.expect_expr()?;
            Ok(Some((k, v)))
        } else {
            Ok(None)
        }
    }

    /// Expect an if expression (if cond {}, if cond {} else cond {}, etc.)
    fn expect_if(&mut self) -> ParseResult<ast::Expr> {
        self.expect1(token![if])?;

        let mut conditionals = vec![(self.expect_expr()?, self.expect_block()?)];
        let mut last = None;

        while self.match1(token![else]) {
            match self.peek_token() {
                Some(&token![if]) => {
                    self.expect1(token![if])?;
                    conditionals.push((self.expect_expr()?, self.expect_block()?));
                },
                Some(&token!["{"]) => {
                    let block = self.expect_block()?;
                    last.replace(block);
                    break;
                },
                _ => Err(ParseErr::ExpectedBlock.at_range(self.peek_loc()))?
            }
        }

        Ok(ast::Expr::If {
            conditionals,
            last
        })
    }

    // Expect a while loop.
    fn expect_while(&mut self) -> ParseResult<ast::Expr> {
        self.expect1(token![while])?;
        let condition = self.expect_expr()?;
        let block = self.expect_block()?;
        
        Ok(ast::Expr::While {
            condition: Box::new(condition), block
        })
    }

    // Expect a for loop.
    fn expect_for(&mut self) -> ParseResult<ast::Expr> {
        self.expect1(token![for])?;
        let ident = self.expect_ident()?;
        self.expect1(token![in])?;
        let iterator = self.expect_expr()?;
        let block = self.expect_block()?;
        
        Ok(ast::Expr::For {
            ident, iterator: Box::new(iterator), block
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::err::{FullGonErr, GonErr};
    use crate::lexer::token::token;
    use crate::lexer::tokenize;
    use crate::ast::*;

    use super::*;

    macro_rules! program {
        ($($e:expr),*) => {
            Program(Block(vec![$($e),*]))
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
    fn parse_str(s: &str) -> ParseResult<Program> {
        parse(unwrap_fe(tokenize(s), s))
    }
    /// Assert that the string provided parses into the program.
    #[allow(unused)]
    fn assert_parse(input: &str, r: Program) {
        assert_eq!(unwrap_fe(parse_str(input), input), r)
    }
    /// Assert that the string provided errors with the given error when parsed.
    #[allow(unused)]
    fn assert_parse_fail<E>(input: &str, result: E) 
        where E: std::fmt::Debug,
            FullParseErr: PartialEq<E>
    {
        match parse_str(input) {
            Ok(t)  => panic!("Lexing resulted in value: {t:?}"),
            Err(e) => assert_eq!(e, result)
        }
    }

    #[test]
    fn bin_op_test() {
        assert_parse("2 + 3;", program![
            Stmt::Expr(Expr::BinaryOp {
                op: op::Binary::Add, 
                left: Box::new(Expr::Literal(Literal::Int(2))), 
                right: Box::new(Expr::Literal(Literal::Int(3)))
            })
        ]);

        assert_parse("2 + 3 * 4;", program![
            Stmt::Expr(Expr::BinaryOp {
                op: op::Binary::Add, 
                left: Box::new(Expr::Literal(Literal::Int(2))), 
                right: Box::new(Expr::BinaryOp {
                    op: op::Binary::Mul, 
                    left: Box::new(Expr::Literal(Literal::Int(3))), 
                    right: Box::new(Expr::Literal(Literal::Int(4)))
                })
            })
        ]);
    }

    #[test]
    fn block_test() {
        assert_parse("{}", program![
            Stmt::Expr(Expr::Block(Block(vec![])))
        ]);

        assert_parse("{{}}", program![
            Stmt::Expr(Expr::Block(Block(vec![
                Stmt::Expr(Expr::Block(Block(vec![])))
            ])))
        ])
    }

    /// Tests if statements.
    #[test]
    fn if_else_test() {
        assert_parse("
            if true {
                // :)
            }
        ", program![
            Stmt::Expr(Expr::If {
                conditionals: vec![
                    (Expr::Literal(Literal::Bool(true)), Block(vec![]))
                ],
                last: None
            })
        ]);

        assert_parse("
            if true {
                // :)
            } else {
                // :(
            }
        ", program![
            Stmt::Expr(Expr::If { 
                conditionals: vec![
                    (Expr::Literal(Literal::Bool(true)), Block(vec![]))
                ],
                last: Some(Block(vec![]))
            })
        ]);

        assert_parse("
            if true {
                // :)
            } else if condition {
                // :|
            } else {
                // :(
            }
        ", program![
            Stmt::Expr(Expr::If { 
                conditionals: vec![
                    (Expr::Literal(Literal::Bool(true)), Block(vec![])),
                    (Expr::Ident("condition".to_string()), Block(vec![]))
                ],
                last: Some(Block(vec![]))
            })
        ]);

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
        ", program![
            Stmt::Expr(Expr::If { 
                conditionals: vec![
                    (Expr::Literal(Literal::Bool(true)), Block(vec![])),
                    (Expr::Ident("condition".to_string()), Block(vec![])),
                    (Expr::Ident("condition".to_string()), Block(vec![])),
                    (Expr::Ident("condition".to_string()), Block(vec![])),
                    (Expr::Ident("condition".to_string()), Block(vec![])),
                ],
                last: Some(Block(vec![]))
            })
        ]);
    }

    /// Tests while and for loop as well as 
    /// declarations, function calls, conditionals, assignment, ranges, and literals.
    #[test]
    fn loop_test() {
        // barebones
        assert_parse("while true {}", program![
            Stmt::Expr(Expr::While {
                condition: Box::new(Expr::Literal(Literal::Bool(true))),
                block: Block(vec![])
            })
        ]);
        assert_parse("for i in it {}", program![
            Stmt::Expr(Expr::For {
                ident: String::from("i"),
                iterator: Box::new(Expr::Ident(String::from("it"))),
                block: Block(vec![])
            })
        ]);

        // full examples
        assert_parse("
            let i = 0;
            while i < 10 {
                print(i);
                i = i + 1;
            }
        ", program![
            Stmt::Decl(Decl { 
                rt: ReasgType::Let, 
                pat: DeclPat::Unit(DeclUnit(String::from("i"), MutType::Immut)), 
                ty: None, 
                val: Expr::Literal(Literal::Int(0))
            }),
            Stmt::Expr(Expr::While {
                condition: Box::new(Expr::Comparison {
                    left: Box::new(Expr::Ident(String::from("i"))), 
                    rights: vec![(op::Cmp::Lt, Expr::Literal(Literal::Int(10)))]
                }), 
                block: Block(vec![
                    Stmt::Expr(Expr::Call {
                        funct: Box::new(Expr::Ident(String::from("print"))),
                        params: vec![Expr::Ident(String::from("i"))]
                    }),
                    Stmt::Expr(Expr::Assign(
                        AsgPat::Unit(AsgUnit::Ident(String::from("i"))), 
                        Box::new(Expr::BinaryOp {
                            op: op::Binary::Add,
                            left: Box::new(Expr::Ident(String::from("i"))),
                            right: Box::new(Expr::Literal(Literal::Int(1)))
                        })
                    ))
                ])
            })
        ]);

        assert_parse("for i in 1..10 { print(i); }", program![
            Stmt::Expr(Expr::For {
                ident: String::from("i"), 
                iterator: Box::new(Expr::Range {
                    left: Box::new(Expr::Literal(Literal::Int(1))), 
                    right: Box::new(Expr::Literal(Literal::Int(10))), 
                    step: None 
                }), 
                block: Block(vec![
                    Stmt::Expr(Expr::Call {
                        funct: Box::new(Expr::Ident(String::from("print"))),
                        params: vec![Expr::Ident(String::from("i"))]
                    })
                ])
            })
        ]);
    }

    #[test]
    fn semicolon_test() {
        assert_parse_fail("2 2", expected_tokens![;]);

        assert_parse("if cond {}", program![
            Stmt::Expr(Expr::If {
                conditionals: vec![
                    (Expr::Ident("cond".to_string()), Block(vec![]))
                ],
                last: None
            })
        ]);
        assert_parse("if cond {};", program![
            Stmt::Expr(Expr::If {
                conditionals: vec![
                    (Expr::Ident("cond".to_string()), Block(vec![]))
                ],
                last: None
            })
        ]);

        assert_parse("
            let a = 0;
            let b = 1;
            let c = 2;
            if cond {
                let d = 3;
            }
        ", program![
            Stmt::Decl(Decl { 
                rt: ReasgType::Let, 
                pat: DeclPat::Unit(DeclUnit(String::from("a"), MutType::Immut)), 
                ty: None, 
                val: Expr::Literal(Literal::Int(0))
            }),
            Stmt::Decl(Decl { 
                rt: ReasgType::Let, 
                pat: DeclPat::Unit(DeclUnit(String::from("b"), MutType::Immut)), 
                ty: None, 
                val: Expr::Literal(Literal::Int(1))
            }),
            Stmt::Decl(Decl { 
                rt: ReasgType::Let, 
                pat: DeclPat::Unit(DeclUnit(String::from("c"), MutType::Immut)), 
                ty: None, 
                val: Expr::Literal(Literal::Int(2))
            }),
            Stmt::Expr(Expr::If {
                conditionals: vec![(
                    Expr::Ident("cond".to_string()),
                    Block(vec![
                        Stmt::Decl(Decl { 
                            rt: ReasgType::Let, 
                            pat: DeclPat::Unit(DeclUnit(String::from("d"), MutType::Immut)), 
                            ty: None, 
                            val: Expr::Literal(Literal::Int(3))
                        })
                    ])
                )],
                last: None
            })
        ])
    }

    #[test]
    fn type_test() {
        fn assert_parse_type(input: &str, ty: Type) {
            let tokens = unwrap_fe(tokenize(input), input);
            assert_eq!(
                unwrap_fe(Parser::new(tokens, false).expect_type(), input),
                ty
            )
        }

        assert_parse_type(
            "int",
            Type("int".to_string(), vec![])
        );

        assert_parse_type(
            "dict<a, b>",
            Type("dict".to_string(), vec![
                Type("a".to_string(), vec![]),
                Type("b".to_string(), vec![])
            ])

        );

        assert_parse_type(
            "dict<list<list<int>>, str>",
            Type("dict".to_string(), vec![
                Type("list".to_string(), vec![
                    Type("list".to_string(), vec![
                        Type("int".to_string(), vec![])
                    ])
                ]),
                Type("str".to_string(), vec![])
            ])
        );
    }

    #[test]
    fn unary_ops_test() {
        assert_parse("+3;", program![
            Stmt::Expr(Expr::UnaryOps {
                ops: vec![token![+].try_into().unwrap()],
                expr: Box::new(Expr::Literal(Literal::Int(3)))
            })
        ]);

        assert_parse("+++++++3;", program![
            Stmt::Expr(Expr::UnaryOps {
                ops: vec![token![+].try_into().unwrap()].repeat(7),
                expr: Box::new(Expr::Literal(Literal::Int(3)))
            })
        ]);

        assert_parse("+-+-+-+-3;", program![
            Stmt::Expr(Expr::UnaryOps {
                ops: vec![token![+].try_into().unwrap(), token![-].try_into().unwrap()].repeat(4),
                expr: Box::new(Expr::Literal(Literal::Int(3)))
            })
        ]);

        assert_parse("!+-+-+-+-3;", program![
            Stmt::Expr(Expr::UnaryOps {
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
            })
        ]);

        assert_parse("+(+2);", program![
            Stmt::Expr(Expr::UnaryOps {
                ops: vec![token![+].try_into().unwrap()].repeat(2),
                expr: Box::new(Expr::Literal(Literal::Int(2)))
            })
        ])
    }
}