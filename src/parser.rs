//! Converts sequences of tokens to an AST.
//! 
//! The parser is implemented as a recursive descent parser.
//! This parser has grammatical rules, which break down into smaller grammatical rules.
//! The string is assigned the top-most rule (`program`) and the individual units of
//! this rule are computed by recursive statements.
//! 
//! This module provides:
//! - [`parse`]: A function to parse [a list of lexed tokens][`crate::lexer`] into an AST.
//! - [`Parser`]: The struct that does all the parsing.

use lazy_static::lazy_static;

use std::collections::VecDeque;
use std::rc::Rc;

use crate::GonErr;
use crate::err::FullGonErr;
use crate::lexer::token::{Token, token, FullToken};
use crate::ast::{self, PatErr};

/// Parses a sequence of tokens to an isolated parseable program tree. 
/// 
/// For more control, see [`Parser`].
/// 
/// # Example
/// ```
/// use poligon_lang::lexer::tokenize;
/// # use poligon_lang::parser::parse;
/// use poligon_lang::ast::*;
/// 
/// let tokens = tokenize("hello;").unwrap();
/// let program = parse(tokens).unwrap();
/// assert_eq!(program, Program(vec![Stmt::Expr(Expr::Ident(String::from("hello")))]));
/// ```
pub fn parse(tokens: impl IntoIterator<Item=FullToken>) -> ParseResult<ast::Program> {
    Parser::new(tokens, false).parse()
}

/// A struct that does the conversion of tokens to a parseable program tree.
/// 
/// This struct uses some terminology in its function declarations:
/// - For functions which "expect X," the next set of tokens should represent X, 
/// otherwise the function errors.
/// - For functions which "match X," if the next tokens represent X, those tokens are consumed,
/// otherwise nothing occurs.
/// 
/// Additionally, functions which match an expression don't only match one type of expression, 
/// but rather can match that one type of expression and expressions which take precedence
/// (for example, [`match_addsub`][Parser::match_addsub] matches (`+`) and (`-`) expressions
/// but also (`*`), (`/`), etc. expressions.)
/// 
/// # Example
/// ```
/// use poligon_lang::lexer::tokenize;
/// # use poligon_lang::parser::Parser;
/// use poligon_lang::ast::*;
/// 
/// let tokens = tokenize("hello;").unwrap();
/// let program = Parser::new(tokens, false).parse().unwrap();
/// assert_eq!(program, Program(vec![Stmt::Expr(Expr::Ident(String::from("hello")))]));
/// ```
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
        #[doc = concat!(
            "Match the next tokens in input if they represent a ( ",
            $(
                "`", stringify!($op), "` "
            ),+,
            ") operation or any expression with higher precedence."
        )]
        #[doc = ""]
        #[doc = concat!(
            "The next expression above in precedence is [`Parser::",
            stringify!($ds),
            "`]."
        )]
        pub fn $n(&mut self) -> ParseResult<Option<ast::Expr>> {
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
    /// The `repl_mode` parameter alters some parser functionality 
    /// to better support the [REPL][`crate::interpreter::Repl`].
    /// In particular, semicolons are not required at the end of blocks.
    /// 
    /// # Example
    /// ```
    /// use poligon_lang::lexer::tokenize;
    /// # use poligon_lang::parser::Parser;
    /// # use poligon_lang::ast::*;
    /// #
    /// # let result = Program(vec![
    /// #     Stmt::Expr(Expr::Ident(String::from("hello")))
    /// # ]);
    /// 
    /// // Not REPL mode
    /// let tokens = tokenize("hello;").unwrap();
    /// let program = Parser::new(tokens, false).parse().unwrap();
    /// # assert_eq!(program, result);
    /// 
    /// // REPL mode
    /// let tokens = tokenize("hello").unwrap();
    /// let program = Parser::new(tokens, true).parse().unwrap();
    /// # assert_eq!(program, result);
    /// ```
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

    /// Consumes the parser and converts the tokens into an [`ast::Program`].
    /// 
    /// # Example
    /// ```
    /// use poligon_lang::lexer::tokenize;
    /// # use poligon_lang::parser::Parser;
    /// use poligon_lang::ast::*;
    /// 
    /// let tokens = tokenize("hello;").unwrap();
    /// let program = Parser::new(tokens, false).parse().unwrap();
    /// assert_eq!(program, Program(vec![Stmt::Expr(Expr::Ident(String::from("hello")))]));
    /// ```
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

    /// Expect that the next token in the input is in the specified token, 
    /// erroring if the next token does not match.
    /// 
    /// # Example
    /// ```
    /// use poligon_lang::lexer::tokenize;
    /// use poligon_lang::lexer::token::token;
    /// # use poligon_lang::parser::Parser;
    /// 
    /// let mut tokens = tokenize("return true && false").unwrap();
    /// let mut parser = Parser::new(tokens, false);
    /// assert!(parser.expect1(token![return]).is_ok());
    /// assert!(parser.expect1(token![true]).is_ok());
    /// assert!(parser.expect1(token![||]).is_err());
    /// ```
    pub fn expect1(&mut self, u: Token) -> ParseResult<()> {
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

    /// Expect that the next token in the input is in the specified list of tokens.
    /// If it is, the token is returned, otherwise an error occurs.
    /// 
    /// # Example
    /// ```
    /// use poligon_lang::lexer::tokenize;
    /// use poligon_lang::lexer::token::token;
    /// # use poligon_lang::parser::Parser;
    /// 
    /// let mut tokens = tokenize("return true + false").unwrap();
    /// let mut parser = Parser::new(tokens, false);
    /// assert!(parser.expect1(token![return]).is_ok());
    /// assert_eq!(parser.expect_n(&[token![true], token![false]]).unwrap(), token![true]);
    /// assert!(parser.expect_n(&[token![||], token![&&]]).is_err());
    /// ```
    pub fn expect_n(&mut self, one_of: &[Token]) -> ParseResult<FullToken> {
        if let Some(ft) = self.tokens.pop_front() {
            if one_of.contains(&ft.tt) {
                Ok(ft)
            } else {
                Err(ParseErr::ExpectedTokens(vec![ft.tt]).at_range(ft.loc))
            }
        } else {
            Err(ParseErr::ExpectedTokens(one_of.into()).at(self.eof))
        }
    }

    /// Test whether the next token in the input matches the specified token,
    /// and consume the token if it does.
    /// 
    /// # Example
    /// ```
    /// use poligon_lang::lexer::tokenize;
    /// use poligon_lang::lexer::token::token;
    /// # use poligon_lang::parser::Parser;
    /// 
    /// let mut tokens = tokenize("return true").unwrap();
    /// let mut parser = Parser::new(tokens, false);
    /// assert!(!parser.match1(token![break]));
    /// assert!(!parser.match1(token![continue]));
    /// assert!(parser.match1(token![return]));
    /// assert!(parser.match1(token![true]));
    /// ```
    pub fn match1(&mut self, u: Token) -> bool {
        match self.peek_token() {
            Some(t) if t == &u => self.tokens.pop_front(),
            _ => None,
        }.is_some()
    }

    /// Check whether the next token in the input is in the specified list of tokens, 
    /// consuming and returning the token if it is.
    /// 
    /// # Example
    /// ```
    /// use poligon_lang::lexer::tokenize;
    /// use poligon_lang::lexer::token::token;
    /// # use poligon_lang::parser::Parser;
    /// 
    /// let mut tokens = tokenize("return true + false").unwrap();
    /// let mut parser = Parser::new(tokens, false);
    /// assert_eq!(
    ///     parser.match_n(&[token![break], token![continue], token![return]]).unwrap(), 
    ///     token![return]
    /// );
    /// assert!(parser.match1(token![true]));
    /// assert_eq!(parser.match_n(&[token![&&], token![||]]), None);
    /// assert_eq!(parser.match_n(&[token![+], token![-]]).unwrap(), token![+]);
    /// ```
    pub fn match_n(&mut self, one_of: &[Token]) -> Option<FullToken> {
        match self.peek_token() {
            Some(t) if one_of.contains(t) => self.tokens.pop_front(),
            _ => None,
        }
    }

    /// Read the next token in the input if present.
    pub fn peek_token(&self) -> Option<&Token> {
        self.tokens.get(0).map(|FullToken {tt, ..}| tt)
    }

    /// Consume the next token in the input and return it if present.
    pub fn next_token(&mut self) -> Option<Token> {
        self.tokens.pop_front().map(|FullToken {tt, ..}| tt)
    }

    /// Look at the range of the next token in the input (or return EOF).
    pub fn peek_loc(&self) -> std::ops::RangeInclusive<(usize, usize)> {
        self.tokens.get(0)
            .map_or(
                self.eof..=self.eof,
                |FullToken {loc, ..}| loc.clone()
            )
    }


    /// Expect that the next tokens in input represent values of type `T` separated by commas 
    /// (and optionally a terminating comma). If successful, this function returns the
    /// matched values and a bool value indicating whether the tuple had a terminating comma.
    /// 
    /// This function requires a `Parser::match_x` function that can match values of type `T`.
    pub fn expect_tuple_of<T, F>(&mut self, mut f: F) -> ParseResult<(Vec<T>, bool /* ended in comma? */)> 
        where F: FnMut(&mut Self) -> ParseResult<Option<T>>
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

    /// Expect that the next tokens in input represent expressions separated by commas 
    /// (optionally a terminating comma), and ending with the closer token.
    /// 
    /// For example, if the closer was `}`, then `1, 2, 3, 4 }` would be valid here.
    /// The `{` would need to be [matched][Parser::match1] before this function is called.
    pub fn expect_closing_tuple(&mut self, closer: Token) -> ParseResult<Vec<ast::Expr>> 
    {
        self.expect_closing_tuple_of(Parser::match_expr, closer, ParseErr::ExpectedExpr)
    }

    /// Expect that the next tokens in input represent values of type `T` separated by commas 
    /// (optionally a terminating comma), and ending with the closer token.
    /// 
    /// For example, if the closer was `}`, then `t, t, t, t }` would be valid here.
    /// The `{` would need to be [matched][Parser::match1] before this function is called.
    /// 
    /// This function requires a `Parser::match_x` function that can match values of type `T`,
    /// and an error to raise if a match was not found.
    pub fn expect_closing_tuple_of<T, F>(
        &mut self, f: F, closer: Token, or_else: ParseErr
    ) -> ParseResult<Vec<T>> 
        where F: FnMut(&mut Self) -> ParseResult<Option<T>>
    {
        let (exprs, comma_end) = self.expect_tuple_of(f)?;

        // if the next token is not the close token,
        // then raise an error, because the tuple did not close properly
        if self.match1(closer) {
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

    /// Match a left angle bracket in type expressions (`<`). 
    /// 
    /// This function differs from [`parser::match1(token![<])`][Parser::match1] 
    /// because the `<<` is also matched and is treated by this function as two `<`s.
    pub fn match_langle(&mut self) -> bool {
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
    ///
    /// This function differs from [`parser::match1(token![>])`][Parser::match1] 
    /// because the `>>` is also matched and is treated by this function as two `>`s.
    pub fn match_rangle(&mut self) -> bool {
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
    
    /// Expect that the next tokens in the input represent a list of statements.
    pub fn expect_stmts(&mut self) -> ParseResult<Vec<ast::Stmt>> {
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

        Ok(stmts)
    }

    /// Expect that the next tokens in the input represent a block.
    pub fn expect_block(&mut self) -> ParseResult<ast::Block> {
        self.expect1(token!["{"])?;
        let p = self.expect_stmts()?;
        self.expect1(token!["}"])?;
        Ok(ast::Block(p))
    }

    /// Expect that the next tokens in the input represent a statement.
    pub fn match_stmt(&mut self) -> ParseResult<Option<ast::Stmt>> {
        let st = match self.peek_token() {
            Some(token![let] | token![const]) => Some(self.expect_decl()?).map(ast::Stmt::Decl),
            Some(token![return])   => Some(self.expect_return()?),
            Some(token![break])    => Some(self.expect_break()?),
            Some(token![continue]) => Some(self.expect_cont()?),
            Some(token![fun])      => Some(self.expect_fun_decl()?).map(ast::Stmt::FunDecl),
            Some(token![extern])   => {
                self.expect1(token![extern])?;
                Some(self.expect_fun_sig()?).map(ast::Stmt::ExternFunDecl)
            },
            Some(token![class])    => Some(self.expect_class_decl()?).map(ast::Stmt::ClassDecl),
            Some(_)                => self.match_expr()?.map(ast::Stmt::Expr),
            None                   => None
        };

        Ok(st)
    }

    /// Expect that the next tokens in the input represent a variable declaration.
    pub fn expect_decl(&mut self) -> ParseResult<ast::Decl> {
        let Some(rt) = self.match_reasg_type() else { unreachable!() };
        
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

    /// Match the next tokens in the input if they represent a variable declaration pattern.
    /// 
    /// This is used by [`Parser::expect_decl`].
    pub fn match_decl_pat(&mut self) -> ParseResult<Option<ast::DeclPat>> {
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

    /// Match the next token to a reassignment type if it represents one.
    pub fn match_reasg_type(&mut self) -> Option<ast::ReasgType> {
        if self.match1(token![let]) {
            Some(ast::ReasgType::Let)
        } else if self.match1(token![const]) {
            Some(ast::ReasgType::Const)
        } else {
            None
        }
    }
    /// Match the next tokens in the input if they represent a function parameter.
    pub fn match_param(&mut self) -> ParseResult<Option<ast::Param>> {
        let mrt = self.match_reasg_type();
        let (mut empty, rt) = match mrt {
            Some(t) => (false, t),
            None => (true, Default::default()),
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

    /// Expect that the next tokens in the input represent a return statement.
    pub fn expect_return(&mut self) -> ParseResult<ast::Stmt> {
        self.expect1(token![return])?;
        let e = self.match_expr()?;

        Ok(ast::Stmt::Return(e))
    }

    /// Expect that the next tokens in the input represent a break statement.
    pub fn expect_break(&mut self) -> ParseResult<ast::Stmt> {
        self.expect1(token![break])?;

        Ok(ast::Stmt::Break)
    }
    /// Expect that the next tokens in the input represent a continue statement.
    pub fn expect_cont(&mut self) -> ParseResult<ast::Stmt> {
        self.expect1(token![continue])?;

        Ok(ast::Stmt::Continue)
    }

    /// Expect that the next tokens in the input represent a function signature.
    pub fn expect_fun_sig(&mut self) -> ParseResult<ast::FunSignature> {
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

    /// Expect that the next tokens in the input represent a function declaration.
    pub fn expect_fun_decl(&mut self) -> ParseResult<ast::FunDecl> {
        let sig = self.expect_fun_sig()?;
        let block = self.expect_block()?;

        Ok(ast::FunDecl {
            sig,
            block: Rc::new(block)
        })
    }

    /// Expect that the next tokens in the input represent a class declaration.
    pub fn expect_class_decl(&mut self) -> ParseResult<ast::Class> {
        self.expect1(token![class])?;
        let ident = self.expect_ident()?;

        self.expect1(token!["{"])?;
        let (fields, _) = self.expect_tuple_of(Parser::match_field_decl)?;
        
        let mut methods = vec![];
        while let Some(m) = self.match_method_decl()? {
            methods.push(m);
        }
        self.expect1(token!["}"])?;

        Ok(ast::Class { ident, fields, methods })
    }

    /// Match the next tokens if they represent a field declaration.
    pub fn match_field_decl(&mut self) -> ParseResult<Option<ast::FieldDecl>> {
        let (mut empty, rt) = match self.match_reasg_type() {
            Some(t) => (false, t),
            None => (true, Default::default()),
        };

        let mt = if self.match1(token![mut]) {
            empty = false;
            ast::MutType::Mut
        } else {
            ast::MutType::Immut
        };

        if empty && !matches!(self.peek_token(), Some(Token::Ident(_))) {
            return Ok(None);
        }

        let ident = self.expect_ident()?;
        self.expect1(token![:])?;
        let ty = self.expect_type()?;

        Ok(Some(ast::FieldDecl { rt, mt, ident, ty }))
    }

    /// Expect that the next tokens represent a method identifier 
    /// (self.ident, Self::ident, .ident, ::ident)
    /// 
    /// The return of this function holds the self parameter, the identifier, 
    /// and whether or not the method is static.
    pub fn expect_method_ident(&mut self) -> ParseResult<(Option<String>, String, bool)> {
        let referent = self.match_ident();

        let is_static = match self.expect_n(&[token![.], token![::]])?.tt {
            token![.] => false,
            token![::] => true,
            _ => unreachable!()
        };
        let method = self.expect_ident()?;

        Ok((referent, method, is_static))
    }

    /// Expect that the next tokens represent a method signature.
    pub fn expect_method_sig(&mut self) -> ParseResult<ast::MethodSignature> {
        let (referent, method_name, is_static) = self.expect_method_ident()?;

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

        Ok(ast::MethodSignature {
            referent,
            is_static,
            name: method_name,
            params,
            ret,
        })
    }

    /// Match the next tokens if they represent a method declaration.
    pub fn match_method_decl(&mut self) -> ParseResult<Option<ast::MethodDecl>> {
        self.match1(token![fun]).then(|| {
            let sig = self.expect_method_sig()?;
            let block = self.expect_block()?;

            Ok(ast::MethodDecl { sig, block: Rc::new(block) })
        }).transpose()
    }

    /// Match the next token in input if it is an identifier token,
    /// returning the identifier's string if successfully matched.
    pub fn match_ident(&mut self) -> Option<String> {
        matches!(self.peek_token(), Some(Token::Ident(_))).then(|| {
            let Some(FullToken { tt: Token::Ident(s), .. }) = self.tokens.pop_front() else {
                unreachable!()
            };
            s
        })
    }

    /// Expect that the next token in the input is an identifier token,
    /// returning the identifier's string if successfully matched.
    pub fn expect_ident(&mut self) -> ParseResult<String> {
        match self.tokens.pop_front() {
            Some(FullToken { tt: Token::Ident(s), .. }) => Ok(s),
            Some(FullToken { loc, .. }) => Err(ParseErr::ExpectedIdent.at_range(loc)),
            None => Err(ParseErr::ExpectedIdent.at(self.eof))
        }
    }
    
    /// Match the next tokens in the input if they represent a type expression.
    /// 
    /// This is used to enable [`parser::expect_tuple_of(Parser::match_type)`][`Parser::expect_tuple_of`].
    /// The function that *should* be used for type expression parsing purposes is [`Parser::expect_type`].
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

    /// Expect that the next tokens in the input represent a type expression.
    pub fn expect_type(&mut self) -> ParseResult<ast::Type> {
        self.match_type()?
            .ok_or_else(|| ParseErr::ExpectedType.at_range(self.peek_loc()))
    }

    /// Expect that the next tokens in input represent some expression.
    pub fn expect_expr(&mut self) -> ParseResult<ast::Expr> {
        self.match_expr()?
            .ok_or_else(|| ParseErr::ExpectedExpr.at_range(self.peek_loc()))
    }
    /// Match the next tokens in input if they represent any expression.
    /// 
    /// The next expression above in precedence is [`Parser::match_asg`].
    pub fn match_expr(&mut self) -> ParseResult<Option<ast::Expr>> {
        self.match_asg()
    }

    /// Match the next tokens in input if they represent an assignment operation (`a = b`)
    /// or any expression with higher precedence.
    /// 
    /// The next expression above in precedence is [`Parser::match_lor`].
    pub fn match_asg(&mut self) -> ParseResult<Option<ast::Expr>> {
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
    // These have a similar structure and don't need to be repeated several times.
    left_assoc_rules! { 
        match_lor  = match_land  ( || match_land )*;
        match_land = match_cmp   ( && match_cmp  )*;
    }

    /// Match the next tokens in input if they represent an comparison operation (`a < b`)
    /// or any expression with higher precedence.
    /// 
    /// Note that compound comparisons (`a < b < c < d`) are considered one comparison
    /// and can be returned by this function.
    /// 
    /// The next expression above in precedence is [`Parser::match_spread`].
    pub fn match_cmp(&mut self) -> ParseResult<Option<ast::Expr>> {
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

    /// Match the next tokens in input if they represent an spread expression (`..[1, 2, 3]`)
    /// or any expression with higher precedence.
    /// 
    /// The next expression above in precedence is [`Parser::match_range`].
    pub fn match_spread(&mut self) -> ParseResult<Option<ast::Expr>> {
        let is_spread = self.match1(token![..]);
        let me = self.match_range()?;
        
        if is_spread {
            let boxed = me.map(Box::new);
            Ok(Some(ast::Expr::Spread(boxed)))
        } else {
            Ok(me)
        }
    }

    /// Match the next tokens in input if they represent an range expression (`1..5`)
    /// or any expression with higher precedence.
    /// 
    /// The next expression above in precedence is [`Parser::match_bor`].
    pub fn match_range(&mut self) -> ParseResult<Option<ast::Expr>> {
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
    // bitwise OR, bitwise XOR, bitwise AND
    left_assoc_rules! {
        match_bor  = match_bxor  ( | match_bxor  )*;
        match_bxor = match_band  ( ^ match_band  )*;
        match_band = match_shift ( & match_shift )*;
    }
    // shifting (<<, >>)
    // addition/subtraction (+, -)
    // multiplication, division, modulo (+, -, %)
    left_assoc_rules! {
        match_shift  = match_addsub ( ( << , >> ) match_addsub )* ;
        match_addsub = match_muldiv ( ( + , - ) match_muldiv )* ;
        match_muldiv = match_unary ( ( * , / , % ) match_unary )* ;
    }

    /// Match the next tokens in input if they represent a unary expression (`!expr`, `-expr`)
    /// or any expression with higher precedence.
    /// 
    /// The next expression above in precedence is [`Parser::match_call_index`].
    pub fn match_unary(&mut self) -> ParseResult<Option<ast::Expr>> {
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

    /// Helper function that constructs a [`ast::Expr::UnaryOps`] node.
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

    /// Match the next tokens in input if they represent a indexing or function call 
    /// (`a[1]`, `f(1, 2, 3, 4)`)
    /// or any expression with higher precedence.
    /// 
    /// The next expression above in precedence is [`Parser::match_path`].

    pub fn match_call_index(&mut self) -> ParseResult<Option<ast::Expr>> {
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

    /// Match the next tokens in input if they represent a path expression (`Type::expr`, or `a.b.c.d.e`)
    /// or any expression with higher precedence.
    /// 
    /// The next expression above in precedence is [`Parser::match_unit`].
    pub fn match_path(&mut self) -> ParseResult<Option<ast::Expr>> {
        macro_rules! pat_arr {
            ($($p:pat),*) => {[$(
                FullToken { tt: $p, .. }
            ),*]}
        }

        if let Some(pat_arr![Token::Ident(_), token![::]]) = self.tokens.make_contiguous().get(0..2) {
            // TODO: support generics?
            let ty = self.expect_type()?;
            self.expect1(token![::])?;
            let attr = self.expect_ident()?;

            return Ok(Some(ast::Expr::StaticPath(ty, attr)));
        } 
        
        if let Some(mut e) = self.match_unit()? {
            let mut attrs = vec![];
            while self.match1(token![.]) {
                attrs.push(self.expect_ident()?);
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

    /// Match the next tokens in input if they represent any expression 
    /// with a higher precedence with a path.
    pub fn match_unit(&mut self) -> ParseResult<Option<ast::Expr>> {
        if let Some(t) = self.peek_token() {
            let unit = match t {
                Token::Ident(_) => self.expect_ii()?,
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

    /// Expect that the next tokens in input represent some primitive literal (int, str, char, etc.).
    pub fn expect_literal(&mut self) -> ParseResult<ast::Expr> {
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

    /// Expect that the next tokens in input represent a list literal (`[1, 2, 3]`).
    pub fn expect_list(&mut self) -> ParseResult<ast::Expr> {
        self.expect1(token!["["])?;
        let exprs = self.expect_closing_tuple(token!["]"])?;
        
        Ok(ast::Expr::ListLiteral(exprs))
    }

    /// Expect that the next tokens are an identifier, 
    /// a set literal (`set {1, 2, 3}`), 
    /// a dict literal (`dict {"a": 1, "b": 2, "c": 3}`),
    /// or a class initializer (`Class {a: 2, b: 3, c: 4}`).
    fn expect_ii(&mut self) -> ParseResult<ast::Expr> {
        /// Entry for dict literals
        fn match_entry(this: &mut Parser) -> ParseResult<Option<(ast::Expr, ast::Expr)>> {
            if let Some(k) = this.match_expr()? {
                this.expect1(token![:])?;
                let v = this.expect_expr()?;
                Ok(Some((k, v)))
            } else {
                Ok(None)
            }
        }
        /// Entry for class initializers
        fn match_init_entry(this: &mut Parser) -> ParseResult<Option<(String, ast::Expr)>> {
            if let Some(k) = this.match_ident() {
                this.expect1(token![:])?;
                let v = this.expect_expr()?;
                Ok(Some((k, v)))
            } else {
                Ok(None)
            }
        }

        let id = self.expect_ident()?;
        let e = if self.match1(token![#]) {
            self.expect1(token!["{"])?;
            match id.as_str() {
                "set" => {
                    let exprs = self.expect_closing_tuple(token!["}"])?;
                    ast::Expr::SetLiteral(exprs)
                },
                "dict" => {
                    let entries = self.expect_closing_tuple_of(
                        match_entry,
                        token!["}"], 
                        ParseErr::ExpectedEntry
                    )?;
                    ast::Expr::DictLiteral(entries)
                },
                _ => {
                    let entries = self.expect_closing_tuple_of(
                        match_init_entry, 
                        token!["}"], 
                        ParseErr::ExpectedIdent
                    )?;
                    ast::Expr::ClassLiteral(ast::Type(id, vec![]), entries)
                }
            }
        } else {
            ast::Expr::Ident(id)
        };

        Ok(e)
    }

    /// Expect that the next tokens in input represent an if-else expression 
    /// (`if cond {}`, `if cond {} else cond {}`, etc.)
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

    /// Expect that the next tokens in input represent an `while` loop.
    fn expect_while(&mut self) -> ParseResult<ast::Expr> {
        self.expect1(token![while])?;
        let condition = self.expect_expr()?;
        let block = self.expect_block()?;
        
        Ok(ast::Expr::While {
            condition: Box::new(condition), block
        })
    }

    /// Expect that the next tokens in input represent an `for` loop.
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
            Program(vec![$($e),*])
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