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

use ast::Located;

use std::collections::VecDeque;
use std::ops::RangeInclusive;
use std::rc::Rc;

use crate::GonErr;
use crate::err::FullGonErr;
use crate::lexer::token::{Token, token, FullToken, TokenPattern};
use crate::ast::{self, PatErr};
use crate::span::CursorRange;

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
/// assert_eq!(program, Program(vec![
///     Located::new(Stmt::Expr(
///         Located::new(Expr::Ident(String::from("hello")), (0, 0) ..= (0, 4))),
///         (0, 0) ..= (0, 5)
///     )
/// ]));
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
/// assert_eq!(program, Program(vec![
///     Located::new(Stmt::Expr(
///         Located::new(Expr::Ident(String::from("hello")), (0, 0) ..= (0, 4))),
///         (0, 0) ..= (0, 5)
///     )
/// ]));
/// ```
pub struct Parser {
    tokens: VecDeque<FullToken>,
    repl_mode: bool,
    eof: (usize, usize),
    tree_locs: Vec<RangeBlock>,
    intrinsic_mode: bool
}

#[derive(Clone, Debug)]
struct RangeBlock(&'static str, Option<CursorRange>);

/// An error that occurs in the parsing process.
#[derive(Debug, PartialEq, Eq)]
pub enum ParseErr {
    /// The parser expected one of the tokens.
    ExpectedTokens(Vec<Token>),

    /// The parser expected an identifier.
    ExpectedIdent,

    /// The parser expected an expression here, but failed to match an expression.
    ExpectedExpr,

    /// The parser expected a str literal here, but got something else.
    ExpectedStrLiteral,

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
    AsgPatErr(PatErr),

    /// Parser is not in intrinsic mode and therefore cannot use an intrinsic identifier
    NoIntrinsicIdents,

    /// Parser is not in intrinsic mode and therefore this statement cannot be defined
    NoIntrinsicStmts
}
impl GonErr for ParseErr {
    fn err_name(&self) -> &'static str {
        "syntax error"
    }
}

impl std::fmt::Display for ParseErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseErr::ExpectedTokens(tokens) => match tokens.len() {
                0 => write!(f, "expected eof"),
                1 => write!(f, "expected '{}'", tokens[0]),
                _ => {
                    let (first, rest) = tokens.split_first().unwrap();
                    write!(f, "expected one of '{first}'")?;

                    for t in rest {
                        write!(f, ", '{t}'")?;
                    }

                    Ok(())
                }
            },
            ParseErr::ExpectedIdent      => write!(f, "expected identifier"),
            ParseErr::ExpectedExpr       => write!(f, "expected expression"),
            ParseErr::ExpectedStrLiteral => write!(f, "expected string literal"),
            ParseErr::CannotParseNumeric => write!(f, "could not parse numeric"),
            ParseErr::ExpectedBlock      => write!(f, "expected block"),
            ParseErr::ExpectedType       => write!(f, "expected type expression"),
            ParseErr::ExpectedPattern    => write!(f, "expected pattern"),
            ParseErr::ExpectedEntry      => write!(f, "expected entry"),
            ParseErr::ExpectedParam      => write!(f, "expected param"),
            ParseErr::NoIntrinsicIdents  => write!(f, "cannot use intrinsic identifier"),
            ParseErr::NoIntrinsicStmts   => write!(f, "cannot use intrinsic statement"),
            ParseErr::AsgPatErr(e)       => e.fmt(f),
        }
    }
}
impl std::error::Error for ParseErr {
    fn cause(&self) -> Option<&dyn std::error::Error> {
        match self {
            ParseErr::AsgPatErr(t) => Some(t),
            _ => None
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
        pub fn $n(&mut self) -> ParseResult<Option<Located<ast::Expr>>> {
            self.push_loc_block(stringify!($n));
            if let Some(mut e) = self.$ds()? {
                while let Some(op) = self.match_([$(token![$op]),+]) {
                    let binop = ast::Expr::BinaryOp {
                        op: op.tt.try_into().unwrap(),
                        left: Box::new(e),
                        right: self.$ds()?
                            .map(Box::new)
                            .ok_or(ParseErr::ExpectedExpr.at_range(self.peek_loc()))?
                    };

                    e = Located::new(binop, self.peek_loc_block().unwrap());
                }

                self.pop_loc_block(stringify!($n));
                Ok(Some(e))
            } else {
                self.pop_loc_block(stringify!($n));
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
        ParseErr::ExpectedTokens(vec![$(token![$t]),*])
    }
}

/// Combine two ranges, such that the new range at least spans over the two provided ranges.
/// `l` should be left of `r`.
fn merge_ranges<T>(l: RangeInclusive<T>, r: RangeInclusive<T>) -> RangeInclusive<T> {
    let (start, _) = l.into_inner();
    let (_, end) = r.into_inner();
    
    start ..= end
}

/// Combine two ranges, such that `l` spans over the two provided ranges.
fn merge_ranges_in_place<T: Clone>(mr1: &mut Option<RangeInclusive<T>>, r2: RangeInclusive<T>) {
    let new_range = match mr1.as_ref() {
        Some(r1) => merge_ranges(r1.clone(), r2),
        None => r2
    };

    mr1.replace(new_range);
}

impl Iterator for Parser {
    type Item = FullToken;

    fn next(&mut self) -> Option<Self::Item> {
        let ft = self.tokens.pop_front()?;
        self.append_range(ft.loc.clone());
        Some(ft)
    }
}

impl Parser {
    /// Create a new Parser to read a given set of tokens.
    /// 
    /// The `repl_mode` parameter alters some parser functionality 
    /// to better support REPLs.
    /// In particular, semicolons are not required at the end of blocks.
    /// 
    /// # Example
    /// ```
    /// use poligon_lang::lexer::tokenize;
    /// # use poligon_lang::parser::Parser;
    /// # use poligon_lang::ast::*;
    /// #
    /// 
    /// // Not REPL mode
    /// let tokens = tokenize("hello;").unwrap();
    /// let program = Parser::new(tokens, false).parse().unwrap();
    /// # let result = Program(vec![
    /// #     Located::new(Stmt::Expr(
    /// #         Located::new(Expr::Ident(String::from("hello")), (0, 0) ..= (0, 4))
    /// #     ), (0, 0) ..= (0, 5))
    /// # ]);
    /// # assert_eq!(program, result);
    /// 
    /// // REPL mode
    /// let tokens = tokenize("hello").unwrap();
    /// let program = Parser::new(tokens, true).parse().unwrap();
    /// # let result = Program(vec![
    /// #     Located::new(Stmt::Expr(
    /// #         Located::new(Expr::Ident(String::from("hello")), (0, 0) ..= (0, 4))
    /// #     ), (0, 0) ..= (0, 4))
    /// # ]);
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

        Self { tokens, repl_mode, eof, tree_locs: vec![], intrinsic_mode: false }
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
    /// assert_eq!(program, Program(vec![
    ///     Located::new(Stmt::Expr(
    ///         Located::new(Expr::Ident(String::from("hello")), (0, 0) ..= (0, 4))),
    ///         (0, 0) ..= (0, 5)
    ///     )
    /// ]));
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
    /// 
    /// Checking that the next token is the specified token.
    /// 
    /// ```
    /// use poligon_lang::lexer::tokenize;
    /// use poligon_lang::lexer::token::token;
    /// # use poligon_lang::parser::Parser;
    /// 
    /// let mut tokens = tokenize("return true && false").unwrap();
    /// let mut parser = Parser::new(tokens, false);
    /// assert!(parser.expect(token![return]).is_ok());
    /// assert!(parser.expect(token![true]).is_ok());
    /// assert!(parser.expect(token![||]).is_err());
    /// ```
    /// 
    /// Checking that the next token is one of a few given tokens.
    /// 
    /// ```
    /// use poligon_lang::lexer::tokenize;
    /// use poligon_lang::lexer::token::token;
    /// # use poligon_lang::parser::Parser;
    /// 
    /// let mut tokens = tokenize("return true + false").unwrap();
    /// let mut parser = Parser::new(tokens, false);
    /// assert!(parser.expect(token![return]).is_ok());
    /// assert_eq!(parser.expect(&[token![true], token![false]]).unwrap(), token![true]);
    /// assert!(parser.expect(&[token![||], token![&&]]).is_err());
    /// ```
    pub fn expect<P: TokenPattern>(&mut self, u: P) -> ParseResult<FullToken> {
        match self.tokens.get_mut(0) {
            Some(t) if u.fully_accepts(t) => self.next().ok_or_else(|| unreachable!()),
            Some(t) => match u.strip_strict_prefix_of(t) {
                Some(prefix) => Ok(prefix),
                None => Err(ParseErr::ExpectedTokens(u.expected_tokens()).at_range(t.loc.clone()))
            },
            _ => Err(ParseErr::ExpectedTokens(u.expected_tokens()).at(self.eof))
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
    /// assert!(parser.expect(token![return]).is_ok());
    /// assert_eq!(parser.expect_n(&[token![true], token![false]]).unwrap(), token![true]);
    /// assert!(parser.expect_n(&[token![||], token![&&]]).is_err());
    /// ```

    /// Test whether the next token in the input matches the specified token pattern,
    /// and consume the token if it does.
    /// 
    /// # Example
    /// 
    /// Checking if the next token matches a specified token:
    /// 
    /// ```
    /// use poligon_lang::lexer::tokenize;
    /// use poligon_lang::lexer::token::token;
    /// # use poligon_lang::parser::Parser;
    /// 
    /// let mut tokens = tokenize("return true").unwrap();
    /// let mut parser = Parser::new(tokens, false);
    /// assert!(!parser.match_(token![break]));
    /// assert!(!parser.match_(token![continue]));
    /// assert!(parser.match_(token![return]));
    /// assert!(parser.match_(token![true]));
    /// ```
    /// 
    /// Checking if the next token matches one of a few given tokens:
    /// ```
    /// use poligon_lang::lexer::tokenize;
    /// use poligon_lang::lexer::token::token;
    /// # use poligon_lang::parser::Parser;
    /// 
    /// let mut tokens = tokenize("return true + false").unwrap();
    /// let mut parser = Parser::new(tokens, false);
    /// assert_eq!(
    ///     parser.match_(&[token![break], token![continue], token![return]]).unwrap(), 
    ///     token![return]
    /// );
    /// assert!(parser.match_(token![true]));
    /// assert_eq!(parser.match_(&[token![&&], token![||]]), None);
    /// assert_eq!(parser.match_(&[token![+], token![-]]).unwrap(), token![+]);
    /// ```
    pub fn match_<P: TokenPattern>(&mut self, u: P) -> Option<FullToken> {
        match self.tokens.get_mut(0) {
            Some(t) if u.fully_accepts(t) => self.next(),
            Some(t) => {
                let mprefix = u.strip_strict_prefix_of(t);
                
                if let Some(prefix) = &mprefix {
                    self.append_range(prefix.loc.clone());
                }

                mprefix
            },
            _ => None
        }
    }

    /// Read the next token in the input if present.
    pub fn peek_token(&self) -> Option<&Token> {
        self.tokens.get(0).map(|FullToken {tt, ..}| tt)
    }

    /// Read the nth following token in the input if present.
    pub fn peek_nth_token(&self, i: usize) -> Option<&Token> {
        self.tokens.get(i).map(|FullToken {tt, ..}| tt)
    }

    /// Consumes the next token in input.
    /// 
    /// If you want a FullToken, see [`Parser::next`].
    pub fn next_token(&mut self) -> Option<Token> {
        self.next().map(|FullToken {tt, ..}| tt)
    }

    /// Look at the range of the next token in the input (or return EOF).
    pub fn peek_loc(&self) -> CursorRange {
        self.tokens.get(0)
            .map_or(
                self.eof..=self.eof,
                |FullToken {loc, ..}| loc.clone()
            )
    }

    /// Add a cursor-tracking block to the parser.
    /// 
    /// While this block is on top, any statements used to remove tokens
    /// are added to this block's cursor range.
    /// 
    /// To remove the block and obtain the cursor range, use [`Parser::pop_loc_block`].
    pub fn push_loc_block(&mut self, name: &'static str) {
        self.tree_locs.push(RangeBlock(name, None));
    }

    /**
     * Looks at the top cursor-tracking block without removing it.
     */
    pub fn peek_loc_block(&mut self) -> Option<CursorRange> {
        self.tree_locs.last()
            .and_then(|RangeBlock(_, r)| r.clone())
    }

    /// Remove a cursor-tracking block from the parser.
    /// 
    /// This function expects that a given loc block will have at least 1 token,
    /// otherwise it will return None.
    /// When this function is called, the top block's range is added to the range of 
    /// the block under it.
    pub fn pop_loc_block(&mut self, name: &'static str) -> Option<CursorRange> {
        let RangeBlock(pushed, mr2) = self.tree_locs.pop()
            .expect("pop_loc_block called without a push");
        assert_eq!(pushed, name, "requested {name}, popped {pushed}");

        let r2 = mr2?;
        self.append_range(r2.clone());

        Some(r2)
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

            if self.match_(token![,]).is_none() {
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
    /// The `{` would need to be [matched][Parser::match_] before this function is called.
    pub fn expect_closing_tuple(&mut self, closer: Token) -> ParseResult<Vec<Located<ast::Expr>>> 
    {
        self.expect_closing_tuple_of(Parser::match_expr, closer, ParseErr::ExpectedExpr)
    }

    /// Expect that the next tokens in input represent values of type `T` separated by commas 
    /// (optionally a terminating comma), and ending with the closer token.
    /// 
    /// For example, if the closer was `}`, then `t, t, t, t }` would be valid here.
    /// The `{` would need to be [matched][Parser::match_] before this function is called.
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
        if self.match_(closer).is_some() {
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
    
    /// Expect that the next tokens in the input represent a list of statements.
    pub fn expect_stmts(&mut self) -> ParseResult<Vec<Located<ast::Stmt>>> {
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

            self.push_loc_block("expect_stmts");
            if let Some(st) = self.match_stmt()? {
                if st.ends_with_block() {
                    // for block-terminating statements, semi isn't needed
                    // drop the semi if it exists, but don't do anything with it
                    self.match_(token![;]);
                } else if self.repl_mode {
                    // in REPL mode, semicolon does not need to appear at the end of
                    // the last statement of a block

                    if self.match_(token![;]).is_none() {
                        let stmt = Located::new(st, self.pop_loc_block("expect_stmts").unwrap());
                        stmts.push(stmt);
                        break;
                    }
                } else {
                    // outside of REPL mode, semicolon does need to appear.
                    self.expect(token![;])?;
                };
                
                let stmt = Located::new(st, self.pop_loc_block("expect_stmts").unwrap());
                stmts.push(stmt);
            } else if self.match_(token![;]).is_none() {
                self.pop_loc_block("expect_stmts");
                break; 
            }
        }

        Ok(stmts)
    }

    /// Extends the range of the top cursor-tracking block to reach the bounds of new_range.
    fn append_range(&mut self, new_range: CursorRange) {
        if let Some(rb) = self.tree_locs.last_mut() {
            merge_ranges_in_place(&mut rb.1, new_range);
        }
    }

    /// Expect that the next tokens in the input represent a block.
    pub fn expect_block(&mut self) -> ParseResult<Located<ast::Block>> {
        self.push_loc_block("expect_block");

        self.expect(token!["{"])?;
        let p = self.expect_stmts()?;
        self.expect(token!["}"])?;

        Ok(Located::new(ast::Block(p), self.pop_loc_block("expect_block").unwrap()))
    }

    /// Expect that the next tokens in the input represent a statement.
    /// 
    /// This does not include the statement's ending semicolon.
    pub fn match_stmt(&mut self) -> ParseResult<Option<ast::Stmt>> {
        let st = match self.peek_token() {
            Some(token![let] | token![const]) => Some(self.expect_decl()?).map(ast::Stmt::Decl),
            Some(token![return])   => Some(self.expect_return()?),
            Some(token![break])    => Some(self.expect_break()?),
            Some(token![continue]) => Some(self.expect_cont()?),
            Some(token![throw])    => Some(self.expect_throw()?),
            Some(token![fun])      => Some(self.expect_fun_decl()?).map(ast::Stmt::FunDecl),
            Some(token![extern])   => {
                self.expect(token![extern])?;
                Some(self.expect_fun_sig()?).map(ast::Stmt::ExternFunDecl)
            },
            Some(token![class])    => Some(self.expect_class_decl()?).map(ast::Stmt::ClassDecl),
            Some(token![fit])      => Some(self.expect_fit_class_decl()?),
            Some(token![import])   => self.expect_import_decl()?,
            Some(token![global])   => Some(self.expect_global_decl()?),
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

        let ty = match self.match_(token![:]) {
            Some(_) => Some(self.expect_type()?),
            None    => None,
        };

        self.expect(token![=])?;
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
        let Some(peek) = self.peek_token() else {
            return Ok(None)
        };

        let pat = match peek {
            token!["["] => {
                self.push_loc_block("match_decl_pat list");
                self.expect(token!["["])?;
                let tpl = self.expect_closing_tuple_of(
                    Parser::match_decl_pat, 
                    token!["]"], 
                    ParseErr::ExpectedPattern
                )?;

                Located::new(ast::Pat::List(tpl), self.pop_loc_block("match_decl_pat list").unwrap())
            },
            token![..] => {
                self.push_loc_block("match_decl_pat spread");
                self.expect(token![..])?;
                let item = self.match_decl_pat()?.map(Box::new);

                Located::new(ast::Pat::Spread(item), self.pop_loc_block("match_decl_pat spread").unwrap())
            }
            token![mut] | token![#] | Token::Ident(_) => {
                self.push_loc_block("match_decl_pat unit");
                let mt = match self.match_(token![mut]) {
                    Some(_) => ast::MutType::Mut,
                    None    => ast::MutType::Immut,
                };

                let node = ast::DeclUnit(self.expect_ident()?.0, mt);
                Located::new(ast::Pat::Unit(node), self.pop_loc_block("match_decl_pat unit").unwrap())
            },
            _ => return Ok(None)
        };

        Ok(Some(pat))
    }

    /// Match the next token to a reassignment type if it represents one.
    pub fn match_reasg_type(&mut self) -> Option<ast::ReasgType> {
        match *self.match_([token![let], token![const]])? {
            token![let]   => Some(ast::ReasgType::Let),
            token![const] => Some(ast::ReasgType::Const),
            _ => unreachable!()
        }
    }
    /// Match the next tokens in the input if they represent a function parameter.
    pub fn match_param(&mut self) -> ParseResult<Option<ast::Param>> {
        let mrt = self.match_reasg_type();
        let (mut empty, rt) = match mrt {
            Some(t) => (false, t),
            None => (true, Default::default()),
        };
        
        let mt = match self.match_(token![mut]) {
            Some(_) => {
                empty = false;
                ast::MutType::Mut
            }
            None => ast::MutType::Immut,
        };

        // the param checked so far is fully empty and probably not an actual param:
        if empty && self.has_ident().is_none() {
            return Ok(None);
        }

        let ident = self.expect_ident()?.0;
        let ty = match self.match_(token![:]) {
            Some(_) => Some(self.expect_type()?),
            None    => None,
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
        self.expect(token![return])?;
        let e = self.match_expr()?;

        Ok(ast::Stmt::Return(e))
    }

    /// Expect that the next tokens in the input represent a break statement.
    pub fn expect_break(&mut self) -> ParseResult<ast::Stmt> {
        self.expect(token![break])?;

        Ok(ast::Stmt::Break)
    }
    /// Expect that the next tokens in the input represent a continue statement.
    pub fn expect_cont(&mut self) -> ParseResult<ast::Stmt> {
        self.expect(token![continue])?;

        Ok(ast::Stmt::Continue)
    }
    /// Expect that the next tokens in the input represent a throw statement.
    pub fn expect_throw(&mut self) -> ParseResult<ast::Stmt> {
        self.expect(token![throw])?;
        
        let loc = self.peek_loc();
        if let Some(Token::Str(msg)) = self.next_token() {
            Ok(ast::Stmt::Throw(msg))
        } else {
            Err(ParseErr::ExpectedStrLiteral.at_range(loc))
        }
    }

    /// Expect that the next tokens in the input represent a function signature.
    pub fn expect_fun_sig(&mut self) -> ParseResult<ast::FunSignature> {
        self.expect(token![fun])?;

        let ident = self.expect_ident()?.0;

        let generics = if self.match_(token![<]).is_some() {
            let (generics, _) = self.expect_tuple_of(Parser::match_ident)?;
           self.expect(token![>])?;

           generics.into_iter().map(|t| t.0).collect()
        } else {
            vec![]
        };

        self.expect(token!["("])?;
        let params = self.expect_closing_tuple_of(
            Parser::match_param,
            token![")"], 
            ParseErr::ExpectedParam
        )?;

        let ret = match self.match_(token![->]) {
            Some(_) => Some(self.expect_type()?),
            None    => None,
        };

        Ok(ast::FunSignature { ident, generics, params, varargs: false, ret })
    }

    /// Expect that the next tokens in the input represent a function declaration.
    pub fn expect_fun_decl(&mut self) -> ParseResult<ast::FunDecl> {
        let sig = self.expect_fun_sig()?;
        let block = self.expect_block()?;

        Ok(ast::FunDecl {
            sig,
            block: block.map(Rc::new)
        })
    }

    /// Expect that the next tokens in the input represent a class declaration.
    pub fn expect_class_decl(&mut self) -> ParseResult<ast::Class> {
        self.expect(token![class])?;
        let ident = self.expect_ident()?.0;
        
        let generics = if self.match_(token![<]).is_some() {
            let (generics, _) = self.expect_tuple_of(Parser::match_ident)?;
           self.expect(token![>])?;

           generics.into_iter().map(|t| t.0).collect()
        } else {
            vec![]
        };

        self.expect(token!["{"])?;
        let (fields, _) = self.expect_tuple_of(Parser::match_field_decl)?;
        
        let mut methods = vec![];
        while let Some(m) = self.match_method_decl()? {
            methods.push(m);
        }
        self.expect(token!["}"])?;

        Ok(ast::Class { ident, generics, fields, methods })
    }

    /// Match the next tokens if they represent a field declaration.
    pub fn match_field_decl(&mut self) -> ParseResult<Option<ast::FieldDecl>> {
        let (mut empty, rt) = match self.match_reasg_type() {
            Some(t) => (false, t),
            None => (true, Default::default()),
        };

        let mt = match self.match_(token![mut]) {
            Some(_) => {
                empty = false;
                ast::MutType::Mut
            }
            None => ast::MutType::Immut,
        };

        if empty && self.has_ident().is_none() {
            return Ok(None);
        }

        let ident = self.expect_ident()?.0;
        self.expect(token![:])?;
        let ty = self.expect_type()?;

        Ok(Some(ast::FieldDecl { rt, mt, ident, ty }))
    }

    /// Expect that the next tokens represent a method identifier 
    /// (self.ident, Self::ident, .ident, ::ident)
    /// 
    /// The return of this function holds the self parameter, the identifier, 
    /// and whether or not the method is static.
    pub fn expect_method_ident(&mut self) -> ParseResult<(Option<String>, String, bool)> {
        let referent = self.match_ident()?.map(|ls| ls.0);

        let is_static = match self.expect(&[token![.], token![::]])?.tt {
            token![.] => false,
            token![::] => true,
            _ => unreachable!()
        };
        let method = self.expect_ident()?.0;

        Ok((referent, method, is_static))
    }

    /// Expect that the next tokens represent a method signature.
    pub fn expect_method_sig(&mut self) -> ParseResult<ast::MethodSignature> {
        let (referent, method_name, is_static) = self.expect_method_ident()?;

        let generics = if self.match_(token![<]).is_some() {
            let (generics, _) = self.expect_tuple_of(Parser::match_ident)?;
           self.expect(token![>])?;

           generics.into_iter().map(|t| t.0).collect()
        } else {
            vec![]
        };

        self.expect(token!["("])?;
        let params = self.expect_closing_tuple_of(
            Parser::match_param,
            token![")"], 
            ParseErr::ExpectedParam
        )?;

        let ret = match self.match_(token![->]) {
            Some(_) => Some(self.expect_type()?),
            None    => None,
        };

        Ok(ast::MethodSignature {
            referent,
            is_static,
            name: method_name,
            generics,
            params,
            ret,
        })
    }

    /// Match the next tokens if they represent a method declaration.
    pub fn match_method_decl(&mut self) -> ParseResult<Option<ast::MethodDecl>> {
        self.match_(token![fun]).map(|_| {
            let sig = self.expect_method_sig()?;
            let block = self.expect_block()?;

            Ok(ast::MethodDecl { sig, block: block.map(Rc::new) })
        }).transpose()
    }

    /// Expect the next tokens are an import declaration or "import intrinsic" declaration.
    /// 
    /// If this is an "import intrinsic" declaration, return None and switch the intrinsic mode.
    /// Otherwise, return the import declaration.
    pub fn expect_import_decl(&mut self) -> ParseResult<Option<ast::Stmt>> {
        self.expect(token![import])?;
        
        let ident = self.expect_ident()?
            .map(|id| ast::Type(id, vec![]));
        if let "intrinsic" = (*ident).0.as_str() {
            self.intrinsic_mode = true;
            Ok(None)
        } else {
            self.expect(token![::])?;
            let sub = self.expect_ident()?.0;
            Ok(Some(ast::Stmt::Import(ast::StaticPath {
                ty: ident,
                attr: sub,
            })))
        }
    }

    /// Expect the next tokens are an intrinsic global value.
    /// 
    /// This will error if the program is not in intrinsic mode.
    pub fn expect_global_decl(&mut self) -> ParseResult<ast::Stmt> {
        if self.intrinsic_mode {
            self.expect(token![global])?;
            let ident = self.expect_ident()?.0;
            self.expect(token![=])?;
            
            let literal_tok = self.peek_loc();
            let ast::Expr::Literal(literal) = self.expect_literal()? else { unreachable!() };
            let s = match literal {
                ast::Literal::Char(c) => String::from(c),
                ast::Literal::Str(s)  => s,
                _ => Err(ParseErr::ExpectedStrLiteral.at_range(literal_tok))?
            };

            Ok(ast::Stmt::IGlobal(ident, s))
        } else {
            Err(ParseErr::NoIntrinsicStmts.at_range(self.peek_loc()))
        }
    }

    /// Expect the next tokens are an intrinsic fit class statement.
    /// 
    /// This will error if the program is not in intrinsic mode.
    pub fn expect_fit_class_decl(&mut self) -> ParseResult<ast::Stmt> {
        if self.intrinsic_mode {
            self.expect(token![fit])?;
            self.expect(token![class])?;
            let ty = self.expect_type()?;
            self.expect(token!["{"])?;
                let mut methods = vec![];
                while let Some(m) = self.match_method_decl()? {
                    methods.push(m);
                }
            self.expect(token!["}"])?;

            Ok(ast::Stmt::FitClassDecl(ty, methods))
        } else {
            Err(ParseErr::NoIntrinsicStmts.at_range(self.peek_loc()))
        }
    }

    /// Check if the next input is an identifier.
    /// 
    /// If the next input is an identifier, this function returns the number of tokens
    /// held by this identifier.
    pub fn has_ident(&self) -> Option<usize> {
        match (self.peek_token(), self.peek_nth_token(1)) {
            (Some(Token::Ident(_)), _) => Some(1),
            (Some(token![#]), Some(Token::Ident(_))) if self.intrinsic_mode => Some(2),
            _ => None
        }
    }

    /// Match the next token in input if it is an identifier token,
    /// returning the identifier's string if successfully matched.
    pub fn match_ident(&mut self) -> ParseResult<Option<Located<String>>> {
        match self.has_ident() {
            None => Ok(None),
            Some(1) => {
                let Some(FullToken { tt: Token::Ident(s), loc }) = self.next() else { unreachable!() };

                Ok(Some(Located::new(s, loc)))
            },
            Some(2) => {
                self.push_loc_block("match_ident");

                self.expect(token![#])?;
                let Some(Token::Ident(s)) = self.next_token() else { unreachable!() };

                let ident_loc = self.pop_loc_block("match_ident").unwrap();

                if self.intrinsic_mode {
                    Ok(Some(Located::new(format!("#{s}"), ident_loc)))
                } else {
                    Err(ParseErr::NoIntrinsicIdents.at_range(ident_loc))
                }
            },
            s => unreachable!("has_ident should not return {s:?}")
        }
    }

    /// Expect that the next token in the input is an identifier token,
    /// returning the identifier's string if successfully matched.
    pub fn expect_ident(&mut self) -> ParseResult<Located<String>> {
        self.match_ident()?
            .ok_or_else(|| ParseErr::ExpectedIdent.at_range(self.peek_loc()))
    }

    /// Match the next tokens in the input if they represent a type expression.
    /// 
    /// This is used to enable [`parser::expect_tuple_of(Parser::match_type)`][`Parser::expect_tuple_of`].
    /// The function that *should* be used for type expression parsing purposes is [`Parser::expect_type`].
    fn match_type(&mut self) -> ParseResult<Option<Located<ast::Type>>> {
        self.push_loc_block("match_type");
        if self.has_ident().is_some() {
            let ident = self.expect_ident()?.0;

            let params = if self.match_(token![<]).is_some() {
                let token_pos = self.peek_loc();

                let tpl = self.expect_closing_tuple_of(
                    Parser::match_type,
                    token![>],
                    ParseErr::ExpectedType
                )?;

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

            let result = Located::new(ast::Type(ident, params), self.pop_loc_block("match_type").unwrap());
            Ok(Some(result))
        } else {
            self.pop_loc_block("match_type");
            Ok(None)
        }
    }

    /// Expect that the next tokens in the input represent a type expression.
    pub fn expect_type(&mut self) -> ParseResult<Located<ast::Type>> {
        self.match_type()?
            .ok_or_else(|| ParseErr::ExpectedType.at_range(self.peek_loc()))
    }

    /// Expect that the next tokens in input represent some expression.
    pub fn expect_expr(&mut self) -> ParseResult<Located<ast::Expr>> {
        self.match_expr()?
            .ok_or_else(|| ParseErr::ExpectedExpr.at_range(self.peek_loc()))
    }

    /// Match the next tokens in input if they represent any expression.
    /// 
    /// The next expression above in precedence is [`Parser::match_asg`].
    pub fn match_expr(&mut self) -> ParseResult<Option<Located<ast::Expr>>> {
        self.match_asg()
    }

    /// Match the next tokens in input if they represent an assignment operation (`a = b`)
    /// or any expression with higher precedence.
    /// 
    /// The next expression above in precedence is [`Parser::match_lor`].
    pub fn match_asg(&mut self) -> ParseResult<Option<Located<ast::Expr>>> {
        // a = b = c = d = e = [expr]
        let mut pats = vec![];
        
        self.push_loc_block("match_asg");
        let mut last = self.match_lor()?;
        // TODO: asg ops

        let mut eq_pos = self.peek_loc();
        while self.match_(token![=]).is_some() {
            // if there was an equal sign, this expression must've been a pattern:
            let pat_expr = last
                .ok_or_else(|| ParseErr::ExpectedPattern.at_range(eq_pos.clone()))?;
            let pat = ast::AsgPat::try_from(pat_expr)
                .map_err(|e| e.map(ParseErr::AsgPatErr))?;
            
            pats.push(pat);

            self.push_loc_block("match_asg");
            last = self.match_lor()?;
            eq_pos = self.peek_loc();
        }

        // this is the RHS block's loc block (which is already recorded)
        // and can be trashed
        self.pop_loc_block("match_asg");
        if !pats.is_empty() {
            let rhs = last
                .ok_or_else(|| ParseErr::ExpectedExpr.at_range(self.peek_loc()))?;

            let asg = pats.into_iter()
                .rfold(rhs, |e, pat| {
                    let range = self.pop_loc_block("match_asg").unwrap();
                    
                    Located::new(
                        ast::Expr::Assign(pat, Box::new(e)),
                        range
                    )
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
    pub fn match_cmp(&mut self) -> ParseResult<Option<Located<ast::Expr>>> {
        self.push_loc_block("match_cmp");
        let me = self.match_spread()?;

        if let Some(mut e) = me {
            const CMP_OPS: [Token; 6] = [
                token![<], token![>], 
                token![<=], token![>=], 
                token![==], token![!=]
            ];

            // check if there's a comparison here
            let mut rights = vec![];
            while let Some(t) = self.match_(CMP_OPS) {
                let op = t.tt.try_into().unwrap();
                let rexpr = self.match_spread()?
                    .ok_or_else(|| ParseErr::ExpectedExpr.at_range(self.peek_loc()))?;
                
                rights.push((op, rexpr));

            }

            let range = self.pop_loc_block("match_cmp");
            if !rights.is_empty() {
                let cmp_expr = ast::Expr::Comparison { 
                    left: Box::new(e), 
                    rights 
                };

                e = Located::new(cmp_expr, range.unwrap());
            }

            Ok(Some(e))
        } else {
            self.pop_loc_block("match_cmp");
            Ok(None)
        }
    }

    /// Match the next tokens in input if they represent an spread expression (`..[1, 2, 3]`)
    /// or any expression with higher precedence.
    /// 
    /// The next expression above in precedence is [`Parser::match_range`].
    pub fn match_spread(&mut self) -> ParseResult<Option<Located<ast::Expr>>> {
        self.push_loc_block("match_spread");
        if self.match_(token![..]).is_some() {
            let me = self.match_range()?;
            let inner = me.map(Box::new);

            let spread_expr = ast::Expr::Spread(inner);
            let e = Located::new(spread_expr, self.pop_loc_block("match_spread").unwrap());
            Ok(Some(e))
        } else {
            self.pop_loc_block("match_spread");
            self.match_range()
        }
    }

    /// Match the next tokens in input if they represent an range expression (`1..5`)
    /// or any expression with higher precedence.
    /// 
    /// The next expression above in precedence is [`Parser::match_bor`].
    pub fn match_range(&mut self) -> ParseResult<Option<Located<ast::Expr>>> {
        self.push_loc_block("match_range");
        if let Some(mut e) = self.match_bor()? {
            if self.match_(token![..]).is_some() {
                let right = self.match_bor()?
                .ok_or_else(|| ParseErr::ExpectedExpr.at_range(self.peek_loc()))?;

                let step = if self.match_(token![step]).is_some() {
                    let sexpr = self.match_bor()?
                    .ok_or_else(|| ParseErr::ExpectedExpr.at_range(self.peek_loc()))?;
                    Some(sexpr)
                } else {
                    None
                };

                let range_expr = ast::Expr::Range {
                    left: Box::new(e),
                    right: Box::new(right),
                    step: step.map(Box::new)
                };
                e = Located::new(range_expr, self.pop_loc_block("match_range").unwrap());
            } else {
                self.pop_loc_block("match_range");
            }

            Ok(Some(e))
        } else {
            self.pop_loc_block("match_range");
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
    /// The next expression above in precedence is [`Parser::match_deref`].
    pub fn match_unary(&mut self) -> ParseResult<Option<Located<ast::Expr>>> {
        const UNARY_OPS: [Token; 4] = [token![!], token![~], token![-], token![+]];

        self.push_loc_block("match_unary");
        let mut ops = vec![];
        while let Some(t) = self.match_(UNARY_OPS) {
            ops.push(t.tt.try_into().unwrap());
        }

        let me = if ops.is_empty() {
            self.pop_loc_block("match_unary");
            self.match_deref()?
        } else {
            let e = self.match_deref()?
                .ok_or_else(|| ParseErr::ExpectedExpr.at_range(self.peek_loc()))?;

            let range = self.pop_loc_block("match_unary").unwrap();
            Some(Parser::wrap_unary_op(ops, e, range))
        };

        Ok(me)
    }

    /// Helper function that constructs a [`ast::Expr::UnaryOps`] node.
    /// 
    /// It takes the inner expression and acts as though the unary operators were applied to it.
    /// If the inner expression is a uops node, this also flattens the operators 
    /// (so that we don't have a unary operators node wrapping another one)
    fn wrap_unary_op(
        mut ops: Vec<ast::op::Unary>, 
        inner: Located<ast::Expr>, 
        range: CursorRange
    ) -> Located<ast::Expr> {
        // flatten if unary ops inside
        if let Located(ast::Expr::UnaryOps { ops: ops2, expr }, _) = inner {
            ops.extend(ops2);
            Located(ast::Expr::UnaryOps {
                ops, expr
            }, range)
        } else {
            // wrap otherwise
            Located(ast::Expr::UnaryOps {
                ops,
                expr: Box::new(inner)
            }, range)
        }
    }

    /// Match the next tokens in input if they represent an intrinsic dereferencing expression.
    /// (`*self.a`)
    /// or any expression with higher precedence.
    /// 
    /// The next expression above in precedence is [`Parser::match_call_index_path`].
    pub fn match_deref(&mut self) -> ParseResult<Option<Located<ast::Expr>>> {
        let peek = self.peek_loc();
        let is_deref_expr = self.match_(token![*]).is_some();
        let ci = self.match_call_index_path();

        if is_deref_expr {
            let inner = ci?.ok_or_else(|| ParseErr::ExpectedExpr.at_range(self.peek_loc()))?;

            let outer_range = merge_ranges(peek, inner.range());
            
            let deref_expr = Located::new(ast::Expr::Deref(
                ast::IDeref(Box::new(inner))
            ), outer_range);
            Ok(Some(deref_expr))
        } else {
            ci
        }
    }

    /// Match the next tokens in input if they represent an indexing operation, function call, or path.
    /// (`a[1]`, `f(1, 2, 3, 4)`)
    /// or any expression with higher precedence.
    /// 
    /// The next expression above in precedence is [`Parser::match_unit`].
    pub fn match_call_index_path(&mut self) -> ParseResult<Option<Located<ast::Expr>>> {
        self.push_loc_block("match_call_index_path");
        if let Some(mut e) = self.match_unit()? {
            while let Some(delim) = self.match_(&[token!["("], token!["["], token![.]]) {
                match delim.tt {
                    token!["("] => {
                        let params = self.expect_closing_tuple(token![")"])?;
                        let range = self.peek_loc_block().unwrap();
                        
                        let call_expr = ast::Expr::Call {
                            funct: Box::new(e), 
                            params
                        };
                        e = Located::new(call_expr, range);
                    },
                    token!["["] => {
                        let index = self.expect_expr()?;
                        self.expect(token!["]"])?;
                        let range = self.peek_loc_block().unwrap();

                        let idx_expr = ast::Expr::Index(ast::Index {
                            expr: Box::new(e), 
                            index: Box::new(index)
                        });
                        e = Located::new(idx_expr, range);
                    },
                    token![.] => {
                        let attr = self.expect_ident()?.0;
                        let loc_block = self.peek_loc_block().unwrap();

                        e = match e {
                            Located(ast::Expr::Path(ast::Path { ref mut attrs, .. }), ref mut range) => {
                                attrs.push(attr);
                                *range = loc_block;
                                e
                            },
                            obj => {
                                Located::new(ast::Expr::Path(ast::Path {
                                    obj: Box::new(obj),
                                    attrs: vec![attr]
                                }), loc_block)
                            }
                        };
                    }
                    _ => unreachable!()
                }
            }

            self.pop_loc_block("match_call_index_path");
            Ok(Some(e))
        } else {
            self.pop_loc_block("match_call_index_path");
            Ok(None)
        }
    }

    /// Match the next tokens in input if they represent any expression 
    /// with a higher precedence with a path.
    pub fn match_unit(&mut self) -> ParseResult<Option<Located<ast::Expr>>> {
        self.push_loc_block("match_unit");

        let Some(peek) = self.peek_token() else {
            self.pop_loc_block("match_unit");
            return Ok(None);
        };

        let unit = match peek {
            token![#] | Token::Ident(_) => self.expect_identoid()?,
            token![<] | token![<<]      => self.expect_diamondoid()?,
            Token::Numeric(_) | Token::Str(_) | Token::Char(_) | token![true] | token![false] => self.expect_literal()? ,
            token!["["]       => self.expect_list()?,
            token!["{"]       => self.expect_block().map(ast::Expr::Block)?,
            token![if]        => self.expect_if()?,
            token![while]     => self.expect_while()?,
            token![for]       => self.expect_for()?,
            token!["("] => {
                self.expect(token!["("])?;
                let e = self.expect_expr()?;
                self.expect(token![")"])?;
        
                // We can drop the inner expr in favor of the outer one,
                // it doesn't really matter.
                e.0
            },
            _ => {
                self.pop_loc_block("match_unit");
                return Ok(None);
            }
        };

        Ok(Some(Located::new(unit, self.pop_loc_block("match_unit").unwrap())))
    }

    /// Expect that the next tokens in input represent some primitive literal (int, str, char, etc.).
    pub fn expect_literal(&mut self) -> ParseResult<ast::Expr> {
        let FullToken { tt, loc } = self.next().expect("unreachable");
        
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
        self.expect(token!["["])?;
        let exprs = self.expect_closing_tuple(token!["]"])?;
        
        Ok(ast::Expr::ListLiteral(exprs))
    }

    /// Entry for class initializers
    fn match_init_entry(&mut self) -> ParseResult<Option<(Located<String>, Located<ast::Expr>)>> {
        let krange = self.peek_loc();
        if let Some(Located(k, _)) = self.match_ident()? {
            let v = match self.match_(token![:]) {
                Some(_) => self.expect_expr()?,
                None    => Located::new(ast::Expr::Ident(k.clone()), krange.clone()),
            };
            
            Ok(Some((Located::new(k, krange), v)))
        } else {
            Ok(None)
        }
    }

    /// Expect that the next tokens are some expression that starts with an identifier.
    /// 
    /// This can be:
    /// * a set literal (`set {1, 2, 3}`), 
    /// * a dict literal (`dict {"a": 1, "b": 2, "c": 3}`),
    /// * a class initializer (`Class {a: 2, b: 3, c: 4}`),
    /// * a static path (`Type::attr`), or
    /// * an identifier (`x`)
    pub fn expect_identoid(&mut self) -> ParseResult<ast::Expr> {
        type Entry = (Located<ast::Expr>, Located<ast::Expr>);
        /// Entry for dict literals
        fn match_entry(this: &mut Parser) -> ParseResult<Option<Entry>> {
            if let Some(k) = this.match_expr()? {
                this.expect(token![:])?;
                let v = this.expect_expr()?;
                Ok(Some((k, v)))
            } else {
                Ok(None)
            }
        }

        let next_token_pos = self.has_ident()
            .ok_or_else(|| ParseErr::ExpectedIdent.at_range(self.peek_loc()))?;
        let e = match self.tokens.get(next_token_pos) {
            Some(FullToken { tt: token![#], .. }) => {
                let ty = self.expect_type()?;

                self.expect(token![#])?;
                self.expect(token!["{"])?;
                match ty.0.0.as_str() {
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
                        let params = self.expect_closing_tuple_of(
                            Parser::match_init_entry, 
                            token!["}"], 
                            ParseErr::ExpectedIdent
                        )?;
                        ast::Expr::ClassLiteral(ty, params)
                    }
                }
            },
            Some(FullToken { tt: token![::], .. }) => {
                let ty = self.expect_type()?;
                self.expect(token![::])?;
                let attr = self.expect_ident()?.0;

                ast::Expr::StaticPath(ast::StaticPath { ty, attr })
            },
            _ => ast::Expr::Ident(self.expect_ident()?.0)
        };

        Ok(e)
    }

    /// Expect that the next tokens are some expression that starts with a type diamond.
    /// 
    /// This can be:
    /// * a class initializer (`<Class<T>> {a: 2, b: 3, c: 4}`), or
    /// * a static path (`<Type<T>>::attr`)
    pub fn expect_diamondoid(&mut self) -> ParseResult<ast::Expr> {
        self.expect(token![<])?;
        let ty = self.expect_type()?;
        self.expect(token![>])?;

        let loc = self.peek_loc();
        let e = match self.next_token() {
            Some(token![#]) => {
                self.expect(token!["{"])?;
                let params = self.expect_closing_tuple_of(
                    Parser::match_init_entry, 
                    token!["}"], 
                    ParseErr::ExpectedIdent
                )?;
                ast::Expr::ClassLiteral(ty, params)
            },
            Some(token![::]) => {
                let attr = self.expect_ident()?.0;
    
                ast::Expr::StaticPath(ast::StaticPath { ty, attr })
            },
            _ => Err(expected_tokens![::, #].at_range(loc))?
        };

        Ok(e)
    }

    /// Expect that the next tokens in input represent an if-else expression 
    /// (`if cond {}`, `if cond {} else cond {}`, etc.)
    pub fn expect_if(&mut self) -> ParseResult<ast::Expr> {
        self.expect(token![if])?;

        let mut conditionals = vec![(self.expect_expr()?, self.expect_block()?)];
        let mut last = None;

        while self.match_(token![else]).is_some() {
            match self.peek_token() {
                Some(&token![if]) => {
                    self.expect(token![if])?;
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
    pub fn expect_while(&mut self) -> ParseResult<ast::Expr> {
        self.expect(token![while])?;
        let condition = self.expect_expr()?;
        let block = self.expect_block()?;
        
        Ok(ast::Expr::While {
            condition: Box::new(condition), block
        })
    }

    /// Expect that the next tokens in input represent an `for` loop.
    pub fn expect_for(&mut self) -> ParseResult<ast::Expr> {
        self.expect(token![for])?;
        let ident = self.expect_ident()?.0;
        self.expect(token![in])?;
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
    macro_rules! block {
        ($($e:expr),*; $loc:expr) => {
            Located::new(Block(vec![$($e),*]), $loc)
        }
    }
    macro_rules! block_expr {
        ($($e:expr),*; $loc:expr) => {
            Expr::Block(block![$($e),*; $loc])
        }
    }
    
    macro_rules! expr_stmt {
        ($e:expr, $loc:expr) => {
            Located::new(Stmt::Expr(Located::new($e, $loc)), $loc)
        }
    }
    fn add_one(cr: CursorRange) -> CursorRange {
        let &(el, ec) = cr.end();
        *cr.start() ..= (el, ec + 1)
    }
    macro_rules! expr_stmt_with_semi {
        ($e:expr, $loc:expr) => {
            Located::new(Stmt::Expr(Located::new($e, $loc)), add_one($loc))
        }
    }
    macro_rules! binop {
        ($op:ident, $eloc:expr, $left:expr, $right:expr) => {
            Located::new(Expr::BinaryOp {
                op: op::Binary::$op,
                left: Box::new($left),
                right: Box::new($right)
            }, $eloc)
        };
    }

    macro_rules! ident {
        ($ident:literal, $range:expr) => {
            Located::new(Expr::Ident(String::from($ident)), $range)
        }
    }

    macro_rules! literal {
        ($ident:ident($e:expr), $range:expr) => {
            Located::new(Expr::Literal(Literal::$ident($e)), $range)
        };
        ($l:literal, $range:expr) => {
            Located::new(Expr::Literal(Literal::Str(String::from($l))), $range)
        }
    }

    macro_rules! asg_unit {
        ($id:literal) => {
            AsgUnit::Ident(String::from($id))
        }

    }
    macro_rules! decl_unit {
        (mut $id:literal) => {
            DeclUnit(String::from($id), MutType::Mut)
        };
        ($id:literal) => {
            DeclUnit(String::from($id), MutType::Immut)
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
            Located::new(Stmt::Expr(binop! {
                Add, (0, 0) ..= (0, 4),
                literal!(Int(2), (0, 0) ..= (0, 0)),
                literal!(Int(3), (0, 4) ..= (0, 4))
            }), (0, 0) ..= (0, 5))
        ]);

        assert_parse("2 + 3 * 4;", program![
            Located::new(Stmt::Expr(binop! {
                Add, (0, 0) ..= (0, 8),
                literal!(Int(2), (0, 0) ..= (0, 0)),
                binop! {
                    Mul, (0, 4) ..= (0, 8),
                    literal!(Int(3), (0, 4) ..= (0, 4)),
                    literal!(Int(4), (0, 8) ..= (0, 8))
                }
            }), (0, 0) ..= (0, 9))
        ]);
    }

    #[test]
    fn block_test() {
        assert_parse("{}", program![
            expr_stmt!(block_expr![;(0, 0)..=(0, 1)], (0, 0)..=(0, 1))
        ]);

        assert_parse("{{}}", program![
            expr_stmt!(block_expr![
                expr_stmt!(block_expr![;(0, 1) ..= (0, 2)], (0, 1) ..= (0, 2));
            (0, 0) ..= (0, 3)], (0, 0)..=(0, 3))
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
            expr_stmt!(Expr::If {
                conditionals: vec![
                    (literal!(Bool(true), (1, 15) ..= (1, 18)), block![; (1, 20) ..= (3, 12)])
                ],
                last: None
            }, (1, 12)..=(3, 12))
        ]);

        assert_parse("
            if true {
                // :)
            } else {
                // :(
            }
        ", program![
            expr_stmt!(Expr::If { 
                conditionals: vec![
                    (literal!(Bool(true), (1, 15) ..= (1, 18)), block![; (1, 20) ..= (3, 12)])
                ],
                last: Some(block![; (3, 19) ..= (5, 12)])
            }, (1, 12) ..= (5, 12))
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
            expr_stmt!(Expr::If { 
                conditionals: vec![
                    (literal!(Bool(true), (1, 15) ..= (1, 18)), block![; (1, 20) ..= (3, 12)]),
                    (ident!("condition", (3, 22) ..= (3, 30)), block![; (3, 32) ..= (5, 12)])
                ],
                last: Some(block![; (5, 19) ..= (7, 12)])
            }, (1, 12) ..= (7, 12))
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
            expr_stmt!(Expr::If { 
                conditionals: vec![
                    (literal!(Bool(true), (1, 15) ..= (1, 18)), block![; (1, 20) ..= (3, 12)]),
                    (ident!("condition", (3, 22) ..= (3, 30)), block![; (3, 32) ..= (5, 12)]),
                    (ident!("condition", (5, 22) ..= (5, 30)), block![; (5, 32) ..= (7, 12)]),
                    (ident!("condition", (7, 22) ..= (7, 30)), block![; (7, 32) ..= (9, 12)]),
                    (ident!("condition", (9, 22) ..= (9, 30)), block![; (9, 32) ..= (11, 12)]),
                ],
                last: Some(block![; (11, 19) ..= (13, 12)])
            }, (1, 12) ..= (13, 12))
        ]);
    }

    /// Tests while and for loop as well as 
    /// declarations, function calls, conditionals, assignment, ranges, and literals.
    #[test]
    fn loop_test() {
        // barebones
        assert_parse("while true {}", program![
            expr_stmt!(Expr::While {
                condition: Box::new(literal!(Bool(true), (0, 6) ..= (0, 9))),
                block: block![; (0, 11) ..= (0, 12)]
            }, (0, 0) ..= (0, 12))
        ]);
        assert_parse("for i in it {}", program![
            expr_stmt!(Expr::For {
                ident: String::from("i"),
                iterator: Box::new(ident!("it", (0, 9) ..= (0, 10))),
                block: block![; (0, 12) ..= (0, 13)]
            }, (0, 0) ..= (0, 13))
        ]);

        // full examples
        assert_parse("
            let i = 0;
            while i < 10 {
                print(i);
                i = i + 1;
            }
        ", program![
            Located::new(Stmt::Decl(Decl { 
                rt: ReasgType::Let, 
                pat: Located::new(Pat::Unit(decl_unit!("i")), (1, 16) ..= (1, 16)), 
                ty: None, 
                val: literal!(Int(0), (1, 20) ..= (1, 20))
            }), (1, 12) ..= (1, 21)),
            expr_stmt!(Expr::While {
                condition: Located::boxed(Expr::Comparison {
                    left: Box::new(ident!("i", (2, 18) ..= (2, 18))), 
                    rights: vec![(op::Cmp::Lt, literal!(Int(10), (2, 22) ..= (2, 23)))]
                }, (2, 18) ..= (2, 23)), 
                block: block![
                    expr_stmt_with_semi!(Expr::Call {
                        funct: Box::new(ident!("print", (3, 16) ..= (3, 20))),
                        params: vec![ident!("i", (3, 22) ..= (3, 22))]
                    }, (3, 16) ..= (3, 23)),
                    expr_stmt_with_semi!(Expr::Assign(
                        Located::new(Pat::Unit(asg_unit!("i")), (4, 16) ..= (4, 16)), 
                        Box::new(binop! {
                            Add, (4, 20) ..= (4, 24),
                            ident!("i", (4, 20) ..= (4, 20)),
                            literal!(Int(1), (4, 24) ..= (4, 24))
                        })
                    ), (4, 16) ..= (4, 24));
                (2, 25) ..= (5, 12)]
            }, (2, 12) ..= (5, 12))
        ]);

        assert_parse("for i in 1..10 { print(i); }", program![
            expr_stmt!(Expr::For {
                ident: String::from("i"), 
                iterator: Located::boxed(Expr::Range {
                    left: Box::new(literal!(Int(1), (0, 9) ..= (0, 9))), 
                    right: Box::new(literal!(Int(10), (0, 12) ..= (0, 13))), 
                    step: None 
                }, (0, 9) ..= (0, 13)), 
                block: block![
                    expr_stmt_with_semi!(Expr::Call {
                        funct: Box::new(ident!("print", (0, 17) ..= (0, 21))),
                        params: vec![ident!("i", (0, 23) ..= (0, 23))]
                    }, (0, 17) ..= (0, 24));
                (0, 15) ..= (0, 27)]
            }, (0, 0) ..= (0, 27))
        ]);
    }

    #[test]
    fn semicolon_test() {
        assert_parse_fail("2 2", expected_tokens![;]);

        assert_parse("if cond {}", program![
            expr_stmt!(Expr::If {
                conditionals: vec![
                    (ident!("cond", (0, 3) ..= (0, 6)), block![; (0, 8) ..= (0, 9)])
                ],
                last: None
            }, (0, 0) ..= (0, 9))
        ]);
        assert_parse("if cond {};", program![
            expr_stmt_with_semi!(Expr::If {
                conditionals: vec![
                    (ident!("cond", (0, 3) ..= (0, 6)), block![; (0, 8) ..= (0, 9)])
                ],
                last: None
            }, (0, 0) ..= (0, 9))
        ]);

        assert_parse("
            let a = 1;
            let b = 2;
            let c = 3;
            if cond {
                let d = 5;
            }
        ", program![
            Located::new(Stmt::Decl(Decl { 
                rt: ReasgType::Let, 
                pat: Located::new(Pat::Unit(decl_unit!("a")), (1, 16) ..= (1, 16)), 
                ty: None, 
                val: literal!(Int(1), (1, 20) ..= (1, 20))
            }), (1, 12) ..= (1, 21)),
            Located::new(Stmt::Decl(Decl { 
                rt: ReasgType::Let, 
                pat: Located::new(Pat::Unit(decl_unit!("b")), (2, 16) ..= (2, 16)), 
                ty: None, 
                val: literal!(Int(2), (2, 20) ..= (2, 20))
            }), (2, 12) ..= (2, 21)),
            Located::new(Stmt::Decl(Decl { 
                rt: ReasgType::Let, 
                pat: Located::new(Pat::Unit(decl_unit!("c")), (3, 16) ..= (3, 16)), 
                ty: None, 
                val: literal!(Int(3), (3, 20) ..= (3, 20))
            }), (3, 12) ..= (3, 21)),
            expr_stmt!(Expr::If {
                conditionals: vec![(
                    ident!("cond", (4, 15) ..= (4, 18)),
                    block![
                        Located::new(Stmt::Decl(Decl { 
                            rt: ReasgType::Let, 
                            pat: Located::new(Pat::Unit(decl_unit!("d")), (5, 20) ..= (5, 20)), 
                            ty: None, 
                            val: literal!(Int(5), (5, 24) ..= (5, 24))
                        }), (5, 16) ..= (5, 25));
                    (4, 20) ..= (6, 12)]
                )],
                last: None
            }, (4, 12) ..= (6, 12))
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
                Located::new(Type("a".to_string(), vec![]), (0, 5) ..= (0, 5)),
                Located::new(Type("b".to_string(), vec![]), (0, 8) ..= (0, 8))
            ])

        );

        assert_parse_type(
            "dict<list<list<int>>, str>",
            Type("dict".to_string(), vec![
                Located::new(Type("list".to_string(), vec![
                    Located::new(Type("list".to_string(), vec![
                        Located::new(Type("int".to_string(), vec![]), (0, 15) ..= (0,17))
                    ]), (0, 10) ..= (0, 18))
                ]), (0, 5) ..= (0, 19)),
                Located::new(Type("str".to_string(), vec![]), (0, 22) ..= (0, 24))
            ])
        );
    }

    #[test]
    fn unary_ops_test() {
        assert_parse("+3;", program![
            expr_stmt_with_semi!(Expr::UnaryOps {
                ops: vec![token![+].try_into().unwrap()],
                expr: Box::new(literal!(Int(3), (0, 1) ..= (0, 1)))
            }, (0, 0) ..= (0, 1))
        ]);

        assert_parse("+++++++3;", program![
            expr_stmt_with_semi!(Expr::UnaryOps {
                ops: vec![token![+].try_into().unwrap()].repeat(7),
                expr: Box::new(literal!(Int(3), (0, 7) ..= (0, 7)))
            }, (0, 0) ..= (0, 7))
        ]);

        assert_parse("+-+-+-+-3;", program![
            expr_stmt_with_semi!(Expr::UnaryOps {
                ops: vec![token![+].try_into().unwrap(), token![-].try_into().unwrap()].repeat(4),
                expr: Box::new(literal!(Int(3), (0, 8) ..= (0, 8)))
            }, (0, 0) ..= (0, 8))
        ]);

        assert_parse("!+-+-+-+-3;", program![
            expr_stmt_with_semi!(Expr::UnaryOps {
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
                expr: Box::new(literal!(Int(3), (0, 9) ..= (0, 9)))
            }, (0, 0) ..= (0, 9))
        ]);

        assert_parse("+(+2);", program![
            expr_stmt_with_semi!(Expr::UnaryOps {
                ops: vec![token![+].try_into().unwrap()].repeat(2),
                expr: Box::new(literal!(Int(2), (0, 3) ..= (0, 3)))
            }, (0, 0) ..= (0, 4))
        ])
    }

    #[test]
    fn decl_test() {
        assert_parse("
            let a       = 0;
            let mut b   = 1;
            const c     = 2;
            const mut d = 3;
        ", program![
            Located::new(Stmt::Decl(Decl { 
                rt: ReasgType::Let, 
                pat: Located::new(Pat::Unit(decl_unit!("a")), (1, 16) ..= (1, 16)), 
                ty: None, 
                val: literal!(Int(0), (1, 26) ..= (1, 26))
            }), (1, 12) ..= (1, 27)),
            Located::new(Stmt::Decl(Decl { 
                rt: ReasgType::Let, 
                pat: Located::new(Pat::Unit(decl_unit!(mut "b")), (2, 16) ..= (2, 20)), 
                ty: None, 
                val: literal!(Int(1), (2, 26) ..= (2, 26))
            }), (2, 12) ..= (2, 27)),
            Located::new(Stmt::Decl(Decl { 
                rt: ReasgType::Const, 
                pat: Located::new(Pat::Unit(decl_unit!("c")), (3, 18) ..= (3, 18)), 
                ty: None, 
                val: literal!(Int(2), (3, 26) ..= (3, 26))
            }), (3, 12) ..= (3, 27)),
            Located::new(Stmt::Decl(Decl { 
                rt: ReasgType::Const, 
                pat: Located::new(Pat::Unit(decl_unit!(mut "d")), (4, 18) ..= (4, 22)), 
                ty: None, 
                val: literal!(Int(3), (4, 26) ..= (4, 26))
            }), (4, 12) ..= (4, 27))
        ])
    }
}