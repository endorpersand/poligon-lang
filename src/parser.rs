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
use crate::span::Span;

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
struct RangeBlock(&'static str, Option<Span>);

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
        todo!()
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
        todo!()
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
        todo!()
    }
}