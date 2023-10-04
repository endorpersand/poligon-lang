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

mod expr;

use std::collections::VecDeque;
use std::convert::Infallible;

use crate::GonErr;
use crate::err::FullGonErr;
use crate::lexer::token::{Token, token, FullToken, SPLITTABLES2};
use crate::ast::{self, PatErr};
use crate::span::{Span, Cursor};

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
// pub fn parse(tokens: impl IntoIterator<Item=FullToken>) -> ParseResult<ast::Program> {
//     Parser::new(tokens, false).parse()
// }

pub trait Parseable: Sized {
    type Err;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err>;
}

/// A cursor that allows simple scanning and traversing over a stream of tokens.
pub struct ParCursor<'s> {
    stream: &'s [FullToken],
    
    /// The end character's range
    eof: Cursor
}
impl<'s> ParCursor<'s> {
    /// Creates a new cursor.
    pub fn new(stream: &'s [FullToken]) -> Self {
        let eof = if let Some(tok) = stream.last() {
            let (lno, cno) = tok.span.end();
            (lno, cno + 1)
        } else {
            (0, 0)
        };

        ParCursor { stream, eof }
    }

    /// Peeks the current token.
    pub fn peek(&self) -> Option<&FullToken> {
        self.stream.first()
    }
    /// Peeks the nth token, with 0 being the current token.
    pub fn peek_n(&self, n: usize) -> Option<&FullToken> {
        self.stream.get(n)
    }

    pub fn peek_span(&self) -> Span {
        match self.peek() {
            Some(t) => t.span,
            None => Span::one(self.eof),
        }
    }

    pub fn next_if(&mut self, f: impl FnOnce(&FullToken) -> bool) -> Option<&FullToken> {
        if f(self.peek()?) {
            self.next()
        } else {
            None
        }
    }

    /// The number of tokens remaining in the stream
    pub fn len(&self) -> usize {
        self.stream.len()
    }
    /// Whether the stream has ended
    pub fn is_empty(&self) -> bool {
        self.stream.is_empty()
    }
    pub fn pointing_at(&self) -> Span {
        match self.peek() {
            Some(tok) => tok.span,
            None => Span::one(self.eof),
        }
    }
    /// Creates an error at the current position.
    pub fn error(&self, p: ParseErr) -> FullParseErr {
        p.at_range(self.pointing_at())
    }

    // pub fn partition(&self, mut f: impl FnMut(&FullToken) -> bool) -> Option<(ParCursor<'s>, FullToken, ParCursor<'s>)> {
    //     let sp = (0 .. self.stream.len())
    //         .find(|&i| f(&self.stream[i]))?;

    //     let left = ParCursor {
    //         stream: &self.stream[0..sp],
    //         half: self.half.clone(),
    //         eof: self.stream[sp].loc.clone(),
    //     };
    //     let mid   = self.stream[sp].clone();
    //     let right = ParCursor::new(&self.stream[sp + 1..]);

    //     Some((left, mid, right))
    // }
}

impl<'s> Iterator for ParCursor<'s> {
    type Item = &'s FullToken;

    fn next(&mut self) -> Option<Self::Item> {
        self.stream.split_first().map(|(first, rest)| {
            self.stream = rest;
            first
        })
    }
}
/// A token pattern.
/// 
/// Similar to std [`std::str::pattern::Pattern`], this can be used to
/// search a pattern in a given Token.
/// 
/// [`Token`] == verify if that token is in the given output
/// [`[Token]`] == verify if any one of those tokens are in the given input
pub trait TokenPattern2 {
    /// Tests if the start of the cursor matches the pattern.
    fn is_prefix_of(&self, cur: &ParCursor) -> bool;

    /// Removes the token pattern from the cursor if it matches.
    /// 
    /// This function returns the prefix stripped.
    fn strip_prefix_of(&self, cur: &mut ParCursor) -> Option<FullToken> {
        if self.is_prefix_of(cur) {
            cur.next().cloned()
        } else {
            None
        }
    }

    /// Provides which tokens this pattern expects.
    fn expected_tokens(&self) -> Vec<Token>;
}

impl TokenPattern2 for Token {
    fn is_prefix_of(&self, cur: &ParCursor) -> bool {
        cur.peek().is_some_and(|tok| {
            // matches the first token, or matches the first half
            (self == tok) || (SPLITTABLES2.get(tok).is_some_and(|(l, _)| l == self))
        })
    }

    fn expected_tokens(&self) -> Vec<Token> {
        std::slice::from_ref(self).to_vec()
    }
}
impl TokenPattern2 for [Token] {
    fn is_prefix_of(&self, cur: &ParCursor) -> bool {
        self.iter().any(|t| t.is_prefix_of(cur))
    }

    fn strip_prefix_of(&self, cur: &mut ParCursor) -> Option<FullToken> {
        self.iter().find_map(|t| t.strip_prefix_of(cur))
    }

    fn expected_tokens(&self) -> Vec<Token> {
        self.to_vec()
    }
}
impl<const N: usize> TokenPattern2 for [Token; N] {
    fn is_prefix_of(&self, cur: &ParCursor) -> bool {
        self.as_slice().is_prefix_of(cur)
    }

    fn strip_prefix_of(&self, cur: &mut ParCursor) -> Option<FullToken> {
        self.as_slice().strip_prefix_of(cur)
    }

    fn expected_tokens(&self) -> Vec<Token> {
        self.as_slice().expected_tokens()
    }
}
impl<'a, P: TokenPattern2 + ?Sized> TokenPattern2 for &'a P {
    fn is_prefix_of(&self, cur: &ParCursor) -> bool {
        (*self).is_prefix_of(cur)
    }

    fn strip_prefix_of(&self, cur: &mut ParCursor) -> Option<FullToken> {
        (*self).strip_prefix_of(cur)
    }

    fn expected_tokens(&self) -> Vec<Token> {
        (*self).expected_tokens()
    }
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
            ParseErr::ExpectedLiteral    => write!(f, "expected literal"),
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
impl From<PatErr> for ParseErr {
    fn from(value: PatErr) -> Self {
        ParseErr::AsgPatErr(value)
    }
}

/// A [`Result`] type for operations in the parsing process.
pub type ParseResult<T> = Result<T, FullParseErr>;
type FullParseErr = FullGonErr<ParseErr>;

macro_rules! expected_tokens {
    ($($t:tt),*) => {
        ParseErr::ExpectedTokens(<[_]>::expected_tokens(&[$(token![$t]),*]))
    }
}

mod tspan {
    use crate::span::Span;

    pub trait TransposeSpan<T>: Sized {
        type Transposed;
        fn transpose(el: (Self, Span)) -> Self::Transposed;
    }

    impl<T> TransposeSpan<T> for Option<T> {
        type Transposed = Option<(T, Span)>;
    
        fn transpose(el: (Self, Span)) -> Self::Transposed {
            el.0.map(|t| (t, el.1))
        }
    }
    impl<T, E> TransposeSpan<T> for Result<T, E> {
        type Transposed = Result<(T, Span), E>;
    
        fn transpose(el: (Self, Span)) -> Self::Transposed {
            el.0.map(|t| (t, el.1))
        }
    }
}

pub struct Parser2<'s> {
    cursor: ParCursor<'s>,
    span_collectors: Vec<Span>
}

impl<'s> Parser2<'s> {
    pub fn parse<P: Parseable>(&mut self) -> Result<P, P::Err> {
        P::read(self)
    }
    pub fn try_parse<P>(&mut self) -> Result<Option<P>, <Option<P> as Parseable>::Err>
        where Option<P>: Parseable
    {
        Option::<P>::read(self)
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
    pub fn expect<P: TokenPattern2>(&mut self, pat: P) -> ParseResult<FullToken> {
        self.match_(&pat)
            .ok_or_else(|| {
                self.cursor.error(ParseErr::ExpectedTokens(pat.expected_tokens()))
            })
    }

    fn attach_to_collector(&mut self, span: Span) {
        if let Some(collector) = self.span_collectors.last_mut() {
            *collector += span;
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
    pub fn match_<P: TokenPattern2>(&mut self, pat: P) -> Option<FullToken> {
        let mat = pat.strip_prefix_of(&mut self.cursor);

        if let Some(m) = &mat {
            self.attach_to_collector(m.span);
        }

        mat
    }

    pub fn peek(&self) -> Option<&FullToken> {
        self.cursor.peek()
    }
    pub fn peek_n(&self, n: usize) -> Option<&FullToken> {
        self.cursor.peek_n(n)
    }

    pub fn spanned<T>(&mut self, f: impl FnOnce(&mut Parser2<'s>) -> T) -> (T, Span) {
        self.try_spanned(|p| Ok::<_, Infallible>(f(p)))
            .unwrap()
    }

    pub fn try_spanned<T, S>(&mut self, f: impl FnOnce(&mut Parser2<'s>) -> S) -> S::Transposed 
        where S: tspan::TransposeSpan<T>
    {
        self.span_collectors.push(self.cursor.peek_span());
        let out = f(self);
        let span = self.span_collectors.pop()
            .expect("span block should have existed");
        self.attach_to_collector(span);

        S::transpose((out, span))
    }

    pub fn parse_tuple<T, E>(&mut self, tok: Token) -> ParseResult<Tuple<T>> 
        where Option<T>: Parseable<Err = E>,
              FullParseErr: From<E>
    {
        
        let ((pairs, end), span) = self.try_spanned(|parser| {
            let mut pairs = vec![];
            let mut end = None;

            while let Some(value) = parser.try_parse::<T>()? {
                if let Some(punct) = parser.match_(&tok) {
                    pairs.push((value, punct));
                } else {
                    end.replace(value);
                    break;
                }
            }

            ParseResult::Ok((pairs, end))
        })?;

        Ok(Tuple { pairs, end, span })
    }
}
impl Iterator for Parser2<'_> {
    type Item = FullToken;

    fn next(&mut self) -> Option<Self::Item> {
        let tok = self.cursor.next();
        
        if let Some(t) = tok {
            self.attach_to_collector(t.span);
        }

        tok.cloned()
    }
}

fn parse_stmts_closed(parser: &mut Parser2<'_>) -> ParseResult<Vec<ast::Stmt>> {
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

            ParseResult::Ok(out)
        }?;
        
        if let Some(stmt) = mstmt {
            stmts.push(stmt);
        }
        cf = flow;
    }

    Ok(stmts)
}

pub struct Tuple<T> {
    pairs: Vec<(T, FullToken)>,
    end: Option<T>,
    span: Span
}
impl<T> Tuple<T> {
    pub fn values(self) -> impl Iterator<Item=T> {
        self.pairs.into_iter()
            .map(|(t, _)| t)
            .chain(self.end)
    }

    pub fn ended_on_terminator(&self) -> bool {
        self.end.is_none()
    }

    pub fn is_empty(&self) -> bool {
        self.pairs.is_empty() && self.end.is_none()
    }
    pub fn len(&self) -> usize {
        self.pairs.len() + if self.end.is_some() { 1 } else { 0 }
    }

    pub fn assert_non_empty<E>(self, e: impl FnOnce() -> E) -> Result<Self, E> {
        match self.is_empty() {
            false => Ok(self),
            true  => Err(e()),
        }
    }
    pub fn assert_closed<P: TokenPattern2, E>(self, parser: &mut Parser2<'_>, p: P, needs_comma: impl FnOnce(&mut Parser2<'_>) -> E, needs_t: impl FnOnce(&mut Parser2<'_>) -> E) -> Result<Self, E> {
        match parser.match_(p) {
            Some(_) => Ok(self),
            None => match self.ended_on_terminator() {
                true  => Err(needs_t(parser)),
                false => Err(needs_comma(parser)),
            },
        }
    }
}
impl<T> crate::span::Spanned for Tuple<T> {
    fn span(&self) -> Span {
        self.span
    }
}
pub struct Entry<K, V> {
    key: K,
    val: V,
    span: Span
}
impl<K, V> crate::span::Spanned for Entry<K, V> {
    fn span(&self) -> Span {
        self.span
    }
}
// This cannot be generalized, as it causes recursion problems.
impl<V: Parseable<Err=FullParseErr>> Parseable for Option<Entry<ast::Expr, V>> {
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        let (result, span) = parser.try_spanned(|parser| {
            let Some(k) = parser.try_parse()? else { return ParseResult::Ok(None) };
            parser.expect(token![:])?;
            let v = parser.parse()?;

            Ok(Some((k, v)))
        })?;

        Ok(result.map(|(key, val)| Entry { key, val, span }))
    }
}
impl<V: Parseable<Err=FullParseErr>> Parseable for Option<Entry<ast::Ident, V>> {
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        let (result, span) = parser.try_spanned(|parser| {
            let Some(k) = parser.try_parse()? else { return ParseResult::Ok(None) };
            parser.expect(token![:])?;
            let v = parser.parse()?;

            Ok(Some((k, v)))
        })?;

        Ok(result.map(|(key, val)| Entry { key, val, span }))
    }
}
impl<V: Parseable<Err=FullParseErr>> Parseable for Entry<ast::Expr, V> {
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        let ((key, val), span) = parser.try_spanned(|parser| {
            let k = parser.parse()?;
            parser.expect(token![:])?;
            let v = parser.parse()?;

            ParseResult::Ok((k, v))
        })?;

        Ok(Self { key, val, span })
    }
}
impl<V: Parseable<Err=FullParseErr>> Parseable for Entry<ast::Ident, V> {
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        let ((key, val), span) = parser.try_spanned(|parser| {
            let k = parser.parse()?;
            parser.expect(token![:])?;
            let v = parser.parse()?;

            ParseResult::Ok((k, v))
        })?;

        Ok(Self { key, val, span })
    }
}

impl Parseable for ast::Program {
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        let (stmts, span) = parser.try_spanned(parse_stmts_closed)?;

        if parser.cursor.is_empty() {
            Ok(ast::Program { stmts, span })
        } else {
            // there are more tokens left that couldn't be parsed as a program.
            // we have an issue.
            Err(parser.cursor.error(expected_tokens![;]))
        }
    }
}

impl Parseable for Option<ast::Ident> {
    type Err = Infallible;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        let ident = match (parser.peek().map(|t| &t.kind), parser.peek_n(1).map(|t| &t.kind)) {
            (Some(token![#]), Some(Token::Ident(_))) => {
                let (ident, span) = parser.spanned(|parser| {
                    parser.expect(token![#]).unwrap(); // should be unreachable
                    let Some(FullToken { kind: Token::Ident(ident), span: _ }) = parser.next() else {
                        unreachable!()
                    };

                    String::from("#") + &ident
                });

                Some(ast::Ident { ident, span })
            },
            (Some(Token::Ident(_)), _) => {
                let Some(FullToken { kind: Token::Ident(ident), span }) = parser.next() else {
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
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        parser.try_parse()?
            .ok_or(parser.cursor.error(ParseErr::ExpectedIdent))
    }
}

impl Parseable for Option<ast::StrLiteral> {
    type Err = Infallible;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        let lit = parser.cursor.next_if(|t| matches!(&**t, Token::Str(_)))
            .map(|tok| {
                let FullToken { kind: Token::Str(literal), span } = tok.clone() else { unreachable!() };
                ast::StrLiteral { literal, span }
            });

        Ok(lit)
    }
}
impl Parseable for ast::StrLiteral {
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        parser.try_parse()?
            .ok_or(parser.cursor.error(ParseErr::ExpectedLiteral))
    }
}

const REASG_TOKENS: [Token; 2] = [token![let], token![const]];
impl Parseable for Option<ast::ReasgType> {
    type Err = Infallible;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {

        let rt = match parser.match_(REASG_TOKENS).map(|t| t.kind) {
            Some(token![let]) => Some(ast::ReasgType::Let),
            Some(token![const]) => Some(ast::ReasgType::Const),
            _ => None
        };

        Ok(rt)
    }
}
impl Parseable for ast::ReasgType {
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        parser.try_parse()?
            .ok_or_else(|| {
                parser.cursor.error(ParseErr::ExpectedTokens(REASG_TOKENS.expected_tokens()))
            })
    }
}

impl Parseable for Option<ast::Stmt> {
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        let st = match parser.peek() {
            Some(tok) => match &**tok {
                token![let] | token![const] => Some(ast::Stmt::Decl(parser.parse()?)),
                token![return]   => Some(ast::Stmt::Return(parser.parse()?)),
                token![break]    => Some(ast::Stmt::Break(parser.parse()?)),
                token![continue] => Some(ast::Stmt::Continue(parser.parse()?)),
                token![throw]    => Some(ast::Stmt::Throw(parser.parse()?)),
                token![fun]      => Some(ast::Stmt::FunDecl(parser.parse()?)),
                token![extern]   => Some(ast::Stmt::ExternFunDecl(parser.parse()?)),
                token![class]    => Some(ast::Stmt::Class(parser.parse()?)),
                token![fit]      => Some(ast::Stmt::FitClassDecl(parser.parse()?)),
                token![import]   => match parser.peek_n(1) {
                    Some(t) => match &**t {
                        Token::Ident(id) if id == "intrinsic" => Some(ast::Stmt::ImportIntrinsic(parser.parse()?)),
                        _ => Some(ast::Stmt::Import(parser.parse()?))
                    }
                    None => None
                }
                token![global]   => Some(ast::Stmt::IGlobal(parser.parse()?)),
                _                => parser.try_parse()?.map(ast::Stmt::Expr),
            }
            None => None
        };

        Ok(st)
    }
}

impl Parseable for Option<ast::Type> {
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        let (result, span) = parser.try_spanned(|parser| {
            let Some(ident) = parser.try_parse()? else { return ParseResult::Ok(None) };

            parser.expect(token!["["])?;
            let args = parser.parse_tuple(token![,])?
                .assert_non_empty(|| parser.cursor.error(ParseErr::ExpectedType))?
                .assert_closed(parser, token!["]"], 
                    |parser| parser.cursor.error(expected_tokens![,]),
                    |parser| parser.cursor.error(ParseErr::ExpectedType)
                )?
                .values().collect();

            Ok(Some((ident, args)))
        })?;

        Ok(result.map(|(ident, params)| ast::Type { ident, params, span }))
    }
}
impl Parseable for ast::Type {
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        parser.try_parse()?
            .ok_or_else(|| parser.cursor.error(ParseErr::ExpectedType))
    }
}

impl Parseable for ast::Decl {
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        let ((rt, pat, ty, val), span) = parser.try_spanned(|parser| {
            let rt = parser.parse()?;
            let pat = parser.parse()?;

            let ty = match parser.match_(token![:]).is_some() {
                true => Some(parser.parse()?),
                false => None
            };

            parser.expect(token![=])?;
            let expr = parser.parse()?;
            
            ParseResult::Ok((rt, pat, ty, expr))
        })?;

        Ok(ast::Decl { rt, pat, ty, val, span })
    }
}
impl Parseable for Option<ast::DeclPat> {
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        let Some(peek) = parser.peek().map(|t| &t.kind) else {
            return Ok(None)
        };

        let pat = match peek {
            token!["["] => {
                let (values, span) = parser.try_spanned(|parser| {
                    parser.expect(token!["["])?;
                    let pats = parser.parse_tuple(token![,])?
                        .assert_closed(parser, token!["]"],
                            |parser| parser.cursor.error(expected_tokens![,]),
                            |parser| parser.cursor.error(ParseErr::ExpectedPattern)
                        )?
                        .values().collect();
                    
                    ParseResult::Ok(pats)
                })?;

                ast::DeclPat::List { values, span }
            },
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

                    ParseResult::Ok((mt, parser.parse()?))
                })?;

                ast::DeclPat::Unit(ast::DeclUnit { ident, mt, span })
            },
            _ => return Ok(None)
        };

        Ok(Some(pat))
    }
}
impl Parseable for ast::DeclPat {
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        parser.try_parse()?
            .ok_or_else(|| parser.cursor.error(ParseErr::ExpectedPattern))
    }
}

impl Parseable for ast::Return {
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        let (me, span) = parser.try_spanned(|parser| {
            parser.expect(token![return])?;
            parser.try_parse()
        })?;

        Ok(Self { expr: me, span })
    }
}
impl Parseable for ast::Break {
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        parser.try_spanned(|parser| parser.expect(token![break]))
            .map(|(_, span)| Self { span })
    }
}
impl Parseable for ast::Continue {
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        parser.try_spanned(|parser| parser.expect(token![continue]))
            .map(|(_, span)| Self { span })
    }
}
impl Parseable for ast::Throw {
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        let (message, span) = parser.try_spanned(|parser| {
            parser.expect(token![throw])?;
            parser.parse()
        })?;

        Ok(Self { message, span })
    }
}
impl Parseable for Option<ast::Param> {
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        let (result, span) = parser.try_spanned(|parser| {
            let maybe_rt = parser.try_parse()?;
            let maybe_mt = parser.match_(token![mut]).map(|_| ast::MutType::Mut);
            let maybe_ident = parser.try_parse()?;

            // the param checked so far is fully empty and probably not an actual param:
            if maybe_rt.is_none() && maybe_mt.is_none() && maybe_ident.is_none() {
                return ParseResult::Ok(None);
            }

            let rt = maybe_rt.unwrap_or_default();
            let mt = maybe_mt.unwrap_or_default();
            let ident = maybe_ident.ok_or_else(|| parser.cursor.error(ParseErr::ExpectedIdent))?;

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
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
            parser.try_parse()?
                .ok_or_else(|| parser.cursor.error(ParseErr::ExpectedParam))
    }
}
impl Parseable for ast::FunSignature {
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        let ((ident, generics, params, ret), span) = parser.try_spanned(|parser| {
            parser.expect(token![fun])?;
            let ident = parser.parse()?;

            let generics = if parser.match_(token!["["]).is_some() {
                parser.parse_tuple(token![,])?
                    .assert_non_empty(|| parser.cursor.error(ParseErr::ExpectedIdent))?
                    .assert_closed(parser, token!["]"], 
                        |parser| parser.cursor.error(expected_tokens![,]),
                        |parser| parser.cursor.error(ParseErr::ExpectedParam),
                    )?.values().collect()
            } else {
                vec![]
            };

            parser.expect(token!["("])?;
            let params = parser.parse_tuple(token![,])?
                .assert_closed(parser, token![")"],
                    |parser| parser.cursor.error(expected_tokens![,]),
                    |parser| parser.cursor.error(ParseErr::ExpectedParam),
            )?.values().collect();

            let ret = match parser.match_(token![->]) {
                Some(_) => Some(parser.parse()?),
                None    => None,
            };

            ParseResult::Ok((ident, generics, params, ret))
        })?;

        Ok(Self { ident, generics, params, varargs: false, ret, span })
    }
}
impl Parseable for ast::FunDecl {
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        let ((sig, block), span) = parser.try_spanned(|parser| {
            let sig = parser.parse()?;
            let block = parser.parse()?;
            ParseResult::Ok((sig, block))
        })?;

        Ok(Self { sig, block, span })
    }
}

// TODO: combine MethodSignature & FunSignature
impl Parseable for ast::MethodSignature {
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        let ((referent, is_static, name, generic_params, params, ret), span) = parser.try_spanned(|parser| {
            parser.expect(token![fun])?;
            let referent = parser.try_parse()?;
            let is_static = match parser.expect([token![::], token![.]])?.kind {
                token![::] => true,
                token![.]  => false,
                _ => unreachable!()
            };
            let name = parser.parse()?;

            let generics = if parser.match_(token!["["]).is_some() {
                parser.parse_tuple(token![,])?
                    .assert_non_empty(|| parser.cursor.error(ParseErr::ExpectedIdent))?
                    .assert_closed(parser, token!["]"], 
                        |parser| parser.cursor.error(expected_tokens![,]),
                        |parser| parser.cursor.error(ParseErr::ExpectedParam),
                    )?.values().collect()
            } else {
                vec![]
            };

            parser.expect(token!["("])?;
            let params = parser.parse_tuple(token![,])?
                .assert_closed(parser, token![")"],
                    |parser| parser.cursor.error(expected_tokens![,]),
                    |parser| parser.cursor.error(ParseErr::ExpectedParam),
            )?.values().collect();
            
            let ret = match parser.match_(token![->]) {
                Some(_) => Some(parser.parse()?),
                None    => None,
            };

            ParseResult::Ok((referent, is_static, name, generics, params, ret))
        })?;

        Ok(Self { referent, is_static, name, generic_params, params, ret, span  })
    }
}
impl Parseable for ast::MethodDecl {
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        let ((sig, block), span) = parser.try_spanned(|parser| {
            let sig = parser.parse()?;
            let block = parser.parse()?;
            ParseResult::Ok((sig, block))
        })?;

        Ok(Self { sig, block, span })
    }
}
impl Parseable for ast::ExternFunDecl {
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        let (sig, span) = parser.try_spanned(|parser| {
            parser.expect(token![extern])?;
            parser.parse()
        })?;

        Ok(Self { sig, span })
    }
}
impl Parseable for Option<ast::FieldDecl> {
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        let (result, span) = parser.try_spanned(|parser| {
            let maybe_rt = parser.try_parse()?;
            let maybe_mt = parser.match_(token![mut]).map(|_| ast::MutType::Mut);
            let maybe_ident = parser.try_parse()?;

            // the param checked so far is fully empty and probably not an actual param:
            if maybe_rt.is_none() && maybe_mt.is_none() && maybe_ident.is_none() {
                return ParseResult::Ok(None);
            }

            let rt = maybe_rt.unwrap_or_default();
            let mt = maybe_mt.unwrap_or_default();
            let ident = maybe_ident.ok_or_else(|| parser.cursor.error(ParseErr::ExpectedIdent))?;

            parser.expect(token![:])?;
            let ty = parser.parse()?;

            Ok(Some((rt, mt, ident, ty)))
        })?;
        
        Ok(result.map(|(rt, mt, ident, ty)| ast::FieldDecl { rt, mt, ident, ty, span }))
    }
}
impl Parseable for ast::FieldDecl {
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        parser.try_parse()?
            .ok_or_else(|| parser.cursor.error(ParseErr::ExpectedIdent))
    }
}

impl Parseable for ast::Class {
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        let ((ident, generic_params, fields, methods), span) = parser.try_spanned(|parser| {
            parser.expect(token![class])?;
            let ident = parser.parse()?;
            let generic_params = match parser.match_(token!["["]) {
                Some(_) => {
                    parser.parse_tuple(token![,])?
                        .assert_non_empty(|| parser.cursor.error(ParseErr::ExpectedIdent))?
                        .assert_closed(parser, token!["]"],
                            |parser| parser.cursor.error(expected_tokens![,]),
                            |parser| parser.cursor.error(ParseErr::ExpectedIdent),
                        )?
                        .values().collect()
                },
                None => vec![]
            };

            parser.expect(token!["{"])?;
            let fields = parser.parse_tuple(token![,])?
                .values()
                .collect();
            
            let mut methods = vec![];
            while let Some(FullToken { kind: token![fun], span: _ }) = parser.peek() {
                methods.push(parser.parse()?);
            }

            parser.expect(token!["}"])?;
            
            ParseResult::Ok((ident, generic_params, fields, methods))
        })?;

        Ok(Self { ident, generic_params, fields, methods, span })
    }
}
impl Parseable for ast::Import {
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        let (path, span) = parser.try_spanned(|parser| {
            parser.expect(token![import])?;
            parser.parse()
        })?;

        Ok(Self { path, span })
    }
}
impl Parseable for ast::ImportIntrinsic {
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        let (_, span) = parser.try_spanned(|parser| {
            parser.expect(token![import])?;
            parser.expect( Token::Ident(String::from("intrinsic")) )
        })?;

        Ok(Self { span })
    }
}
impl Parseable for ast::IGlobal {
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        let ((ident, value), span) = parser.try_spanned(|parser| {
            parser.expect(token![global])?;
            let ident = parser.parse()?;
            parser.expect(token![=])?;
            let value = parser.parse()?;

            ParseResult::Ok((ident, value))
        })?;

        Ok(ast::IGlobal { ident, value, span })
    }
}
impl Parseable for ast::FitClassDecl {
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        let ((ty, methods), span) = parser.try_spanned(|parser| {
            parser.expect(token![fit])?;
            parser.expect(token![class])?;
            let ty = parser.parse()?;
            
            parser.expect(token!["{"])?;
            let mut methods = vec![];
            while let Some(FullToken { kind: token![fun], span: _ }) = parser.peek() {
                methods.push(parser.parse()?);
            }
            parser.expect(token!["}"])?;
            
            ParseResult::Ok((ty, methods))
        })?;

        Ok(Self { ty, methods, span })
    }
}
impl Parseable for ast::Block {
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        let (stmts, span) = parser.try_spanned(|parser| {
            parser.expect(token!["{"])?;
            let stmts = parse_stmts_closed(parser)?;
            parser.expect(token!["}"])?;
            
            ParseResult::Ok(stmts)
        })?;

        Ok(ast::Block { stmts, span })
    }
}
impl Parseable for ast::StaticPath {
    type Err = FullParseErr;

    fn read(parser: &mut Parser2<'_>) -> Result<Self, Self::Err> {
        let ((ty, attr), span) = parser.try_spanned(|parser| {
            let ty = parser.parse()?;
            parser.expect(token![::])?;
            let attr = parser.parse()?;

            ParseResult::Ok((ty, attr))
        })?;

        Ok(Self { ty, attr, span })
    }
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
/// 
pub struct Parser {
    tokens: VecDeque<FullToken>,
    repl_mode: bool,
    eof: (usize, usize),
    tree_locs: Vec<RangeBlock>,
    intrinsic_mode: bool
}

#[derive(Clone, Debug)]
struct RangeBlock(&'static str, Option<Span>);

// macro_rules! left_assoc_op {
//     ($n:ident = $ds:ident (($($op:tt),+) $_:ident)*;) => {
//         #[doc = concat!(
//             "Match the next tokens in input if they represent a ( ",
//             $(
//                 "`", stringify!($op), "` "
//             ),+,
//             ") operation or any expression with higher precedence."
//         )]
//         #[doc = ""]
//         #[doc = concat!(
//             "The next expression above in precedence is [`Parser::",
//             stringify!($ds),
//             "`]."
//         )]
//         pub fn $n(&mut self) -> ParseResult<Option<Located<ast::Expr>>> {
//             self.push_loc_block(stringify!($n));
//             if let Some(mut e) = self.$ds()? {
//                 while let Some(op) = self.match_([$(token![$op]),+]) {
//                     let binop = ast::Expr::BinaryOp {
//                         op: op.tt.try_into().unwrap(),
//                         left: Box::new(e),
//                         right: self.$ds()?
//                             .map(Box::new)
//                             .ok_or(ParseErr::ExpectedExpr.at_range(self.peek_loc()))?
//                     };

//                     e = Located::new(binop, self.peek_loc_block().unwrap());
//                 }

//                 self.pop_loc_block(stringify!($n));
//                 Ok(Some(e))
//             } else {
//                 self.pop_loc_block(stringify!($n));
//                 Ok(None)
//             }
//         }
//     };
//     ($n:ident = $ds:ident ($op:tt $_:ident)*;) => {
//         left_assoc_op!($n = $ds (($op) $_)*;);
//     };
// }

// macro_rules! left_assoc_rules {
//     ($($n:ident = $ds:ident (($($op:tt),+) $_:ident)*;)+) => {
//         $(
//             left_assoc_op!($n = $ds (($($op),+) $_)*;);
//         )+
//     };
//     ($($n:ident = $ds:ident ($op:tt $_:ident)*;)+) => {
//         $(
//             left_assoc_op!($n = $ds (($op) $_)*;);
//         )+
//     };
// }

// impl Parser {
//     /// Expect that the next tokens in input represent some expression.
//     pub fn expect_expr(&mut self) -> ParseResult<Located<ast::Expr>> {
//         self.match_expr()?
//             .ok_or_else(|| ParseErr::ExpectedExpr.at_range(self.peek_loc()))
//     }

//     /// Match the next tokens in input if they represent any expression.
//     /// 
//     /// The next expression above in precedence is [`Parser::match_asg`].
//     pub fn match_expr(&mut self) -> ParseResult<Option<Located<ast::Expr>>> {
//         self.match_asg()
//     }

//     /// Match the next tokens in input if they represent an assignment operation (`a = b`)
//     /// or any expression with higher precedence.
//     /// 
//     /// The next expression above in precedence is [`Parser::match_lor`].
//     pub fn match_asg(&mut self) -> ParseResult<Option<Located<ast::Expr>>> {
//         // a = b = c = d = e = [expr]
//         let mut pats = vec![];
        
//         self.push_loc_block("match_asg");
//         let mut last = self.match_lor()?;
//         // TODO: asg ops

//         let mut eq_pos = self.peek_loc();
//         while self.match_(token![=]).is_some() {
//             // if there was an equal sign, this expression must've been a pattern:
//             let pat_expr = last
//                 .ok_or_else(|| ParseErr::ExpectedPattern.at_range(eq_pos.clone()))?;
//             let pat = ast::AsgPat::try_from(pat_expr)
//                 .map_err(|e| e.map(ParseErr::AsgPatErr))?;
            
//             pats.push(pat);

//             self.push_loc_block("match_asg");
//             last = self.match_lor()?;
//             eq_pos = self.peek_loc();
//         }

//         // this is the RHS block's loc block (which is already recorded)
//         // and can be trashed
//         self.pop_loc_block("match_asg");
//         if !pats.is_empty() {
//             let rhs = last
//                 .ok_or_else(|| ParseErr::ExpectedExpr.at_range(self.peek_loc()))?;

//             let asg = pats.into_iter()
//                 .rfold(rhs, |e, pat| {
//                     let range = self.pop_loc_block("match_asg").unwrap();
                    
//                     Located::new(
//                         ast::Expr::Assign(pat, Box::new(e)),
//                         range
//                     )
//                 });
            
//             Ok(Some(asg))
//         } else {
//             // if pats is empty this is not an assignment expression
//             Ok(last)
//         }
//     }

//     // pattern follows for all left_assoc_rules!
//     // fn match_lor(&mut self) -> ParseResult<Option<tree::Expr>> {
//     //     if let Some(mut e) = self.match_land()? {
//     //         while let Some(op) = self.match_n(token![||]) {
//     //             e = tree::Expr::BinaryOp(tree::BinaryOp {
//     //                 op,
//     //                 left: Box::new(e),
//     //                 right: self.match_land()?
//     //                     .map(Box::new)
//     //                     .ok_or(ParseErr::ExpectedExpr)?
//     //             });
//     //         }

//     //         Ok(Some(e))
//     //     } else {
//     //         Ok(None)
//     //     }
//     // }

//     // This creates the matching function for:
//     // logical OR, logical AND
//     // These have a similar structure and don't need to be repeated several times.
//     left_assoc_rules! { 
//         match_lor  = match_land  ( || match_land )*;
//         match_land = match_cmp   ( && match_cmp  )*;
//     }

//     /// Match the next tokens in input if they represent an comparison operation (`a < b`)
//     /// or any expression with higher precedence.
//     /// 
//     /// Note that compound comparisons (`a < b < c < d`) are considered one comparison
//     /// and can be returned by this function.
//     /// 
//     /// The next expression above in precedence is [`Parser::match_spread`].
//     pub fn match_cmp(&mut self) -> ParseResult<Option<Located<ast::Expr>>> {
//         self.push_loc_block("match_cmp");
//         let me = self.match_spread()?;

//         if let Some(mut e) = me {
//             lazy_static! {
//                 static ref CMP_OPS: [Token; 6] = [
//                     token![<], token![>], 
//                     token![<=], token![>=], 
//                     token![==], token![!=]
//                 ];
//             }

//             // check if there's a comparison here
//             let mut rights = vec![];
//             while let Some(t) = self.match_(&*CMP_OPS) {
//                 let op = t.tt.try_into().unwrap();
//                 let rexpr = self.match_spread()?
//                     .ok_or_else(|| ParseErr::ExpectedExpr.at_range(self.peek_loc()))?;
                
//                 rights.push((op, rexpr));

//             }

//             let range = self.pop_loc_block("match_cmp");
//             if !rights.is_empty() {
//                 let cmp_expr = ast::Expr::Comparison { 
//                     left: Box::new(e), 
//                     rights 
//                 };

//                 e = Located::new(cmp_expr, range.unwrap());
//             }

//             Ok(Some(e))
//         } else {
//             self.pop_loc_block("match_cmp");
//             Ok(None)
//         }
//     }

//     /// Match the next tokens in input if they represent an spread expression (`..[1, 2, 3]`)
//     /// or any expression with higher precedence.
//     /// 
//     /// The next expression above in precedence is [`Parser::match_range`].
//     pub fn match_spread(&mut self) -> ParseResult<Option<Located<ast::Expr>>> {
//         self.push_loc_block("match_spread");
//         if self.match_(token![..]).is_some() {
//             let me = self.match_range()?;
//             let inner = me.map(Box::new);

//             let spread_expr = ast::Expr::Spread(inner);
//             let e = Located::new(spread_expr, self.pop_loc_block("match_spread").unwrap());
//             Ok(Some(e))
//         } else {
//             self.pop_loc_block("match_spread");
//             self.match_range()
//         }
//     }

//     /// Match the next tokens in input if they represent an range expression (`1..5`)
//     /// or any expression with higher precedence.
//     /// 
//     /// The next expression above in precedence is [`Parser::match_bor`].
//     pub fn match_range(&mut self) -> ParseResult<Option<Located<ast::Expr>>> {
//         self.push_loc_block("match_range");
//         if let Some(mut e) = self.match_bor()? {
//             if self.match_(token![..]).is_some() {
//                 let right = self.match_bor()?
//                 .ok_or_else(|| ParseErr::ExpectedExpr.at_range(self.peek_loc()))?;

//                 let step = if self.match_(token![step]).is_some() {
//                     let sexpr = self.match_bor()?
//                     .ok_or_else(|| ParseErr::ExpectedExpr.at_range(self.peek_loc()))?;
//                     Some(sexpr)
//                 } else {
//                     None
//                 };

//                 let range_expr = ast::Expr::Range {
//                     left: Box::new(e),
//                     right: Box::new(right),
//                     step: step.map(Box::new)
//                 };
//                 e = Located::new(range_expr, self.pop_loc_block("match_range").unwrap());
//             } else {
//                 self.pop_loc_block("match_range");
//             }

//             Ok(Some(e))
//         } else {
//             self.pop_loc_block("match_range");
//             Ok(None)
//         }
//     }

//     // This creates the matching function for:
//     // bitwise OR, bitwise XOR, bitwise AND
//     left_assoc_rules! {
//         match_bor  = match_bxor  ( | match_bxor  )*;
//         match_bxor = match_band  ( ^ match_band  )*;
//         match_band = match_shift ( & match_shift )*;
//     }
//     // shifting (<<, >>)
//     // addition/subtraction (+, -)
//     // multiplication, division, modulo (+, -, %)
//     left_assoc_rules! {
//         match_shift  = match_addsub ( ( << , >> ) match_addsub )* ;
//         match_addsub = match_muldiv ( ( + , - ) match_muldiv )* ;
//         match_muldiv = match_unary ( ( * , / , % ) match_unary )* ;
//     }

//     /// Match the next tokens in input if they represent a unary expression (`!expr`, `-expr`)
//     /// or any expression with higher precedence.
//     /// 
//     /// The next expression above in precedence is [`Parser::match_deref`].
//     pub fn match_unary(&mut self) -> ParseResult<Option<Located<ast::Expr>>> {
//         lazy_static! {
//             static ref UNARY_OPS: [Token; 4] = [token![!], token![~], token![-], token![+]];
//         }

//         self.push_loc_block("match_unary");
//         let mut ops = vec![];
//         while let Some(t) = self.match_(&*UNARY_OPS) {
//             ops.push(t.tt.try_into().unwrap());
//         }

//         let me = if ops.is_empty() {
//             self.pop_loc_block("match_unary");
//             self.match_deref()?
//         } else {
//             let e = self.match_deref()?
//                 .ok_or_else(|| ParseErr::ExpectedExpr.at_range(self.peek_loc()))?;

//             let range = self.pop_loc_block("match_unary").unwrap();
//             Some(Parser::wrap_unary_op(ops, e, range))
//         };

//         Ok(me)
//     }

//     /// Helper function that constructs a [`ast::Expr::UnaryOps`] node.
//     /// 
//     /// It takes the inner expression and acts as though the unary operators were applied to it.
//     /// If the inner expression is a uops node, this also flattens the operators 
//     /// (so that we don't have a unary operators node wrapping another one)
//     fn wrap_unary_op(
//         mut ops: Vec<ast::op::Unary>, 
//         inner: Located<ast::Expr>, 
//         range: CursorRange
//     ) -> Located<ast::Expr> {
//         // flatten if unary ops inside
//         if let Located(ast::Expr::UnaryOps { ops: ops2, expr }, _) = inner {
//             ops.extend(ops2);
//             Located(ast::Expr::UnaryOps {
//                 ops, expr
//             }, range)
//         } else {
//             // wrap otherwise
//             Located(ast::Expr::UnaryOps {
//                 ops,
//                 expr: Box::new(inner)
//             }, range)
//         }
//     }

//     /// Match the next tokens in input if they represent an intrinsic dereferencing expression.
//     /// (`*self.a`)
//     /// or any expression with higher precedence.
//     /// 
//     /// The next expression above in precedence is [`Parser::match_call_index_path`].
//     pub fn match_deref(&mut self) -> ParseResult<Option<Located<ast::Expr>>> {
//         let peek = self.peek_loc();
//         let is_deref_expr = self.match_(token![*]).is_some();
//         let ci = self.match_call_index_path();

//         if is_deref_expr {
//             let inner = ci?.ok_or_else(|| ParseErr::ExpectedExpr.at_range(self.peek_loc()))?;

//             let outer_range = merge_ranges(peek, inner.range());
            
//             let deref_expr = Located::new(ast::Expr::Deref(
//                 ast::IDeref(Box::new(inner))
//             ), outer_range);
//             Ok(Some(deref_expr))
//         } else {
//             ci
//         }
//     }

//     /// Match the next tokens in input if they represent an indexing operation, function call, or path.
//     /// (`a[1]`, `f(1, 2, 3, 4)`)
//     /// or any expression with higher precedence.
//     /// 
//     /// The next expression above in precedence is [`Parser::match_unit`].
//     pub fn match_call_index_path(&mut self) -> ParseResult<Option<Located<ast::Expr>>> {
//         self.push_loc_block("match_call_index_path");
//         if let Some(mut e) = self.match_unit()? {
//             while let Some(delim) = self.match_(&[token!["("], token!["["], token![.]]) {
//                 match delim.tt {
//                     token!["("] => {
//                         let params = self.expect_closing_tuple(token![")"])?;
//                         let range = self.peek_loc_block().unwrap();
                        
//                         let call_expr = ast::Expr::Call {
//                             funct: Box::new(e), 
//                             params
//                         };
//                         e = Located::new(call_expr, range);
//                     },
//                     token!["["] => {
//                         let index = self.expect_expr()?;
//                         self.expect(token!["]"])?;
//                         let range = self.peek_loc_block().unwrap();

//                         let idx_expr = ast::Expr::Index(ast::Index {
//                             expr: Box::new(e), 
//                             index: Box::new(index)
//                         });
//                         e = Located::new(idx_expr, range);
//                     },
//                     token![.] => {
//                         let attr = self.expect_ident()?.0;
//                         let loc_block = self.peek_loc_block().unwrap();

//                         e = match e {
//                             Located(ast::Expr::Path(ast::Path { ref mut attrs, .. }), ref mut range) => {
//                                 attrs.push(attr);
//                                 *range = loc_block;
//                                 e
//                             },
//                             obj => {
//                                 Located::new(ast::Expr::Path(ast::Path {
//                                     obj: Box::new(obj),
//                                     attrs: vec![attr]
//                                 }), loc_block)
//                             }
//                         };
//                     }
//                     _ => unreachable!()
//                 }
//             }

//             self.pop_loc_block("match_call_index_path");
//             Ok(Some(e))
//         } else {
//             self.pop_loc_block("match_call_index_path");
//             Ok(None)
//         }
//     }

//     /// Match the next tokens in input if they represent any expression 
//     /// with a higher precedence with a path.
//     pub fn match_unit(&mut self) -> ParseResult<Option<Located<ast::Expr>>> {
//         self.push_loc_block("match_unit");

//         let Some(peek) = self.peek_token() else {
//             self.pop_loc_block("match_unit");
//             return Ok(None);
//         };

//         let unit = match peek {
//             token![#] | Token::Ident(_) => self.expect_identoid()?,
//             token![<] | token![<<]      => self.expect_diamondoid()?,
//             Token::Numeric(_) | Token::Str(_) | Token::Char(_) | token![true] | token![false] => self.expect_literal()? ,
//             token!["["]       => self.expect_list()?,
//             token!["{"]       => self.expect_block().map(ast::Expr::Block)?,
//             token![if]        => self.expect_if()?,
//             token![while]     => self.expect_while()?,
//             token![for]       => self.expect_for()?,
//             token!["("] => {
//                 self.expect(token!["("])?;
//                 let e = self.expect_expr()?;
//                 self.expect(token![")"])?;
        
//                 // We can drop the inner expr in favor of the outer one,
//                 // it doesn't really matter.
//                 e.0
//             },
//             _ => {
//                 self.pop_loc_block("match_unit");
//                 return Ok(None);
//             }
//         };

//         Ok(Some(Located::new(unit, self.pop_loc_block("match_unit").unwrap())))
//     }

//     /// Expect that the next tokens in input represent some primitive literal (int, str, char, etc.).
//     pub fn expect_literal(&mut self) -> ParseResult<ast::Expr> {
//         let FullToken { tt, loc } = self.next().expect("unreachable");
        
//         let lit = match tt {
//             Token::Numeric(s) => ast::Literal::from_numeric(&s)
//                 .ok_or_else(|| ParseErr::CannotParseNumeric.at_range(loc))?,
//             Token::Str(s)  => ast::Literal::Str(s),
//             Token::Char(c) => ast::Literal::Char(c),
//             token![true]   => ast::Literal::Bool(true),
//             token![false]  => ast::Literal::Bool(false),
//             _ => unreachable!()
//         };

//         Ok(ast::Expr::Literal(lit))
//     }

//     /// Expect that the next tokens in input represent a list literal (`[1, 2, 3]`).
//     pub fn expect_list(&mut self) -> ParseResult<ast::Expr> {
//         self.expect(token!["["])?;
//         let exprs = self.expect_closing_tuple(token!["]"])?;
        
//         Ok(ast::Expr::ListLiteral(exprs))
//     }

//     /// Entry for class initializers
//     fn match_init_entry(&mut self) -> ParseResult<Option<(Located<String>, Located<ast::Expr>)>> {
//         let krange = self.peek_loc();
//         if let Some(Located(k, _)) = self.match_ident()? {
//             let v = match self.match_(token![:]) {
//                 Some(_) => self.expect_expr()?,
//                 None    => Located::new(ast::Expr::Ident(k.clone()), krange),
//             };
            
//             Ok(Some((Located::new(k, krange), v)))
//         } else {
//             Ok(None)
//         }
//     }

//     /// Expect that the next tokens are some expression that starts with an identifier.
//     /// 
//     /// This can be:
//     /// * a set literal (`set {1, 2, 3}`), 
//     /// * a dict literal (`dict {"a": 1, "b": 2, "c": 3}`),
//     /// * a class initializer (`Class {a: 2, b: 3, c: 4}`),
//     /// * a static path (`Type::attr`), or
//     /// * an identifier (`x`)
//     pub fn expect_identoid(&mut self) -> ParseResult<ast::Expr> {
//         type Entry = (Located<ast::Expr>, Located<ast::Expr>);
//         /// Entry for dict literals
//         fn match_entry(this: &mut Parser) -> ParseResult<Option<Entry>> {
//             if let Some(k) = this.match_expr()? {
//                 this.expect(token![:])?;
//                 let v = this.expect_expr()?;
//                 Ok(Some((k, v)))
//             } else {
//                 Ok(None)
//             }
//         }

//         let next_token_pos = self.has_ident()
//             .ok_or_else(|| ParseErr::ExpectedIdent.at_range(self.peek_loc()))?;
//         let e = match self.tokens.get(next_token_pos) {
//             Some(FullToken { tt: token![#], .. }) => {
//                 let ty = self.expect_type()?;

//                 self.expect(token![#])?;
//                 self.expect(token!["{"])?;
//                 match ty.0.0.as_str() {
//                     "set" => {
//                         let exprs = self.expect_closing_tuple(token!["}"])?;
//                         ast::Expr::SetLiteral(exprs)
//                     },
//                     "dict" => {
//                         let entries = self.expect_closing_tuple_of(
//                             match_entry,
//                             token!["}"], 
//                             ParseErr::ExpectedEntry
//                         )?;
//                         ast::Expr::DictLiteral(entries)
//                     },
//                     _ => {
//                         let params = self.expect_closing_tuple_of(
//                             Parser::match_init_entry, 
//                             token!["}"], 
//                             ParseErr::ExpectedIdent
//                         )?;
//                         ast::Expr::ClassLiteral(ty, params)
//                     }
//                 }
//             },
//             Some(FullToken { tt: token![::], .. }) => {
//                 let ty = self.expect_type()?;
//                 self.expect(token![::])?;
//                 let attr = self.expect_ident()?.0;

//                 ast::Expr::StaticPath(ast::StaticPath { ty, attr })
//             },
//             _ => ast::Expr::Ident(self.expect_ident()?.0)
//         };

//         Ok(e)
//     }

//     /// Expect that the next tokens are some expression that starts with a type diamond.
//     /// 
//     /// This can be:
//     /// * a class initializer (`<Class<T>> {a: 2, b: 3, c: 4}`), or
//     /// * a static path (`<Type<T>>::attr`)
//     pub fn expect_diamondoid(&mut self) -> ParseResult<ast::Expr> {
//         self.expect(token![<])?;
//         let ty = self.expect_type()?;
//         self.expect(token![>])?;

//         let loc = self.peek_loc();
//         let e = match self.next_token() {
//             Some(token![#]) => {
//                 self.expect(token!["{"])?;
//                 let params = self.expect_closing_tuple_of(
//                     Parser::match_init_entry, 
//                     token!["}"], 
//                     ParseErr::ExpectedIdent
//                 )?;
//                 ast::Expr::ClassLiteral(ty, params)
//             },
//             Some(token![::]) => {
//                 let attr = self.expect_ident()?.0;
    
//                 ast::Expr::StaticPath(ast::StaticPath { ty, attr })
//             },
//             _ => Err(expected_tokens![::, #].at_range(loc))?
//         };

//         Ok(e)
//     }

//     /// Expect that the next tokens in input represent an if-else expression 
//     /// (`if cond {}`, `if cond {} else cond {}`, etc.)
//     pub fn expect_if(&mut self) -> ParseResult<ast::Expr> {
//         self.expect(token![if])?;

//         let mut conditionals = vec![(self.expect_expr()?, self.expect_block()?)];
//         let mut last = None;

//         while self.match_(token![else]).is_some() {
//             match self.peek_token() {
//                 Some(&token![if]) => {
//                     self.expect(token![if])?;
//                     conditionals.push((self.expect_expr()?, self.expect_block()?));
//                 },
//                 Some(&token!["{"]) => {
//                     let block = self.expect_block()?;
//                     last.replace(block);
//                     break;
//                 },
//                 _ => Err(ParseErr::ExpectedBlock.at_range(self.peek_loc()))?
//             }
//         }

//         Ok(ast::Expr::If {
//             conditionals,
//             last
//         })
//     }

//     /// Expect that the next tokens in input represent an `while` loop.
//     pub fn expect_while(&mut self) -> ParseResult<ast::Expr> {
//         self.expect(token![while])?;
//         let condition = self.expect_expr()?;
//         let block = self.expect_block()?;
        
//         Ok(ast::Expr::While {
//             condition: Box::new(condition), block
//         })
//     }

//     /// Expect that the next tokens in input represent an `for` loop.
//     pub fn expect_for(&mut self) -> ParseResult<ast::Expr> {
//         self.expect(token![for])?;
//         let ident = self.expect_ident()?.0;
//         self.expect(token![in])?;
//         let iterator = self.expect_expr()?;
//         let block = self.expect_block()?;
        
//         Ok(ast::Expr::For {
//             ident, iterator: Box::new(iterator), block
//         })
//     }
// }

// #[cfg(test)]
// mod tests {
//     use crate::err::{FullGonErr, GonErr};
//     use crate::lexer::token::token;
//     use crate::lexer::tokenize;
//     use crate::ast::*;

//     use super::*;

//     macro_rules! program {
//         ($($e:expr),*) => {
//             Program(vec![$($e),*])
//         }
//     }
//     macro_rules! block {
//         ($($e:expr),*; $loc:expr) => {
//             Located::new(Block(vec![$($e),*]), $loc)
//         }
//     }
//     macro_rules! block_expr {
//         ($($e:expr),*; $loc:expr) => {
//             Expr::Block(block![$($e),*; $loc])
//         }
//     }
    
//     macro_rules! expr_stmt {
//         ($e:expr, $loc:expr) => {
//             Located::new(Stmt::Expr(Located::new($e, $loc)), $loc)
//         }
//     }
//     fn add_one(cr: CursorRange) -> CursorRange {
//         let &(el, ec) = cr.end();
//         *cr.start() ..= (el, ec + 1)
//     }
//     macro_rules! expr_stmt_with_semi {
//         ($e:expr, $loc:expr) => {
//             Located::new(Stmt::Expr(Located::new($e, $loc)), add_one($loc))
//         }
//     }
//     macro_rules! binop {
//         ($op:ident, $eloc:expr, $left:expr, $right:expr) => {
//             Located::new(Expr::BinaryOp {
//                 op: op::Binary::$op,
//                 left: Box::new($left),
//                 right: Box::new($right)
//             }, $eloc)
//         };
//     }

//     macro_rules! ident {
//         ($ident:literal, $range:expr) => {
//             Located::new(Expr::Ident(String::from($ident)), $range)
//         }
//     }

//     macro_rules! literal {
//         ($ident:ident($e:expr), $range:expr) => {
//             Located::new(Expr::Literal(Literal::$ident($e)), $range)
//         };
//         ($l:literal, $range:expr) => {
//             Located::new(Expr::Literal(Literal::Str(String::from($l))), $range)
//         }
//     }

//     macro_rules! asg_unit {
//         ($id:literal) => {
//             AsgUnit::Ident(String::from($id))
//         }

//     }
//     macro_rules! decl_unit {
//         (mut $id:literal) => {
//             DeclUnit(String::from($id), MutType::Mut)
//         };
//         ($id:literal) => {
//             DeclUnit(String::from($id), MutType::Immut)
//         }
//     }
//     /// Unwrap the result (or print error if not possible).
//     fn unwrap_fe<T>(result: Result<T, FullGonErr<impl GonErr>>, input: &str) -> T {
//         match result {
//             Ok(t) => t,
//             Err(e) => panic!("{}", e.full_msg(input)),
//         }
//     }
//     /// Lex and parse string.
//     fn parse_str(s: &str) -> ParseResult<Program> {
//         parse(unwrap_fe(tokenize(s), s))
//     }
//     /// Assert that the string provided parses into the program.
//     #[allow(unused)]
//     fn assert_parse(input: &str, r: Program) {
//         assert_eq!(unwrap_fe(parse_str(input), input), r)
//     }
//     /// Assert that the string provided errors with the given error when parsed.
//     #[allow(unused)]
//     fn assert_parse_fail<E>(input: &str, result: E) 
//         where E: std::fmt::Debug,
//             FullParseErr: PartialEq<E>
//     {
//         match parse_str(input) {
//             Ok(t)  => panic!("Lexing resulted in value: {t:?}"),
//             Err(e) => assert_eq!(e, result)
//         }
//     }

//     #[test]
//     fn bin_op_test() {
//         assert_parse("2 + 3;", program![
//             Located::new(Stmt::Expr(binop! {
//                 Add, (0, 0) ..= (0, 4),
//                 literal!(Int(2), (0, 0) ..= (0, 0)),
//                 literal!(Int(3), (0, 4) ..= (0, 4))
//             }), (0, 0) ..= (0, 5))
//         ]);

//         assert_parse("2 + 3 * 4;", program![
//             Located::new(Stmt::Expr(binop! {
//                 Add, (0, 0) ..= (0, 8),
//                 literal!(Int(2), (0, 0) ..= (0, 0)),
//                 binop! {
//                     Mul, (0, 4) ..= (0, 8),
//                     literal!(Int(3), (0, 4) ..= (0, 4)),
//                     literal!(Int(4), (0, 8) ..= (0, 8))
//                 }
//             }), (0, 0) ..= (0, 9))
//         ]);
//     }

//     #[test]
//     fn block_test() {
//         assert_parse("{}", program![
//             expr_stmt!(block_expr![;(0, 0)..=(0, 1)], (0, 0)..=(0, 1))
//         ]);

//         assert_parse("{{}}", program![
//             expr_stmt!(block_expr![
//                 expr_stmt!(block_expr![;(0, 1) ..= (0, 2)], (0, 1) ..= (0, 2));
//             (0, 0) ..= (0, 3)], (0, 0)..=(0, 3))
//         ])
//     }

//     /// Tests if statements.
//     #[test]
//     fn if_else_test() {
//         assert_parse("
//             if true {
//                 // :)
//             }
//         ", program![
//             expr_stmt!(Expr::If {
//                 conditionals: vec![
//                     (literal!(Bool(true), (1, 15) ..= (1, 18)), block![; (1, 20) ..= (3, 12)])
//                 ],
//                 last: None
//             }, (1, 12)..=(3, 12))
//         ]);

//         assert_parse("
//             if true {
//                 // :)
//             } else {
//                 // :(
//             }
//         ", program![
//             expr_stmt!(Expr::If { 
//                 conditionals: vec![
//                     (literal!(Bool(true), (1, 15) ..= (1, 18)), block![; (1, 20) ..= (3, 12)])
//                 ],
//                 last: Some(block![; (3, 19) ..= (5, 12)])
//             }, (1, 12) ..= (5, 12))
//         ]);

//         assert_parse("
//             if true {
//                 // :)
//             } else if condition {
//                 // :|
//             } else {
//                 // :(
//             }
//         ", program![
//             expr_stmt!(Expr::If { 
//                 conditionals: vec![
//                     (literal!(Bool(true), (1, 15) ..= (1, 18)), block![; (1, 20) ..= (3, 12)]),
//                     (ident!("condition", (3, 22) ..= (3, 30)), block![; (3, 32) ..= (5, 12)])
//                 ],
//                 last: Some(block![; (5, 19) ..= (7, 12)])
//             }, (1, 12) ..= (7, 12))
//         ]);

//         assert_parse("
//             if true {
//                 // :)
//             } else if condition {
//                 // :|
//             } else if condition {
//                 // :|
//             } else if condition {
//                 // :|
//             } else if condition {
//                 // :|
//             } else {
//                 // :(
//             }
//         ", program![
//             expr_stmt!(Expr::If { 
//                 conditionals: vec![
//                     (literal!(Bool(true), (1, 15) ..= (1, 18)), block![; (1, 20) ..= (3, 12)]),
//                     (ident!("condition", (3, 22) ..= (3, 30)), block![; (3, 32) ..= (5, 12)]),
//                     (ident!("condition", (5, 22) ..= (5, 30)), block![; (5, 32) ..= (7, 12)]),
//                     (ident!("condition", (7, 22) ..= (7, 30)), block![; (7, 32) ..= (9, 12)]),
//                     (ident!("condition", (9, 22) ..= (9, 30)), block![; (9, 32) ..= (11, 12)]),
//                 ],
//                 last: Some(block![; (11, 19) ..= (13, 12)])
//             }, (1, 12) ..= (13, 12))
//         ]);
//     }

//     /// Tests while and for loop as well as 
//     /// declarations, function calls, conditionals, assignment, ranges, and literals.
//     #[test]
//     fn loop_test() {
//         // barebones
//         assert_parse("while true {}", program![
//             expr_stmt!(Expr::While {
//                 condition: Box::new(literal!(Bool(true), (0, 6) ..= (0, 9))),
//                 block: block![; (0, 11) ..= (0, 12)]
//             }, (0, 0) ..= (0, 12))
//         ]);
//         assert_parse("for i in it {}", program![
//             expr_stmt!(Expr::For {
//                 ident: String::from("i"),
//                 iterator: Box::new(ident!("it", (0, 9) ..= (0, 10))),
//                 block: block![; (0, 12) ..= (0, 13)]
//             }, (0, 0) ..= (0, 13))
//         ]);

//         // full examples
//         assert_parse("
//             let i = 0;
//             while i < 10 {
//                 print(i);
//                 i = i + 1;
//             }
//         ", program![
//             Located::new(Stmt::Decl(Decl { 
//                 rt: ReasgType::Let, 
//                 pat: Located::new(Pat::Unit(decl_unit!("i")), (1, 16) ..= (1, 16)), 
//                 ty: None, 
//                 val: literal!(Int(0), (1, 20) ..= (1, 20))
//             }), (1, 12) ..= (1, 21)),
//             expr_stmt!(Expr::While {
//                 condition: Located::boxed(Expr::Comparison {
//                     left: Box::new(ident!("i", (2, 18) ..= (2, 18))), 
//                     rights: vec![(op::Cmp::Lt, literal!(Int(10), (2, 22) ..= (2, 23)))]
//                 }, (2, 18) ..= (2, 23)), 
//                 block: block![
//                     expr_stmt_with_semi!(Expr::Call {
//                         funct: Box::new(ident!("print", (3, 16) ..= (3, 20))),
//                         params: vec![ident!("i", (3, 22) ..= (3, 22))]
//                     }, (3, 16) ..= (3, 23)),
//                     expr_stmt_with_semi!(Expr::Assign(
//                         Located::new(Pat::Unit(asg_unit!("i")), (4, 16) ..= (4, 16)), 
//                         Box::new(binop! {
//                             Add, (4, 20) ..= (4, 24),
//                             ident!("i", (4, 20) ..= (4, 20)),
//                             literal!(Int(1), (4, 24) ..= (4, 24))
//                         })
//                     ), (4, 16) ..= (4, 24));
//                 (2, 25) ..= (5, 12)]
//             }, (2, 12) ..= (5, 12))
//         ]);

//         assert_parse("for i in 1..10 { print(i); }", program![
//             expr_stmt!(Expr::For {
//                 ident: String::from("i"), 
//                 iterator: Located::boxed(Expr::Range {
//                     left: Box::new(literal!(Int(1), (0, 9) ..= (0, 9))), 
//                     right: Box::new(literal!(Int(10), (0, 12) ..= (0, 13))), 
//                     step: None 
//                 }, (0, 9) ..= (0, 13)), 
//                 block: block![
//                     expr_stmt_with_semi!(Expr::Call {
//                         funct: Box::new(ident!("print", (0, 17) ..= (0, 21))),
//                         params: vec![ident!("i", (0, 23) ..= (0, 23))]
//                     }, (0, 17) ..= (0, 24));
//                 (0, 15) ..= (0, 27)]
//             }, (0, 0) ..= (0, 27))
//         ]);
//     }

//     #[test]
//     fn semicolon_test() {
//         assert_parse_fail("2 2", expected_tokens![;]);

//         assert_parse("if cond {}", program![
//             expr_stmt!(Expr::If {
//                 conditionals: vec![
//                     (ident!("cond", (0, 3) ..= (0, 6)), block![; (0, 8) ..= (0, 9)])
//                 ],
//                 last: None
//             }, (0, 0) ..= (0, 9))
//         ]);
//         assert_parse("if cond {};", program![
//             expr_stmt_with_semi!(Expr::If {
//                 conditionals: vec![
//                     (ident!("cond", (0, 3) ..= (0, 6)), block![; (0, 8) ..= (0, 9)])
//                 ],
//                 last: None
//             }, (0, 0) ..= (0, 9))
//         ]);

//         assert_parse("
//             let a = 1;
//             let b = 2;
//             let c = 3;
//             if cond {
//                 let d = 5;
//             }
//         ", program![
//             Located::new(Stmt::Decl(Decl { 
//                 rt: ReasgType::Let, 
//                 pat: Located::new(Pat::Unit(decl_unit!("a")), (1, 16) ..= (1, 16)), 
//                 ty: None, 
//                 val: literal!(Int(1), (1, 20) ..= (1, 20))
//             }), (1, 12) ..= (1, 21)),
//             Located::new(Stmt::Decl(Decl { 
//                 rt: ReasgType::Let, 
//                 pat: Located::new(Pat::Unit(decl_unit!("b")), (2, 16) ..= (2, 16)), 
//                 ty: None, 
//                 val: literal!(Int(2), (2, 20) ..= (2, 20))
//             }), (2, 12) ..= (2, 21)),
//             Located::new(Stmt::Decl(Decl { 
//                 rt: ReasgType::Let, 
//                 pat: Located::new(Pat::Unit(decl_unit!("c")), (3, 16) ..= (3, 16)), 
//                 ty: None, 
//                 val: literal!(Int(3), (3, 20) ..= (3, 20))
//             }), (3, 12) ..= (3, 21)),
//             expr_stmt!(Expr::If {
//                 conditionals: vec![(
//                     ident!("cond", (4, 15) ..= (4, 18)),
//                     block![
//                         Located::new(Stmt::Decl(Decl { 
//                             rt: ReasgType::Let, 
//                             pat: Located::new(Pat::Unit(decl_unit!("d")), (5, 20) ..= (5, 20)), 
//                             ty: None, 
//                             val: literal!(Int(5), (5, 24) ..= (5, 24))
//                         }), (5, 16) ..= (5, 25));
//                     (4, 20) ..= (6, 12)]
//                 )],
//                 last: None
//             }, (4, 12) ..= (6, 12))
//         ])
//     }

//     #[test]
//     fn type_test() {
//         fn assert_parse_type(input: &str, ty: Type) {
//             let tokens = unwrap_fe(tokenize(input), input);
//             assert_eq!(
//                 unwrap_fe(Parser::new(tokens, false).expect_type(), input),
//                 ty
//             )
//         }

//         assert_parse_type(
//             "int",
//             Type("int".to_string(), vec![])
//         );

//         assert_parse_type(
//             "dict<a, b>",
//             Type("dict".to_string(), vec![
//                 Located::new(Type("a".to_string(), vec![]), (0, 5) ..= (0, 5)),
//                 Located::new(Type("b".to_string(), vec![]), (0, 8) ..= (0, 8))
//             ])

//         );

//         assert_parse_type(
//             "dict<list<list<int>>, str>",
//             Type("dict".to_string(), vec![
//                 Located::new(Type("list".to_string(), vec![
//                     Located::new(Type("list".to_string(), vec![
//                         Located::new(Type("int".to_string(), vec![]), (0, 15) ..= (0,17))
//                     ]), (0, 10) ..= (0, 18))
//                 ]), (0, 5) ..= (0, 19)),
//                 Located::new(Type("str".to_string(), vec![]), (0, 22) ..= (0, 24))
//             ])
//         );
//     }

//     #[test]
//     fn unary_ops_test() {
//         assert_parse("+3;", program![
//             expr_stmt_with_semi!(Expr::UnaryOps {
//                 ops: vec![token![+].try_into().unwrap()],
//                 expr: Box::new(literal!(Int(3), (0, 1) ..= (0, 1)))
//             }, (0, 0) ..= (0, 1))
//         ]);

//         assert_parse("+++++++3;", program![
//             expr_stmt_with_semi!(Expr::UnaryOps {
//                 ops: vec![token![+].try_into().unwrap()].repeat(7),
//                 expr: Box::new(literal!(Int(3), (0, 7) ..= (0, 7)))
//             }, (0, 0) ..= (0, 7))
//         ]);

//         assert_parse("+-+-+-+-3;", program![
//             expr_stmt_with_semi!(Expr::UnaryOps {
//                 ops: vec![token![+].try_into().unwrap(), token![-].try_into().unwrap()].repeat(4),
//                 expr: Box::new(literal!(Int(3), (0, 8) ..= (0, 8)))
//             }, (0, 0) ..= (0, 8))
//         ]);

//         assert_parse("!+-+-+-+-3;", program![
//             expr_stmt_with_semi!(Expr::UnaryOps {
//                 ops: vec![
//                     token![!].try_into().unwrap(), 
//                     token![+].try_into().unwrap(), 
//                     token![-].try_into().unwrap(), 
//                     token![+].try_into().unwrap(), 
//                     token![-].try_into().unwrap(), 
//                     token![+].try_into().unwrap(), 
//                     token![-].try_into().unwrap(), 
//                     token![+].try_into().unwrap(), 
//                     token![-].try_into().unwrap()],
//                 expr: Box::new(literal!(Int(3), (0, 9) ..= (0, 9)))
//             }, (0, 0) ..= (0, 9))
//         ]);

//         assert_parse("+(+2);", program![
//             expr_stmt_with_semi!(Expr::UnaryOps {
//                 ops: vec![token![+].try_into().unwrap()].repeat(2),
//                 expr: Box::new(literal!(Int(2), (0, 3) ..= (0, 3)))
//             }, (0, 0) ..= (0, 4))
//         ])
//     }

//     #[test]
//     fn decl_test() {
//         assert_parse("
//             let a       = 0;
//             let mut b   = 1;
//             const c     = 2;
//             const mut d = 3;
//         ", program![
//             Located::new(Stmt::Decl(Decl { 
//                 rt: ReasgType::Let, 
//                 pat: Located::new(Pat::Unit(decl_unit!("a")), (1, 16) ..= (1, 16)), 
//                 ty: None, 
//                 val: literal!(Int(0), (1, 26) ..= (1, 26))
//             }), (1, 12) ..= (1, 27)),
//             Located::new(Stmt::Decl(Decl { 
//                 rt: ReasgType::Let, 
//                 pat: Located::new(Pat::Unit(decl_unit!(mut "b")), (2, 16) ..= (2, 20)), 
//                 ty: None, 
//                 val: literal!(Int(1), (2, 26) ..= (2, 26))
//             }), (2, 12) ..= (2, 27)),
//             Located::new(Stmt::Decl(Decl { 
//                 rt: ReasgType::Const, 
//                 pat: Located::new(Pat::Unit(decl_unit!("c")), (3, 18) ..= (3, 18)), 
//                 ty: None, 
//                 val: literal!(Int(2), (3, 26) ..= (3, 26))
//             }), (3, 12) ..= (3, 27)),
//             Located::new(Stmt::Decl(Decl { 
//                 rt: ReasgType::Const, 
//                 pat: Located::new(Pat::Unit(decl_unit!(mut "d")), (4, 18) ..= (4, 22)), 
//                 ty: None, 
//                 val: literal!(Int(3), (4, 26) ..= (4, 26))
//             }), (4, 12) ..= (4, 27))
//         ])
//     }
// }