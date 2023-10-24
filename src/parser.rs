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
pub mod pat;

use std::convert::Infallible;

use crate::{GonErr, delim};
use crate::err::FullGonErr;
use crate::lexer::token::{Token, token, FullToken, Stream, TokenTree, Group, TTKind};
use crate::ast::{self, PatErr};
use crate::span::{Span, Cursor, Spanned};
pub use pat::TokenPattern;

use self::pat::MatchFn;

/// Parses a sequence of tokens to an isolated parseable program tree. 
/// 
/// This function will attempt to consume the entire stream as a value, 
/// raising a [`ParseErr::StreamNotFullyConsumed`] if not fully consumed.
/// 
/// for more control, use [`Parser::parse`].
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
pub fn parse<P: Parseable<Err = FullParseErr>>(stream: Stream) -> ParseResult<P> {
    let mut parser = Parser::new(stream);

    let result = parser.parse()?;
    parser.close()?;
    Ok(result)
}

/// Trait indicating that a type can be created out of a stream of tokens.
pub trait Parseable: Sized {
    /// Error to raise if a failure happens during the parsing of an item.
    type Err;

    /// Try to parse the item out of the stream, raising the error if failing.
    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err>;
}

/// A cursor that allows simple scanning and traversing over a stream of tokens.
pub struct ParCursor<'s> {
    stream: Stream<'s>,
    
    /// The end character's range
    eof: Cursor
}
impl<'s> ParCursor<'s> {
    /// Creates a new cursor.
    pub fn new(stream: Stream<'s>) -> Self {
        let eof = if let Some(tok) = stream.last() {
            let (lno, cno) = tok.span().end();
            (lno, cno + 1)
        } else {
            (0, 0)
        };

        ParCursor { stream, eof }
    }

    /// Peeks the current token.
    pub fn peek(&self) -> Option<&TokenTree> {
        self.stream.first()
    }

    /// Peeks the nth next token in the stream, with 0 being the current token.
    pub fn peek_nth(&self, n: usize) -> Option<&TokenTree> {
        self.stream.get(n)
    }

    /// Peeks at the span of the current token.
    pub fn peek_span(&self) -> Span {
        match self.peek() {
            Some(t) => t.span(),
            None => Span::one(self.eof),
        }
    }
    fn peek_token_span(&self) -> Span {
        match self.peek() {
            Some(TokenTree::Token(token)) => token.span(),
            Some(TokenTree::Group(group)) => group.left_span,
            None => Span::one(self.eof),
        }
    }

    /// Advances the cursor only if the current token tree matches the predicate.
    pub fn next_if(&mut self, f: impl FnOnce(&TokenTree) -> bool) -> Option<&TokenTree> {
        match f(self.peek()?) {
            true  => self.next(),
            false => None,
        }
    }
    /// Advances the cursor only if the predicate is Some for the next TokenTree.
    pub fn next_if_map<T>(&mut self, f: impl FnOnce(&TokenTree) -> Option<&T>) -> Option<&T> {
        self.next_if_generic(f)
    }
    /// Advances the cursor only if the predicate is Some for the next TokenTree.
    pub fn next_if_generic<'a, T: 'a>(&mut self, f: impl FnOnce(&'a TokenTree) -> Option<T>) -> Option<T> 
        where 's: 'a
    {
        let (first, rest) = self.stream.split_first()?;
        
        let out = f(first);
        if out.is_some() {
            self.stream = rest;
        }
        out
    }

    /// The number of tokens remaining in the stream
    pub fn len(&self) -> usize {
        self.stream.len()
    }
    /// Whether the stream has ended
    pub fn is_empty(&self) -> bool {
        self.stream.is_empty()
    }

    /// Creates an error at the current position.
    pub fn error<E: GonErr>(&self, err: E) -> FullGonErr<E> {
        err.at_range(self.peek_token_span())
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
    type Item = &'s TokenTree;

    fn next(&mut self) -> Option<Self::Item> {
        let (first, rest) = self.stream.split_first()?;
        
        self.stream = rest;
        Some(first)
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

    /// The parser tried to match a pattern and it did not work!
    UnexpectedToken,

    /// An error occurred in creating an assignment pattern.
    AsgPatErr(PatErr),

    /// Parser is not in intrinsic mode and therefore cannot use an intrinsic identifier
    NoIntrinsicIdents,

    /// Parser is not in intrinsic mode and therefore this statement cannot be defined
    NoIntrinsicStmts,
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
            ParseErr::UnexpectedToken    => write!(f, "unexpected token"),
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
        <[_]>::fail_err(&[$(token![$t]),*])
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

impl AsRef<[TokenTree]> for Group {
    fn as_ref(&self) -> &[TokenTree] {
        &self.content
    }
}

#[derive(Default)]
struct SpanCollectors(Vec<Span>);
impl SpanCollectors {
    fn push(&mut self, span: Span) {
        self.0.push(span);
    }
    fn pop(&mut self) -> Option<Span> {
        self.0.pop()
    }
    fn attach(&mut self, span: Span) {
        if let Some(collector) = self.0.last_mut() {
            *collector += span;
        }
    }
}

/// A struct that can read a token stream into any [`Parseable`] items.
/// 
/// This parser is used to more ergonomically parse items and provides the following utilities:
/// - [`Parser::parse`], [`Parser::try_parse`]: Parses items
/// - [`Parser::match_`], [`Parser::expect`]: Parses individual tokens
/// - [`Parser::parse_tuple`]: Parses a repeated set of items, divided by separators
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
pub struct Parser<'s> {
    cursor: ParCursor<'s>,
    span_collectors: SpanCollectors
}

impl<'s> Parser<'s> {
    /// Creates a new parser.
    pub fn new<S: AsRef<[TokenTree]> + ?Sized>(stream: &'s S) -> Self {
        Parser { cursor: ParCursor::new(stream.as_ref()), span_collectors: Default::default() }
    }

    /// Parses a [`Parseable`] item.
    pub fn parse<P: Parseable>(&mut self) -> Result<P, P::Err> {
        P::read(self)
    }

    /// Tries to parse an item, returning `None` if it fails.
    /// 
    /// This tries to parse any items P such that `Option<P>` can be parsed.
    pub fn try_parse<P>(&mut self) -> Result<Option<P>, <Option<P> as Parseable>::Err>
        where Option<P>: Parseable
    {
        Option::<P>::read(self)
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
    pub fn match_<'tt, P: TokenPattern<'tt>>(&mut self, pat: P) -> Option<P::Munched> 
        where 's: 'tt
    {
        let hit = self.cursor.next_if_generic(|tt| P::try_munch(&pat, tt))?;

        self.span_collectors.attach(hit.span());
        Some(hit)
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
    pub fn expect<'tt, P: TokenPattern<'tt>>(&mut self, pat: P) -> ParseResult<P::Munched> 
        where 's: 'tt
    {
        let hit = self.cursor.next_if_generic(|tt| pat.try_munch(tt))
            .ok_or_else(|| self.cursor.error(pat.fail_err()))?;

        self.span_collectors.attach(hit.span());
        Ok(hit)
    }

    /// Peeks at the current token tree in the stream, returning its [`TTKind`].
    pub fn peek(&self) -> Option<TTKind> {
        self.cursor.peek().map(TokenTree::kind)
    }
    /// Peeks at the nth next token tree in the stream, returning its [`TTKind`].
    /// 
    /// `0` here represents the current token in the stream.
    pub fn peek_nth(&self, n: usize) -> Option<TTKind> {
        self.cursor.peek_nth(n).map(TokenTree::kind)
    }
    /// Peeks at the next few token trees in the stream, returning their [`TTKind`]s in a slice.
    /// 
    /// This will return at most `n` token trees, but it can return fewer 
    /// if there are fewer than `n` token trees remaining in the stream.
    pub fn peek_slice(&self, n: usize) -> Box<[TTKind]> {
        let stream = self.cursor.stream;
        let end = usize::max(n, stream.len());

        stream[0..end].iter()
            .map(TokenTree::kind)
            .collect()
    }

    /// Peks at the current token tree in the stream
    pub fn peek_tree(&self) -> Option<&TokenTree> {
        self.cursor.peek()
    }

    /// Tests if there are any more token trees in the parser's stream.
    pub fn is_empty(&self) -> bool {
        self.cursor.is_empty()
    }

    /// Creates a span block over the function call, which keeps track of the tokens
    /// that the parsed item spans and returns the joined span of all the tokens.
    pub fn spanned<T>(&mut self, f: impl FnOnce(&mut Parser<'s>) -> T) -> (T, Span) {
        self.try_spanned(|p| Ok::<_, Infallible>(f(p)))
            .unwrap()
    }

    /// Similar to [`Parser::spanned`], but has the ability to be fallible.
    /// - If the provided function returns `Option<T>`, this function returns `Option<(T, Span)>`.
    /// - If the provided function returns `Result<T, E>`, this function returns `Result<(T, Span), E>`.
    pub fn try_spanned<T, S>(&mut self, f: impl FnOnce(&mut Parser<'s>) -> S) -> S::Transposed 
        where S: tspan::TransposeSpan<T>
    {
        self.span_collectors.push(self.cursor.peek_span());
        let out = f(self);
        let span = self.span_collectors.pop()
            .expect("span block should have existed");
        
        self.span_collectors.attach(span);

        S::transpose((out, span))
    }

    /// This parses a tuple (a repeated set of items divided by a separator).
    /// 
    /// This function requires a separator token and 
    /// the tuple's items must be of a type which can be created by [`Parse::try_parse`].
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

    /// Attempts to close the parser, failing if the stream is not empty.
    pub fn close(self) -> ParseResult<()> {
        self.close_or_else(|| ParseErr::UnexpectedToken)
    }

    /// Attempts to close the parser, failing (and calling the error function) if the stream is not empty.
    pub fn close_or_else<E: GonErr>(self, e: impl FnOnce() -> E) -> Result<(), FullGonErr<E>> {
        match self.is_empty() {
            true  => Ok(()),
            false => Err(self.cursor.error(e()))
        } 
    }
}
impl Iterator for Parser<'_> {
    type Item = FullToken;

    fn next(&mut self) -> Option<Self::Item> {
        let tok = self.cursor.next();
        
        if let Some(t) = tok {
            self.span_collectors.attach(t.span());
        }

        tok.cloned().map(|t| t.try_into().unwrap()) // TODO
    }
}

fn parse_stmts_closed(parser: &mut Parser<'_>) -> ParseResult<Vec<ast::Stmt>> {
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

/// A repeated set of items, separated by some separator.
pub struct Tuple<T> {
    pairs: Vec<(T, FullToken)>,
    end: Option<T>,
    span: Span
}
impl<T> Tuple<T> {
    /// The items held by this tuple.
    /// 
    /// This erases the information on what separators were used.
    pub fn values(self) -> impl Iterator<Item=T> {
        self.pairs.into_iter()
            .map(|(t, _)| t)
            .chain(self.end)
    }

    /// Tests if the tuple ended with a terminator token 
    /// or if it ended with a tuple item.
    pub fn ended_on_terminator(&self) -> bool {
        self.end.is_none()
    }

    /// Tests if the tuple has no items.
    pub fn is_empty(&self) -> bool {
        self.pairs.is_empty() && self.end.is_none()
    }
    /// Counts how many items the tuple has.
    pub fn len(&self) -> usize {
        self.pairs.len() + if self.end.is_some() { 1 } else { 0 }
    }
    /// Asserts that the tuple is not empty, calling the provided error function if it is empty.
    pub fn assert_non_empty<E>(self, e: impl FnOnce() -> E) -> Result<Self, E> {
        match self.is_empty() {
            false => Ok(self),
            true  => Err(e()),
        }
    }
    /// Asserts that the parser used for this tuple is not empty.
    /// 
    /// If the parser is not empty, 
    /// this function will execute the provided `needs_sep` function if the tuple ended on an item,
    /// and it will execute the provided `needs_t` function if the tuple ended on a separator.
    pub fn assert_closed<'s, 'tt, E: GonErr>(
        self, 
        parser: Parser<'s>, 
        needs_sep: impl FnOnce() -> E, 
        needs_item: impl FnOnce() -> E
    ) -> Result<Self, FullGonErr<E>> 
        where 's: 'tt
    {
        parser.close_or_else(|| match self.ended_on_terminator() {
            true  => needs_item(),
            false => needs_sep()
        })?;

        Ok(self)
    }
}
impl<T> crate::span::Spanned for Tuple<T> {
    fn span(&self) -> Span {
        self.span
    }
}

/// An entry, consisting of a `K` item, followed by `:`, followed by a `V` item.
pub struct Entry<K, V> {
    /// The key item
    pub key: K,
    /// The value item
    pub val: V,
    /// The span of characters ranged by the entire entry
    pub span: Span
}
impl<K, V> crate::span::Spanned for Entry<K, V> {
    fn span(&self) -> Span {
        self.span
    }
}
// This cannot be generalized, as it causes recursion problems.
impl<V: Parseable<Err=FullParseErr>> Parseable for Option<Entry<ast::Expr, V>> {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
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

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
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

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
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

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
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

impl Parseable for Option<ast::Ident> {
    type Err = Infallible;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        use TTKind::Token as Tk;

        let ident = match parser.peek_slice(2).as_ref() {
            [Tk(token![#]), Tk(Token::Ident(_))] => {
                let (ident, span) = parser.spanned(|parser| {
                    parser.expect(token![#]).unwrap(); // should be unreachable
                    let Some(FullToken { kind: Token::Ident(ident), span: _ }) = parser.next() else {
                        unreachable!()
                    };

                    String::from("#") + &ident
                });

                Some(ast::Ident { ident, span })
            },
            [Tk(Token::Ident(_)), ..] => {
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

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        parser.try_parse()?
            .ok_or(parser.cursor.error(ParseErr::ExpectedIdent))
    }
}

impl Parseable for Option<ast::StrLiteral> {
    type Err = Infallible;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
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
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        parser.try_parse()?
            .ok_or(parser.cursor.error(ParseErr::ExpectedLiteral))
    }
}

const REASG_TOKENS: [Token; 2] = [token![let], token![const]];
impl Parseable for Option<ast::ReasgType> {
    type Err = Infallible;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {

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

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        parser.try_parse()?
            .ok_or_else(|| {
                parser.cursor.error(REASG_TOKENS.fail_err())
            })
    }
}

impl Parseable for Option<ast::Stmt> {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let st = match parser.peek() {
            Some(TTKind::Token(tok)) => match tok {
                token![let] | token![const] => Some(ast::Stmt::Decl(parser.parse()?)),
                token![return]   => Some(ast::Stmt::Return(parser.parse()?)),
                token![break]    => Some(ast::Stmt::Break(parser.parse()?)),
                token![continue] => Some(ast::Stmt::Continue(parser.parse()?)),
                token![throw]    => Some(ast::Stmt::Throw(parser.parse()?)),
                token![fun]      => Some(ast::Stmt::FunDecl(parser.parse()?)),
                token![extern]   => Some(ast::Stmt::ExternFunDecl(parser.parse()?)),
                token![class]    => Some(ast::Stmt::Class(parser.parse()?)),
                token![fit]      => Some(ast::Stmt::FitClassDecl(parser.parse()?)),
                token![import]   => match parser.peek_nth(1) {
                    Some(TTKind::Token(t)) => match t {
                        Token::Ident(id) if id == "intrinsic" => Some(ast::Stmt::ImportIntrinsic(parser.parse()?)),
                        _ => Some(ast::Stmt::Import(parser.parse()?))
                    }
                    _ => None
                }
                token![global]   => Some(ast::Stmt::IGlobal(parser.parse()?)),
                _                => parser.try_parse()?.map(ast::Stmt::Expr),
            }
            _ => None
        };

        Ok(st)
    }
}

impl Parseable for Option<ast::Type> {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let (result, span) = parser.try_spanned(|parser| {
            let Some(ident) = parser.try_parse()? else { return ParseResult::Ok(None) };

            let mut group = Parser::new(parser.expect(delim!["[]"])?);
            let args = group.parse_tuple(token![,])?
                .assert_non_empty(|| parser.cursor.error(ParseErr::ExpectedType))?
                .assert_closed(group, 
                    || token![,].fail_err(),
                    || ParseErr::ExpectedType
                )?;

            Ok(Some((ident, args.values().collect())))
        })?;

        Ok(result.map(|(ident, params)| ast::Type { ident, params, span }))
    }
}
impl Parseable for ast::Type {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        parser.try_parse()?
            .ok_or_else(|| parser.cursor.error(ParseErr::ExpectedType))
    }
}

impl Parseable for ast::Decl {
    type Err = FullParseErr;

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
            
            ParseResult::Ok((rt, pat, ty, expr))
        })?;

        Ok(ast::Decl { rt, pat, ty, val, span })
    }
}
impl Parseable for Option<ast::DeclPat> {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
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
                        || ParseErr::ExpectedPattern,
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
    
                        ParseResult::Ok((mt, parser.parse()?))
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
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        parser.try_parse()?
            .ok_or_else(|| parser.cursor.error(ParseErr::ExpectedPattern))
    }
}

impl Parseable for ast::Return {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let (me, span) = parser.try_spanned(|parser| {
            parser.expect(token![return])?;
            parser.try_parse()
        })?;

        Ok(Self { expr: me, span })
    }
}
impl Parseable for ast::Break {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        parser.try_spanned(|parser| parser.expect(token![break]))
            .map(|(_, span)| Self { span })
    }
}
impl Parseable for ast::Continue {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        parser.try_spanned(|parser| parser.expect(token![continue]))
            .map(|(_, span)| Self { span })
    }
}
impl Parseable for ast::Throw {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let (message, span) = parser.try_spanned(|parser| {
            parser.expect(token![throw])?;
            parser.parse()
        })?;

        Ok(Self { message, span })
    }
}
impl Parseable for Option<ast::Param> {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
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

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
            parser.try_parse()?
                .ok_or_else(|| parser.cursor.error(ParseErr::ExpectedParam))
    }
}
impl Parseable for ast::FunSignature {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let ((ident, generics, params, ret), span) = parser.try_spanned(|parser| {
            parser.expect(token![fun])?;
            let ident = parser.parse()?;

            let generics = if let Some(group) = parser.match_(delim!["[]"]) {
                let mut content = Parser::new(group);
                
                content.parse_tuple(token![,])?
                    .assert_non_empty(|| parser.cursor.error(ParseErr::ExpectedIdent))?
                    .assert_closed(content, 
                        || expected_tokens![,],
                        || ParseErr::ExpectedParam,
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
                    || ParseErr::ExpectedParam,
                )?
                .values()
                .collect();

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

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
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
                    .assert_non_empty(|| parser.cursor.error(ParseErr::ExpectedIdent))?
                    .assert_closed(content, 
                        || expected_tokens![,],
                        || ParseErr::ExpectedParam,
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
                    || ParseErr::ExpectedParam,
                )?
                .values()
                .collect();
            
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

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
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

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let (sig, span) = parser.try_spanned(|parser| {
            parser.expect(token![extern])?;
            parser.parse()
        })?;

        Ok(Self { sig, span })
    }
}
impl Parseable for Option<ast::FieldDecl> {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
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

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        parser.try_parse()?
            .ok_or_else(|| parser.cursor.error(ParseErr::ExpectedIdent))
    }
}

impl Parseable for ast::Class {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let ((ident, generic_params, fields, methods), span) = parser.try_spanned(|parser| {
            parser.expect(token![class])?;
            let ident = parser.parse()?;
            let generic_params = match parser.match_(delim!["[]"]) {
                Some(group) => {
                    let mut content = Parser::new(group);
                    content.parse_tuple(token![,])?
                        .assert_non_empty(|| parser.cursor.error(ParseErr::ExpectedIdent))?
                        .assert_closed(content,
                            || expected_tokens![,],
                            || ParseErr::ExpectedIdent,
                        )?
                        .values()
                        .collect()
                },
                None => vec![]
            };

            let mut block_parser = Parser::new(parser.expect(delim!["{}"])?);

            let fields = block_parser.parse_tuple(token![,])?
                .values()
                .collect();
            
            let mut methods = vec![];
            while let Some(TTKind::Token(token![fun])) = block_parser.peek() {
                methods.push(block_parser.parse()?);
            }

            block_parser.close()?;
            
            ParseResult::Ok((ident, generic_params, fields, methods))
        })?;

        Ok(Self { ident, generic_params, fields, methods, span })
    }
}
impl Parseable for ast::Import {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let (path, span) = parser.try_spanned(|parser| {
            parser.expect(token![import])?;
            parser.parse()
        })?;

        Ok(Self { path, span })
    }
}
impl Parseable for ast::ImportIntrinsic {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let (_, span) = parser.try_spanned(|parser| {
            parser.expect(token![import])?;
            parser.expect( Token::Ident(String::from("intrinsic")) )
        })?;

        Ok(Self { span })
    }
}
impl Parseable for ast::IGlobal {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
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
            content.close()?;
            
            ParseResult::Ok((ty, methods))
        })?;

        Ok(Self { ty, methods, span })
    }
}
impl Parseable for ast::Block {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let group = parser.expect(delim!["{}"])?;

        let mut content = Parser::new(group);
        let stmts = parse_stmts_closed(&mut content)?;
        content.close()?;

        Ok(ast::Block { stmts, span: group.span() })
    }
}
impl Parseable for ast::StaticPath {
    type Err = FullParseErr;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let ((ty, attr), span) = parser.try_spanned(|parser| {
            let ty = parser.parse()?;
            parser.expect(token![::])?;
            let attr = parser.parse()?;

            ParseResult::Ok((ty, attr))
        })?;

        Ok(Self { ty, attr, span })
    }
}