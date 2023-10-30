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

pub mod pat;

use std::convert::Infallible;

use crate::err::{GonErr, FullGonErr, impl_from_err};
use crate::lexer::token::{Token, token, FullToken, Stream, TokenTree, Group, TTKind};
use crate::ast::PatErr;
use crate::span::{Span, Cursor, Spanned};
pub use pat::TokenPattern;

/// Parses a sequence of tokens to an isolated parseable program tree. 
/// 
/// This function will attempt to consume the entire stream as a value, 
/// raising a [`ParseErr::UnexpectedToken`] error if not fully consumed.
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
pub fn parse<P: Parseable>(stream: Stream) -> Result<P, P::Err> 
    where P::Err: From<FullParseErr>
{
    let mut parser = Parser::new(stream);

    let result = parser.parse::<P>()?;
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
/// Trait indicating that a type *may* be created out of a stream of tokens.
pub trait TryParseable: Sized {
    /// Error to raise if a failure happens during the parsing of an item.
    type Err;

    /// Try to parse the item out of the stream, 
    /// returning `None` if the format does not match,
    /// and raising an error if the format matches but an error occurs during parsing.
    fn try_read(parser: &mut Parser<'_>) -> Result<Option<Self>, Self::Err>;
}
impl<T: TryParseable> Parseable for Option<T> {
    type Err = T::Err;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        T::try_read(parser)
    }
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
impl_from_err! { PatErr => ParseErr: err => { Self::AsgPatErr(err) } }

/// A [`Result`] type for operations in the parsing process.
pub type ParseResult<T> = Result<T, FullParseErr>;
type FullParseErr = FullGonErr<ParseErr>;

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
    pub(crate) cursor: ParCursor<'s>,
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
    pub fn try_parse<P: TryParseable>(&mut self) -> Result<Option<P>, P::Err> {
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
    pub fn expect<'tt, P: TokenPattern<'tt>>(&mut self, pat: P) -> Result<P::Munched, FullGonErr<P::Err>>
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
        let end = usize::min(n, stream.len());

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
    /// the tuple's items must be of a type which can be created by [`Parser::try_parse`].
    pub fn parse_tuple<T: TryParseable>(&mut self, tok: Token) -> Result<Tuple<T>, T::Err> {
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

            Ok((pairs, end))
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
impl<'s> Iterator for Parser<'s> {
    type Item = &'s TokenTree;

    fn next(&mut self) -> Option<Self::Item> {
        let tok = self.cursor.next();
        
        if let Some(t) = tok {
            self.span_collectors.attach(t.span());
        }

        tok
    }
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
pub struct Entry<K, V, E> {
    /// The key item
    pub key: K,
    /// The value item
    pub val: V,
    /// The span of characters ranged by the entire entry
    pub span: Span,
    /// The error this entry raises if it could not be parsed.
    /// 
    /// This is attached on the type so that Parseable/TryParseable 
    /// have something to bind to.
    _err: std::marker::PhantomData<*const E>
}
impl<K, V, E> crate::span::Spanned for Entry<K, V, E> {
    fn span(&self) -> Span {
        self.span
    }
}

impl<K, V, E> TryParseable for Entry<K, V, E> 
    where K: TryParseable,
          V: Parseable,
          E: From<K::Err> + From<V::Err> + From<FullParseErr>
{
    type Err = E;

    fn try_read(parser: &mut Parser<'_>) -> Result<Option<Self>, Self::Err> {
        let (result, span) = parser.try_spanned(|parser| {
            let Some(k) = parser.try_parse()? else { return Result::<_, E>::Ok(None) };
            parser.expect(token![:])?;
            let v = parser.parse()?;

            Ok(Some((k, v)))
        })?;

        Ok(result.map(|(key, val)| Entry { key, val, span, _err: std::marker::PhantomData }))
    }
}
impl<K, V, E> Parseable for Entry<K, V, E> 
    where K: Parseable,
          V: Parseable,
          E: From<K::Err> + From<V::Err> + From<FullParseErr>
{
    type Err = E;

    fn read(parser: &mut Parser<'_>) -> Result<Self, Self::Err> {
        let ((key, val), span) = parser.try_spanned(|parser| {
            let k = parser.parse()?;
            parser.expect(token![:])?;
            let v = parser.parse()?;

            Result::<_, E>::Ok((k, v))
        })?;

        Ok(Self { key, val, span, _err: std::marker::PhantomData })
    }
}