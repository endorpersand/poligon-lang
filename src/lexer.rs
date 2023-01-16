//! Converts strings to sequences of tokens.
//! 
//! In a general sense, lexing is performed by reading the string, 
//! and repeatedly matching specific token patterns until the entire string is consumed.
//! 
//! This module provides:
//! - [`tokenize`]: A utility function that opaquely does the lexing from string to tokens.
//! - [`Lexer`]: The struct which does the entire lexing process.

use std::cmp::Ordering;
use std::collections::{VecDeque, HashMap};
use std::ops::RangeInclusive;

use lazy_static::lazy_static;
use regex::Regex;

use crate::err::{GonErr, FullGonErr};

use self::token::{Token, Keyword, OPMAP, Delimiter, token, FullToken};
pub mod token;

/// Convert a string and lex it into a sequence of tokens.
/// 
/// For more control, see the [`Lexer`] struct.
pub fn tokenize(input: &str) -> LexResult<Vec<FullToken>> {
    let mut lx = Lexer::new(input);
    lx.lex()?;
    lx.close()
}

/// An error that occurs in the lexing process.
#[derive(PartialEq, Eq, Debug)]
pub enum LexErr {
    /// Lexer found character that isn't used in Poligon code (e.g. emojis)
    UnknownChar(char),

    /// The lexer tried to parse a string or character literal 
    /// but there was no closing quote. (e.g. `"hello!`)
    UnclosedQuote,

    /// A specific character was expected at some given position
    /// 
    /// This is used for when a character literal exceeds one character: `'hello!'`.
    ExpectedChar(char),
    
    /// A char literal was empty (`''`).
    EmptyChar,

    /// The string of characters are punctuation but they don't create a valid operator ('@')
    UnknownOp(String),

    /// A delimiter was closed with the wrong type (e.g. `[ ... )`)
    MismatchedDelimiter,

    /// A bracket was not closed (e.g. `( ... `)
    UnclosedDelimiter,

    /// A delimiter got closed with a semicolon (e.g. `( ...; `)
    DelimiterClosedSemi,

    /// A comment was not closed (e.g. `/* ... `)
    UnclosedComment,

    /// Escape of the form '\x00' was invalid
    InvalidX,

    /// Escape of the form '\u{000000}' was invalid
    InvalidU,

    /// Character with given hex does not exist
    InvalidChar(u32)
}
/// A [`Result`] type for operations in the lexing process.
pub type LexResult<T> = Result<T, FullLexErr>;
type FullLexErr = FullGonErr<LexErr>;

impl GonErr for LexErr {
    fn err_name(&self) -> &'static str {
        "syntax error"
    }

    fn message(&self) -> String {
        match self {
            LexErr::UnknownChar(c)      => format!("invalid character {}", wrapq(*c)),
            LexErr::UnclosedQuote       => String::from("quote was never terminated"),
            LexErr::ExpectedChar(c)     => format!("expected character {}", wrapq(*c)),
            LexErr::EmptyChar           => String::from("char literal cannot be empty"),
            LexErr::UnknownOp(op)       => format!("operator \"{}\" does not exist", op),
            LexErr::MismatchedDelimiter => String::from("mismatched delimiter"),
            LexErr::UnclosedDelimiter   => String::from("delimiter was never terminated"),
            LexErr::DelimiterClosedSemi => String::from("unexpected ';'"),
            LexErr::UnclosedComment     => String::from("comment was never terminated"),
            LexErr::InvalidX            => String::from("invalid \\xXX escape"),
            LexErr::InvalidU            => String::from("invalid \\u{XXXX} escape"),
            LexErr::InvalidChar(c)      => format!("invalid char {:x}", c)
        }
    }
}

/// Enclose quotes around a character
/// 
/// For most characters, it will appear as: `'a', 'b', '"', '@'`, etc.
/// 
/// For ', it appears as `"'"`.
fn wrapq(c: char) -> String {
    if c == '\'' { format!("\"{}\"", c) } else { format!("'{}'", c) }
}

type Cursor = (usize, usize);

/// Shift cursor forward along a line
fn cur_shift((lno, cno): Cursor, chars: usize) -> Cursor {
    (lno, cno + chars)
}
/// Advance cursor given the character at the cursor and the characters following it
fn advance_cursor((lno, cno): Cursor, current: Option<char>, extra: &[char]) -> Cursor {
    let (mut dl, mut nc) = match current {
        Some('\n') => (1, 0),
        Some(_) => (0, cno + 1),
        None => (0, cno)
    };

    let mut split = extra.split(|&c| c == '\n');
    if let Some(fline) = split.next() {
        nc += fline.len();
        for line in split {
            dl += 1;
            nc = line.len();
        }
    }
    
    (lno + dl, nc)
}

/// Character classes that are treated differently in the lexer
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
enum CharClass {
    /// An alphabetic character (`a-z`, `A-Z`)
    Alpha,

    /// A numeric character (`0-9`)
    Numeric,

    /// An underscore (`_`)
    Underscore,

    /// Quote that encloses a char (`'`)
    CharQuote,

    /// Quote that encloses a str (`"`)
    StrQuote,

    /// Semicolon / line separator (`;`)
    Semi,

    /// Any miscellaneous ASCII punctuation
    Punct,

    /// Whitespace
    Whitespace,
}

impl CharClass {
    fn of(c: char) -> Option<Self> {
        if c.is_alphabetic()             { Some(Self::Alpha) }
        else if c.is_numeric()           { Some(Self::Numeric) }
        else if c == '_'                 { Some(Self::Underscore) }
        else if c == '\''                { Some(Self::CharQuote) }
        else if c == '"'                 { Some(Self::StrQuote) }
        else if c == ';'                 { Some(Self::Semi) }
        else if c.is_ascii_punctuation() { Some(Self::Punct) }
        else if c.is_whitespace()        { Some(Self::Whitespace) }
        else { None }
    }

    fn of_or_err(c: char, pt: (usize, usize)) -> LexResult<Self> {
        Self::of(c).ok_or_else(|| LexErr::UnknownChar(c).at(pt))
    }
}

/// A buffer that computes string and char literals 
/// and specially deals with escapes.
struct LiteralBuffer<'lx> {
    lexer: &'lx mut Lexer,

    /// Character which ends the literal.
    terminal: char,

    /// Buffer that represents what the literal holds.
    /// This should not contain the unescaped terminal character.
    buf: String
}

/// Methods for an attempt to add to the buffer to halt
enum LCErr {
    /// Cannot add because the next character is an unescaped terminal.
    HitTerminal,

    /// Cannot add because there are no more characters to parse.
    HitEOF,

    /// [`LexErr::InvalidX`]
    InvalidX,
    /// [`LexErr::InvalidU`]
    InvalidU,
    /// [`LexErr::InvalidChar`]
    InvalidChar(u32)
}
type LiteralCharResult<T> = Result<T, LCErr>;

impl<'lx> LiteralBuffer<'lx> {
    /// Borrow the lexer to create a LiteralBuffer.
    fn new(lexer: &'lx mut Lexer, terminal: char) -> Self {
        Self { lexer, terminal, buf: String::new() }
    }

    /// Attempt to add characters to the buffer.
    fn try_add(&mut self) -> LiteralCharResult<()> {
        match self.lexer.peek() {
            Some('\\') => self.try_add_esc(),
            Some(c) if c == self.terminal => Err(LCErr::HitTerminal),
            Some(_) => {
                let take_point = self.lexer.remaining.iter()
                    .position(|&c| c == self.terminal || c == '\\')
                    // take point doesn't appear, so consume entire lexer buffer
                    .unwrap_or(self.lexer.remaining.len());
                
                self.buf.extend(self.lexer.next_n(take_point));
                Ok(())
            },
            None => Err(LCErr::HitEOF),
        }
    }

    /// Parse an escape at the front of the lexer buffer and add to the literal buffer.
    fn try_add_esc(&mut self) -> LiteralCharResult<()> {
        lazy_static! {
            static ref BASIC_ESCAPES: HashMap<char, &'static str> = {
                let mut m = HashMap::new();
    
                m.insert('0',  "\0");
                m.insert('\\', "\\");
                m.insert('n',  "\n");
                m.insert('t',  "\t");
                m.insert('r',  "\r");
                m.insert('\'', "'");
                m.insert('"',  "\"");
                m.insert('\n', "");
                m
            };
        }

        match self.next_raw(false)? {
            '\\' => {
                let c = self.next_raw(true)?;

                // match a basic escape? add that basic escape to buffer
                if let Some(&escaped) = BASIC_ESCAPES.get(&c) {
                    self.buf.push_str(escaped);
                } else {
                    match c {
                        'u' => {
                            let c8: String = std::iter::repeat_with(|| self.next_raw(false))
                                .take(8)
                                .take_while(|c| !matches!(c, Ok('}')))
                                .collect::<Result<_, _>>()
                                .map_err(|_| LCErr::InvalidU)?;
                            
                            if c8.starts_with('{') && c8.len() < 8 {
                                let codepoint = u32::from_str_radix(&c8[1..], 16)
                                    .map_err(|_| LCErr::InvalidU)?;
                                
                                let chr = char::from_u32(codepoint)
                                    .ok_or(LCErr::InvalidChar(codepoint))?;
                                
                                self.buf.push(chr);
                            } else {
                                Err(LCErr::InvalidU)?
                            }
                        }
                        'x' => {
                            let c2: String = std::iter::repeat_with(|| self.next_raw(false))
                                .take(2)
                                .collect::<Result<_, _>>()
                                .map_err(|_| LCErr::InvalidX)?;
                            
                            let codepoint = u32::from_str_radix(&c2, 16)
                                .map_err(|_| LCErr::InvalidX)?;
                            
                            let chr = char::from_u32(codepoint)
                                .ok_or(LCErr::InvalidChar(codepoint))?;
                            
                            self.buf.push(chr);
                        },
                        c => {
                            self.buf.push('\\');
                            self.buf.push(c);
                        }
                    }
                }

                Ok(())
            },
            _ => unimplemented!("try_add_esc should not be called unless the first character is \\")
        }
    }

    /// Pull the next character from the lexer buffer.
    fn next_raw(&mut self, allow_term: bool) -> LiteralCharResult<char> {
        match self.lexer.peek() {
            Some(c) if c == self.terminal && !allow_term => { Err(LCErr::HitTerminal) }
            Some(_) => {
                Ok(self.lexer.next().unwrap())
            }
            None => { Err(LCErr::HitEOF) }
        }
    }

    fn cursor(&self) -> Cursor {
        self.lexer.cursor
    }

    fn cursor_range(&self) -> RangeInclusive<Cursor> {
        self.lexer.token_start ..= self.lexer.cursor
    }
}

/// The struct does the full lexing process.
pub struct Lexer {
    /// The tokens generated from the string.
    tokens: Vec<FullToken>,
    /// A stack to keep track of delimiters (and their original position)
    delimiters: Vec<(Cursor, Delimiter)>,
    
    /// The current position of the _current char.
    cursor: Cursor,
    /// The char that was last read.
    _current: Option<char>,

    /// The start position of the current token being evaluated.
    token_start: Cursor,
    /// The remaining characters in the buffer to be read.
    remaining: VecDeque<char>,
}

impl Lexer {
    /// Create a new lexer with an initial string input.
    pub fn new(input: &str) -> Self {
        Self {
            tokens: vec![],
            delimiters: vec![],

            cursor: (0, 0),
            token_start: (0, 0),
            _current: None,
            remaining: input.chars().collect(), 
        }
    }

    /// Lex what is currently in the input.
    pub fn lex(&mut self) -> LexResult<()> {
        while let Some(chr) = self.peek() {
            self.token_start = self.peek_cursor();
            let cls = CharClass::of_or_err(chr, self.token_start)?;
            
            match cls {
                CharClass::Alpha | CharClass::Underscore => self.push_ident()?,
                CharClass::Numeric => self.push_num(),
                CharClass::CharQuote  => self.push_char()?,
                CharClass::StrQuote   => self.push_str()?,
                CharClass::Punct      => {
                    if chr == '.' {
                        // dot acts as numeric if followed by more numerics
                        if let Some(&next) = self.remaining.get(1) {
                            let next_pos = advance_cursor(self.token_start, Some(next), &[]);
                            if CharClass::of_or_err(next, next_pos)? == CharClass::Numeric {
                                self.push_num()
                            } else {
                                self.push_punct()?
                            }
                        } else {
                            self.push_punct()?
                        }
                    } else {
                        self.push_punct()?
                    }
                },
                CharClass::Whitespace => { self.next(); },
                CharClass::Semi       => {
                    self.next();
                    self.push_token(token![;]);

                    if let Some((_, de)) = self.delimiters.last() {
                        match de {
                            Delimiter::LParen
                            | Delimiter::LSquare  => {
                                Err(LexErr::DelimiterClosedSemi.at(self.cursor))?
                            },

                            Delimiter::LCurly
                            | Delimiter::LComment => {},

                            Delimiter::RParen
                            | Delimiter::RSquare
                            | Delimiter::RCurly
                            | Delimiter::RComment => unreachable!(),
                        }
                    }
                },
            }
        }

        Ok(())
    }

    /// Check if the lexer can be consumed without error.
    /// 
    /// Lexer cannot be consumed if:
    /// - There are any unclosed delimiters.
    /// 
    /// This is used by [`Repl`][`crate::interpreter::Repl`] 
    /// to determine whether or not the lexer should be preserved between lines.
    pub fn try_close(&self) -> LexResult<()> {
        if let Some((p, _)) = self.delimiters.last() {
            return Err(LexErr::UnclosedDelimiter.at(*p));
        }

        Ok(())
    }

    /// Consume the lexer, returning the tokens in it 
    /// if the lexer is in a closable state ([`Lexer::try_close`]).
    pub fn close(self) -> LexResult<Vec<FullToken>> {
        self.try_close()?;
        Ok(self.tokens)
    }

    /// Append extra characters to the end of the lexer's input buffer.
    pub fn append(&mut self, input: &str) {
        self.remaining.extend(input.chars());
    }

    /// Look at the cursor of the next character in the input.
    fn peek_cursor(&self) -> Cursor {
        advance_cursor(self.cursor, self._current, &[])
    }

    /// Look at the next character in the input.
    /// 
    /// If there are no more characters in the input, return None.
    fn peek(&self) -> Option<char> {
        self.remaining.get(0).copied()
    }

    /// Consume the next character in the input and return it.
    fn next(&mut self) -> Option<char> {
        self.cursor = self.peek_cursor();
        
        let mcd = self.remaining.pop_front();
        self._current = mcd;
        mcd
    }

    /// Consume the next `n` characters in the input and return them.
    fn next_n(&mut self, n: usize) -> VecDeque<char> {
        let mut front = self.remaining.split_off(n);
        std::mem::swap(&mut self.remaining, &mut front);

        let slice = front.make_contiguous();
        self.cursor = advance_cursor(self.cursor, self._current, slice);
        self._current = slice.last().copied();
        front
    }

    /// Get the range of the current token being generated.
    fn token_range(&self) -> RangeInclusive<Cursor> {
        self.token_start ..= self.cursor
    }

    /// Add token to the token buffer, using the default range
    fn push_token(&mut self, t: Token) {
        self.push_token_with_range(t, self.token_range());
    }

    /// Add token to the token buffer, with a custom defined range
    fn push_token_with_range(&mut self, t: Token, r: RangeInclusive<Cursor>) {
        let ft = FullToken::new(t, r);
        self.tokens.push(ft);
    }

    /// Check if the next character in the input matches the given character class.
    /// 
    /// If it does, consume it and return the character.
    /// If it does not, return None.
    fn match_cls(&mut self, match_cls: CharClass) -> Option<char> {
        if self.peek().and_then(CharClass::of) == Some(match_cls) {
            self.next()
        } else {
            None
        }
    }

    /// Analyzes the next characters in the input as an identifier (e.g. abc, ade, aVariable, a123, a_).
    /// 
    /// This function consumes characters from the input and adds an identifier token in the output.
    fn push_ident(&mut self) -> LexResult<()> {
        let mut buf = String::new();

        while let Some(chr) = self.peek() {
            let cls = CharClass::of_or_err(chr, self.peek_cursor())?;
            match cls {
                CharClass::Alpha | CharClass::Underscore | CharClass::Numeric => {
                    buf.push(chr);
                    self.next();
                }
                _ => break
            }
        }

        let token = Keyword::get_kw(&buf)
            .unwrap_or(Token::Ident(buf));

        self.push_token(token);
        Ok(())
    }

    /// Analyzes the next characters in the input as a numeric value (e.g. 123, 123., 123.4).
    /// 
    /// This function consumes characters from the input and adds a numeric literal token in the output.
    fn push_num(&mut self) {
        let mut buf = String::new();

        while let Some(c) = self.match_cls(CharClass::Numeric) {
            buf.push(c);
        }

        // 123.ident  => [123][.][ident]
        // 123.444    => [123.444]
        // 123..444   => [123][..][444]
        // 123. + 444 => [123.] [+] [444]
        
        // peek next character. check if it's .
        if self.peek() == Some('.') {
            // whether the "." is part of the numeric or if it's a part of a spread/call operator
            // depends on the character after the "."
            
            // the "." is part of the numeric UNLESS
            // - the next character is a "."
            // - the next character is alpha/underscore

            // then scan for any further numerics after that "."
            let dot_is_numeric = match self.remaining.get(1) {
                Some('.') => false,
                Some(&chr) => !matches!(CharClass::of(chr), Some(CharClass::Alpha | CharClass::Underscore)),
                None => true,
            };

            if dot_is_numeric {
                buf.push(self.next().unwrap()); // "."

                while let Some(c) = self.match_cls(CharClass::Numeric) {
                    buf.push(c);
                }
            }
        }

        self.push_token(Token::Numeric(buf));
    }

    /// Analyzes the next characters in the input as a str (e.g. "hello").
    /// 
    /// This function consumes characters from the input and adds a str literal token in the output.
    fn push_str(&mut self) -> LexResult<()> {
        // UNWRAP: this should only be called if there's a quote character
        let qt = self.next().unwrap();
        
        let mut reader = LiteralBuffer::new(self, qt);
        let buf = loop {
            let chr_cursor = cur_shift(reader.cursor(), 1);
            match reader.try_add() {
                Ok(()) => {},
                Err(e) => Err(match e {
                    LCErr::HitTerminal => break reader.buf,
                    LCErr::HitEOF => {
                        LexErr::UnclosedQuote.at_range(reader.cursor_range())
                    },
                    LCErr::InvalidX         => LexErr::InvalidX.at_range(chr_cursor..reader.cursor()),
                    LCErr::InvalidU         => LexErr::InvalidU.at_range(chr_cursor..reader.cursor()),
                    LCErr::InvalidChar(c)   => LexErr::InvalidChar(c).at_range(chr_cursor..reader.cursor()),
                })?,
            }
        };
        
        // Assert next char matches quote:
        if self.next().unwrap() == qt {
            self.tokens.push(FullToken::new(Token::Str(buf), self.token_range()));
            Ok(())
        } else {
            // Loop can only be exited when terminal character is found.
            unreachable!();
        }
    }

    /// Analyzes the next characters in the input as a char (e.g. 'h').
    /// 
    /// This function consumes characters from the input and adds a char literal token in the output.
    fn push_char(&mut self) -> LexResult<()> {
        // UNWRAP: this should only be called if there's a quote character
        let qt = self.next().unwrap();
        
        let token_start = self.token_start;
        let token_start_p1 = cur_shift(token_start, 1);

        let mut reader = LiteralBuffer::new(self, qt);
        let c = match reader.try_add() {
            Ok(()) => {
                let buf = reader.buf;
                let len = buf.chars().count();
                
                // check that the add added only 1 char
                match len.cmp(&1) {
                    Ordering::Less    => Err(LexErr::UnclosedQuote.at(token_start)),
                    Ordering::Equal   => Ok(buf.chars().next().unwrap()),
                    Ordering::Greater => Err(LexErr::ExpectedChar(qt).at(cur_shift(token_start, 2))),
                }
            },
            Err(e) => Err(match e {
                LCErr::HitTerminal => LexErr::EmptyChar.at(self.cursor),
                LCErr::HitEOF      => LexErr::UnclosedQuote.at(token_start),
                LCErr::InvalidX       => LexErr::InvalidX.at_range(token_start_p1..self.cursor),
                LCErr::InvalidU       => LexErr::InvalidU.at_range(token_start_p1..self.cursor),
                LCErr::InvalidChar(c) => LexErr::InvalidChar(c).at_range(token_start_p1..self.cursor),
            }),
        }?;

        // Assert next char matches quote:
        match self.next() {
            Some(chr) if chr == qt => {
                self.push_token(Token::Char(c));
                Ok(())
            },
            Some(_) => Err(LexErr::ExpectedChar(qt).at(self.cursor)),
            None    => Err(LexErr::UnclosedQuote.at(token_start))
        }
    }

    /// Analyzes the next characters in the input as a set of punctuation marks.
    /// 
    /// This function consumes characters from the input and can add 
    /// operator, delimiter, or comment tokens to the output.
    fn push_punct(&mut self) -> LexResult<()> {
        let mut buf = String::new();

        while let Some(c) = self.match_cls(CharClass::Punct) {
            buf.push(c);
        }

        while !buf.is_empty() {
            let left = &buf[..1];
            let right = &buf[..];
    
            // Find the largest length operator that matches the start of the operator buffer.
            let (op, token) = OPMAP.range(left..=right)
                .rev() // largest length
                .find(|(&op, _)| buf.starts_with(op)) // that occurs in the text
                .ok_or_else(|| {
                    LexErr::UnknownOp(buf.clone())
                        .at_range(self.token_start..=self.cursor)
                })?;
            
            // Keep track of the delimiters.
            // If left delimiter, add to delimiter stack.
            // If right delimiter, verify the top of the stack is the matching left delimiter 
            //      (or error if mismatch).
            if let Token::Delimiter(d) = token {
                let pos = self.token_start;
                
                if !d.is_right() {
                    self.delimiters.push((pos, *d));
                } else {
                    self.match_delimiter(pos, *d)?;
                }
            }

            // Stop tokenizing when we're dealing with comments:
            if token == &token!["//"] {
                buf.drain(..2);
                return self.push_line_comment(buf);
            } else if token == &token!["/*"] {
                buf.drain(..2);
                return self.push_multi_comment(buf);
            }
            
            
            let len = op.len();

            let token_end = cur_shift(self.token_start, len - 1);
            self.push_token_with_range(token.clone(), self.token_start..=token_end);

            self.token_start = cur_shift(self.token_start, len);
            buf.drain(..len);
        }
        Ok(())
    }

    /// In `push_punct`, this is called to create a line comment from 
    /// `push_punct`'s leftover string buffer and characters in the input.
    /// 
    /// This function consumes characters from the input and adds a line comment 
    /// (a comment that goes to the end of a line) to the output.
    fn push_line_comment(&mut self, mut buf: String) -> LexResult<()> {
        while let Some(chr) = self.next() {
            if chr == '\n' { break; }
            buf.push(chr);
        }

        let buf = String::from(buf.trim()); // lame allocation.
        self.push_token(Token::Comment(buf, true));
        Ok(())
    }

    /// In `push_punct`, this is called to create a multi-line comment from 
    /// `push_punct`'s leftover string buffer and characters in the input.
    /// 
    /// This function consumes characters from the input and adds a multi-line comment 
    /// (/* this kind of comment */) to the output.
    fn push_multi_comment(&mut self, mut buf: String) -> LexResult<()> {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"/\*|\*/").unwrap();
        }

        // since it is not possible to get \n from anything here it is safe to use in this function only
        fn cur_shift_back((lno, cno): Cursor, chars: usize) -> Cursor {
            (lno, cno - chars)
        }

        let comment_start = cur_shift(self.token_start, 2);

        //read the current buffer to determine if any comments have been opened or closed
        // note that there are recursive comments:
        /* /* */ */
        /*/ */
        for m in RE.find_iter(&buf) {
            let start = m.start();
            match m.as_str() {
                "/*" => self.delimiters.push(
                    (cur_shift(comment_start, start), Delimiter::LComment)
                ),
                "*/" => self.match_delimiter(
                    cur_shift(comment_start, start), Delimiter::RComment
                )?,
                _ => {}
            }
        }

        'com: while let Some((_, Delimiter::LComment)) = self.delimiters.last() {
            while let Some(chr) = self.next() {
                buf.push(chr);

                if buf.ends_with("/*") {
                    self.delimiters.push((cur_shift_back(self.cursor, 1), Delimiter::LComment));
                } else if buf.ends_with("*/") {
                    self.match_delimiter(cur_shift_back(self.cursor, 1), Delimiter::RComment)?;
                    continue 'com;
                }
            }

            // if we got here, the entire string got consumed... 
            // and the comment is still open.
            return Err(LexErr::UnclosedComment.at_range(self.token_start..=self.cursor));
        }

        // there is a */ remaining:
        let (com, nbuf) = buf.rsplit_once("*/").expect("Expected closing */");
        
        // reinsert any remaining characters into the input
        let len = nbuf.len();
        self.remaining.extend(nbuf.chars());
        self.remaining.rotate_left(len);
        // move cursor back to the end of the comment
        self.cursor = cur_shift_back(self.cursor, len);

        self.push_token(Token::Comment(String::from(com.trim()), false));

        Ok(())
    }

    /// Verify that the top delimiter in the delimiter stack is the same as the argument's 
    /// delimiter type (Paren, Square, Curly, Comment) and pop it if they are the same.
    fn match_delimiter(&mut self, pos: Cursor, d: Delimiter) -> LexResult<()> {
        let left = if d.is_right() { d.reversed() } else { d };
        
        match self.delimiters.last() {
            Some((_, l)) if l == &left => {
                self.delimiters.pop();
                Ok(())
            },
            Some((p, _)) => Err(LexErr::MismatchedDelimiter.at_points(&[*p, pos])),
            None => Err(LexErr::MismatchedDelimiter.at(pos)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Assert that the string provided lexes into the vector of tokens.
    #[allow(unused)]
    fn assert_lex<T>(input: &str, result: &[T]) 
        where T: std::fmt::Debug,
            FullToken: PartialEq<T>
    {
        match tokenize(input) {
            Ok(t) => assert_eq!(&t, result),
            Err(e) => panic!("{}", e.full_msg(input)),
        }
    }

    /// Assert that the string provided errors with the given error when lexed.
    #[allow(unused)]
    fn assert_lex_fail<E>(input: &str, result: E) 
        where E: std::fmt::Debug,
            FullLexErr: PartialEq<E>
    {
        match tokenize(input) {
            Ok(t)  => panic!("Lexing resulted in value: {t:?}"),
            Err(e) => assert_eq!(e, result)
        }
    }

    #[test]
    fn ident_lex() {
        assert_lex("123 + abc * def", &[
            Token::Numeric("123".to_string()),
            token![+],
            Token::Ident("abc".to_string()),
            token![*],
            Token::Ident("def".to_string())
        ])
    }

    /// Tests operator resolution.
    #[test]
    fn op_lex() {
        assert_lex("!~==!~&&.=+-*<><<3", &[
            token![!],
            token![~],
            token![==],
            token![!],
            token![~],
            token![&&],
            token![.],
            token![=],
            token![+],
            token![-],
            token![*],
            token![<],
            token![>],
            token![<<],
            Token::Numeric("3".to_string())
        ]);

        assert_lex("<<<", &[token![<<], token![<]]);
    }

    /// Tests keywords, multiple lines, semicolon detection.
    #[test]
    fn declaration_lex() {
        assert_lex("
            let x = 1;
            const y = abc;
            const mut z = 5;
        ", &[
            token![let],
            Token::Ident("x".to_string()),
            token![=],
            Token::Numeric("1".to_string()),
            token![;],

            token![const],
            Token::Ident("y".to_string()),
            token![=],
            Token::Ident("abc".to_string()),
            token![;],

            token![const],
            token![mut],
            Token::Ident("z".to_string()),
            token![=],
            Token::Numeric("5".to_string()),
            token![;],
        ])
    }

    /// Tests delimiter matching & mismatching.
    #[test]
    fn delimiter_lex() {
        assert_lex("(1)", &[
            token!["("],
            Token::Numeric("1".to_string()),
            token![")"]
        ]);

        assert_lex_fail("(1", LexErr::UnclosedDelimiter.at((0, 0)));
        assert_lex_fail("1)", LexErr::MismatchedDelimiter.at((0, 1)));
        assert_lex_fail("(1]", LexErr::MismatchedDelimiter.at_points(&[(0, 0), (0, 2)]));
    }

    /// Tests numeric edge cases.
    #[test]
    fn numeric_lex() {
        assert_lex("123.444", &[
            Token::Numeric("123.444".to_string())
        ]);
        assert_lex("123.ident", &[
            Token::Numeric("123".to_string()), 
            token![.], 
            Token::Ident("ident".to_string())
        ]);
        assert_lex("123..444", &[
            Token::Numeric("123".to_string()),
            token![..],
            Token::Numeric("444".to_string())
        ]);
        assert_lex("123. + 444", &[
            Token::Numeric("123.".to_string()),
            token![+],
            Token::Numeric("444".to_string())
        ]);

        assert_lex(".4", &[Token::Numeric(".4".to_string())]);
        assert_lex(".", &[token![.]]);
    }

    #[test]
    fn comment_lex() {
        // single line
        assert_lex("
            // there is whitespace up to here v
            // abc 123! :)                     
            1;
        ", &[
            Token::Comment("there is whitespace up to here v".to_string(), true),
            Token::Comment("abc 123! :)".to_string(), true),
            Token::Numeric("1".to_string()),
            token![;]
        ]);

        // multiline
        assert_lex("
            /* multiline :O */
            2;
        ", &[
            Token::Comment("multiline :O".to_string(), false),
            Token::Numeric("2".to_string()),
            token![;]
        ]);


        assert_lex("
            /*
                edge whitespace should be trimmed.
            */
            3;
        ", &[
            Token::Comment("edge whitespace should be trimmed.".to_string(), false),
            Token::Numeric("3".to_string()),
            token![;]
        ]);

        // recursive comments
        assert_lex("
            /* /* 
                comments in comments :)
            */ */
            recursive;
        ", &[
            Token::Comment("/* 
                comments in comments :)
            */".to_string(), false),
            Token::Ident("recursive".to_string()),
            token![;],
        ]);

        assert_lex_fail("
            /*
            /*
                unclosed comment
            */
        ", LexErr::UnclosedComment);
    }

    #[test]
    fn char_lex() {
        // basic char checks
        assert_lex("'a'", &[Token::Char('a')]);
        assert_lex_fail("'ab'", LexErr::ExpectedChar('\'').at((0, 2)));
        assert_lex_fail("''", LexErr::EmptyChar.at((0, 0)));

        // basic escape tests
        assert_lex("'\\''", &[Token::Char('\'')]); // '\''
        assert_lex("'\\n'", &[Token::Char('\n')]); // '\n'
        assert_lex("\"\\e\"", &[Token::Str(String::from("\\e"))]); // '\e'
        assert_lex_fail("'\\n", LexErr::UnclosedQuote); // '\n
        assert_lex_fail("'\\\n'", LexErr::UnclosedQuote); // '\[new line]'
        
        // \x test
        assert_lex("'\\x14'", &[Token::Char('\x14')]);   // '\x14'
        assert_lex_fail("'\\x'", LexErr::InvalidX);   // '\x'
        assert_lex_fail("'\\x0'", LexErr::InvalidX);  // '\x0'
        assert_lex_fail("'\\xqq'", LexErr::InvalidX); // '\xqq'

        // \u test
        assert_lex("'\\u{0}'", &[Token::Char('\0')]); // '\u{0}'
        assert_lex("'\\u{1f97a}'", &[Token::Char('\u{1f97a}')]); // '\u{1f97a}'
        assert_lex_fail("'\\u{21f97a}'", LexErr::InvalidChar(0x21F97Au32)); // '\u{21f97a}'
        assert_lex_fail("'\\u{0000000}'", LexErr::InvalidU); // '\u{0000000}'
    }
}