//! Converts strings to sequences of tokens.
//! 
//! TODO! more doc

use std::collections::{VecDeque, HashMap};

use lazy_static::lazy_static;
use regex::Regex;

use crate::err::{GonErr, FullGonErr};

use self::token::{Token, Keyword, OPMAP, Delimiter, token, FullToken};
pub mod token;

pub fn tokenize(input: &str) -> LexResult<Vec<Token>> {
    Lexer::new(input)?.lex()
}

/// An error that occurs when a string could not be parsed into tokens.
#[derive(PartialEq, Eq, Debug)]
pub enum LexErr {
    /// There was a character that isn't used in Poligon code (e.g. emojis)
    UnknownChar(char),   // Char isn't used in Poligon code

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

    /// A comment was not closed (e.g. `/* ... `)
    UnclosedComment,

    InvalidEscape(char),
    InvalidX,
    InvalidU,
    InvalidChar(u32)
}
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
            LexErr::UnclosedComment     => String::from("comment was never terminated"),
            LexErr::InvalidEscape(e)    => format!("unknown character escape '{}'", 
                if e == &'\n' {
                    String::from("\\n") 
                } else {
                    String::from(*e)
                }),
            LexErr::InvalidX            => String::from("invalid \\xXX escape"),
            LexErr::InvalidU            => String::from("invalid \\u{XXXX} escape"),
            LexErr::InvalidChar(c)      => format!("invalid char {:x}", c),
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

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
enum CharClass {
    Alpha,
    Numeric,
    Underscore,
    CharQuote,
    StrQuote,
    Punct,
    Whitespace
}

impl CharClass {
    fn of(c: &char) -> Option<Self> {
        if c.is_alphabetic()             { Some(Self::Alpha) }
        else if c.is_numeric()           { Some(Self::Numeric) }
        else if c == &'_'                { Some(Self::Underscore) }
        else if c == &'\''               { Some(Self::CharQuote) }
        else if c == &'"'                { Some(Self::StrQuote) }
        else if c.is_ascii_punctuation() { Some(Self::Punct) }
        else if c.is_whitespace()        { Some(Self::Whitespace) }
        else { None }
    }
}

type Cursor = (usize, usize);

fn cur_shift((lno, cno): Cursor, chars: usize) -> Cursor {
    (lno, cno + chars)
}

struct LiteralCharReader<'lx> {
    lexer: &'lx mut Lexer,
    terminal: char
}

enum LCError {
    HitTerminal,
    HitEOF,
    InvalidEscape(char),
    InvalidX,
    InvalidU,
    InvalidChar(u32)
}
type LiteralCharResult<T> = Result<T, LCError>;


impl<'lx> LiteralCharReader<'lx> {
    fn new(lexer: &'lx mut Lexer, terminal: char) -> Self {
        Self { lexer, terminal }
    }

    fn next_raw(&mut self, allow_term: bool) -> LiteralCharResult<char> {
        match self.lexer.peek() {
            Some(c) if c == &self.terminal && !allow_term => { Err(LCError::HitTerminal) }
            Some(_) => {
                Ok(self.lexer.next().unwrap())
            }
            None => { Err(LCError::HitEOF) }
        }
    }

    fn next(&mut self) -> LiteralCharResult<Option<char>> {
        lazy_static! {
            static ref BASIC_ESCAPES: HashMap<char, Option<char>> = {
                let mut m = HashMap::new();
    
                m.insert('0',  Some('\0'));
                m.insert('\\', Some('\\'));
                m.insert('n',  Some('\n'));
                m.insert('t',  Some('\t'));
                m.insert('r',  Some('\r'));
                m.insert('\'', Some( '\''));
                m.insert('"',  Some('"'));
                m.insert('\n', None);
                m
            };
        }
        
        match self.next_raw(false)? {
            '\\' => {
                let c = self.next_raw(true)?;

                if let Some(escaped) = BASIC_ESCAPES.get(&c) {
                    Ok(*escaped)
                } else {
                    match c {
                        'u' => {
                            let c8: String = std::iter::repeat_with(|| self.next_raw(false))
                                .take(8)
                                .take_while(|c| !matches!(c, Ok('}')))
                                .collect::<Result<_, _>>()
                                .map_err(|_| LCError::InvalidU)?;
                            
                            if c8.starts_with('{') && c8.len() < 8 {
                                let codepoint = u32::from_str_radix(&c8[1..], 16)
                                    .map_err(|_| LCError::InvalidU)?;
                                
                                let chr = char::from_u32(codepoint)
                                    .ok_or(LCError::InvalidChar(codepoint))?;
                                
                                Ok(Some(chr))
                            } else {
                                Err(LCError::InvalidU)
                            }
                        }
                        'x' => {
                            let c2: String = std::iter::repeat_with(|| self.next_raw(false))
                                .take(2)
                                .collect::<Result<_, _>>()
                                .map_err(|_| LCError::InvalidX)?;
                            
                            let codepoint = u32::from_str_radix(&c2, 16)
                                .map_err(|_| LCError::InvalidX)?;
                            
                            let chr = char::from_u32(codepoint)
                                .ok_or(LCError::InvalidChar(codepoint))?;
                            
                            Ok(Some(chr))
                        },
                        c => {
                            Err(LCError::InvalidEscape(c))
                        }
                    }
                }
            },
            c => Ok(Some(c))
        }
        // // if terminal, mark the char as terminal
        // if self.peek() != Some(&term_chr) {
        //     // if there's a 
        //     match unwrap_chr!(self.next()) {
        //         '\\' => {
        //             let c = unwrap_chr!(self.next());

        //             match BASIC_ESCAPES.get(&c) {
        //                 Some(c) => Ok((c.clone(), 2)),
        //                 None => match c {
        //                     'x' => {},
        //                     'u' => {}
        //                 },
        //             }
        //         },
        //         c => Ok((Some(c), 1)),
        //     }
        // } else {
        //     Err(LCError::HitTerminal)
        // }
    }

    fn one(lexer: &'lx mut Lexer, terminal: char) -> LiteralCharResult<char> {
        let mut reader = Self::new(lexer, terminal);
        
        reader.next()?.ok_or(LCError::InvalidChar('\n' as u32))
    }

    fn cursor(&self) -> Cursor {
        self.lexer.cursor
    }

    fn cursor_range(&self) -> std::ops::RangeInclusive<Cursor> {
        self.lexer.token_start ..= self.lexer.cursor
    }
}

macro_rules! char_class_or_err {
    ($c:expr, $e:expr) => {
        CharClass::of($c).ok_or_else(|| LexErr::UnknownChar(*$c).at($e))
    }
}

pub struct Lexer {
    tokens: Vec<FullToken>,
    delimiters: Vec<(Cursor, Delimiter)>,
    
    cursor: Cursor,
    token_start: Cursor,
    _current: Option<char>,
    remaining: VecDeque<char>,
}

impl Lexer {
    pub fn new(input: &str) -> LexResult<Self> {
        Ok(Self {
            tokens: vec![],
            delimiters: vec![],

            cursor: (0, 0),
            token_start: (0, 0),
            _current: None,
            remaining: input.chars().collect(), 
        })
    }

    /// Perform the actual lexing. 
    /// 
    /// Takes a string and converts it into a list of tokens.
    pub fn lex(mut self) -> LexResult<Vec<Token>> {
        self.partial_lex()?;
        self.close()
    }

    /// Lex whatever is currently in the input, but do not consume the lexer.
    pub fn partial_lex(&mut self) -> LexResult<()> {
        while let Some(&chr) = self.peek() {
            self.token_start = self.peek_cursor();
            let cls = char_class_or_err!(&chr, self.token_start)?;
            
            match cls {
                CharClass::Alpha | CharClass::Underscore => self.push_ident()?,
                CharClass::Numeric    => self.push_numeric(),
                CharClass::CharQuote  => self.push_char()?,
                CharClass::StrQuote   => self.push_str()?,
                CharClass::Punct      => self.push_punct()?,
                CharClass::Whitespace => { self.next(); },
            }
        }

        Ok(())
    }

    /// Add characters to the end of the input.
    pub fn append_input(&mut self, input: &str) {
        self.remaining.extend(input.chars());
    }

    pub fn try_close(&self) -> LexResult<()> {
        if let Some((p, _)) = self.delimiters.last() {
            return Err(LexErr::UnclosedDelimiter.at(*p));
        }

        Ok(())
    }
    /// Consume the lexer and return the tokens in it.
    pub fn close(self) -> LexResult<Vec<Token>> {
        self.try_close()?;
        // TODO: FullToken
        Ok(self.tokens.into_iter().map(|t| t.tt).collect())
    }

    fn peek_cursor(&self) -> Cursor {
        let (lno, cno) = &self.cursor;

        match &self._current {
            Some('\n') => (lno + 1, 0),
            Some(_)    => (*lno, cno + 1),
            None       => (*lno, *cno)
        }
    }

    /// Look at the next character in the input.
    /// 
    /// If there are no more characters in the input, return None.
    fn peek(&self) -> Option<&char> {
        self.remaining.get(0)
    }

    /// Consume the next character in the input and return it.
    fn next(&mut self) -> Option<char> {
        self.cursor = self.peek_cursor();
        
        let mcd = self.remaining.pop_front();
        self._current = mcd;
        mcd
    }

    fn token_range(&self) -> std::ops::RangeInclusive<Cursor> {
        self.token_start ..= self.cursor
    }

    fn push_token(&mut self, t: Token) {
        self.push_token_with_range(t, self.token_range());
    }
    fn push_token_with_range(&mut self, t: Token, r: std::ops::RangeInclusive<Cursor>) {
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
            let cls = char_class_or_err!(chr, self.peek_cursor())?;
            match cls {
                CharClass::Alpha | CharClass::Underscore | CharClass::Numeric => {
                    buf.push(*chr);
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
    fn push_numeric(&mut self) {
        let mut buf = String::new();

        while let Some(c) = self.match_cls(CharClass::Numeric) {
            buf.push(c);
        }

        // 123.ident  => [123][.][ident]
        // 123.444    => [123.444]
        // 123..444   => [123][..][444]
        // 123. + 444 => [123.] [+] [444]
        
        // peek next character. check if it's .
        if self.peek() == Some(&'.') {
            // whether the "." is part of the numeric or if it's a part of a spread/call operator
            // depends on the character after the "."
            
            // the "." is part of the numeric UNLESS
            // - the next character is a "."
            // - the next character is alpha/underscore

            // then scan for any further numerics after that "."
            let dot_is_numeric = match self.remaining.get(1) {
                Some('.') => false,
                Some(chr) => !matches!(CharClass::of(chr), Some(CharClass::Alpha | CharClass::Underscore)),
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
        
        let mut buf = String::new();
        let mut reader = LiteralCharReader::new(self, qt);

        loop {
            let chr_cursor = cur_shift(reader.cursor(), 1);
            match reader.next() {
                Ok(c) => buf.extend(c),
                Err(e) => Err(match e {
                    LCError::HitTerminal => break,
                    LCError::HitEOF => {
                        LexErr::UnclosedQuote.at_range(reader.cursor_range())
                    },
                    LCError::InvalidEscape(e) => LexErr::InvalidEscape(e).at_range(chr_cursor..reader.cursor()),
                    LCError::InvalidX         => LexErr::InvalidX.at_range(chr_cursor..reader.cursor()),
                    LCError::InvalidU         => LexErr::InvalidU.at_range(chr_cursor..reader.cursor()),
                    LCError::InvalidChar(c)   => LexErr::InvalidChar(c).at_range(chr_cursor..reader.cursor()),
                })?,
            }
        }
        
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

        let c = match LiteralCharReader::one(self, qt) {
            Ok(c) => Ok(c),
            Err(e) => Err(match e {
                LCError::HitTerminal => LexErr::EmptyChar.at(self.cursor),
                LCError::HitEOF      => LexErr::UnclosedQuote.at(token_start),
                LCError::InvalidEscape(e) => LexErr::InvalidEscape(e).at_range(token_start_p1..self.cursor),
                LCError::InvalidX         => LexErr::InvalidX.at_range(token_start_p1..self.cursor),
                LCError::InvalidU         => LexErr::InvalidU.at_range(token_start_p1..self.cursor),
                LCError::InvalidChar(c)   => LexErr::InvalidChar(c).at_range(token_start_p1..self.cursor),
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

    macro_rules! assert_lex {
        ($e1:literal => $e2:expr) => {
            assert_eq!(tokenize($e1), Ok($e2))
        }
    }

    macro_rules! assert_lex_fail {
        ($e1:literal => $e2:expr) => {
            assert_eq!(tokenize($e1), Err($e2))
        }
    }

    macro_rules! assert_lex_fail_basic {
        ($e1:literal => $e2:expr) => {
            if let Err(e) = tokenize($e1) {
                assert_eq!(&e.err, &$e2)
            } else {
                assert!(false, "Lexing did not result in error.");
            }


        }
    }

    #[test]
    fn basic_lex() {
        assert_lex!("123 + abc * def" => vec![
            Token::Numeric("123".to_string()),
            token![+],
            Token::Ident("abc".to_string()),
            token![*],
            Token::Ident("def".to_string())
        ])
    }

    #[test]
    fn multiline_lex() {
        assert_lex!("
        let x = 1;
        const y = abc;
        const mut z = 5;
        " => vec![
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

    #[test]
    fn delimiter_lex() {
        assert_lex!("(1)" => vec![
            token!["("],
            Token::Numeric("1".to_string()),
            token![")"]
        ]);
        assert_lex_fail!("(1" => LexErr::UnclosedDelimiter.at((0, 0)));
        assert_lex_fail!("1)" => LexErr::MismatchedDelimiter.at((0, 1)));
        assert_lex_fail!("(1]" => LexErr::MismatchedDelimiter.at_points(&[(0, 0), (0, 2)]));
    }

    #[test]
    fn numeric_lex() {
        assert_lex!("123.444"    => vec![
            Token::Numeric("123.444".to_string())
        ]);
        assert_lex!("123.ident"  => vec![
            Token::Numeric("123".to_string()), 
            token![.], 
            Token::Ident("ident".to_string())
        ]);
        assert_lex!("123..444"   => vec![
            Token::Numeric("123".to_string()),
            token![..],
            Token::Numeric("444".to_string())
        ]);
        assert_lex!("123. + 444" => vec![
            Token::Numeric("123.".to_string()),
            token![+],
            Token::Numeric("444".to_string())
        ]);
    }

    #[test]
    fn comment_lex() {
        // assert_lex!("
        // // abc 123! :)               
        // 1;" => vec![
        //     Token::Comment("abc 123! :)".to_string(), true),
        //     Token::Numeric("1".to_string()),
        //     token![;]
        // ]);

        // assert_lex!("
        // /* multiline :O */
        // 2;" => vec![
        //     Token::Comment("multiline :O".to_string(), false),
        //     Token::Numeric("2".to_string()),
        //     token![;]
        // ]);


        // assert_lex!("
        // /*
        //     line!
        // */
        // 3;
        // " => vec![
        //     Token::Comment("line!".to_string(), false),
        //     Token::Numeric("3".to_string()),
        //     token![;]
        // ]);

        assert_lex!("
        /* /* 
            comments in comments :)
        */ */
        recursive;
        " => vec![
            Token::Comment("/* 
            comments in comments :)
        */".to_string(), false),
            Token::Ident("recursive".to_string()),
            token![;],
        ]);
    }

    #[test]
    fn char_lex() {
        assert_lex!("'a'" => vec![Token::Char('a')]);
        assert_lex_fail!("'ab'" => LexErr::ExpectedChar('\'').at((0, 2)));
        assert_lex_fail!("''" => LexErr::EmptyChar.at((0, 0)));
    }

    #[test]
    fn escape_char_lex() {
        // basic escape tests
        assert_lex!("'\\''" => vec![Token::Char('\'')]); // '\''
        assert_lex!("'\\n'" => vec![Token::Char('\n')]); // '\n'
        assert_lex_fail_basic!("'\\n" => LexErr::UnclosedQuote); // '\n
        assert_lex_fail_basic!("'\\e'" => LexErr::InvalidEscape('e')); // '\e'
        
        // \x test
        assert_lex!("'\\x14'" => vec![Token::Char('\x14')]);   // '\x14'
        assert_lex_fail_basic!("'\\x'" => LexErr::InvalidX);   // '\x'
        assert_lex_fail_basic!("'\\x0'" => LexErr::InvalidX);  // '\x0'
        assert_lex_fail_basic!("'\\xqq'" => LexErr::InvalidX); // '\xqq'

        assert_lex!("'\\u{0}'" => vec![Token::Char('\0')]); // '\u{0}'
        assert_lex!("'\\u{1f97a}'" => vec![Token::Char('\u{1f97a}')]); // '\u{1f97a}'
        assert_lex_fail_basic!("'\\u{21f97a}'" => LexErr::InvalidChar(0x21F97Au32)); // '\u{21f97a}'
        assert_lex_fail_basic!("'\\u{0000000}'" => LexErr::InvalidU); // '\u{0000000}'
    }
}