use std::collections::VecDeque;

use lazy_static::lazy_static;
use regex::Regex;

use crate::err::{GonErr, FullGonErr};

use self::token::{Token, Keyword, OPMAP, Delimiter, token};
pub mod token;

pub fn tokenize(input: &str) -> LexResult<Vec<Token>> {
    Lexer::new(input)?.lex()
}

#[derive(PartialEq, Eq, Debug)]
pub enum LexErr {
    UnknownChar(char),   // Char isn't used in Poligon code
    UnclosedQuote,       // Hit EOF instead of closing quote
    ExpectedChar(char),  // Expected a specific character, f.e. '
    EmptyChar,           // ''
    UnknownOp(String),   // This operator cannot be resolved
    MismatchedDelimiter, // A bracket was closed with the wrong type
    UnclosedDelimiter,   // A bracket wasn't closed
    UnclosedComment,     // Hit EOF on /* */
}
type LexResult<T> = Result<T, LexErr>;
type FullLexErr = FullGonErr<LexErr>;

impl GonErr for LexErr {
    fn err_name(&self) -> &'static str {
        "syntax error"
    }

    fn message(&self) -> String {
        match self {
            LexErr::UnknownChar(c)      => format!("invalid character {}", wrapq(*c)),
            LexErr::UnclosedQuote       => format!("quote was never terminated"),
            LexErr::ExpectedChar(c)     => format!("expected character {}", wrapq(*c)),
            LexErr::EmptyChar           => format!("char literal cannot be empty"),
            LexErr::UnknownOp(_op)      => format!("unexpected operator"),
            LexErr::MismatchedDelimiter => format!("mismatched delimiter"),
            LexErr::UnclosedDelimiter   => format!("delimiter was never terminated"),
            LexErr::UnclosedComment     => format!("comment was never terminated"),
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

pub struct Lexer {
    tokens: Vec<Token>,
    delimiters: Vec<Delimiter>,
    
    cursor: Cursor,
    _current: Option<char>,
    remaining: VecDeque<CharData>,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
enum CharClass {
    Alpha,
    Numeric,
    Underscore,
    CharQuote,
    StrQuote,
    Punct,
    // NewLine,
    Whitespace
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
struct CharData {
    chr: char,
    cls: CharClass
}

impl CharClass {
    fn of(c: char) -> Option<Self> {
        if c.is_alphabetic()             { Some(Self::Alpha) }
        else if c.is_numeric()           { Some(Self::Numeric) }
        else if c == '_'                 { Some(Self::Underscore) }
        else if c == '\''                { Some(Self::CharQuote) }
        else if c == '"'                 { Some(Self::StrQuote) }
        else if c.is_ascii_punctuation() { Some(Self::Punct) }
        // else if c == '\n'                { Some(Self::NewLine) }
        else if c.is_whitespace()        { Some(Self::Whitespace) }
        else { None }
    }
}

impl CharData {
    fn new(c: char) -> Result<Self, char> {
        match CharClass::of(c) {
            Some(cls) => Ok(Self { chr: c, cls }),
            None => Err(c),
        }
    }
}

type Cursor = (usize, usize);

struct CursorTicker {
    // assume cursor is pointing to the character AFTER the last character
    cursor: Cursor
}

impl CursorTicker {
    fn increment_by(&mut self, dlno: usize, dcno: usize) {
        let (ilno, icno) = self.cursor;

        self.cursor = if dlno != 0 {
            (ilno + dlno, dcno)
        } else {
            (ilno, icno + dcno)
        }
    }

    fn fwd_cd(&mut self, cds: &[CharData]) {
        // lines from bottom to top
        let mut chiter = cds.rsplit(|cd| cd.chr == '\n');

        let (dlno, dcno) = match chiter.next() {
            // non-empty:
            // count how many lines we've traversed, and the length of the last line
            Some(line) => (chiter.count(), line.len()),
            // empty:
            None => (0, 0),
        };

        self.increment_by(dlno, dcno);
    }

    fn fwd_str(&mut self, s: &str) {
        // lines from bottom to top
        let mut chiter = s.rsplit('\n');

        let (dlno, dcno) = match chiter.next() {
            // non-empty:
            // count how many lines we've traversed, and the length of the last line
            Some(line) => (chiter.count(), line.len()),
            // empty:
            None => (0, 0),
        };

        self.increment_by(dlno, dcno);
    }

    fn fwd_chr(&mut self, c: char) {
        let (dlno, dcno) = if c == '\n' { (1, 0) } else { (0, 1) };
        self.increment_by(dlno, dcno);
    }
}

impl Lexer {
    pub fn new(input: &str) -> LexResult<Self> {
        let mut lexer = Self {
            tokens: vec![],
            delimiters: vec![],

            cursor: (0, 0),
            _current: None,
            remaining: VecDeque::new(), 
        };
        lexer.append_input(input)?;

        Ok(lexer)
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
        while let Some(CharData { cls, .. }) = self.peek() {
            match cls {
                CharClass::Alpha | CharClass::Underscore => self.push_ident(),
                CharClass::Numeric    => self.push_numeric(),
                CharClass::CharQuote  => self.push_char()?,
                CharClass::StrQuote   => self.push_str()?,
                CharClass::Punct      => self.push_punct()?,
                CharClass::Whitespace => { self.next(); },
            }
        }

        Ok(())
    }

    /// Add characters to the back of the input.
    /// This will output a lex error if a character is not recognized.
    pub fn append_input(&mut self, input: &str) -> LexResult<()> {
        let mut ticker = CursorTicker { cursor: self.peek_cursor() };
        ticker.fwd_cd(self.remaining.make_contiguous());

        for mcd in input.chars().map(CharData::new) {
            match mcd {
                Ok(cd) => {
                    ticker.fwd_chr(cd.chr);
                    self.remaining.push_back(cd);
                },
                Err(c) => return Err(LexErr::UnknownChar(c)),
            }
        }

        Ok(())
    }

    pub fn try_close(&self) -> LexResult<()> {
        if !self.delimiters.is_empty() {
            Err(LexErr::UnclosedDelimiter)?
        }

        Ok(())
    }
    /// Consume the lexer and return the tokens in it.
    pub fn close(self) -> LexResult<Vec<Token>> {
        self.try_close()?;
        Ok(self.tokens)
    }

    fn peek_cursor(&self) -> Cursor {
        let (lno, cno) = &self.cursor;

        match &self._current {
            Some('\n') => (lno + 1, *cno),
            Some(_)    => (*lno, cno + 1),
            None       => (*lno, *cno)
        }
    }

    /// Look at the next character in the input.
    /// 
    /// If there are no more characters in the input, return None.
    fn peek(&self) -> Option<&CharData> {
        self.remaining.get(0)
    }

    /// Consume the next character in the input and return it.
    fn next(&mut self) -> Option<CharData> {
        self.cursor = self.peek_cursor();
        
        let mcd = self.remaining.pop_front();
        self._current = mcd.map(|cd| cd.chr);
        mcd
    }

    /// Check if the next character in the input matches the given character class.
    /// 
    /// If it does, consume it and return the character.
    /// If it does not, return None.
    fn match_cls(&mut self, match_cls: CharClass) -> Option<char> {
        match self.peek() {
            Some(CharData { cls, .. }) if cls == &match_cls => {
                self.next().map(|cd| cd.chr)
            }
            _ => None
        }
    }

    /// Analyzes the next characters in the input as an identifier (e.g. abc, ade, aVariable, a123, a_).
    /// 
    /// This function consumes characters from the input and adds an identifier token in the output.
    fn push_ident(&mut self) {
        let mut buf = String::new();

        while let Some(CharData { chr, cls }) = self.peek() {
            match cls {
                CharClass::Alpha | CharClass::Underscore | CharClass::Numeric => {
                    buf.push(*chr);
                    self.next();
                }
                _ => break
            }
        }

        let token = Keyword::get_kw(&buf)
            .unwrap_or_else(|| Token::Ident(buf));

        self.tokens.push(token);
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
        if matches!(self.peek(), Some(CharData { chr: '.', .. })) {
            // whether the "." is part of the numeric or if it's a part of a spread/call operator
            // depends on the character after the "."
            
            // the "." is part of the numeric UNLESS
            // - the next character is a "."
            // - the next character is alpha/underscore

            // then scan for any further numerics after that "."
            match self.remaining.get(1) {
                Some(CharData { chr: '.', ..}) => {},
                Some(CharData { cls: CharClass::Alpha | CharClass::Underscore, .. }) => {}

                _ => {
                    buf.push(self.next().unwrap().chr); // "."

                    while let Some(c) = self.match_cls(CharClass::Numeric) {
                        buf.push(c);
                    }
                }
            }
        }
        self.tokens.push(Token::Numeric(buf));
    }

    /// Analyzes the next characters in the input as a str (e.g. "hello").
    /// 
    /// This function consumes characters from the input and adds a str literal token in the output.
    fn push_str(&mut self) -> LexResult<()> {
        let qt = self.next()
            .expect("String was validated to have a character, but failed to pop quotation mark")
            .chr;

        let mut buf = String::new();
        loop {
            let c = self.next()
                .ok_or(LexErr::UnclosedQuote)? // no more chars, hit EOF
                .chr;

            if c == qt { break; }
            buf.push(c);
        }
        
        self.tokens.push(Token::Str(buf));
        Ok(())
    }

    /// Analyzes the next characters in the input as a char (e.g. 'h').
    /// 
    /// This function consumes characters from the input and adds a char literal token in the output.
    fn push_char(&mut self) -> LexResult<()> {
        let qt = self.next()
            .expect("String was validated to have a character, but failed to pop quotation mark")
            .chr;

        // Get the next character:
        let c = self.next().ok_or(LexErr::UnclosedQuote)?.chr;
        if c == qt {
            Err(LexErr::EmptyChar)?;
        }

        // Assert next char matches quote:
        match self.next() {
            Some(CharData { chr, .. }) if chr == qt => {
                self.tokens.push(Token::Char(c));
                Ok(())
            },
            Some(_) => Err(LexErr::ExpectedChar(qt)),
            None => Err(LexErr::UnclosedQuote)
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

        while buf.len() > 0 {
            let left = &buf[..1];
            let right = &buf[..];
    
            // Find the largest length operator that matches the start of the operator buffer.
            let (op, token) = OPMAP.range(left..=right)
                .rev()
                .filter(|(&op, _)| buf.starts_with(op))
                .next()
                .ok_or_else(|| LexErr::UnknownOp(buf.clone()))?;
            
            // Keep track of the delimiters.
            // If left delimiter, add to delimiter stack.
            // If right delimiter, verify the top of the stack is the matching left delimiter 
            //      (or error if mismatch).
            if let Token::Delimiter(d) = token {
                if !d.is_right() {
                    self.delimiters.push(*d);
                } else {
                    self.match_delimiter(*d)?;
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
            
            self.tokens.push(token.clone());
            
            let len = op.len();
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
        while let Some(CharData {chr, ..}) = self.next() {
            if chr == '\n' { break; }
            buf.push(chr);
        }

        let buf = String::from(buf.trim()); // lame allocation.
        self.tokens.push(Token::Comment(buf, true));
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

        // check recursive comments:
        /* /* */ */
        for m in RE.find_iter(&buf) {
            match m.as_str() {
                "/*" => self.delimiters.push(Delimiter::LComment),
                "*/" => self.match_delimiter(Delimiter::RComment)?,
                _ => {}
            }
        }

        'com: while self.delimiters.last() == Some(&Delimiter::LComment) {
            while let Some(CharData {chr, ..}) = self.next() {
                buf.push(chr);

                if buf.ends_with("/*") {
                    self.delimiters.push(Delimiter::LComment);
                } else if buf.ends_with("*/") {
                    self.match_delimiter(Delimiter::RComment)?;
                    continue 'com;
                }
            }

            // if we got here, the entire string got consumed... 
            // and the comment is still open.
            return Err(LexErr::UnclosedComment);
        }

        // there is a */ remaining:
        let (com, nbuf) = buf.rsplit_once("*/").expect("Expected closing */");
        
        self.tokens.push(Token::Comment(String::from(com.trim()), false));

        // reinsert any remaining characters into the input
        let len = nbuf.len();
        let chrs = nbuf
            .chars()
            // EXPECT: ok b/c we should've already seen this data
            .map(|c| CharData::new(c)
                .expect("Invalid character was inexplicably inserted in comment"));

        self.remaining.extend(chrs);
        self.remaining.rotate_left(len);

        Ok(())
    }

    /// Verify that the top delimiter in the delimiter stack is the same as the argument's 
    /// delimiter type (Paren, Square, Curly, Comment) and pop it if they are the same.
    fn match_delimiter(&mut self, d: Delimiter) -> LexResult<()> {
        let left = if d.is_right() { d.reversed() } else { d };
        
        match self.delimiters.last() {
            Some(l) if l == &left => Ok({ self.delimiters.pop(); }),
            Some(_) => Err(LexErr::MismatchedDelimiter),
            None => Err(LexErr::MismatchedDelimiter),
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
        assert_lex_fail!("(1" => LexErr::UnclosedDelimiter);
        assert_lex_fail!("1)" => LexErr::MismatchedDelimiter);
        assert_lex_fail!("(1]" => LexErr::MismatchedDelimiter);
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
        assert_lex_fail!("'ab'" => LexErr::ExpectedChar('\''));
        assert_lex_fail!("''" => LexErr::EmptyChar);
    }
}