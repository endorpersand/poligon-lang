use std::collections::VecDeque;

use lazy_static::lazy_static;
use regex::Regex;

use self::token::{Token, Keyword, OPMAP, Delimiter, token};
pub mod token;

pub fn tokenize(input: &str) -> Result<Vec<Token>, LexErr> {
    Lexer::new(input)?.lex()
}

#[derive(PartialEq, Eq, Debug)]
pub enum LexErr {
    UnrecognizedChar(char),       // Char isn't used in Poligon code
    UnexpectedEOF,                // Reached end of file, when expecting a token
    ExpectedChar(char),           // Expected a specific character, f.e. '
    EmptyChar,                    // ''
    UnrecognizedOperator(String), // This operator cannot be resolved
    MismatchedDelimiter,          // A bracket was closed with the wrong type
    UnclosedDelimiter,            // A bracket wasn't closed
    UnclosedComment,              // Hit EOF on /* */
}

struct Lexer {
    input: VecDeque<CharData>,
    tokens: Vec<Token>,
    delimiters: Vec<Delimiter>
}

#[derive(PartialEq, Eq, Debug)]
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

#[derive(PartialEq, Eq, Debug)]
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
    fn new(c: char) -> Result<Self, LexErr> {
        CharClass::of(c)
            .ok_or(LexErr::UnrecognizedChar(c))
            .map(|cls| Self { chr: c, cls })
    }
}

impl Lexer {
    fn new(input: &str) -> Result<Self, LexErr> {
        let cd = input.chars()
            .map(CharData::new)
            .collect::<Result<_, _>>()?;

        Ok(Self {
            input: cd, 
            tokens: vec![],
            delimiters: vec![]
        })
    }

    /// Return the first non-consumed character in the input.
    /// 
    /// If there is no more characters in the input, return None.
    fn peek(&self) -> Option<&CharData> {
        self.input.get(0)
    }

    /// Return the next character and consume it.
    fn next(&mut self) -> Option<CharData> {
        self.input.pop_front()
    }

    /// Check if the character class matches. 
    /// 
    /// If yes, pop it and return the char.
    /// If not, return None.
    fn match_cls(&mut self, match_cls: CharClass) -> Option<char> {
        match self.peek() {
            Some(CharData { cls, .. }) if cls == &match_cls => {
                self.next().map(|cd| cd.chr)
            }
            _ => None
        }
    }

    fn lex(mut self) -> Result<Vec<Token>, LexErr> {
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
        
        if !self.delimiters.is_empty() {
            Err(LexErr::UnclosedDelimiter)?
        }

        Ok(self.tokens)
    }

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
            match self.input.get(1) {
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

    fn push_str(&mut self) -> Result<(), LexErr> {
        let qt = self.next()
            .expect("String was validated to have a character, but failed to pop quotation mark")
            .chr;

        let mut buf = String::new();
        loop {
            let c = self.next()
                .ok_or(LexErr::UnexpectedEOF)? // no more chars, hit EOF
                .chr;

            if c == qt { break; }
            buf.push(c);
        }
        
        self.tokens.push(Token::Str(buf));
        Ok(())
    }

    fn push_char(&mut self) -> Result<(), LexErr> {
        let qt = self.next()
            .expect("String was validated to have a character, but failed to pop quotation mark")
            .chr;

        // Get the next character:
        let c = self.next().ok_or(LexErr::UnexpectedEOF)?.chr;
        if c == qt {
            Err(LexErr::EmptyChar)?;
        }

        // Assert next char matches:
        match self.next() {
            Some(CharData { chr, .. }) if chr == qt => {
                self.tokens.push(Token::Char(c));
                Ok(())
            },
            Some(_) => Err(LexErr::ExpectedChar(qt)),
            None => Err(LexErr::UnexpectedEOF)
        }
    }

    fn push_punct(&mut self) -> Result<(), LexErr> {
        let mut buf = String::new();

        while let Some(c) = self.match_cls(CharClass::Punct) {
            buf.push(c);
        }

        while buf.len() > 0 {
            let left = &buf[..1];
            let right = &buf[..];
    
            // Find the largest length operator that matches the start of the operator buffer.
            let (op, token) = OPMAP.range(left..=right)
                .next_back()
                .ok_or_else(|| LexErr::UnrecognizedOperator(buf.clone()))?;
            
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

    fn push_line_comment(&mut self, mut buf: String) -> Result<(), LexErr> {
        while let Some(CharData {chr, ..}) = self.next() {
            if chr == '\n' { break; }
            buf.push(chr);
        }

        let buf = String::from(buf.trim()); // lame allocation.
        self.tokens.push(Token::Comment(buf, true));
        Ok(())
    }

    fn push_multi_comment(&mut self, mut buf: String) -> Result<(), LexErr> {
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
        let chrs: Vec<CharData> = nbuf
            .chars()
            .map(CharData::new)
            .collect::<Result<_, _>>()?;

        self.input.extend(chrs);
        self.input.rotate_left(len);

        Ok(())
    }

    fn match_delimiter(&mut self, d: Delimiter) -> Result<(), LexErr> {
        let left = if d.is_right() { d.reversed() } else { d };
        
        match self.delimiters.last() {
            Some(l) if l == &left => Ok({ self.delimiters.pop(); }),
            Some(_) => Err(LexErr::MismatchedDelimiter),
            None => Err(LexErr::MismatchedDelimiter),
        }
    }
}

#[cfg(test)]
mod test {
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