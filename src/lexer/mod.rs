use std::collections::VecDeque;

use self::token::{Token, Keyword};
pub mod token;

pub fn tokenize(input: &str) -> Result<Vec<Token>, LexErr> {
    Lexer::new(input)?.lex()
}

#[derive(PartialEq, Eq, Debug)]
pub enum LexErr {
    UnrecognizedChar(char), // Char isn't used in Poligon code
    UnexpectedEOF,          // Lexing ended unexpectedly
}

struct Lexer {
    input: VecDeque<CharData>,
    tokens: Vec<Token>
}

#[derive(PartialEq, Eq)]
enum CharClass {
    Alpha,
    Numeric,
    Underscore,
    Quote,
    Punct,
    NewLine,
    Whitespace
}

#[derive(PartialEq, Eq)]
struct CharData {
    chr: char,
    cls: CharClass
}

impl CharClass {
    fn of(c: char) -> Option<Self> {
        if c.is_alphabetic()             { Some(Self::Alpha) }
        else if c.is_numeric()           { Some(Self::Numeric) }
        else if c == '_'                 { Some(Self::Underscore) }
        else if c == '"' || c == '\''    { Some(Self::Quote) }
        else if c.is_ascii_punctuation() { Some(Self::Punct) }
        else if c == '\n'                { Some(Self::NewLine) }
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
            tokens: vec![]
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

    /// Check if the character matches. 
    /// 
    /// If yes, pop it and return the char.
    /// If not, return None.
    fn match_chr(&mut self, match_chr: char) -> Option<char> {
        match self.peek() {
            Some(CharData { chr, .. }) if chr == &match_chr => {
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
                CharClass::Quote      => self.push_str()?,
                CharClass::Punct      => todo!(),
                CharClass::NewLine    => todo!(),
                CharClass::Whitespace => { self.next(); },
            }
        }
        
        Ok(self.tokens)
    }

    fn push_ident(&mut self) {
        let cd = self.next()
            .expect("String was validated to have a character, but failed to pop identifier");
        let mut buf = String::from(cd.chr);

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
        let cd = self.next()
            .expect("String was validated to have a character, but failed to pop numeric");
        let mut buf = String::from(cd.chr);

        while let Some(c) = self.match_cls(CharClass::Numeric) {
            buf.push(c);
        }

        // 123.ident  => [123][.][ident]
        // 123.444    => [123.444]
        // 123..444   => [123][..][444]
        // 123. + 444 => [123.] [+] [444]
        
        // peek next character. check if it's .
        if matches!(self.peek(), Some(CharData { chr: '.', .. })) {
            // determine if this "." is part of the number or if it's part of a spread/call or such
            match self.input.get(1) {
                Some(CharData { chr: '.', ..}) => {},

                // only consider if it is an operator or numeric after the .
                Some(CharData { cls: CharClass::Numeric | CharClass::Punct, .. }) => {
                    buf.push(self.next().unwrap().chr); // "."

                    while let Some(c) = self.match_cls(CharClass::Numeric) {
                        buf.push(c);
                    }
                }

                _ => {}
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
            let c = self.next().map(|cd| cd.chr);

            match c {
                // hit quote? stop parsing string.
                Some(c) if c == qt => break,

                // hit any other character (incl \n)? add to string.
                Some(c)            => buf.push(c),

                // hit EOF? error. string ended without closing.
                None               => Err(LexErr::UnexpectedEOF)?
            }
        }
        
        self.tokens.push(Token::Str(buf));
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! assert_lex {
        ($e1:literal == $e2:expr) => {
            assert_eq!(tokenize($e1), Ok($e2))
        }
    }

    #[test]
    fn numeric_lex() {
        assert_lex!("123.444"    == vec![
            Token::Numeric("123.444".to_string())
        ]);
        assert_lex!("123.ident"  == vec![
            Token::Numeric("123".to_string()), 
            Token::Dot, 
            Token::Ident("ident".to_string())
        ]);
        assert_lex!("123..444"   == vec![
            Token::Numeric("123".to_string()),
            Token::DDot,
            Token::Numeric("444".to_string())
        ]);
        assert_lex!("123. + 444" == vec![
            Token::Numeric("123.".to_string()),
            Token::Plus,
            Token::Numeric("444".to_string())
        ]);
    }
}