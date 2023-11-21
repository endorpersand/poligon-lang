//! Converts strings to sequences of tokens.
//! 
//! In a general sense, lexing is performed by reading the string, 
//! and repeatedly matching specific token patterns until the entire string is consumed.
//! 
//! This module provides:
//! - [`tokenize`]: A utility function that opaquely does the lexing from string to tokens.
//! - [`Lexer`]: The struct which does the entire lexing process.

use std::collections::HashMap;

use logos::Logos;
use once_cell::sync::Lazy;

use crate::err::{GonErr, FullGonErr};
use crate::span::Span;

use crate::token::{Token, Group, Keyword, token, FullToken, TokenTree, OwnedStream, Operator, DE_MAP};

/// Convert a string and lex it into a sequence of tokens.
/// 
/// For more control, see the [`Lexer`] struct.
/// 
/// # Example
/// ```
/// # use poligon_lang::tokenize;
/// use poligon_lang::token::{Token, token};
/// 
/// let code = "a + b + c + d";
/// assert_eq!(tokenize(code).unwrap(), vec![
///     Token::Ident(String::from("a")),
///     token![+],
///     Token::Ident(String::from("b")),
///     token![+],
///     Token::Ident(String::from("c")),
///     token![+],
///     Token::Ident(String::from("d"))
/// ]);
/// ```
pub fn tokenize(input: &str) -> LexResult<OwnedStream> {
    let lx = LexTokenizer::new(input);
    lex_groupify(lx)
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

    /// The string of characters are punctuation but they don't create a valid operator (e.g. `@`)
    UnknownOp(String),

    /// A delimiter was closed with the wrong type (e.g. `[ ... )`)
    MismatchedDelimiter,

    /// A bracket was not closed (e.g. `( ... `)
    UnclosedDelimiter,

    /// A delimiter was never opened (e.g. ` ... )`)
    UnmatchedDelimiter,

    /// A comment was not closed (e.g. `/* ... `)
    UnclosedComment,

    /// A comment was never opened (e.g. ` ... */`)
    UnmatchedComment,

    /// There is still input to be lexed in the lexer
    NotFullyLexed,

    /// Character does not have a basic escape
    InvalidBasic(char),
    
    /// Escape hit EOF
    InvalidBasicEOF,

    /// Escape of the form `'\x00'` was invalid
    InvalidX,

    /// Escape of the form '`\u{000000}'` was invalid
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
}

impl std::fmt::Display for LexErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexErr::UnknownChar(c)      => write!(f, "invalid character {}", wrapq(*c)),
            LexErr::UnclosedQuote       => write!(f, "quote was never terminated"),
            LexErr::ExpectedChar(c)     => write!(f, "expected character {}", wrapq(*c)),
            LexErr::EmptyChar           => write!(f, "char literal cannot be empty"),
            LexErr::UnknownOp(op)       => write!(f, "operator \"{op}\" does not exist"),
            LexErr::MismatchedDelimiter => write!(f, "mismatched delimiter"),
            LexErr::UnclosedDelimiter   => write!(f, "delimiter was never terminated"),
            LexErr::UnmatchedDelimiter  => write!(f, "delimiter was never opened"),
            LexErr::UnclosedComment     => write!(f, "comment was never terminated"),
            LexErr::UnmatchedComment    => write!(f, "comment was never opened"),
            LexErr::NotFullyLexed       => write!(f, "input not fully read"),
            LexErr::InvalidBasic(c)     => write!(f, "invalid escape: \\{c}"),
            LexErr::InvalidBasicEOF     => write!(f, "invalid escape"),
            LexErr::InvalidX            => write!(f, "invalid \\xXX escape"),
            LexErr::InvalidU            => write!(f, "invalid \\u{{XXXX}} escape"),
            LexErr::InvalidChar(c)      => write!(f, "invalid char {:x}", c),
        }
    }
}
impl std::error::Error for LexErr {}

/// Enclose quotes around a character
/// 
/// For most characters, it will appear as: `'a', 'b', '"', '@'`, etc.
/// 
/// For ', it appears as `"'"`.
fn wrapq(c: char) -> String {
    if c == '\'' { format!("\"{c}\"") } else { format!("'{c}'") }
}

#[derive(Clone, Default, Debug, PartialEq, Eq)]
struct UnknownChar;

#[derive(Debug, Logos)]
#[logos(skip r"\s+", error = UnknownChar)]
enum TokenClass {
    /// An identifier or keyword.
    #[regex(r"[A-Za-z_][A-Za-z_0-9]*")]
    Ident,

    /// A numeric value (e.g. `123`, `123.1`, `1.11`, `14.`)
    #[regex(r"\.?\d+")]
    Numeric,

    /// The start of a string literal
    #[token(r#"""#)]
    StrQuote,

    /// The start of a char literal
    #[token("'")]
    CharQuote,

    /// The start of a single-line comment
    #[token("//")]
    SingleLineComment,

    /// The start of a multi-line comment
    #[token("/*")]
    MultiLineComment,
    
    /// Operators (e.g. `+`, `-`, `/`)
    #[regex(r##"[!"#$%&'*+,\-./:;<=>?@\\^`|~]"##)]
    Operator,

    /// Delimiters (e.g. `(`, `)`, `[`, `]`)
    #[regex(r"[\[\](){}]")]
    Delimiter,

    /// End of line (`;`)
    #[token(";")]
    LineSep
}

#[derive(Clone, Default, Debug, PartialEq, Eq)]
enum LiteralErr {
    InvalidBasic(char),
    InvalidBasicEOF,
    InvalidX,
    InvalidU,
    InvalidChar(u32),
    // Logos requires errors to be default, 
    // but StrClass can never trigger a Default call,
    // so this should never actually be used as a variant
    #[default]
    Unreachable
}

#[derive(Default)]
struct Terminal(char);

#[derive(Logos)]
#[logos(error = LiteralErr, extras = Terminal)]
enum StrClass {
    #[token(r"\", parse_basic_escape)]
    BasicEscape(char),
    #[token(r"\x", parse_x_escape)]
    XEscape(char),
    #[token(r"\u", parse_u_escape)]
    UEscape(char),
    #[regex(r"\\\n\s*")]
    NLEscape,
    #[regex(r"[\s\S]", |lx| lx.slice().parse::<char>().expect("regex matched one character"))]
    Other(char)
}
fn parse_basic_escape(lx: &mut logos::Lexer<StrClass>) -> Result<char, LiteralErr> {
    static BASIC_ESCAPES: Lazy<HashMap<char, char>> = Lazy::new(|| {
        let mut m = HashMap::new();

        m.insert('0',  '\0');
        m.insert('\\', '\\');
        m.insert('n',  '\n');
        m.insert('t',  '\t');
        m.insert('r',  '\r');
        m.insert('\'', '\'');
        m.insert('"',  '"');
        m
    });

    let chr = lx.remainder()
        .chars()
        .next()
        .ok_or(LiteralErr::InvalidBasicEOF)?;
    
    lx.bump(chr.len_utf8());
    BASIC_ESCAPES.get(&chr)
        .copied()
        .ok_or(LiteralErr::InvalidBasic(chr))
}
fn parse_x_escape(lx: &mut logos::Lexer<StrClass>) -> Result<char, LiteralErr> {
    let mut rem = lx.remainder().chars();
    let Some(c1) = rem.next().filter(|&c| c != lx.extras.0) else { return Err(LiteralErr::InvalidX) };
    lx.bump(c1.len_utf8());
    let Some(c0) = rem.next().filter(|&c| c != lx.extras.0) else { return Err(LiteralErr::InvalidX) };
    lx.bump(c0.len_utf8());

    let codepoint = u32::from_str_radix(&lx.slice()[2..], 16)
        .map_err(|_| LiteralErr::InvalidX)?;
    
    char::try_from(codepoint)
        .map_err(|_| LiteralErr::InvalidChar(codepoint))
}
fn parse_u_escape(lx: &mut logos::Lexer<StrClass>) -> Result<char, LiteralErr> {
    #[derive(Logos)]
    enum UEscape {
        #[regex(r"\{[A-Fa-f0-9]{1, 6}\}", priority=2)]
        Hex,
        #[regex(r"[\s\S]")]
        Other
    }

    let mut esclx = UEscape::lexer(lx.remainder());
    
    let next = esclx.next();
    lx.bump(esclx.span().end);
    let Some(Ok(UEscape::Hex)) = next else { return Err(LiteralErr::InvalidU) };
    
    let outer = lx.slice();
    let inner = &outer[3..(outer.len() - 1)];

    let codepoint = u32::from_str_radix(inner, 16)
        .map_err(|_| LiteralErr::InvalidU)?;
    
    char::try_from(codepoint)
        .map_err(|_| LiteralErr::InvalidChar(codepoint))
}
impl From<LiteralErr> for LexErr {
    fn from(value: LiteralErr) -> Self {
        match value {
            LiteralErr::InvalidBasic(c) => LexErr::InvalidBasic(c),
            LiteralErr::InvalidBasicEOF => LexErr::InvalidBasicEOF,
            LiteralErr::InvalidX => LexErr::InvalidX,
            LiteralErr::InvalidU => LexErr::InvalidU,
            LiteralErr::InvalidChar(c) => LexErr::InvalidChar(c),
            LiteralErr::Unreachable => unreachable!("LiteralErr::Unreachable hit"),
        }
    }
}
/// Converts a string into a stream of tokens.
pub struct LexTokenizer<'s> {
    logos: logos::Lexer<'s, TokenClass>
}
impl<'s> LexTokenizer<'s> {
    /// Creates a new tokenizer.
    pub fn new(stream: &'s str) -> Self {
        Self {
            logos: logos::Lexer::new(stream)
        }
    }
}
impl<'s> Iterator for LexTokenizer<'s> {
    type Item = LexResult<FullToken>;

    fn next(&mut self) -> Option<Self::Item> {
        let result = 'res: {
            match self.logos.next()? {
                Ok(TokenClass::Ident) => {
                    let mut kwlx = Keyword::lexer(self.logos.slice());
                    let Some(m_keyword) = kwlx.next() else {
                        unreachable!("identifier was matched, so there should be at least 1 token");
                    };
                    let span = Span::new(self.logos.span());

                    match m_keyword {
                        Ok(kw) => Ok(FullToken { kind: Token::Keyword(kw), span }),
                        Err(_) => Ok(FullToken { kind: Token::Ident(self.logos.slice().to_string()), span }),
                    }
                },
                Ok(TokenClass::Numeric) => {
                    if !self.logos.slice().starts_with('.') {
                        #[derive(Logos)]
                        enum NumSeg {
                            #[token(".")]
                            Dot,
                            #[regex(r"\d+")]
                            Fraction,
                            #[regex(r"[A-Za-z_]")]
                            Alphabetic,
                            #[regex(r"[^A-Za-z_0-9]")]
                            Other
                        }

                        let mut numlx = NumSeg::lexer(self.logos.remainder());

                        // 123.ident  => [123][.][ident]
                        // 123.444    => [123.444]
                        // 123..444   => [123][..][444]
                        // 123. + 444 => [123.] [+] [444]

                        if let Some(Ok(NumSeg::Dot)) = numlx.next() {
                            // whether the "." is part of the numeric or if it's a part of a spread/call operator
                            // depends on the character after the "."
                            
                            // the "." is part of the numeric UNLESS
                            // - the next character is a "."
                            // - the next character is alpha/underscore
                            let dot_end = numlx.span().end;

                            match numlx.next() {
                                Some(Ok(NumSeg::Fraction)) => self.logos.bump(numlx.span().end),
                                Some(Ok(NumSeg::Alphabetic | NumSeg::Dot)) => {},
                                _ => self.logos.bump(dot_end)
                            }
                        }
                    }

                    Ok(FullToken { 
                        kind: Token::Numeric(self.logos.slice().to_string()), 
                        span: Span::new(self.logos.span())
                    })}
                Ok(TokenClass::StrQuote)  => {
                    let mut litlx = StrClass::lexer_with_extras(self.logos.remainder(), Terminal('"'));
                    let mut buf = String::new();
                    loop {
                        let Some(m_token) = litlx.next() else {
                            break 'res Err(LexErr::UnclosedQuote.at_range(self.logos.span()))
                        };
                        let token = match m_token {
                            Ok(t)  => t,
                            Err(e) => {
                                let span_end = self.logos.span().end;
                                let span = litlx.span().start + span_end .. litlx.span().end + span_end;
                                break 'res Err(LexErr::from(e).at_range(span))
                            },
                        };

                        match token {
                            StrClass::Other(c) if c == litlx.extras.0 => break,
                            | StrClass::BasicEscape(c) 
                            | StrClass::XEscape(c) 
                            | StrClass::UEscape(c) 
                            | StrClass::Other(c)
                            => buf.push(c),
                            StrClass::NLEscape => {}
                        }
                    }

                    self.logos.bump(litlx.span().end);
                    Ok(FullToken {
                        kind: Token::Str(buf),
                        span: Span::new(self.logos.span())
                    })
                },
                Ok(TokenClass::CharQuote) => {
                    let mut litlx = StrClass::lexer_with_extras(self.logos.remainder(), Terminal('\''));
                    let mut chr = None;
                    loop {
                        let Some(m_token) = litlx.next() else {
                            break 'res Err(LexErr::UnclosedQuote.at_range(self.logos.span()))
                        };
                        let token = match m_token {
                            Ok(t)  => t,
                            Err(e) => {
                                let span_end = self.logos.span().end;
                                let span = litlx.span().start + span_end .. litlx.span().end + span_end;
                                break 'res Err(LexErr::from(e).at_range(span))
                            },
                        };

                        match token {
                            StrClass::Other(c) if c == litlx.extras.0 => break,
                            | StrClass::BasicEscape(c) 
                            | StrClass::XEscape(c) 
                            | StrClass::UEscape(c) 
                            | StrClass::Other(c)
                            => {
                                match chr {
                                    Some(_) => {
                                        let start = self.logos.span().end;
                                        let cspan = litlx.span();
                                        let span = (start + cspan.start)..(start + cspan.end);
                                        break 'res Err(LexErr::ExpectedChar('\'').at_range(span));
                                    },
                                    None => { chr.replace(c); },
                                }
                            },
                            StrClass::NLEscape => {
                                let start = self.logos.span().end;
                                let cspan = litlx.span();
                                let span = (start + cspan.start)..(start + cspan.end);
                                break 'res Err(LexErr::InvalidBasic('\n').at_range(span))
                            }
                        }
                    }

                    self.logos.bump(litlx.span().end);
                    match chr {
                        Some(c) => Ok(FullToken { kind: Token::Char(c), span: Span::new(self.logos.span()) }),
                        None => Err(LexErr::EmptyChar.at_range(self.logos.span())),
                    }
                },
                Ok(TokenClass::SingleLineComment) => {
                    #[derive(Logos)]
                    enum SLCText {
                        #[token("\n")]
                        Newline,
                        #[regex(r".+")]
                        Other
                    }

                    let mut comlx = SLCText::lexer(self.logos.remainder());
                    let mut text = String::new();
                    loop {
                        match comlx.next() {
                            Some(Ok(SLCText::Other)) => text.push_str(comlx.slice()),
                            Some(Ok(SLCText::Newline)) | None => break,
                            Some(Err(_)) => unreachable!("all tokens can be matched: {:?}", comlx.slice()),
                        }
                    }
                    self.logos.bump(comlx.span().end);
                    
                    Ok(FullToken {
                        kind: Token::Comment(String::from(text.trim()), true),
                        span: Span::new(self.logos.span())
                    })
                },
                Ok(TokenClass::MultiLineComment) => {
                    #[derive(Logos)]
                    enum MLCText {
                        #[token("/*")]
                        Left,
                        #[token("*/")]
                        Right,
                        #[token("*")]
                        Asterisk,
                        #[token("/")]
                        Slash,
                        #[regex(r"[^*/]+")]
                        Other
                    }

                    let mut depth: usize = 1;
                    let mut comlx = MLCText::lexer(self.logos.remainder());
                    let mut text = String::new();
                    while depth > 0 {
                        match comlx.next() {
                            Some(Ok(MLCText::Left))  => depth += 1,
                            Some(Ok(MLCText::Right)) => depth -= 1,
                            Some(Ok(MLCText::Other | MLCText::Asterisk | MLCText::Slash)) => {},
                            Some(Err(_)) => unreachable!("all conditions can be matched: {:?}", comlx.slice()),
                            None => break 'res Err(LexErr::UnclosedComment.at_range(self.logos.span())),
                        }

                        text.push_str(comlx.slice());
                    }

                    // assert it's */
                    let mut drain = text.drain(text.len() - 2..);
                    debug_assert_eq!(drain.next(), Some('*'));
                    debug_assert_eq!(drain.next(), Some('/'));
                    std::mem::drop(drain);
                    self.logos.bump(comlx.span().end);

                    Ok(FullToken {
                        kind: Token::Comment(String::from(text.trim()), false),
                        span: Span::new(self.logos.span())
                    })
                },
                Ok(TokenClass::Operator) => {
                    let logos_span = self.logos.span();

                    let mut oplx = Operator::lexer(&self.logos.source()[logos_span.start ..]);
                    let Some(m_op) = oplx.next() else {
                        unreachable!("operator was matched, so there should be at least 1 token");
                    };

                    match m_op {
                        Ok(op) => {
                            let op_span = oplx.span();
                            let op_size = op_span.end - op_span.start;
                            let logos_size = logos_span.end - logos_span.start;
                            self.logos.bump(op_size - logos_size);

                            Ok(FullToken { kind: Token::Operator(op), span: Span::new(self.logos.span()) })
                        },
                        Err(_) => Err(LexErr::UnknownOp(self.logos.slice().to_string()).at_range(self.logos.span())),
                    }
                },
                Ok(TokenClass::Delimiter) => {
                    let delim = DE_MAP.get(self.logos.slice())
                        .unwrap_or_else(|| unreachable!("delimiter was matched, so this should be a delimiter"));

                    Ok(FullToken { kind: delim.clone(), span: Span::new(self.logos.span()) })
                },
                Ok(TokenClass::LineSep) => {
                    Ok(FullToken { 
                        kind: token![;], 
                        span: Span::new(self.logos.span())
                    })},
                Err(_) => {
                    let chr = self.logos.slice()
                        .chars()
                        .next()
                        .expect("valid character");
                    
                    Err(LexErr::UnknownChar(chr).at_range(self.logos.span()))
                },
            }
        };

        Some(result)
    }
}

/// Wraps groups into their tree.
pub fn lex_groupify(it: impl IntoIterator<Item=LexResult<FullToken>>) -> LexResult<Vec<TokenTree>> {
    let mut top = vec![];
    let mut delim_stack = vec![];

    for m_token in it {
        let token = m_token?;
        match token.kind {
            Token::Delimiter(ldelim, false) => delim_stack.push((ldelim, token.span, vec![])),
            Token::Delimiter(rdelim, true)  => {
                match delim_stack.pop() {
                    Some((ldelim, left_span, content)) if ldelim == rdelim => {
                        let group = TokenTree::Group(Group {
                            delimiter: ldelim,
                            content,
                            left_span,
                            right_span: token.span,
                        });

                        delim_stack.last_mut()
                            .map_or(&mut top, |(_, _, content)| content)
                            .push(group);
                    },
                    Some((_, left_span, _)) => {
                        Err({
                            LexErr::MismatchedDelimiter
                                .at_range(left_span)
                                .and_at_range(token.span)
                        })?
                    },
                    None => Err(LexErr::UnmatchedDelimiter.at_range(token.span))?,
                }
            },
            _ => {
                delim_stack.last_mut()
                    .map_or(&mut top, |(_, _, content)| content)
                    .push(TokenTree::Token(token));
            }
        }
    }

    if let Some(&(_, lspan, _)) = delim_stack.last() {
        Err(LexErr::UnclosedDelimiter.at_range(lspan))
    } else {
        Ok(top)
    }
}

#[cfg(test)]
mod tests {
    use crate::delim;

    use super::*;

    /// Assert that the string provided lexes into the vector of tokens.
    #[allow(unused)]
    fn assert_lex<T>(input: &str, result: &[T]) 
        where T: std::fmt::Debug,
            TokenTree: PartialEq<T>
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
            FullToken { kind: token![!],  span: Span::new(0..1) },
            FullToken { kind: token![~],  span: Span::new(1..2) },
            FullToken { kind: token![==], span: Span::new(2..4) },
            FullToken { kind: token![!],  span: Span::new(4..5) },
            FullToken { kind: token![~],  span: Span::new(5..6) },
            FullToken { kind: token![&&], span: Span::new(6..8) },
            FullToken { kind: token![.],  span: Span::new(8..9) },
            FullToken { kind: token![=],  span: Span::new(9..10) },
            FullToken { kind: token![+],  span: Span::new(10..11) },
            FullToken { kind: token![-],  span: Span::new(11..12) },
            FullToken { kind: token![*],  span: Span::new(12..13) },
            FullToken { kind: token![<],  span: Span::new(13..14) },
            FullToken { kind: token![>],  span: Span::new(14..15) },
            FullToken { kind: token![<<], span: Span::new(15..17) },
            FullToken { kind: Token::Numeric("3".to_string()), span: Span::new(17..18) },
        ]);

        assert_lex("<<<", &[
            FullToken { kind: token![<<], span: Span::new(0..2)},
            FullToken { kind: token![<],  span: Span::new(2..3)},
        ]);
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
            TokenTree::Group(Group {
                delimiter: delim!["()"], 
                content: vec![TokenTree::Token(FullToken {
                    kind: Token::Numeric("1".to_string()),
                    span: Span::new(1..2)
                })], 
                left_span: Span::new(0..1), 
                right_span: Span::new(2..3)
            })
        ]);

        assert_lex_fail("(1", LexErr::UnclosedDelimiter.at(0));
        assert_lex_fail("1)", LexErr::UnmatchedDelimiter.at(1));
        assert_lex_fail("(1]", LexErr::MismatchedDelimiter.at_points(&[0, 2]));
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
        macro_rules! literal {
            (Char($e:expr), $r:expr) => {
                &[FullToken::new(Token::Char($e), $r)]
            };
            (Str($e:literal), $r:expr) => {
                &[FullToken::new(Token::Str(String::from($e)), $r)]
            };
        }

        // basic char checks
        assert_lex("'a'", literal![Char('a'), Span::new(0..3)]);
        assert_lex_fail("'ab'", LexErr::ExpectedChar('\'').at(2));
        assert_lex_fail("''", LexErr::EmptyChar.at_range(0..2));

        // length check
        assert_lex("\"abc\"", literal![Str("abc"), Span::new(0..5)]); // "abc"
        assert_lex("\"abc\n\"", literal![Str("abc\n"), Span::new(0..6)]); // "abc[new line]"
        assert_lex("\"abc\nde\"", literal![Str("abc\nde"), Span::new(0..8)]); // "abc[new line]de"

        // basic escape tests
        assert_lex("'\\''", literal![Char('\''), Span::new(0..4)]); // '\''
        assert_lex("'\\n'", literal![Char('\n'), Span::new(0..4)]); // '\n'
        assert_lex_fail("'\\e'", LexErr::InvalidBasic('e')); // '\e'
        assert_lex_fail("\"!!!\\e\"", LexErr::InvalidBasic('e')); // "!!!\e"
        assert_lex_fail("'\\n", LexErr::UnclosedQuote); // '\n
        assert_lex_fail("'\\\n'", LexErr::InvalidBasic('\n')); // '\[new line]'
        assert_lex_fail("\"\\", LexErr::InvalidBasicEOF); // "\

        // \x test
        assert_lex("'\\x14'", literal![Char('\x14'), Span::new(0..6)]);   // '\x14'
        assert_lex_fail("'\\x'", LexErr::InvalidX);   // '\x'
        assert_lex_fail("'\\x0'", LexErr::InvalidX);  // '\x0'
        assert_lex_fail("'\\xqq'", LexErr::InvalidX); // '\xqq'

        // \u test
        assert_lex("'\\u{0}'", literal![Char('\0'), Span::new(0..7)]); // '\u{0}'
        assert_lex("'\\u{1f97a}'", literal![Char('\u{1f97a}'), Span::new(0..11)]); // '\u{1f97a}'
        assert_lex_fail("'\\u{21f97a}'", LexErr::InvalidChar(0x21F97Au32)); // '\u{21f97a}'
        assert_lex_fail("'\\u{0000000}'", LexErr::InvalidU); // '\u{0000000}'
        assert_lex_fail("'\\u{0'", LexErr::InvalidU); // '\u{0'
        assert_lex_fail("'\\u{!}'", LexErr::InvalidU); // '\u{!}'
    }
}