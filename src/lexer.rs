use std::collections::VecDeque;

pub fn tokenize(input: &str) -> Result<Vec<Token>, LexErr> {
    Lexer::new(input).lex()
}

pub enum Token {
    Ident(String),
    Numeric(String),
    Str(String),
    Comment(String),
    
    // keywords
    Let,   // variable declarations
    Const, // variable declarations
    Mut,   // mutable variables and parameters
    While, 
    For, 
    If, 
    Else, 
    Fun,   // functions
    Shape, // shape X {}
    Class, // class X {}
    Fit,   // fit X to Y {}
    To, 
    Fits,    // class X fits Y {}
    Extends, // shape X extends Y {}
    Match,   // match x {}
    Unit,    // units and measures (nominal primitives)
    Measure, // units and measures (nominal primitives)
    Of,      // units and measures (nominal primitives)

    // operators
    Plus,        // +
    Minus,       // -
    Star,        // *
    Slash,       // /
    Perc,        // %
    
    Dot,   // .
    DDot,  // ..
    Or,    // |
    And,   // &
    Tilde, // ~
    Caret, // ^

    DAnd,  // &&
    DOr,   // ||
    Exc,   // !

    Lt,     // <
    Le,     // <=
    Gt,     // >
    Ge,     // >=
    Equal,  // =
    DEqual, // ==
    Ne, // !=

    Shl, // <<
    Shr, // >>

    Semi,   // ;
    Comma,  // ,
    DSlash, // //

    Hash,   // #

    // delimiters
    LParen,   RParen,   // ()
    LSquare,  RSquare,  // []
    LCurly,   RCurly,   // {}
    LComment, RComment, // /* */
}

pub enum LexErr {
    UnrecognizedChar(char), // Char isn't used in Poligon code
    UnexpectedEOF,          // Lexing ended unexpectedly
}

struct Lexer {
    input: VecDeque<char>,
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

impl Lexer {
    fn new(input: &str) -> Self {
        Lexer { input: input.chars().collect(), tokens: vec![] }
    }

    /// Return the first non-consumed character in the input.
    /// 
    /// If there is no more characters in the input, return None.
    fn peek(&self) -> Option<&char> {
        self.input.get(0)
    }

    fn lex(mut self) -> Result<Vec<Token>, LexErr> {
        while let Some(chr) = self.peek() {
            let cls = CharClass::of(*chr).ok_or(LexErr::UnrecognizedChar(*chr))?;
            match cls {
                CharClass::Alpha | CharClass::Underscore => self.push_ident(),
                CharClass::Numeric    => self.push_numeric(),
                CharClass::Quote      => self.push_str()?,
                CharClass::Punct      => todo!(),
                CharClass::NewLine    => todo!(),
                CharClass::Whitespace => {
                    self.input.pop_front();
                },
            }
        }
        
        Ok(self.tokens)
    }

    fn push_ident(&mut self) {
        let c = self.input.pop_front().expect("String was validated to have a character, but failed to pop identifier");
        let mut buf = String::from(c);

        while let Some(c) = self.peek() {
            match CharClass::of(*c).expect("String was validated to have a valid identifier character, but got None") {
                CharClass::Alpha | CharClass::Underscore | CharClass::Numeric => {
                    buf.push(*c);
                    self.input.pop_front();
                }
                _ => break
            }
        }

        // todo, deal with keywords
        self.tokens.push(Token::Ident(buf));
    }
    fn push_numeric(&mut self) {
        let c = self.input.pop_front().expect("String was validated to have a character, but failed to pop numeric");
        let mut buf = String::from(c);

        while let Some(c) = self.peek() {
            let cls = CharClass::of(*c).expect("String was validated to have a valid identifier character, but got None");
            if cls == CharClass::Numeric {
                buf.push(*c);
                self.input.pop_front();
            }
        }

        // TODO, deal with decimals vs range
        self.tokens.push(Token::Numeric(buf));
    }

    fn push_str(&mut self) -> Result<(), LexErr> {
        let qt = self.input.pop_front().expect("String was validated to have a character, but failed to pop quotation mark");
        let mut buf = String::new();
        loop {
            match self.input.pop_front() {
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