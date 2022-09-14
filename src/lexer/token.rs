use std::fmt::Debug;

#[derive(PartialEq, Eq, Debug)]
pub enum Token {
    Ident(String),
    Numeric(String),
    Str(String),
    Comment(String),
    Keyword(Keyword),

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

macro_rules! keywords {
    ($($id:ident: $ex:expr),*) => {
        #[derive(PartialEq, Eq, Debug)]
        pub enum Keyword {
            $(
                $id
            ),+
        }

        impl Keyword {
            pub fn get_kw(s: &str) -> Option<Token> {
                match s {
                    $(
                        $ex => Some(Token::Keyword(Self::$id))
                    ),+ ,
                    _ => None
                }
            }
        }
    };
}

keywords! {
    Let:     "let",   // variable declarations
    Const:   "const", // variable declarations
    Mut:     "mut",   // mutable variables and parameters
    While:   "while", 
    For:     "for", 
    If:      "if", 
    Else:    "else", 
    Fun:     "fun",   // functions
    Shape:   "shape", // shape X {}
    Class:   "class", // class X {}
    Fit:     "fit",   // fit X to Y {}
    To:      "to", 
    Fits:    "fits",    // class X fits Y {}
    Extends: "extends", // shape X extends Y {}
    Match:   "match",   // match x {}
    Unit:    "unit",    // units and measures (nominal primitives)
    Measure: "measure", // units and measures (nominal primitives)
    Of:      "of"       // units and measures (nominal primitives)
}