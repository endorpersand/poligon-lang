use std::fmt::Debug;
use std::collections::BTreeMap;
use lazy_static::lazy_static;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Token {
    Ident(String),
    Numeric(String),
    Str(String),
    Comment(String),
    Keyword(Keyword),
    Operator(Operator),
    LineSep,

    // delimiters
    LParen,   RParen,   // ()
    LSquare,  RSquare,  // []
    LCurly,   RCurly,   // {}
    LComment, RComment, // /* */
}

macro_rules! keywords {
    ($($id:ident: $ex:literal),*) => {
        #[derive(PartialEq, Eq, Debug, Clone)]
        pub enum Keyword {
            $(
                $id
            ),*
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

macro_rules! operators {
    ($($id:ident: $ex:literal),*) => {
        #[derive(PartialEq, Eq, Debug, Clone)]
        pub enum Operator {
            $(
                $id
            ),*
        }

        lazy_static! {
            pub(super) static ref OPMAP: BTreeMap<&'static str, Token> = {
                let mut m = BTreeMap::new();

                $(m.insert($ex, Token::Operator(Operator::$id));)*
                m.insert(";", Token::LineSep);
                
                m
            };
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

operators! {
    Plus:    "+",
    Minus:   "-",
    Star:    "*",
    Slash:   "/",
    Percent: "%",
    
    Dot:   ".",
    DDot:  "..",
    Or:    "|",
    And:   "&",
    Tilde: "~",
    Caret: "^",

    DAnd: "&&",
    DOr:  "||",
    Excl: "!",

    Lt:     "<",
    Le:     "<=",
    Gt:     ">",
    Ge:     ">=",
    Equal:  "=",
    DEqual: "==",
    Ne:     "!=",

    Shl: "<<",
    Shr: ">>",

    Comma:  ",",
    DSlash: "//",

    Hash: "#"
}