use std::fmt::Debug;
use std::collections::BTreeMap;
use lazy_static::lazy_static;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Token {
    Ident(String),
    Numeric(String),
    Str(String),
    Char(char),
    Comment(String, bool /* single-line? */), // this is a token in case we want documentation or something?
    Keyword(Keyword),
    Operator(Operator),
    Delimiter(Delimiter),
    LineSep
}

macro_rules! define_keywords {
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

macro_rules! define_operators_and_delimiters {
    (
        operators: {$($id:ident: $ex:literal),*},
        delimiters: {$($idl:ident: $exl:literal, $idr:ident: $exr:literal),*}
    ) => {
        #[derive(PartialEq, Eq, Debug, Clone)]
        pub enum Operator {
            $(
                $id
            ),*
        }

        #[derive(PartialEq, Eq, Debug, Clone, Copy)]
        pub enum Delimiter {
            $(
                $idl, $idr
            ),*
        }

        impl Delimiter {
            pub fn reversed(&self) -> Self {
                match self {
                    $(Self::$idl => Self::$idr),+,
                    $(Self::$idr => Self::$idl),+
                }
            }

            pub fn is_right(&self) -> bool {
                match self {
                    $(Self::$idl => false),+,
                    $(Self::$idr => true),+
                }
            }
        }

        lazy_static! {
            pub(super) static ref OPMAP: BTreeMap<&'static str, Token> = {
                let mut m = BTreeMap::new();

                $(m.insert($ex, Token::Operator(Operator::$id));)*
                $(m.insert($exl, Token::Delimiter(Delimiter::$idl));)*
                $(m.insert($exr, Token::Delimiter(Delimiter::$idr));)*
                m.insert(";", Token::LineSep);

                m
            };
        }
    };
}

define_keywords! {
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

define_operators_and_delimiters! {
    operators: {
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
        Comment: "//",
        Colon: ":",
    
        Hash: "#"
    },

    delimiters: {
        LParen: "(",    RParen: ")",
        LSquare: "[",   RSquare: "]",
        LCurly: "{",    RCurly: "}",
        LComment: "/*", RComment: "*/"
    }
}