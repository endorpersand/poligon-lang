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
    Let:      "let",   // variable declarations
    Const:    "const", // variable declarations
    Mut:      "mut",   // mutable variables and parameters
    While:    "while", 
    For:      "for", 
    If:       "if", 
    Else:     "else", 
    Fun:      "fun",   // functions
    Shape:    "shape", // shape X {}
    Class:    "class", // class X {}
    Fit:      "fit",   // fit X to Y {}
    To:       "to", 
    Fits:     "fits",    // class X fits Y {}
    Extends:  "extends", // shape X extends Y {}
    Match:    "match",   // match x {}
    Unit:     "unit",    // units and measures (nominal primitives)
    Measure:  "measure", // units and measures (nominal primitives)
    Of:       "of",      // units and measures (nominal primitives)
    Return:   "return",
    Break:    "break",
    Continue: "continue"
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
    
        Hash: "#",
        Arrow: "->"
    },

    delimiters: {
        LParen: "(",    RParen: ")",
        LSquare: "[",   RSquare: "]",
        LCurly: "{",    RCurly: "}",
        LComment: "/*", RComment: "*/"
    }
}

macro_rules! token {
    (let)      => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::Let)      };
    (const)    => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::Const)    };
    (mut)      => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::Mut)      };
    (while)    => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::While)    };
    (for)      => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::For)      };
    (if)       => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::If)       };
    (else)     => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::Else)     };
    (fun)      => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::Fun)      };
    (shape)    => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::Shape)    };
    (class)    => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::Class)    };
    (fit)      => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::Fit)      };
    (to)       => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::To)       };
    (fits)     => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::Fits)     };
    (extends)  => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::Extends)  };
    (match)    => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::Match)    };
    (unit)     => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::Unit)     };
    (measure)  => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::Measure)  };
    (of)       => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::Of)       };
    (return)   => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::Return)   };
    (break)    => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::Break)    };
    (continue) => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::Continue) };

    (+)    => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Plus)    };
    (-)    => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Minus)   };
    (*)    => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Star)    };
    (/)    => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Slash)   };
    (%)    => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Percent) };
    (.)    => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Dot)     };
    (..)   => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::DDot)    };
    (|)    => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Or)      };
    (&)    => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::And)     };
    (~)    => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Tilde)   };
    (^)    => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Caret)   };
    (&&)   => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::DAnd)    };
    (||)   => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::DOr)     };
    (!)    => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Excl)    };
    (<)    => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Lt)      };
    (<=)   => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Le)      };
    (>)    => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Gt)      };
    (>=)   => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Ge)      };
    (=)    => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Equal)   };
    (==)   => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::DEqual)  };
    (!=)   => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Ne)      };
    (<<)   => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Shl)     };
    (>>)   => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Shr)     };
    (,)    => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Comma)   };
    ("//") => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Comment) };
    (:)    => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Colon)   };
    (#)    => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Hash)    };
    (->)   => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Arrow)   };

    ("(")  => { $crate::lexer::token::Token::Delimiter($crate::lexer::token::Delimiter::LParen)   };
    (")")  => { $crate::lexer::token::Token::Delimiter($crate::lexer::token::Delimiter::RParen)   };
    ("[")  => { $crate::lexer::token::Token::Delimiter($crate::lexer::token::Delimiter::LSquare)  };
    ("]")  => { $crate::lexer::token::Token::Delimiter($crate::lexer::token::Delimiter::RSquare)  };
    ("{")  => { $crate::lexer::token::Token::Delimiter($crate::lexer::token::Delimiter::LCurly)   };
    ("}")  => { $crate::lexer::token::Token::Delimiter($crate::lexer::token::Delimiter::RCurly)   };
    ("/*") => { $crate::lexer::token::Token::Delimiter($crate::lexer::token::Delimiter::LComment) };
    ("*/") => { $crate::lexer::token::Token::Delimiter($crate::lexer::token::Delimiter::RComment) };

    (;) => { $crate::lexer::token::Token::LineSep };
}

#[allow(unused_imports)]
pub(crate) use token;