//! The tokens that the string can be parsed into.
//! 
//! See [`Token`] for more information.

use std::fmt::{Debug, Display};
use std::collections::{BTreeMap, HashMap};
use lazy_static::lazy_static;
use crate::err::CursorRange;

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
/// A specific unit that carries some graphemic value in Poligon.
pub enum Token {
    /// An identifier, such as function names or variable names. (e.g. `abcd`, `a_b`, `a1`)
    Ident(String),
    
    /// A numeric value (e.g. `123`, `123.1`, `1.11`, `14.`)
    Numeric(String),

    /// A string literal (e.g. `"hello!"`)
    Str(String), // "abcd"

    /// A character literal (e.g. `'a'`, `'b'`)
    Char(char), // 'a'

    /// A comment (e.g. `// text`, `/* text */`)
    /// 
    /// The second parameter indicates if the comment is multiline.
    Comment(String, bool /* single-line? */), // this is a token in case we want documentation or something?
    
    /// Keywords (e.g. `let`, `const`, `fun`). 
    /// 
    /// These cannot be identifiers in any circumstance.
    Keyword(Keyword),

    /// Operators (e.g. `+`, `-`, `/`)
    Operator(Operator),

    /// Delimiters (e.g. `()`, `[]`)
    Delimiter(Delimiter),

    /// End of line (`;`)
    LineSep
}

impl Token {
    /// Checks if one token contains another in it.
    pub fn starts_with<P: TokenPattern>(&self, lhs: P) -> bool {
        lhs.fully_accepts(self) || lhs.is_strict_prefix_of(self)
    }
}

/// A token with position information.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct FullToken {
    pub(crate) loc: CursorRange,
    pub(crate) tt: Token
}

impl FullToken {
    /// Create a FullToken using a token and its given position.
    pub fn new(tt: Token, loc: CursorRange) -> Self {
        Self { loc, tt }
    }

    /// Attempts to split the full token into two. 
    /// 
    /// If successful, the LHS token is returned (and removed from this token).
    /// 
    /// If unsuccessful, the token stays unchanged.
    pub fn try_split<P: TokenPattern>(&mut self, lhs: P) -> Option<Self> {
        lhs.strip_strict_prefix_of(self)
    }
}

impl std::ops::Deref for FullToken {
    type Target = Token;

    fn deref(&self) -> &Self::Target {
        &self.tt
    }
}

impl PartialEq<Token> for FullToken {
    fn eq(&self, other: &Token) -> bool {
        &self.tt == other
    }
}
impl PartialEq<FullToken> for Token {
    fn eq(&self, other: &FullToken) -> bool {
        self == &other.tt
    }
}

mod pat {
    use super::{Token, FullToken, SPLITTABLES};

    /// A token pattern.
    /// 
    /// Similar to std [`std::str::pattern::Pattern`], this can be used to
    /// search a pattern in a given Token.
    /// 
    /// [`Token`] == verify if that token is in the given output
    /// [`[Token]`] == verify if any one of those tokens are in the given input
    pub trait TokenPattern: Sized {
        /// Tests if this pattern matches the token pattern exactly.
        fn fully_accepts(&self, t: &Token) -> bool;
        /// Tests if this pattern strictly prefixes the token.
        /// 
        /// To check for non-strict prefixes, 
        /// one can perform `self.contains(t) || self.is_strict_prefix_of(t)`.
        fn is_strict_prefix_of(&self, t: &Token) -> bool;
        /// Removes the prefix from a given FullToken, returning it if present.
        /// 
        /// This will not work if the pattern encompasses the token fully.
        fn strip_strict_prefix_of(&self, t: &mut FullToken) -> Option<FullToken>;
        /// Provides which tokens this pattern expects.
        fn expected_tokens(self) -> Vec<Token>;
    }
    
    impl TokenPattern for Token {
        fn fully_accepts(&self, t: &Token) -> bool {
            (&self).fully_accepts(t)
        }

        fn is_strict_prefix_of(&self, t: &Token) -> bool {
            (&self).is_strict_prefix_of(t)
        }
    
        fn strip_strict_prefix_of(&self, t: &mut FullToken) -> Option<FullToken> {
            (&self).strip_strict_prefix_of(t)
        }

        fn expected_tokens(self) -> Vec<Token> {
            vec![self]
        }
    }
    impl<const N: usize> TokenPattern for [Token; N] {
        fn fully_accepts(&self, t: &Token) -> bool {
            (&self[..]).fully_accepts(t)
        }

        fn is_strict_prefix_of(&self, t: &Token) -> bool {
            (&self[..]).is_strict_prefix_of(t)
        }
    
        fn strip_strict_prefix_of(&self, t: &mut FullToken) -> Option<FullToken> {
            (&self[..]).strip_strict_prefix_of(t)
        }

        fn expected_tokens(self) -> Vec<Token> {
            self.into()
        }
    }
    impl<'a> TokenPattern for &'a Token {
        fn fully_accepts(&self, t: &Token) -> bool {
            self == &t
        }

        fn is_strict_prefix_of(&self, t: &Token) -> bool {
            SPLITTABLES.contains_key(&(t, self))
        }
    
        fn strip_strict_prefix_of(&self, t: &mut FullToken) -> Option<FullToken> {
            let FullToken { tt, loc } = t;
            if let Some(rhs) = SPLITTABLES.get(&(tt, self)) {
                let (&(slno, scno), &(elno, ecno)) = (loc.start(), loc.end());
    
                let lloc = (slno, scno) ..= (slno, scno);
                let rloc = (elno, ecno) ..= (elno, ecno);
    
                *t = FullToken { tt: rhs.clone(), loc: rloc };
                Some(FullToken { tt: (*self).clone(), loc: lloc })
            } else {
                None
            }
        }

        fn expected_tokens(self) -> Vec<Token> {
            self.clone().expected_tokens()
        }
    }
    impl<'a> TokenPattern for &'a [Token] {
        fn fully_accepts(&self, t: &Token) -> bool {
            self.contains(t)
        }

        fn is_strict_prefix_of(&self, t: &Token) -> bool {
            self.iter().any(|pat| pat.is_strict_prefix_of(t))
        }
    
        fn strip_strict_prefix_of(&self, t: &mut FullToken) -> Option<FullToken> {
            self.iter().find_map(|pat| pat.strip_strict_prefix_of(t))
        }

        fn expected_tokens(self) -> Vec<Token> {
            self.to_vec()
        }
    }
    impl<'a, const N: usize> TokenPattern for &'a [Token; N] {
        fn fully_accepts(&self, t: &Token) -> bool {
            self[..].contains(t)
        }

        fn is_strict_prefix_of(&self, t: &Token) -> bool {
            self[..].iter().any(|pat| pat.is_strict_prefix_of(t))
        }
    
        fn strip_strict_prefix_of(&self, t: &mut FullToken) -> Option<FullToken> {
            self[..].iter().find_map(|pat| pat.strip_strict_prefix_of(t))
        }

        fn expected_tokens(self) -> Vec<Token> {
            self[..].to_vec()
        }
    }
}
pub use pat::TokenPattern;

macro_rules! define_keywords {
    ($($id:ident: $ex:literal),*) => {
        /// Enum that provides all the given Poligon keywords
        #[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
        pub enum Keyword {
            $(
                #[allow(missing_docs)] $id
            ),*
        }

        impl Keyword {
            /// If the string is a keyword, return the `Token` it represents 
            /// or `None` if it does not represent a token.
            pub fn get_kw(s: &str) -> Option<Token> {
                match s {
                    $(
                        $ex => Some(Token::Keyword(Self::$id))
                    ),+ ,
                    _ => None
                }
            }
        }

        impl Display for Keyword {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.write_str(match self {
                    $(Self::$id => $ex),*
                })
            }
        }
    };
}

macro_rules! define_operators_and_delimiters {
    (
        operators: {$($id:ident: $ex:literal),*},
        delimiters: {$($idl:ident: $exl:literal, $idr:ident: $exr:literal),*}
    ) => {
        /// The defined Poligon operators.
        #[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
        pub enum Operator {
            $(
                #[allow(missing_docs)] $id
            ),*
        }

        impl Display for Operator {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.write_str(match self {
                    $(Self::$id => $ex),*
                })
            }
        }

        /// The defined Poligon delimiters (`()`, `[]`, etc.).
        #[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
        pub enum Delimiter {
            $(
                #[allow(missing_docs)] $idl,
                #[allow(missing_docs)] $idr
            ),*
        }

        impl Delimiter {
            /// "Reverses" the bracket, providing the left variant if given the right and
            /// the right variant if given the left.
            pub fn reversed(&self) -> Self {
                match self {
                    $(Self::$idl => Self::$idr),+,
                    $(Self::$idr => Self::$idl),+
                }
            }

            /// Test if this variant is a right bracket.
            pub fn is_right(&self) -> bool {
                match self {
                    $(Self::$idl => false),+,
                    $(Self::$idr => true),+
                }
            }
        }


        impl Display for Delimiter {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.write_str(match self {
                    $(Self::$idl => $exl),*,
                    $(Self::$idr => $exr),*
                })
            }
        }

        lazy_static! {
            pub(super) static ref OPMAP: BTreeMap<&'static str, Token> = {
                let mut m = BTreeMap::new();

                $(m.insert($ex, Token::Operator(Operator::$id));)*
                $(m.insert($exl, Token::Delimiter(Delimiter::$idl));)*
                $(m.insert($exr, Token::Delimiter(Delimiter::$idr));)*

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
    // Fits:     "fits",    // class X fits Y {}, shape X fits Y {}
    // Extend:   "extend", // extend class X {}
    Match:    "match",   // match x {}
    // Unit:     "unit",    // units and measures (nominal primitives)
    // Measure:  "measure", // units and measures (nominal primitives)
    // Of:       "of",      // units and measures (nominal primitives)
    Return:   "return",
    Break:    "break",
    Continue: "continue",
    Step:     "step",    // 1..2 step 4
    In:       "in",      // for x in y
    True:     "true",
    False:    "false",
    Extern:   "extern",
    Import:   "import",
    Global:   "global",
    Throw:    "throw"
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
        DColon: "::",
    
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

lazy_static! {
    /// Should only be used to define 2-char tokens that can be split into 2 1-char tokens.
    static ref SPLITTABLES: HashMap<(&'static Token, &'static Token), Token> = {
        let mut m = HashMap::new();
        // m.insert((&token![..], &token![.]), token![.]);
        m.insert((&token![&&], &token![&]), token![&]);
        m.insert((&token![||], &token![|]), token![|]);
        m.insert((&token![<=], &token![<]), token![=]);
        m.insert((&token![>=], &token![>]), token![=]);
        m.insert((&token![==], &token![=]), token![=]);
        m.insert((&token![<<], &token![<]), token![<]);
        m.insert((&token![>>], &token![>]), token![>]);
        m.insert((&token![::], &token![:]), token![:]);
        m.insert((&token![->], &token![-]), token![>]);
        m
    };
}

/// Utility macro that can be used as a shorthand for [`Keyword`], [`Operator`], or [`Delimiter`] tokens.
#[macro_export]
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
    // (fits)     => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::Fits)     };
    // (extend)   => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::Extend)  };
    (match)    => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::Match)    };
    // (unit)     => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::Unit)     };
    // (measure)  => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::Measure)  };
    // (of)       => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::Of)       };
    (return)   => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::Return)   };
    (break)    => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::Break)    };
    (continue) => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::Continue) };
    (step)     => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::Step)     };
    (in)       => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::In)       };
    (true)     => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::True)     };
    (false)    => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::False)    };
    (extern)   => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::Extern)   };
    (import)   => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::Import)   };
    (global)   => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::Global)   };
    (throw)    => { $crate::lexer::token::Token::Keyword($crate::lexer::token::Keyword::Throw)    };
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
    (::)   => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::DColon)  };
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
#[doc(inline)]
pub use token;

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Ident(s) => f.write_str(s),
            Token::Numeric(n) => f.write_str(n),
            Token::Str(s)  => write!(f, "{:?}", s),
            Token::Char(c) => write!(f, "{:?}", c),
            Token::Comment(c, single_line) => if *single_line {
                write!(f, "// {}", c)
            } else {
                write!(f, "/* {} */", c)
            },
            Token::Keyword(kw)  => Display::fmt(kw, f),
            Token::Operator(op) => Display::fmt(op, f),
            Token::Delimiter(d) => Display::fmt(d, f),
            Token::LineSep => f.write_str(";"),
        }
    }
}