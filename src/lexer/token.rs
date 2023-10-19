//! The tokens that the string can be parsed into.
//! 
//! See [`Token`] for more information.

use std::fmt::{Debug, Display};
use std::collections::{BTreeMap, HashMap};
use once_cell::sync::Lazy;
use crate::span::{Span, Spanned};

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
    Delimiter(Delimiter, bool /* direction: false = L, true = R */),

    /// End of line (`;`)
    LineSep
}

/// A token with position information.
#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct FullToken {
    pub(crate) kind: Token,
    pub(crate) span: Span,
}

impl FullToken {
    /// Create a FullToken using a token and its given position.
    pub fn new(kind: Token, span: Span) -> Self {
        Self { kind, span }
    }
}

impl std::ops::Deref for FullToken {
    type Target = Token;

    fn deref(&self) -> &Self::Target {
        &self.kind
    }
}

impl PartialEq<Token> for FullToken {
    fn eq(&self, other: &Token) -> bool {
        &self.kind == other
    }
}
impl PartialEq<FullToken> for Token {
    fn eq(&self, other: &FullToken) -> bool {
        self == &other.kind
    }
}
impl Spanned for FullToken {
    fn span(&self) -> Span {
        self.span
    }
}

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

macro_rules! define_operators {
    ($($id:ident: $ex:literal),*) => {
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

        pub(super) static OP_MAP: Lazy<BTreeMap<&'static str, Token>> = Lazy::new(|| {
            let mut m = BTreeMap::new();

            $(m.insert($ex, Token::Operator(Operator::$id));)*

            m
        });
    };
}

macro_rules! define_delimiters {
    ($($id:ident: $exl:literal, $exr:literal),*) => {
        /// The defined Poligon delimiters (`()`, `[]`, etc.).
        #[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
        pub enum Delimiter {
            $(
                #[allow(missing_docs)] $id
            ),*
        }
    
        impl Delimiter {
            fn display_left(&self) -> &'static str {
                match self {
                    $(Self::$id => $exl),*
                }
            }
            fn display_right(&self) -> &'static str {
                match self {
                    $(Self::$id => $exr),*
                }
            }
        }
    
        pub(super) static DE_MAP: Lazy<BTreeMap<&'static str, Token>> = Lazy::new(|| {
            let mut m = BTreeMap::new();
    
            $(m.insert($exl, Token::Delimiter(Delimiter::$id, false));)*
            $(m.insert($exr, Token::Delimiter(Delimiter::$id, true));)*
    
            m
        });
    };
}
impl Display for Delimiter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.display_left())?;
        f.write_str(self.display_right())
    }
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

define_operators! {
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
    Arrow: "->",

    LComment: "/*",
    RComment: "*/"
}
define_delimiters! {
    Paren:  "(", ")",
    Square: "[", "]",
    Curly:  "{", "}"
}

/// A group of tokens, wrapped with a delimiter.
#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct Group {
    /// The delimiter
    pub delimiter: Delimiter,
    /// The tokens encapsulated by this group
    pub content: Vec<TokenTree>,
    /// The span of the left delimiter
    pub left_span: Span,
    /// The span of the right delimiter
    pub right_span: Span
}
impl Spanned for Group {
    fn span(&self) -> Span {
        self.left_span + self.right_span
    }
}

/// A struct that either can hold a token unit or a group.
#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum TokenTree {
    #[allow(missing_docs)]
    Token(FullToken),
    #[allow(missing_docs)]
    Group(Group)
}
impl TokenTree {
    pub fn kind(&self) -> TTKind {
        match self {
            TokenTree::Token(t) => TTKind::Token(&t.kind),
            TokenTree::Group(g) => TTKind::Group(&g.delimiter),
        }
    }
}
impl Spanned for TokenTree {
    fn span(&self) -> Span {
        match self {
            TokenTree::Token(e) => e.span(),
            TokenTree::Group(e) => e.span(),
        }
    }
}
impl PartialEq<Token> for TokenTree {
    fn eq(&self, other: &Token) -> bool {
        match self {
            TokenTree::Token(t) => t == other,
            TokenTree::Group(_) => false,
        }
    }
}
impl PartialEq<FullToken> for TokenTree {
    fn eq(&self, other: &FullToken) -> bool {
        match self {
            TokenTree::Token(t) => t == other,
            TokenTree::Group(_) => false,
        }
    }
}
impl PartialEq<TokenTree> for Token {
    fn eq(&self, other: &TokenTree) -> bool {
        match other {
            TokenTree::Token(t) => t == self,
            TokenTree::Group(_) => false,
        }
    }
}
impl PartialEq<TokenTree> for FullToken {
    fn eq(&self, other: &TokenTree) -> bool {
        match other {
            TokenTree::Token(t) => t == self,
            TokenTree::Group(_) => false,
        }
    }
}
impl TryFrom<TokenTree> for FullToken {
    type Error = &'static str;

    fn try_from(value: TokenTree) -> Result<Self, Self::Error> {
        match value {
            TokenTree::Token(t) => Ok(t),
            TokenTree::Group(_) => Err("poor support for TokenTree"),
        }
    }
}

pub enum TTKind<'t> {
    Token(&'t Token),
    Group(&'t Delimiter)
}

pub type Stream<'s> = &'s [TokenTree];
pub type OwnedStream = Vec<TokenTree>;

/// Should only be used to define 2-char tokens that can be split into 2 1-char tokens.
pub(crate) static SPLITTABLES: Lazy<HashMap<Token, (Token, Token)>> = Lazy::new(|| {
    let mut m = HashMap::new();
    m.insert(token![..], (token![.], token![.]));
    m.insert(token![&&], (token![&], token![&]));
    m.insert(token![||], (token![|], token![|]));
    m.insert(token![<=], (token![<], token![=]));
    m.insert(token![>=], (token![>], token![=]));
    m.insert(token![==], (token![=], token![=]));
    m.insert(token![<<], (token![<], token![<]));
    m.insert(token![>>], (token![>], token![>]));
    m.insert(token![::], (token![:], token![:]));
    m.insert(token![->], (token![-], token![>]));
    m
});

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
    (+)    => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Plus)     };
    (-)    => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Minus)    };
    (*)    => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Star)     };
    (/)    => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Slash)    };
    (%)    => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Percent)  };
    (.)    => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Dot)      };
    (..)   => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::DDot)     };
    (|)    => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Or)       };
    (&)    => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::And)      };
    (~)    => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Tilde)    };
    (^)    => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Caret)    };
    (&&)   => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::DAnd)     };
    (||)   => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::DOr)      };
    (!)    => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Excl)     };
    (<)    => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Lt)       };
    (<=)   => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Le)       };
    (>)    => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Gt)       };
    (>=)   => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Ge)       };
    (=)    => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Equal)    };
    (==)   => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::DEqual)   };
    (!=)   => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Ne)       };
    (<<)   => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Shl)      };
    (>>)   => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Shr)      };
    (,)    => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Comma)    };
    ("//") => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Comment)  };
    (:)    => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Colon)    };
    (::)   => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::DColon)   };
    (#)    => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Hash)     };
    (->)   => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::Arrow)    };
    ("/*") => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::LComment) };
    ("*/") => { $crate::lexer::token::Token::Operator($crate::lexer::token::Operator::RComment) };

    ("(")  => { $crate::lexer::token::Token::Delimiter($crate::lexer::token::Delimiter::Paren,  false) };
    (")")  => { $crate::lexer::token::Token::Delimiter($crate::lexer::token::Delimiter::Paren,  true)  };
    ("[")  => { $crate::lexer::token::Token::Delimiter($crate::lexer::token::Delimiter::Square, false) };
    ("]")  => { $crate::lexer::token::Token::Delimiter($crate::lexer::token::Delimiter::Square, true)  };
    ("{")  => { $crate::lexer::token::Token::Delimiter($crate::lexer::token::Delimiter::Curly,  false) };
    ("}")  => { $crate::lexer::token::Token::Delimiter($crate::lexer::token::Delimiter::Curly,  true)  };

    (;) => { $crate::lexer::token::Token::LineSep };
}
#[doc(inline)]
pub use token;

#[macro_export]
macro_rules! delim {
    ("(")   => { $crate::lexer::token::Delimiter::Paren  };
    (")")   => { $crate::lexer::token::Delimiter::Paren  };
    ("()")  => { $crate::lexer::token::Delimiter::Paren  };

    ("[")   => { $crate::lexer::token::Delimiter::Square };
    ("]")   => { $crate::lexer::token::Delimiter::Square };
    ("[]")  => { $crate::lexer::token::Delimiter::Square };

    ("{")   => { $crate::lexer::token::Delimiter::Curly  };
    ("}")   => { $crate::lexer::token::Delimiter::Curly  };
    ("{}")  => { $crate::lexer::token::Delimiter::Curly  };
}

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
            Token::Delimiter(delim, dir) => match dir {
                false => f.write_str(delim.display_left()),
                true  => f.write_str(delim.display_right())
            },
            Token::LineSep => f.write_str(";"),
        }
    }
}