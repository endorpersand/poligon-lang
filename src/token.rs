//! The tokens that the string can be parsed into.
//! 
//! See [`Token`] for more information.

use std::fmt::{Debug, Display};
use std::collections::BTreeMap;
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
    Comment(Comment),
    
    /// Keywords (e.g. `let`, `const`, `fun`). 
    /// 
    /// These cannot be identifiers in any circumstance.
    Keyword(Keyword),

    /// Operators (e.g. `+`, `-`, `/`)
    Operator(Operator),

    /// Delimiters (e.g. `()`, `[]`)
    Delimiter(DelimiterToken),

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

/// A comment token (e.g. `// text`, `/* text */`).
#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct Comment {
    /// The text stored by this comment, trimmed
    pub text: String,
    /// Whether this comment was a single-line or multi-line comment
    pub single_line: bool
}
impl Display for Comment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.single_line {
            true  => write!(f, "// {}", self.text),
            false => write!(f, "/* {} */", self.text),
        }
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
            /// Gets the left delimiter token.
            pub const fn left(&self) -> Token {
                Token::Delimiter(DelimiterToken{
                    delimiter: *self, 
                    is_right: false
                })
            }
            /// Gets the right delimiter token.
            pub const fn right(&self) -> Token {
                Token::Delimiter(DelimiterToken{
                    delimiter: *self, 
                    is_right: true
                })
            }

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
    
            $(m.insert($exl, Delimiter::$id.left());)*
            $(m.insert($exr, Delimiter::$id.right());)*
    
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

/// A delimiter token (e.g. `(`, `)`, `[`, `]`)
/// 
/// In a `Vec<FullToken>`, these may be present, but in a `Vec<TokenTree>`,
/// these must not exist.
#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct DelimiterToken {
    /// The type of delimiter
    pub delimiter: Delimiter,
    /// Whether the delimiter is a left or right delimiter
    pub is_right: bool
}
impl Display for DelimiterToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.is_right {
            false => f.write_str(self.delimiter.display_left()),
            true  => f.write_str(self.delimiter.display_right())
        }
    }
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
    /// Converts this TokenTree into its kind.
    /// 
    /// See [`TTKind`] for more details.
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

/// Enum which holds the type of token/group that a [`TokenTree`] represents.
/// 
/// This is made to be simpler to pattern match against than [`TokenTree`] 
/// (when combined with the [`token`] and [`delim`] macros).
/// 
/// A `TTKind` can be created with the [`TokenTree::kind`] method.
pub enum TTKind<'t> {
    /// This variant indicates the given token tree is some unit token.
    Token(&'t Token),
    /// This variant indicates the given token tree is some group
    /// and provides its delimiter for matching.
    Group(&'t Delimiter)
}

/// A stream of tokens that can be parsed over.
pub type Stream<'s> = &'s [TokenTree];
/// An owned version of [`Stream`], which holds a stream of tokens that can be parsed over.
pub type OwnedStream = Vec<TokenTree>;

/// Utility macro that can be used as a shorthand for [`Keyword`] or [`Operator`] tokens.
#[macro_export]
macro_rules! token {
    (let)      => { $crate::token::Token::Keyword($crate::token::Keyword::Let)      };
    (const)    => { $crate::token::Token::Keyword($crate::token::Keyword::Const)    };
    (mut)      => { $crate::token::Token::Keyword($crate::token::Keyword::Mut)      };
    (while)    => { $crate::token::Token::Keyword($crate::token::Keyword::While)    };
    (for)      => { $crate::token::Token::Keyword($crate::token::Keyword::For)      };
    (if)       => { $crate::token::Token::Keyword($crate::token::Keyword::If)       };
    (else)     => { $crate::token::Token::Keyword($crate::token::Keyword::Else)     };
    (fun)      => { $crate::token::Token::Keyword($crate::token::Keyword::Fun)      };
    (shape)    => { $crate::token::Token::Keyword($crate::token::Keyword::Shape)    };
    (class)    => { $crate::token::Token::Keyword($crate::token::Keyword::Class)    };
    (fit)      => { $crate::token::Token::Keyword($crate::token::Keyword::Fit)      };
    (to)       => { $crate::token::Token::Keyword($crate::token::Keyword::To)       };
    // (fits)     => { $crate::token::Token::Keyword($crate::token::Keyword::Fits)     };
    // (extend)   => { $crate::token::Token::Keyword($crate::token::Keyword::Extend)  };
    (match)    => { $crate::token::Token::Keyword($crate::token::Keyword::Match)    };
    // (unit)     => { $crate::token::Token::Keyword($crate::token::Keyword::Unit)     };
    // (measure)  => { $crate::token::Token::Keyword($crate::token::Keyword::Measure)  };
    // (of)       => { $crate::token::Token::Keyword($crate::token::Keyword::Of)       };
    (return)   => { $crate::token::Token::Keyword($crate::token::Keyword::Return)   };
    (break)    => { $crate::token::Token::Keyword($crate::token::Keyword::Break)    };
    (continue) => { $crate::token::Token::Keyword($crate::token::Keyword::Continue) };
    (step)     => { $crate::token::Token::Keyword($crate::token::Keyword::Step)     };
    (in)       => { $crate::token::Token::Keyword($crate::token::Keyword::In)       };
    (true)     => { $crate::token::Token::Keyword($crate::token::Keyword::True)     };
    (false)    => { $crate::token::Token::Keyword($crate::token::Keyword::False)    };
    (extern)   => { $crate::token::Token::Keyword($crate::token::Keyword::Extern)   };
    (import)   => { $crate::token::Token::Keyword($crate::token::Keyword::Import)   };
    (global)   => { $crate::token::Token::Keyword($crate::token::Keyword::Global)   };
    (throw)    => { $crate::token::Token::Keyword($crate::token::Keyword::Throw)    };
    (+)    => { $crate::token::Token::Operator($crate::token::Operator::Plus)     };
    (-)    => { $crate::token::Token::Operator($crate::token::Operator::Minus)    };
    (*)    => { $crate::token::Token::Operator($crate::token::Operator::Star)     };
    (/)    => { $crate::token::Token::Operator($crate::token::Operator::Slash)    };
    (%)    => { $crate::token::Token::Operator($crate::token::Operator::Percent)  };
    (.)    => { $crate::token::Token::Operator($crate::token::Operator::Dot)      };
    (..)   => { $crate::token::Token::Operator($crate::token::Operator::DDot)     };
    (|)    => { $crate::token::Token::Operator($crate::token::Operator::Or)       };
    (&)    => { $crate::token::Token::Operator($crate::token::Operator::And)      };
    (~)    => { $crate::token::Token::Operator($crate::token::Operator::Tilde)    };
    (^)    => { $crate::token::Token::Operator($crate::token::Operator::Caret)    };
    (&&)   => { $crate::token::Token::Operator($crate::token::Operator::DAnd)     };
    (||)   => { $crate::token::Token::Operator($crate::token::Operator::DOr)      };
    (!)    => { $crate::token::Token::Operator($crate::token::Operator::Excl)     };
    (<)    => { $crate::token::Token::Operator($crate::token::Operator::Lt)       };
    (<=)   => { $crate::token::Token::Operator($crate::token::Operator::Le)       };
    (>)    => { $crate::token::Token::Operator($crate::token::Operator::Gt)       };
    (>=)   => { $crate::token::Token::Operator($crate::token::Operator::Ge)       };
    (=)    => { $crate::token::Token::Operator($crate::token::Operator::Equal)    };
    (==)   => { $crate::token::Token::Operator($crate::token::Operator::DEqual)   };
    (!=)   => { $crate::token::Token::Operator($crate::token::Operator::Ne)       };
    (<<)   => { $crate::token::Token::Operator($crate::token::Operator::Shl)      };
    (>>)   => { $crate::token::Token::Operator($crate::token::Operator::Shr)      };
    (,)    => { $crate::token::Token::Operator($crate::token::Operator::Comma)    };
    ("//") => { $crate::token::Token::Operator($crate::token::Operator::Comment)  };
    (:)    => { $crate::token::Token::Operator($crate::token::Operator::Colon)    };
    (::)   => { $crate::token::Token::Operator($crate::token::Operator::DColon)   };
    (#)    => { $crate::token::Token::Operator($crate::token::Operator::Hash)     };
    (->)   => { $crate::token::Token::Operator($crate::token::Operator::Arrow)    };
    ("/*") => { $crate::token::Token::Operator($crate::token::Operator::LComment) };
    ("*/") => { $crate::token::Token::Operator($crate::token::Operator::RComment) };

    (;) => { $crate::token::Token::LineSep };
}
#[doc(inline)]
pub use token;

/// Utility macro that can be used as a shorthand for [`Delimiter`]s.
#[macro_export]
macro_rules! delim {
    ("(")   => { $crate::token::Delimiter::Paren  };
    (")")   => { $crate::token::Delimiter::Paren  };
    ("()")  => { $crate::token::Delimiter::Paren  };

    ("[")   => { $crate::token::Delimiter::Square };
    ("]")   => { $crate::token::Delimiter::Square };
    ("[]")  => { $crate::token::Delimiter::Square };

    ("{")   => { $crate::token::Delimiter::Curly  };
    ("}")   => { $crate::token::Delimiter::Curly  };
    ("{}")  => { $crate::token::Delimiter::Curly  };
}
#[doc(inline)]
pub use delim;

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Ident(s) => f.write_str(s),
            Token::Numeric(n) => f.write_str(n),
            Token::Str(s)  => write!(f, "{:?}", s),
            Token::Char(c) => write!(f, "{:?}", c),
            Token::Comment(c) => Display::fmt(c, f),
            Token::Keyword(kw)  => Display::fmt(kw, f),
            Token::Operator(op) => Display::fmt(op, f),
            Token::Delimiter(dt) => Display::fmt(dt, f),
            Token::LineSep => f.write_str(";"),
        }
    }
}