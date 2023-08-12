//! The operator AST nodes.
//! 
//! This differs from [operator tokens][crate::lexer::token] 
//! because these nodes have established meanings within the Poligon language
//! and can be used to apply operations in runtimes.

use std::fmt::Display;

macro_rules! define_ops {
    (#[$mm:meta] $t:ident {$(#[$m:meta] $id:ident: $ex:literal),*}) => {
        #[$mm]
        #[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
        pub enum $t {
            $(
                #[$m] $id
            ),*
        }

        impl Display for $t {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.write_str(match self {
                    $(Self::$id => $ex),*
                })
            }
        }
    }
}

define_ops! {
    #[doc = "A unary operator AST node."]
    Unary {
        #[doc = "Unary plus (`+x`)"]
        Plus: "+",
        
        #[doc = "Unary minus (`-x`)"]
        Minus: "-",
    
        #[doc = "Logical not (`!x`)"]
        LogNot: "!",
    
        #[doc = "Bitwise not (`~x`)"]
        BitNot: "~"
    }
}

define_ops! {
    #[doc = "A binary operator AST node."]
    Binary {
        #[doc = "Binary plus (`x + y`)"]
        Add: "+",

        #[doc = "Binary subtract (`x - y`)"]
        Sub: "-",

        #[doc = "Multiplication (`x * y`)"]
        Mul: "*",

        #[doc = "Division (`x / y`)"]
        Div: "/",

        #[doc = "Modulo (`x % y`)"]
        Mod: "%",

        #[doc = "Shift left (`x << y`)"]
        Shl: "<<",

        #[doc = "Shift right (`x >> y`)"]
        Shr: ">>",

        #[doc = "Bitwise or (`x | y`)"]
        BitOr: "|",

        #[doc = "Bitwise and (`x & y`)"]
        BitAnd: "&",

        #[doc = "Bitwise xor (`x ^ y`)"]
        BitXor: "^",

        #[doc = "Logical and (`x && y`)"]
        LogAnd: "&&",

        #[doc = "Logical or (`x || y`)"]
        LogOr: "||"
    }
}

define_ops! {
    #[doc = "A comparison operator AST node."]
    Cmp {
        #[doc = "Less than (`<`)"]
        Lt: "<", 
        
        #[doc = "Greater than (`>`)"]
        Gt: ">", 
        
        #[doc = "Less than or equal (`<=`)"]
        Le: "<=", 
        
        #[doc = "Greater than or equal (`>=`)"]
        Ge: ">=", 
        
        #[doc = "Equal (`==`)"]
        Eq: "==", 
        
        #[doc = "Not equal (`!=`)"]
        Ne: "!="
    }
}

impl Cmp {
    /// Apply the operator to a partially ordered type.
    pub fn cmp<E>(&self, l: E, r: E) -> bool
        where E: PartialOrd + PartialEq
    {
        match self {
            Cmp::Lt => l < r,
            Cmp::Gt => l > r,
            Cmp::Le => l >= r,
            Cmp::Ge => l >= r,
            Cmp::Eq => l == r,
            Cmp::Ne => l != r,
        }
    }

    /// Test if comparison operator is an order comparison (`true`) 
    /// or an equality comparison (`false`).
    pub fn is_ord_cmp(&self) -> bool {
        matches!(self, Cmp::Lt | Cmp::Gt | Cmp::Le | Cmp::Ge)
    }
}