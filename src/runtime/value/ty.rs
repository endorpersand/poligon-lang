use std::fmt::Display;

use crate::tree;

use super::{FunType, FunParamType};

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ValueType { Int, Float, Char, Str, Bool, List(Box<VArbType>), Unit, Fun(FunType) }

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum VArbType {
    Value(ValueType),
    Unk
}

impl Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueType::Int     => f.write_str("int"),
            ValueType::Float   => f.write_str("float"),
            ValueType::Char    => f.write_str("char"),
            ValueType::Str     => f.write_str("string"),
            ValueType::Bool    => f.write_str("bool"),
            ValueType::List(t) => f.write_fmt(format_args!("list<{}>", t)),
            ValueType::Unit    => f.write_str("void"),
            ValueType::Fun(FunType(params, ret)) => {
                let pstrs = match &**params {
                    FunParamType::Positional(p) => p.iter()
                        .map(ToString::to_string)
                        .collect::<Vec<_>>(),

                    FunParamType::PosSpread(p, s) => {
                        let mut ps = p.iter()
                        .map(ToString::to_string)
                        .collect::<Vec<_>>();

                        ps.push(format!("..{}", s));

                        ps
                    },
                };

                write!(f, "({}) -> {}", pstrs.join(", "), ret)
            },
        }
    }
}

impl Display for VArbType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VArbType::Value(v) => v.fmt(f),
            VArbType::Unk => f.write_str("unk"),
        }
    }
}

impl VArbType {
    pub fn lookup(t: &tree::Type) -> Self {
        // TODO, consider generics + resolve properly if not found
        
        let tree::Type(s, g) = t;
        
        match s.as_str() {
            "int"    => Self::Value(ValueType::Int),
            "float"  => Self::Value(ValueType::Float),
            "char"   => Self::Value(ValueType::Char),
            "string" => Self::Value(ValueType::Str),
            "bool"   => Self::Value(ValueType::Bool),
            "list"   => {
                if let [t] = &g[..] {
                    Self::Value(ValueType::List(Box::new(Self::lookup(t))))
                } else {
                    Self::Unk
                }
            },
            "void"   => Self::Value(ValueType::Unit),
            _        => Self::Unk
        }
    }
}