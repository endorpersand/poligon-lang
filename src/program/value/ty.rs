use std::fmt::Display;

use super::{FunType, FunParams};

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ValueType { Int, Float, Char, Str, Bool, List, Unit, Fun(FunType) }

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum VArbType {
    Value(ValueType),
    Unk
}

impl Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueType::Int   => f.write_str("int"),
            ValueType::Float => f.write_str("float"),
            ValueType::Char  => f.write_str("char"),
            ValueType::Str   => f.write_str("string"),
            ValueType::Bool  => f.write_str("bool"),
            ValueType::List  => f.write_str("list"),
            ValueType::Unit  => f.write_str("void"),
            ValueType::Fun(FunType(params, ret)) => {
                let pstrs = match &**params {
                    FunParams::Positional(p) => p.iter()
                        .map(ToString::to_string)
                        .collect::<Vec<_>>(),

                    FunParams::PosSpread(p, s) => {
                        let mut ps = p.iter()
                        .map(ToString::to_string)
                        .collect::<Vec<_>>();

                        ps.push(format!("..{}", ret));

                        ps
                    },
                };

                f.write_str(
                    &format!("({}) -> {}", pstrs.join(", "), ret)
                )
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