use std::collections::HashMap;

use super::{RtResult, RuntimeErr};
use super::value::{Value, FunParamType, VArbType, ValueType, FunType, fun_type};

macro_rules! str_map {
    ($($k:literal: $v:expr),*) => {
        let mut m = HashMap::new();

        $(
            m.insert(String::from($k), $v);
        )*

        m
    }
}

fn std_print(args: Vec<Value>) -> RtResult<Value> {
    let strs = args.into_iter()
        .map(|v| v.str())
        .collect::<Vec<_>>()
        .join(" ");

    println!("{}", strs);

    Ok(Value::Unit)
}
fn std_is(args: Vec<Value>) -> RtResult<Value> {
    if let [a, b] = &args[..] {
        let eval = match (a, b) {
            (Value::List(al), Value::List(bl)) => al.ref_eq(bl),
            (av, bv) => av == bv
        };

        Ok(Value::Bool(eval))
    } else {
        Err(RuntimeErr::WrongArity(2))
    }
}

fn std_type(args: Vec<Value>) -> RtResult<Value> {
    if let [a] = &args[..] {
        Ok(Value::Str(a.ty().to_string()))
    } else {
        Err(RuntimeErr::WrongArity(1))
    }
}

pub(super) fn std_map() -> HashMap<String, Value> {
    str_map! {
        "print": Value::new_rust_fn(
            Some("print"), 
            fun_type! { (~VArbType::Unk) -> VArbType::Value(ValueType::Unit) },
            std_print
        ),
        "is": Value::new_rust_fn(
            Some("is"),
            fun_type! { (VArbType::Unk, VArbType::Unk) -> VArbType::Value(ValueType::Bool) },
            std_is
        ),
        "type": Value::new_rust_fn(
            Some("type"),
            fun_type! { (VArbType::Unk) -> VArbType::Value(ValueType::Str) },
            std_type
        )
    }
}