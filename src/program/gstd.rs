use std::collections::HashMap;
use std::rc::Rc;

use crate::Printable;

use super::{RtResult, RuntimeErr};
use super::value::{Value, FunParamType, VArbType, ValueType, FunType};

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
            (Value::List(al), Value::List(bl)) => Rc::ptr_eq(&al, &bl),
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
        "print": Value::new_fun(
            Some("print"), 
            FunType::new(FunParamType::PosSpread(vec![], VArbType::Unk), VArbType::Value(ValueType::Unit)),
            std_print
        ),
        "is": Value::new_fun(
            Some("is"),
            FunType::new(FunParamType::Positional(vec![VArbType::Unk, VArbType::Unk]), VArbType::Value(ValueType::Bool)),
            std_is
        ),
        "type": Value::new_fun(
            Some("type"),
            FunType::new(FunParamType::Positional(vec![VArbType::Unk]), VArbType::Value(ValueType::Str)),
            std_type
        )
    }
}