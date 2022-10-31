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

fn std_contains(args: Vec<Value>) -> RtResult<Value> {
    if let [collection, item] = &args[..] {
        let b = match collection {
            Value::Char(c1) => match item {
                Value::Char(c2) => c1 == c2,
                Value::Str(s2) => s2.len() == 0 || (s2.len() == 1 && s2 == &c1.to_string()),
                _ => Err(RuntimeErr::ExpectedType(ValueType::Str))?
            },
            Value::Str(s)  => match item {
                Value::Char(c2) => s.contains(*c2),
                Value::Str(s2) => s.contains(s2),
                _ => Err(RuntimeErr::ExpectedType(ValueType::Str))?
            },
            Value::List(l) => l.borrow().contains(item),
            _ => Err(RuntimeErr::ExpectedType(ValueType::List(Box::new(VArbType::Unk))))?
        };
        Ok(Value::Bool(b))
    } else {
        Err(RuntimeErr::WrongArity(2))
    }
}
fn std_push(args: Vec<Value>) -> RtResult<Value> {
    if let [lst, item] = &args[..] {
        if let Value::List(l) = lst {
            l.try_borrow_mut()?.push(item.clone());
            Ok(Value::Unit)
        } else {
            Err(RuntimeErr::ExpectedType(ValueType::List(Box::new(VArbType::Unk))))
        }
    } else {
        Err(RuntimeErr::WrongArity(2))
    }
}
fn std_pop(args: Vec<Value>) -> RtResult<Value> {
    if let [lst] = &args[..] {
        if let Value::List(l) = lst {
            Ok(l.try_borrow_mut()?.pop().unwrap_or(Value::Unit))
        } else {
            Err(RuntimeErr::ExpectedType(ValueType::List(Box::new(VArbType::Unk))))
        }
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
        ),
        "contains": Value::new_rust_fn(
            Some("contains"),
            fun_type! { (VArbType::Value(ValueType::List(Box::new(VArbType::Unk))), VArbType::Unk) -> VArbType::Value(ValueType::Bool) },
            std_contains
        ),
        "@@push": Value::new_rust_fn(
            Some("push"),
            fun_type! { (VArbType::Value(ValueType::List(Box::new(VArbType::Unk))), VArbType::Unk) -> VArbType::Value(ValueType::Unit) },
            std_push
        ),
        "@@pop": Value::new_rust_fn(
            Some("pop"),
            fun_type! { (VArbType::Value(ValueType::List(Box::new(VArbType::Unk)))) -> VArbType::Unk },
            std_pop
        )
    }
}