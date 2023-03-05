use crate::ast::op;
use crate::interpreter::runtime::{TypeErr, ValueErr};

use super::{Value, RefValue};

enum NumOperands {
    Float(f64, f64),
    Int(isize, isize),
    Neither(Value, Value)
}

enum CollValue {
    Str(String),
    List(RefValue<Vec<Value>>),
    Not(Value)
}

impl NumOperands {
    fn new(a: Value, b: Value) -> Self {
        match (a, b) {
            (Value::Float(a), Value::Float(b)) => Self::Float(a, b),
            (Value::Int(a), Value::Float(b))   => Self::Float(a as _, b),
            (Value::Float(a), Value::Int(b))   => Self::Float(a, b as _),
            (Value::Int(a), Value::Int(b))     => Self::Int(a, b),
            (a, b) => Self::Neither(a, b)
        }
    }
}

impl CollValue {
    fn new(t: Value) -> Self {
        match t {
            Value::Char(c) => Self::Str(String::from(c)),
            Value::Str(s)  => Self::Str(s),
            Value::List(l) => Self::List(l),
            t => Self::Not(t),
        }
    }

    fn repeat(self, n: isize) -> Result<Value, Value> {
        let n = usize::try_from(n).unwrap_or(0);

        match self {
            CollValue::Str(s)  => Ok(Value::Str(s.repeat(n))),
            CollValue::List(l) => Ok(Value::new_list(
                l.clone_deep().into_iter()
                    .cycle()
                    .take(l.borrow().len() * n)
                    .collect()
            )),
            CollValue::Not(n) => Err(n),
        }
    }

    fn revert(self) -> Value {
        match self {
            CollValue::Str(s)  => Value::Str(s),
            CollValue::List(l) => Value::List(l),
            CollValue::Not(n)  => n,
        }
    }
}

fn as_float_pairs(lhs: Value, rhs: Value) -> Result<(f64, f64), (Value, Value)> {
    match (lhs.as_float(), rhs.as_float()) {
        (Some(a), Some(b)) => Ok((a, b)),
        _ => Err((lhs, rhs))
    }
}
fn as_int_pairs(lhs: Value, rhs: Value) -> Result<(isize, isize), (Value, Value)> {
    match (lhs, rhs) {
        (Value::Int(a), Value::Int(b)) => Ok((a, b)),
        (a, b) => Err((a, b))
    }
}

impl Value {
    /// Apply a binary operation to two given values.
    pub fn apply_binary(self, o: op::Binary, rhs: Self) -> super::BasicRtResult<Value> {
        macro_rules! cannot_binary {
            ($l:expr, $r:expr) => { TypeErr::CannotBinary(o, $l.ty(), $r.ty()).into() }
        }

        macro_rules! int_only_op {
            ($l:ident $t:tt $r:ident) => {
                match as_int_pairs($l, $r) {
                    Ok((a, b)) => Ok(Value::Int(a $t b)),
                    Err((a, b)) => Err(cannot_binary!(a, b)),
                }
            }
        }
        match o {
            op::Binary::Add => match NumOperands::new(self, rhs) {
                NumOperands::Float(a, b) => Ok(Value::Float(a + b)),
                NumOperands::Int(a, b)   => Ok(Value::Int(a + b)),
                NumOperands::Neither(a, b) => Err(cannot_binary!(a, b)),
            },
            op::Binary::Sub => match NumOperands::new(self, rhs) {
                NumOperands::Float(a, b)   => Ok(Value::Float(a - b)),
                NumOperands::Int(a, b)     => Ok(Value::Int(a - b)),
                NumOperands::Neither(a, b) => Err(cannot_binary!(a, b)),
            },
            op::Binary::Mul => match NumOperands::new(self, rhs) {
                // numeric
                NumOperands::Float(a, b) => Ok(Value::Float(a * b)),
                NumOperands::Int(a, b)   => Ok(Value::Int(a * b)),

                // 2 * "chr" == "chrchr"
                NumOperands::Neither(a, b) => {
                    let aty = a.ty();
                    let bty = b.ty();
                    match (CollValue::new(a), CollValue::new(b)) {
                        (a, CollValue::Not(Value::Int(b))) => a.repeat(b).ok(),
                        (CollValue::Not(Value::Int(a)), b) => b.repeat(a).ok(),
                        _ => None
                    }.ok_or(TypeErr::CannotBinary(o, aty, bty).into())
                },
            },

            // int -> float for div
            op::Binary::Div => match as_float_pairs(self, rhs) {
                Ok((a, b)) => Ok(Value::Float(a / b)),
                Err((a, b)) => Err(cannot_binary!(a, b))
            },
            op::Binary::Mod => match NumOperands::new(self, rhs) {
                NumOperands::Float(a, b) => Ok(Value::Float(a % b)),
                NumOperands::Int(a, b) => a.checked_rem(b)
                    .map(Value::Int)
                    .ok_or_else(|| ValueErr::DivisionByZero.into()),
                NumOperands::Neither(a, b) => Err(cannot_binary!(a, b)),
            },

            // <<, >>
            op::Binary::Shl => int_only_op!(self << rhs),
            op::Binary::Shr => int_only_op!(self >> rhs),

            op::Binary::BitOr => match as_int_pairs(self, rhs) {
                Ok((a, b)) => Ok(Value::Int(a | b)),
                Err((a, b)) => match (CollValue::new(a), CollValue::new(b)) {
                    (CollValue::Str(mut s1), CollValue::Str(s2)) => {
                        s1.push_str(&s2);
                        Ok(Value::Str(s1))
                    },
                    (CollValue::List(l1), CollValue::List(l2)) => {
                        let lst = l1.clone_deep()
                            .into_iter()
                            .chain(l2.clone_deep())
                            .collect();
    
                        Ok(Value::new_list(lst))
                    },
                    (a, b) => Err(cannot_binary!(a.revert(), b.revert())),
                },
            },
            op::Binary::BitAnd => int_only_op!(self & rhs),
            op::Binary::BitXor => int_only_op!(self ^ rhs),
            op::Binary::LogAnd => Ok(if self.truth() { rhs } else { self }),
            op::Binary::LogOr  => Ok(if self.truth() { self } else { rhs }),
        }
    }
}