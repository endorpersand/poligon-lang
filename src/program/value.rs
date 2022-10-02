use super::tree::{self, op};

#[derive(PartialEq, Clone)]
pub enum Value {
    Int(isize),
    Float(f64),
    Char(char),
    Str(String),
    Bool(bool)
}

impl Value {
    /// Truthiness of a value: when it is cast to bool, what truth value should it have?
    /// 
    /// Numerics: Non-zero => true
    /// Collections: Non-empty => true
    pub(super) fn truth(&self) -> bool {
        match self {
            Value::Int(v) => v != &0,
            Value::Float(v) => v != &0.0,
            Value::Char(_) => true,
            Value::Str(v) => !v.is_empty(),
            Value::Bool(v) => *v
        }
    }

    pub fn is_numeric(&self) -> bool {
        matches!(self, Value::Int(_) | Value::Float(_))
    }
}

impl From<tree::Literal> for Value {
    fn from(literal: tree::Literal) -> Self {
        match literal {
            tree::Literal::Int(v)   => Value::Int(v),
            tree::Literal::Float(v) => Value::Float(v),
            tree::Literal::Char(v)  => Value::Char(v),
            tree::Literal::Str(v)   => Value::Str(v),
            tree::Literal::Bool(v)  => Value::Bool(v),
        }
    }
}

impl op::Applicable for Value {
    type Return = Option<Value>;

    fn apply_unary(&self, o: op::Unary) -> Self::Return {
        match o {
            op::Unary::Plus   => if self.is_numeric() { Some(self.clone()) } else { None },
            op::Unary::Minus  => match self {
                Value::Int(e)   => Some(Value::Int(-e)),
                Value::Float(e) => Some(Value::Float(-e)),
                _ => None,
            },
            op::Unary::LogNot => Some(Value::Bool(!self.truth())),
            op::Unary::BitNot => if let Value::Int(e) = self { Some(Value::Int(!e)) } else { None },
            op::Unary::Spread => if let Value::Str(_e) = self { todo!() } else { None },
        }
    }

    fn apply_binary(&self, o: op::Binary, right: &Self) -> Self::Return {
        match (self, right) {
            (Value::Int(_), Value::Int(_)) => todo!(),
            (Value::Int(_), Value::Float(_)) => todo!(),
            (Value::Int(_), Value::Char(_)) => todo!(),
            (Value::Int(_), Value::Str(_)) => todo!(),
            (Value::Int(_), Value::Bool(_)) => todo!(),
            (Value::Float(_), Value::Int(_)) => todo!(),
            (Value::Float(_), Value::Float(_)) => todo!(),
            (Value::Float(_), Value::Char(_)) => todo!(),
            (Value::Float(_), Value::Str(_)) => todo!(),
            (Value::Float(_), Value::Bool(_)) => todo!(),
            (Value::Char(_), Value::Int(_)) => todo!(),
            (Value::Char(_), Value::Float(_)) => todo!(),
            (Value::Char(_), Value::Char(_)) => todo!(),
            (Value::Char(_), Value::Str(_)) => todo!(),
            (Value::Char(_), Value::Bool(_)) => todo!(),
            (Value::Str(_), Value::Int(_)) => todo!(),
            (Value::Str(_), Value::Float(_)) => todo!(),
            (Value::Str(_), Value::Char(_)) => todo!(),
            (Value::Str(_), Value::Str(_)) => todo!(),
            (Value::Str(_), Value::Bool(_)) => todo!(),
            (Value::Bool(_), Value::Int(_)) => todo!(),
            (Value::Bool(_), Value::Float(_)) => todo!(),
            (Value::Bool(_), Value::Char(_)) => todo!(),
            (Value::Bool(_), Value::Str(_)) => todo!(),
            (Value::Bool(_), Value::Bool(_)) => todo!(),
        }
    }

    fn apply_cmp(&self, o: op::Cmp, right: &Self) -> Self::Return {
        match o {
            op::Cmp::Lt | op::Cmp::Gt | op::Cmp::Le | op::Cmp::Ge => {
                match (self, right) {
                    // booleans cannot be compared
                    (Value::Bool(_), _) => None,
                    (_, Value::Bool(_)) => None,

                    // str, char can be compared against each other but not anything else
                    (Value::Str(_), Value::Str(_)) => todo!(),
                    (Value::Char(_), Value::Char(_)) => todo!(),
                    (Value::Str(_),  _) => None,
                    (Value::Char(_), _) => None,
                    (_, Value::Str(_))  => None,
                    (_, Value::Char(_)) => None,

                    // integer/float cmp
                    (Value::Int(_), Value::Int(_)) => todo!(),
                    (Value::Int(_), Value::Float(_)) => todo!(),
                    (Value::Float(_), Value::Int(_)) => todo!(),
                    (Value::Float(_), Value::Float(_)) => todo!(),
                }
            },
            op::Cmp::Eq | op::Cmp::Ne => {
                match (self, right) {
                    // booleans cannot be compared
                    (Value::Bool(_), Value::Bool(_)) => todo!(),
                    (Value::Bool(_), _) => None,
                    (_, Value::Bool(_)) => None,

                    // str, char can be compared against each other but not anything else
                    (Value::Str(_), Value::Str(_)) => todo!(),
                    (Value::Char(_), Value::Char(_)) => todo!(),
                    (Value::Str(_),  _) => None,
                    (Value::Char(_), _) => None,
                    (_, Value::Str(_))  => None,
                    (_, Value::Char(_)) => None,

                    // integer/float cmp
                    (Value::Int(_), Value::Int(_)) => todo!(),
                    (Value::Int(_), Value::Float(_)) => todo!(),
                    (Value::Float(_), Value::Int(_)) => todo!(),
                    (Value::Float(_), Value::Float(_)) => todo!(),
                }
            },
        }
    }
}