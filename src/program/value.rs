use super::tree::{self, op};

#[derive(PartialEq, Clone)]
pub enum Value {
    Int(isize),
    Float(f64),
    Char(char),
    Str(String),
    Bool(bool),
    List(Vec<Value>)
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
            Value::Bool(v) => *v,
            Value::List(v) => !v.is_empty(),
        }
    }

    pub fn as_float(&self) -> Option<f64> {
        match self {
            Value::Int(i) => Some(*i as _),
            Value::Float(f) => Some(*f),
            _ => None,
        }
    }

    pub fn is_numeric(&self) -> bool {
        matches!(self, Value::Int(_) | Value::Float(_))
    }

    pub fn ty(&self) -> String {
        match self {
            Value::Int(_) => "int",
            Value::Float(_) => "float",
            Value::Char(_) => "char",
            Value::Str(_) => "string",
            Value::Bool(_) => "bool",
            Value::List(_) => "list",
        }.into()
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

impl op::UnaryApplicable for Value {
    type Return = Result<Value, super::RuntimeErr>;

    fn apply_unary(&self, o: &op::Unary) -> Self::Return {
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
        }.ok_or_else(|| super::RuntimeErr::CannotApplyUnary(*o, self.ty()))
    }
}

impl op::BinaryApplicable for Value {
    type Return = Result<Value, super::RuntimeErr>;

    fn apply_binary(&self, o: &op::Binary, right: &Self) -> Self::Return {
        match o {
            op::Binary::Add => todo!(),
            op::Binary::Sub => todo!(),
            op::Binary::Mul => todo!(),
            op::Binary::Div => todo!(),
            op::Binary::Mod => todo!(),
            op::Binary::BitOr => todo!(),
            op::Binary::BitAnd => todo!(),
            op::Binary::BitXor => todo!(),
            op::Binary::LogAnd => todo!(),
            op::Binary::LogOr => todo!(),
        }
    }
}

impl op::CmpApplicable for Value {
    type Return = Result<bool, super::RuntimeErr>;

    fn apply_cmp(&self, o: &op::Cmp, right: &Self) -> Self::Return {
        match o {
            op::Cmp::Lt | op::Cmp::Gt | op::Cmp::Le | op::Cmp::Ge => {
                match (self, right) {
                    // integer/float cmp
                    (Value::Float(af), b) => b.as_float().map(|bf| o.cmp(*af, bf)),
                    (a, Value::Float(bf)) => a.as_float().map(|af| o.cmp(af, *bf)),
                    (Value::Int(a), Value::Int(b)) => Some(o.cmp(a, b)),
    
                    // str, char can be compared against each other but not anything else
                    (Value::Str(a), Value::Str(b)) => Some(o.cmp(a, b)),
                    (Value::Char(a), Value::Char(b)) => Some(o.cmp(a, b)),
                    
                    // booleans cannot be compared,
                    // and cross-type comparisons cannot occur
                    _ => None
                }
            },
            op::Cmp::Eq | op::Cmp::Ne => {
                match (self, right) {
                    // integer/float cmp
                    (Value::Float(af), b) => b.as_float().map(|bf| o.cmp(*af, bf)),
                    (a, Value::Float(bf)) => a.as_float().map(|af| o.cmp(af, *bf)),
                    (Value::Int(a), Value::Int(b)) => Some(o.cmp(a, b)),
    
                    // str, char can be compared against each other but not anything else
                    (Value::Str(a), Value::Str(b)) => Some(o.cmp(a, b)),
                    (Value::Char(a), Value::Char(b)) => Some(o.cmp(a, b)),
                    
                    // booleans
                    (Value::Bool(a), Value::Bool(b)) => Some(o.cmp(a, b)),
    
                    // and cross-type comparisons cannot occur
                    _ => None
                }
            },
        }.ok_or_else(|| super::RuntimeErr::CannotCompare(*o, self.ty(), right.ty()))
    }
}