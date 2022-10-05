use super::tree::{self, op};

#[derive(PartialEq, Clone, Debug)]
pub enum Value {
    Int(isize),
    Float(f64),
    Char(char),
    Str(String),
    Bool(bool),
    List(Vec<Value>),
    Unit
}

/// Utility to cast values onto float and compare them
fn float_cmp(a: impl TryInto<f64>, b: impl TryInto<f64>, o: &op::Cmp) -> Option<bool> {
    if let (Ok(af), Ok(bf)) = (a.try_into(), b.try_into()) {
        Some(o.cmp(af, bf))
    } else {
        None
    }
}

fn numeric_binary<FF, FI>(a: &Value, o: &op::Binary, b: &Value, ff: FF, fi: FI) -> super::RtResult<Value> 
    where FF: FnOnce(f64, f64) -> super::RtResult<Value>,
          FI: FnOnce(isize, isize) -> super::RtResult<Value>
{
    match (a, b) {
        (Value::Float(a), Value::Float(b)) => ff(*a, *b),
        (Value::Float(a), Value::Int(b))   => ff(*a, *b as _),
        (Value::Int(a), Value::Float(b))   => ff(*a as _, *b),
        (Value::Int(a), Value::Int(b))     => fi(*a, *b),

        _ => Err(super::RuntimeErr::CannotApplyBinary(*o, a.ty(), b.ty()))
    }
}

macro_rules! int_only_op {
    ($oenum:expr => $a:ident $o:tt $b:ident) => {
        if let (Value::Int(a), Value::Int(b)) = ($a, $b) {
            Ok(Value::Int(a $o b))
        } else {
            Err(super::RuntimeErr::CannotApplyBinary(*$oenum, $a.ty(), $b.ty()))
        }
    }
}

impl Value {
    /// Truthiness of a value: when it is cast to bool, what truth value should it have?
    /// 
    /// Numerics: Non-zero => true
    /// Collections: Non-empty => true
    pub(super) fn truth(&self) -> bool {
        match self {
            Value::Int(v)   => v != &0,
            Value::Float(v) => v != &0.0,
            Value::Char(_)  => true,
            Value::Str(v)   => !v.is_empty(),
            Value::Bool(v)  => *v,
            Value::List(v)  => !v.is_empty(),
            Value::Unit     => false
        }
    }

    pub fn is_numeric(&self) -> bool {
        matches!(self, Value::Int(_) | Value::Float(_))
    }

    pub fn ty(&self) -> String {
        match self {
            Value::Int(_)   => "int",
            Value::Float(_) => "float",
            Value::Char(_)  => "char",
            Value::Str(_)   => "string",
            Value::Bool(_)  => "bool",
            Value::List(_)  => "list",
            Value::Unit     => "void"
        }.into()
    }

    pub(super) fn as_iterator<'a>(&'a self) -> Option<Box<dyn Iterator<Item=Value> + 'a>> {
        match self {
            Value::Str(s)   => Some(Box::new(s.chars().map(Value::Char))),
            Value::List(l)  => Some(Box::new(l.iter().cloned())),
            Value::Int(_)   => None,
            Value::Float(_) => None,
            Value::Char(_)  => None,
            Value::Bool(_)  => None,
            Value::Unit     => None,
        }
    }

    pub fn as_float(&self) -> Option<f64> {
        match self {
            Value::Int(i) => Some(*i as _),
            Value::Float(f) => Some(*f),
            _ => None,
        }
    }

    pub fn apply_unary(&self, o: &op::Unary) -> super::RtResult<Value> {
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
    
    pub fn apply_binary(&self, o: &op::Binary, right: &Self) -> super::RtResult<Value> {
        match o {
            op::Binary::Add => numeric_binary(self, o, right, 
                |a, b| Ok(Value::Float(a + b)), 
                |a, b| Ok(Value::Int(a + b))),
            
            op::Binary::Sub => numeric_binary(self, o, right, 
                |a, b| Ok(Value::Float(a - b)), 
                |a, b| Ok(Value::Int(a - b))),
        
            op::Binary::Mul => match (self, right) {
                // numeric multiplication
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
                (Value::Float(a), Value::Int(b))   => Ok(Value::Float(a * (*b as f64))),
                (Value::Int(a), Value::Float(b))   => Ok(Value::Float((*a as f64) * b)),
                (Value::Int(a), Value::Int(b))     => Ok(Value::Int(a * b)),
        
                // 2 * "a" == "aa"
                (Value::Str(a), Value::Int(b)) => Ok(Value::Str(a.repeat(*b as usize))),
                (Value::Int(a), Value::Str(b)) => Ok(Value::Str(b.repeat(*a as usize))),
                _ => Err(super::RuntimeErr::CannotApplyBinary(*o, self.ty(), right.ty()))
            },
            op::Binary::Div => numeric_binary(self, o, right, 
                |a, b| Ok(Value::Float(a / b)), // IEEE 754
                |a, b| Ok(Value::Float((a as f64) / (b as f64)))),
        
            op::Binary::Mod => numeric_binary(self, o, right, 
                |a, b| Ok(Value::Float(a % b)), 
                |a, b| a.checked_rem(b).map(Value::Int).ok_or(super::RuntimeErr::DivisionByZero)),
        
                op::Binary::BitOr  => int_only_op!(o => self | right),
                op::Binary::BitAnd => int_only_op!(o => self & right),
                op::Binary::BitXor => int_only_op!(o => self ^ right),
            
            op::Binary::LogAnd => Ok(Value::Bool(self.truth() && right.truth())),
            op::Binary::LogOr =>  Ok(Value::Bool(self.truth() || right.truth())),
        }
    }
    
    pub fn apply_cmp(&self, o: &op::Cmp, right: &Self) -> super::RtResult<bool> {
        match o {
            op::Cmp::Lt | op::Cmp::Gt | op::Cmp::Le | op::Cmp::Ge => {
                match (self, right) {
                    // integer/float cmp
                    (Value::Float(af), b) => float_cmp(*af, b, o),
                    (a, Value::Float(bf)) => float_cmp(a, *bf, o),
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
                    (Value::Float(af), b) => float_cmp(*af, b, o),
                    (a, Value::Float(bf)) => float_cmp(a, *bf, o),
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

impl TryFrom<&Value> for f64 {
    type Error = ();

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        value.as_float().ok_or(())
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