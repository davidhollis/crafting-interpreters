use std::fmt::Debug;

#[derive(Clone, Copy)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Nil,
}

impl Value {
    pub fn show(&self) -> String {
        match self {
            Value::Number(n) => format!("{}", n),
            Value::Boolean(true) => "true".to_string(),
            Value::Boolean(false) => "false".to_string(),
            Value::Nil => "nil".to_string(),
        }
    }

    pub fn is_falsy(&self) -> bool {
        match self {
            // false and nil are falsy
            Value::Boolean(false) | Value::Nil => true,
            // any other value is truthy
            _ => false,
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(n) => write!(f, "{} (of type number)", n),
            Self::Boolean(true) => write!(f, "true (of type boolean)"),
            Self::Boolean(false) => write!(f, "false (of type boolean)"),
            Self::Nil => write!(f, "nil"),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::Boolean(l0), Self::Boolean(r0)) => l0 == r0,
            (Self::Nil, Self::Nil) => true,
            _ => false,
        }
    }
}
