use std::{fmt::Debug, sync::Arc};

use crate::object::{Object, ObjectType};

#[derive(PartialEq, Eq)]
pub enum DataType {
    Number,
    Boolean,
    String,
    Nil,
}

#[derive(Clone)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Object(Arc<Object>),
    Nil,
}

impl Value {
    pub fn string(contents: &str) -> Value {
        Value::Object(Arc::new(Object::string(contents)))
    }

    pub fn wrap(object: Object) -> Value {
        Value::Object(Arc::new(object))
    }

    pub fn show(&self) -> String {
        match self {
            Value::Number(n) => format!("{}", n),
            Value::Boolean(true) => "true".to_string(),
            Value::Boolean(false) => "false".to_string(),
            Value::Object(obj) => obj.show(),
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

    pub fn is(&self, data_type: DataType) -> bool {
        match self {
            Value::Number(_) => data_type == DataType::Number,
            Value::Boolean(_) => data_type == DataType::Boolean,
            Value::Object(obj_ref) => match obj_ref.as_ref() {
                Object {
                    body: ObjectType::String { .. },
                    ..
                } => data_type == DataType::String,
            },
            Value::Nil => data_type == DataType::Nil,
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(n) => write!(f, "{} (of type number)", n),
            Self::Boolean(true) => write!(f, "true (of type boolean)"),
            Self::Boolean(false) => write!(f, "false (of type boolean)"),
            Self::Object(obj) => obj.fmt(f),
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
