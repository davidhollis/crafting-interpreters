use std::fmt::Debug;

#[derive(Clone)]
pub struct Object {
    pub body: ObjectType,
}

impl Object {
    pub fn string(contents: &str) -> Object {
        Object {
            body: ObjectType::String {
                contents: contents.into(),
            },
        }
    }

    pub fn show(&self) -> String {
        match &self.body {
            ObjectType::String { contents } => format!(r#""{}""#, contents),
        }
    }
}

impl Debug for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.body {
            ObjectType::String { contents } => write!(f, r#""{}" (of type string)"#, contents),
        }
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (&self.body, &other.body) {
            (
                ObjectType::String { contents: this_str },
                ObjectType::String {
                    contents: other_str,
                },
            ) => this_str == other_str,
        }
    }
}

#[derive(Clone)]
pub enum ObjectType {
    String { contents: Box<str> },
}

pub mod string {
    use super::{Object, ObjectType};

    pub fn concatenate(left: &Object, right: &Object) -> Object {
        match (&left.body, &right.body) {
            (
                ObjectType::String {
                    contents: left_contents,
                },
                ObjectType::String {
                    contents: right_contents,
                },
            ) => Object {
                body: ObjectType::String {
                    contents: left_contents
                        .chars()
                        .chain(right_contents.chars())
                        .collect::<String>()
                        .into(),
                },
            },
        }
    }
}
