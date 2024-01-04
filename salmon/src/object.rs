use std::fmt::Debug;

#[derive(Clone)]
pub struct Object {
    pub body: ObjectType,
    pub hash: u32,
}

impl Object {
    pub fn string(contents: &str) -> Object {
        let hash = string::hash(contents);
        Object {
            body: ObjectType::String {
                contents: contents.into(),
            },
            hash,
        }
    }

    pub fn show(&self) -> String {
        match &self.body {
            ObjectType::String { contents } => format!(r#""{}""#, contents),
        }
    }

    pub fn print(&self) -> String {
        match &self.body {
            ObjectType::String { contents } => contents.to_string(),
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
    use std::sync::Arc;

    use crate::table::Table;

    use super::{Object, ObjectType};

    pub fn concatenate(left: &Object, right: &Object, strings: &mut Table) -> Arc<Object> {
        match (&left.body, &right.body) {
            (
                ObjectType::String {
                    contents: left_contents,
                },
                ObjectType::String {
                    contents: right_contents,
                },
            ) => {
                let contents = left_contents
                    .chars()
                    .chain(right_contents.chars())
                    .collect::<String>();
                strings.intern_string(&contents)
            }
        }
    }

    // FNV-1a
    pub fn hash(key: &str) -> u32 {
        let mut hash = 2166136261u32;
        for b in key.bytes() {
            hash ^= b as u32;
            hash = hash.wrapping_mul(16777619u32);
        }
        hash
    }
}
