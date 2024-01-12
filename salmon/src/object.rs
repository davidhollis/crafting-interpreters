use std::{fmt::Debug, sync::Arc};

use crate::{chunk::Chunk, table::Table};

#[derive(Clone)]
pub enum Object {
    String(Arc<StringData>),
    Function(Arc<FunctionData>),
}

impl Object {
    pub fn string(contents: &str) -> Object {
        Object::String(StringData::new(contents))
    }

    pub fn show(&self) -> String {
        match self {
            Object::String(data) => format!(r#""{}""#, data.contents),
            Object::Function(data) => match data.as_ref() {
                FunctionData {
                    arity,
                    name: Some(name),
                    ..
                } => format!("<fn {}/{}>", name.contents, arity),
                FunctionData {
                    arity, name: None, ..
                } => format!("<anonymous fn/{}>", arity),
            },
        }
    }

    pub fn print(&self) -> String {
        match self {
            Object::String(data) => data.contents.to_string(),
            Object::Function(_) => self.show(),
        }
    }
}

impl Debug for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::String(data) => write!(f, r#""{}" (of type string)"#, data.contents),
            Object::Function(_) => write!(f, "{}", self.show()),
        }
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Object::String(this_data), Object::String(other_data)) => {
                Arc::ptr_eq(this_data, other_data)
            }
            (Object::Function(this_data), Object::Function(other_data)) => {
                Arc::ptr_eq(this_data, other_data)
            }
            _ => false,
        }
    }
}

pub struct StringData {
    contents: Box<str>,
    pub hash: u32,
}

impl StringData {
    pub fn new(contents: &str) -> Arc<StringData> {
        let hash = Self::hash(contents);
        Arc::new(StringData {
            contents: contents.into(),
            hash,
        })
    }

    pub fn prehashed(contents: &str, hash: u32) -> Arc<StringData> {
        Arc::new(StringData {
            contents: contents.into(),
            hash,
        })
    }

    pub fn concatenate(&self, right: &StringData, strings: &mut Table) -> Arc<StringData> {
        strings.intern_string(
            &self
                .contents
                .chars()
                .chain(right.contents.chars())
                .collect::<String>(),
        )
    }

    pub fn to_string(&self) -> String {
        self.contents.to_string()
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

    pub fn equals_interned_string(&self, other_hash: u32, other_contents: &str) -> bool {
        self.hash == other_hash && self.contents.as_ref() == other_contents
    }
}

pub struct FunctionData {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: Option<Arc<StringData>>,
}

impl FunctionData {
    pub fn undefined() -> FunctionData {
        FunctionData {
            arity: 0,
            chunk: Chunk::new(),
            name: None,
        }
    }

    pub fn finalize(self) -> Arc<FunctionData> {
        Arc::new(self)
    }
}
