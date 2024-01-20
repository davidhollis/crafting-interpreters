use miette::Result;
use std::{fmt::Debug, sync::Arc};

use crate::{chunk::Chunk, table::Table, value::Value};

#[derive(Clone)]
pub enum Object {
    String(Arc<StringData>),
    Function(Arc<FunctionData>),
    Native(Arc<NativeData>),
    Closure(Arc<ClosureData>),
    Upvalue(Arc<UpvalueData>),
}

impl Object {
    pub fn string(contents: &str) -> Object {
        Object::String(StringData::new(contents))
    }

    pub fn closure(function: &Arc<FunctionData>) -> Object {
        Object::Closure(ClosureData::new(Arc::clone(function)))
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
            Object::Native(data) => format!("<native fn {}>", data.name.to_string()),
            Object::Closure(data) => Object::Function(Arc::clone(&data.function)).show(),
            Object::Upvalue(data) => format!("<upvalue {:p}>", data.slot),
        }
    }

    pub fn print(&self) -> String {
        match self {
            Object::String(data) => data.contents.to_string(),
            Object::Function(_) | Object::Native(_) | Object::Upvalue(_) => self.show(),
            Object::Closure(data) => Object::Function(Arc::clone(&data.function)).show(),
        }
    }
}

impl Debug for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::String(data) => write!(f, r#"<string "{}">"#, data.contents),
            Object::Function(_) | Object::Native(_) | Object::Upvalue(_) => {
                write!(f, "{}", self.show())
            }
            Object::Closure(data) => {
                write!(f, "{}", Object::Function(Arc::clone(&data.function)).show())
            }
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
            (Object::Native(this_data), Object::Native(other_data)) => {
                Arc::ptr_eq(this_data, other_data)
            }
            (Object::Closure(this_data), Object::Closure(other_data)) => {
                Arc::ptr_eq(this_data, other_data)
            }
            (Object::Upvalue(this_data), Object::Upvalue(other_data)) => {
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
    pub upvalue_count: usize,
    pub chunk: Chunk,
    pub name: Option<Arc<StringData>>,
}

impl FunctionData {
    pub fn undefined() -> FunctionData {
        FunctionData {
            arity: 0,
            upvalue_count: 0,
            chunk: Chunk::new(),
            name: None,
        }
    }

    pub fn finalize(self) -> Arc<FunctionData> {
        Arc::new(self)
    }

    pub fn debug_name(&self) -> String {
        match &self.name {
            Some(name_data) => name_data.to_string(),
            None => "<anonymous function>".to_string(),
        }
    }
}

pub type NativeFn = fn(usize, &[Value]) -> Result<Value>;

pub struct NativeData {
    name: Arc<StringData>,
    pub function: NativeFn,
}

impl NativeData {
    pub fn new(name: Arc<StringData>, function: NativeFn) -> Arc<NativeData> {
        Arc::new(NativeData { name, function })
    }
}

pub struct ClosureData {
    pub function: Arc<FunctionData>,
    pub upvalues: Vec<Arc<UpvalueData>>,
}

impl ClosureData {
    pub fn new(function: Arc<FunctionData>) -> Arc<ClosureData> {
        let upvalue_count = function.upvalue_count;
        Arc::new(ClosureData {
            function,
            upvalues: Vec::with_capacity(upvalue_count),
        })
    }

    pub fn upvalue_count(&self) -> usize {
        self.upvalues.len()
    }
}

pub struct UpvalueData {
    // SAFETY: this pointer should only refer to a value on the stack or (eventually) another
    //         upvalue's storage. There is no public way to initialize an UpvalueData with
    //         an invalid pointer, and the VM will take care to close any existing UpvalueDatas
    //         when a referenced value would leave the stack.
    slot: *mut Value,
}

// SAFETY: This is mostly a lie. Open upvalues cannot be safely shared across thread boundaries,
//         but this can be mitigated by explicitly closing them.
unsafe impl Send for UpvalueData {}
unsafe impl Sync for UpvalueData {}

impl UpvalueData {
    pub fn capture(value: &Value) -> Arc<UpvalueData> {
        Arc::new(UpvalueData {
            slot: (value as *const _) as *mut _,
        })
    }

    pub fn shift(&mut self, new_slot: *mut Value) {
        self.slot = new_slot
    }

    pub fn read(&self) -> Value {
        unsafe { &*self.slot }.clone()
    }

    pub fn write(&self, new_value: Value) -> () {
        let old_value = unsafe { std::ptr::replace(self.slot, new_value) };
        drop(old_value);
    }
}
