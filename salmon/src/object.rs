use miette::Result;
use std::{
    fmt::Debug,
    ops::{Deref, DerefMut},
    sync::{Arc, RwLock},
};

use crate::{chunk::Chunk, table::Table, value::Value};

#[derive(Clone)]
pub enum Object {
    String(Arc<StringData>),
    Function(Arc<FunctionData>),
    Native(Arc<NativeData>),
    Closure(Arc<ClosureData>),
    Upvalue(Arc<UpvalueData>),
    Class(Arc<ClassData>),
    Instance(Arc<InstanceData>),
    BoundMethod(Arc<BoundMethodData>),
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
            Object::Upvalue(data) => data.show_reference(),
            Object::Class(data) => format!("<class {}>", data.name.to_string()),
            Object::Instance(data) => {
                format!("#<{}:{:p}>", data.class.name.to_string(), Arc::as_ptr(data))
            }
            Object::BoundMethod(data) => format!(
                "<bound method {:?}.{}/{}>",
                data.receiver,
                data.method.function.debug_name(),
                data.method.function.arity
            ),
        }
    }

    pub fn print(&self) -> String {
        match self {
            Object::String(data) => data.contents.to_string(),
            Object::Function(_)
            | Object::Native(_)
            | Object::Upvalue(_)
            | Object::Class(_)
            | Object::Instance(_)
            | Object::BoundMethod(_) => self.show(),
            Object::Closure(data) => Object::Function(Arc::clone(&data.function)).show(),
        }
    }
}

impl Debug for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::String(data) => write!(f, r#"<string "{}">"#, data.contents),
            Object::Function(_)
            | Object::Native(_)
            | Object::Upvalue(_)
            | Object::Class(_)
            | Object::Instance(_)
            | Object::BoundMethod(_) => {
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
            (Object::Class(this_data), Object::Class(other_data)) => {
                Arc::ptr_eq(this_data, other_data)
            }
            (Object::Instance(this_data), Object::Instance(other_data)) => {
                Arc::ptr_eq(this_data, other_data)
            }
            (Object::BoundMethod(this_data), Object::BoundMethod(other_data)) => {
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
    state: RwLock<UpvalueReference>,
    pub slot: usize,
}

impl PartialEq for UpvalueData {
    fn eq(&self, other: &Self) -> bool {
        if self.slot == other.slot {
            if let (Ok(this_state), Ok(other_state)) = (self.state.read(), other.state.read()) {
                match (this_state.deref(), other_state.deref()) {
                    (UpvalueReference::Open, UpvalueReference::Open) => true,
                    (UpvalueReference::Closed(_), UpvalueReference::Closed(_)) => true,
                    _ => false,
                }
            } else {
                false
            }
        } else {
            false
        }
    }
}

impl Eq for UpvalueData {}

impl PartialOrd for UpvalueData {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.slot.partial_cmp(&other.slot)
    }
}

impl Ord for UpvalueData {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.slot.cmp(&other.slot)
    }
}

enum UpvalueReference {
    Open,
    Closed(Value),
}

impl UpvalueData {
    pub fn capture(slot: usize) -> Arc<UpvalueData> {
        Arc::new(UpvalueData {
            state: RwLock::new(UpvalueReference::Open),
            slot,
        })
    }

    pub fn points_to_slot(&self, slot: usize) -> bool {
        if let Ok(state) = self.state.read() {
            match state.deref() {
                UpvalueReference::Open => self.slot == slot,
                UpvalueReference::Closed(_) => false,
            }
        } else {
            false
        }
    }

    pub fn read(&self, stack: &[Value]) -> Value {
        if let Ok(state) = self.state.read() {
            match state.deref() {
                UpvalueReference::Open => stack[self.slot].clone(),
                UpvalueReference::Closed(value) => value.clone(),
            }
        } else {
            Value::Nil
        }
    }

    pub fn write(&self, stack: &mut [Value], new_value: Value) -> () {
        let mut state = self.state.write().unwrap();
        match state.deref_mut() {
            UpvalueReference::Open => stack[self.slot] = new_value,
            UpvalueReference::Closed(ref mut value) => *value = new_value,
        }
    }

    pub fn close(&self, stack: &[Value]) -> () {
        let mut state = self.state.write().unwrap();
        match state.deref_mut() {
            UpvalueReference::Open => {
                let closed_value = stack[self.slot].clone();
                *state = UpvalueReference::Closed(closed_value);
            }
            UpvalueReference::Closed(_) => panic!("Attempted to close an already closed upvalue"),
        }
    }

    pub fn show_reference(&self) -> String {
        if let Ok(state) = self.state.read() {
            match state.deref() {
                UpvalueReference::Open => format!("<open upvalue @{}>", self.slot),
                UpvalueReference::Closed(value) => format!("<closed upvalue: {:?}>", value),
            }
        } else {
            "<upvalue ???>".to_string()
        }
    }
}

pub struct ClassData {
    pub name: Arc<StringData>,
    methods: RwLock<Table>,
}

impl ClassData {
    pub fn new(name: &Arc<StringData>) -> Arc<ClassData> {
        Arc::new(ClassData {
            name: Arc::clone(name),
            methods: RwLock::new(Table::new()),
        })
    }

    pub fn add_method(&self, method_name: Arc<StringData>, method_impl: Value) -> () {
        let mut methods = self.methods.write().unwrap();
        methods.set(method_name, method_impl);
    }

    pub fn lookup_method(&self, method_name: &Arc<StringData>) -> Option<Arc<ClosureData>> {
        if let Ok(methods) = self.methods.read() {
            if let Some(Value::Object(Object::Closure(method_impl))) = methods.get(method_name) {
                Some(Arc::clone(&method_impl))
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn method_names(&self) -> Vec<Arc<StringData>> {
        let methods = self.methods.read().unwrap();
        methods.keys()
    }

    pub fn inherit_from(&self, other: &Arc<ClassData>) -> () {
        let mut this_methods = self.methods.write().unwrap();
        let other_methods = other.methods.read().unwrap();

        this_methods.add_all(&other_methods);
    }
}

pub struct InstanceData {
    pub class: Arc<ClassData>,
    fields: RwLock<Table>,
}

impl InstanceData {
    pub fn of_class(class: &Arc<ClassData>) -> Arc<InstanceData> {
        Arc::new(InstanceData {
            class: Arc::clone(class),
            fields: RwLock::new(Table::new()),
        })
    }

    pub fn get_field(&self, name: &Arc<StringData>) -> Option<Value> {
        if let Ok(fields) = self.fields.read() {
            fields.get(name)
        } else {
            None
        }
    }

    pub fn set_field(&self, name: Arc<StringData>, value: Value) -> () {
        let mut fields = self.fields.write().unwrap();
        fields.set(name, value);
    }

    pub fn field_names(&self) -> Vec<Arc<StringData>> {
        let fields = self.fields.read().unwrap();
        fields.keys()
    }
}

pub struct BoundMethodData {
    pub receiver: Value,
    pub method: Arc<ClosureData>,
}

impl BoundMethodData {
    pub fn bind(receiver: Value, method: Arc<ClosureData>) -> Arc<BoundMethodData> {
        Arc::new(BoundMethodData { receiver, method })
    }
}
