use from_u8::FromU8;
use miette::{Diagnostic, ErrReport, Result};
use thiserror::Error;

use crate::{scanner::SourceLocation, table::Table, value::Value};

#[repr(u8)]
#[derive(Debug, FromU8)]
#[from_u8(err_type(ErrReport), err_constructor(DecodeError::NoSuchInstruction))]
pub enum Opcode {
    Constant,
    Nil,
    True,
    False,
    Pop,
    Swap,
    GetLocal,
    SetLocal,
    GetGlobal,
    DefineGlobal,
    SetGlobal,
    GetUpvalue,
    SetUpvalue,
    GetProperty,
    SetProperty,
    GetSuper,
    Equal,
    Greater,
    Less,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Negate,
    Print,
    Jump,
    JumpIfFalse,
    Loop,
    Call,
    Invoke,
    Closure,
    CloseUpvalue,
    Return,
    Class,
    Method,
    Inherit,
}

impl Opcode {
    pub fn op_symbol(&self) -> &'static str {
        match self {
            Opcode::Equal => "==",
            Opcode::Greater => ">",
            Opcode::Less => "<",
            Opcode::Add => "+",
            Opcode::Subtract => "-",
            Opcode::Multiply => "*",
            Opcode::Divide => "/",
            Opcode::Not => "!",
            Opcode::Negate => "-",
            _ => "(not an operator)",
        }
    }
}

#[derive(Error, Debug, Diagnostic)]
pub enum DecodeError {
    #[error("invalid opcode 0x{0:02x}")]
    NoSuchInstruction(u8),
}

pub struct Chunk {
    code: Vec<u8>,
    locations: Vec<SourceLocation>,
    constants: Vec<Value>,
    pub strings: Table,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: vec![],
            locations: vec![],
            constants: vec![],
            strings: Table::new(),
        }
    }

    pub fn write_byte(&mut self, byte: u8, location: SourceLocation) -> &mut Self {
        self.code.push(byte);
        self.locations.push(location);
        self
    }

    pub fn byte(&self, offset: usize) -> Result<&u8> {
        self.code
            .get(offset)
            .ok_or_else(|| ChunkError::InvalidOffset(offset).into())
    }

    pub fn patch(&mut self, offset: usize, new_value: u8) -> Result<()> {
        let patched_address = self
            .code
            .get_mut(offset)
            .ok_or_else(|| Into::<ErrReport>::into(ChunkError::InvalidOffset(offset)))?;
        *patched_address = new_value;
        Ok(())
    }

    pub fn location(&self, offset: usize) -> Result<&SourceLocation> {
        self.locations
            .get(offset)
            .ok_or_else(|| ChunkError::InvalidOffset(offset).into())
    }

    pub fn len(&self) -> usize {
        self.code.len()
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    pub fn constant_at(&self, index: u8) -> Result<&Value> {
        self.constants
            .get(Into::<usize>::into(index))
            .ok_or_else(|| ChunkError::NoSuchConstant(index).into())
    }

    pub fn num_constants(&self) -> usize {
        self.constants.len()
    }
}

#[derive(Error, Debug, Diagnostic)]
pub enum ChunkError {
    #[error("invalid code offset {0}")]
    InvalidOffset(usize),
    #[error("no constant found at index {0}")]
    NoSuchConstant(u8),
}
