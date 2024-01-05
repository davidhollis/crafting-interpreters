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
    GetGlobal,
    DefineGlobal,
    SetGlobal,
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
    Return,
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

    pub fn new_with_strings(strings: &Table) -> Chunk {
        let mut new_table = Table::new();
        new_table.add_all(strings);
        Chunk {
            code: vec![],
            locations: vec![],
            constants: vec![],
            strings: new_table,
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
            .ok_or(ChunkError::InvalidOffset(offset).into())
    }

    pub fn location(&self, offset: usize) -> Result<&SourceLocation> {
        self.locations
            .get(offset)
            .ok_or(ChunkError::InvalidOffset(offset).into())
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
            .ok_or(ChunkError::NoSuchConstant(index).into())
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
