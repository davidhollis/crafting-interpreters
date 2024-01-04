use miette::{Diagnostic, ErrReport, Result};
use thiserror::Error;

use crate::{scanner::SourceLocation, table::Table, value::Value};

#[repr(u8)]
#[derive(Debug)]
pub enum Opcode {
    Constant,
    Nil,
    True,
    False,
    Equal,
    Greater,
    Less,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Negate,
    Return,
}

impl TryFrom<u8> for Opcode {
    type Error = ErrReport;

    fn try_from(byte: u8) -> Result<Self, Self::Error> {
        match byte {
            x if x == Opcode::Constant as u8 => Ok(Opcode::Constant),
            x if x == Opcode::Nil as u8 => Ok(Opcode::Nil),
            x if x == Opcode::True as u8 => Ok(Opcode::True),
            x if x == Opcode::False as u8 => Ok(Opcode::False),
            x if x == Opcode::Equal as u8 => Ok(Opcode::Equal),
            x if x == Opcode::Greater as u8 => Ok(Opcode::Greater),
            x if x == Opcode::Less as u8 => Ok(Opcode::Less),
            x if x == Opcode::Add as u8 => Ok(Opcode::Add),
            x if x == Opcode::Subtract as u8 => Ok(Opcode::Subtract),
            x if x == Opcode::Multiply as u8 => Ok(Opcode::Multiply),
            x if x == Opcode::Divide as u8 => Ok(Opcode::Divide),
            x if x == Opcode::Not as u8 => Ok(Opcode::Not),
            x if x == Opcode::Negate as u8 => Ok(Opcode::Negate),
            x if x == Opcode::Return as u8 => Ok(Opcode::Return),
            _ => Err(DecodeError::NoSuchInstruction(byte).into()),
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
