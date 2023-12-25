use miette::{Diagnostic, Result};
use thiserror::Error;

use crate::value::Value;

#[repr(u8)]
#[derive(Debug)]
pub enum Opcode {
    Constant,
    Return,
}

impl TryFrom<u8> for Opcode {
    type Error = DecodeError;

    fn try_from(byte: u8) -> Result<Self, Self::Error> {
        match byte {
            x if x == Opcode::Constant as u8 => Ok(Opcode::Constant),
            x if x == Opcode::Return as u8 => Ok(Opcode::Return),
            _ => Err(DecodeError::NoSuchInstruction(byte)),
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
    lines: Vec<usize>,
    constants: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: vec![],
            lines: vec![],
            constants: vec![],
        }
    }

    pub fn write_byte(&mut self, byte: u8, line: usize) -> &mut Self {
        self.code.push(byte);
        self.lines.push(line);
        self
    }

    pub fn byte(&self, offset: usize) -> Result<&u8> {
        self.code
            .get(offset)
            .ok_or(ChunkError::InvalidOffset(offset).into())
    }

    pub fn line(&self, offset: usize) -> Result<&usize> {
        self.lines
            .get(offset)
            .ok_or(ChunkError::InvalidOffset(offset).into())
    }

    pub fn len(&self) -> usize {
        self.code.len()
    }

    pub fn add_constant(&mut self, value: Value) -> u8 {
        assert!(
            self.constants.len() < (u8::MAX - 1).into(),
            "Cannot have more than 255 constants in a single chunk"
        );
        self.constants.push(value);
        self.constants.len() as u8 - 1
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
