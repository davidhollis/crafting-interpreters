use miette::Diagnostic;
use thiserror::Error;

#[repr(u8)]
#[derive(Debug)]
pub enum Opcode {
    Return = 0,
}

impl TryFrom<u8> for Opcode {
    type Error = DecodeError;

    fn try_from(byte: u8) -> Result<Self, Self::Error> {
        match byte {
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

pub type Chunk<'a> = &'a [u8];
