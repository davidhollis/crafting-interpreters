use miette::Result;
use salmon::{
    chunk::{Chunk, Opcode},
    value::Value,
};

fn main() -> Result<()> {
    let mut test_chunk = Chunk::new();

    let const_idx = test_chunk.add_constant(Value(1.2));
    test_chunk
        // Constant <idx>
        .write_byte(Opcode::Constant as u8, 123)
        .write_byte(const_idx, 123)
        // Return
        .write_byte(Opcode::Return as u8, 123);

    salmon::debug::disassemble_chunk("test_chunk", &test_chunk)?;

    Ok(())
}
