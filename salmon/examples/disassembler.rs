use miette::Result;
use salmon::{
    chunk::{Chunk, Opcode},
    scanner::SourceLocation,
    value::Value,
};

fn main() -> Result<()> {
    let mut test_chunk = Chunk::new();

    let const_idx = test_chunk.add_constant(Value::Number(1.2));
    test_chunk
        // Constant <idx>
        .write_byte(
            Opcode::Constant as u8,
            SourceLocation {
                span: (0, 3),
                line: 1,
            },
        )
        .write_byte(
            const_idx as u8,
            SourceLocation {
                span: (0, 3),
                line: 1,
            },
        )
        // Return
        .write_byte(
            Opcode::Return as u8,
            SourceLocation {
                span: (0, 0),
                line: 2,
            },
        );

    salmon::debug::disassemble_chunk("test_chunk", &test_chunk)?;

    Ok(())
}
