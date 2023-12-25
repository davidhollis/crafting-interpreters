use miette::Result;
use salmon::chunk::Opcode;

fn main() -> Result<()> {
    let test_chunk = vec![Opcode::Return as u8];

    salmon::debug::disassemble_chunk("test_chunk", &test_chunk)?;

    Ok(())
}
