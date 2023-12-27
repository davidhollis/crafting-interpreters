use miette::Result;
use salmon::{
    chunk::{Chunk, Opcode},
    debug::DisassemblingTracer,
    value::Value,
    vm::{self},
};

fn main() -> Result<()> {
    let mut test_chunk = Chunk::new();

    let const1_idx = test_chunk.add_constant(Value(1.2));
    let const2_idx = test_chunk.add_constant(Value(3.4));
    let const3_idx = test_chunk.add_constant(Value(5.6));
    test_chunk
        // Constant <idx>
        .write_byte(Opcode::Constant as u8, 123)
        .write_byte(const1_idx, 123)
        // Constant <idx>
        .write_byte(Opcode::Constant as u8, 123)
        .write_byte(const2_idx, 123)
        // Add
        .write_byte(Opcode::Add as u8, 123)
        // Constant <idx>
        .write_byte(Opcode::Constant as u8, 123)
        .write_byte(const3_idx, 123)
        // Divide
        .write_byte(Opcode::Divide as u8, 123)
        // Negate
        .write_byte(Opcode::Negate as u8, 123)
        // Return
        .write_byte(Opcode::Return as u8, 123);

    let mut tracer = DisassemblingTracer::new();

    let vm = vm::new();
    let vm = vm.trace(&test_chunk, &mut tracer);
    vm.finish()
}
