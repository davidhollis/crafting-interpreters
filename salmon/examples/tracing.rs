use miette::Result;
use salmon::{
    chunk::{Chunk, Opcode},
    debug::DisassemblingTracer,
    value::Value,
    vm::{self},
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

    let mut tracer = DisassemblingTracer::new();

    let vm = vm::new();
    let vm = vm.trace(&test_chunk, &mut tracer);
    vm.result()
}
