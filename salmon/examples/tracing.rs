use miette::Result;
use salmon::{
    chunk::Opcode,
    debug::DisassemblingTracer,
    object::FunctionData,
    scanner::SourceLocation,
    value::Value,
    vm::{self},
};

fn main() -> Result<()> {
    let mut test_function = FunctionData::undefined();
    let test_chunk = &mut test_function.chunk;

    let const1_idx = test_chunk.add_constant(Value::Number(1.2));
    let const2_idx = test_chunk.add_constant(Value::Number(3.4));
    let const3_idx = test_chunk.add_constant(Value::Number(5.6));
    test_chunk
        // Constant <idx>
        .write_byte(
            Opcode::Constant as u8,
            SourceLocation {
                span: (3, 3),
                line: 1,
            },
        )
        .write_byte(
            const1_idx as u8,
            SourceLocation {
                span: (3, 3),
                line: 1,
            },
        )
        // Constant <idx>
        .write_byte(
            Opcode::Constant as u8,
            SourceLocation {
                span: (9, 3),
                line: 1,
            },
        )
        .write_byte(
            const2_idx as u8,
            SourceLocation {
                span: (9, 3),
                line: 1,
            },
        )
        // Add
        .write_byte(
            Opcode::Add as u8,
            SourceLocation {
                span: (7, 1),
                line: 1,
            },
        )
        // Constant <idx>
        .write_byte(
            Opcode::Constant as u8,
            SourceLocation {
                span: (16, 3),
                line: 1,
            },
        )
        .write_byte(
            const3_idx as u8,
            SourceLocation {
                span: (16, 3),
                line: 1,
            },
        )
        // Divide
        .write_byte(
            Opcode::Divide as u8,
            SourceLocation {
                span: (14, 1),
                line: 1,
            },
        )
        // Negate
        .write_byte(
            Opcode::Negate as u8,
            SourceLocation {
                span: (0, 1),
                line: 1,
            },
        )
        // Return
        .write_byte(
            Opcode::Return as u8,
            SourceLocation {
                span: (20, 0),
                line: 2,
            },
        );

    let mut tracer = DisassemblingTracer::new();

    let vm = vm::new();
    let vm = vm.trace(test_function.finalize(), &mut tracer);
    vm.finish()
}
