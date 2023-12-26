use miette::{IntoDiagnostic, Result};
use std::io::Write;
use termcolor::{Buffer, BufferWriter, Color, ColorChoice, ColorSpec, WriteColor};

use crate::{
    chunk::{Chunk, Opcode},
    value::Value,
    vm::tracing::Tracer,
};

pub fn disassemble_chunk(name: &str, chunk: &Chunk) -> Result<()> {
    let mut offset = 0usize;
    let out = BufferWriter::stdout(ColorChoice::Auto);

    let mut header = out.buffer();
    color(&mut header, Color::Yellow)?;
    writeln!(header, "=== {} ===", name).into_diagnostic()?;
    out.print(&header).into_diagnostic()?;

    while offset < chunk.len() {
        let mut line = out.buffer();

        // Write out the chunk offset and line number
        line.set_color(ColorSpec::new().set_fg(Some(Color::White)).set_dimmed(true))
            .into_diagnostic()?;
        write!(line, "{:04} ", offset).into_diagnostic()?;
        let line_number = chunk.location(offset)?;
        if offset > 0 && line_number == chunk.location(offset - 1)? {
            write!(line, "   . ").into_diagnostic()?;
        } else {
            write!(line, "{:4} ", line_number).into_diagnostic()?;
        }

        // Write out the instruction
        let new_offset = disassemble_instruction_at(chunk, offset, &mut line);
        if new_offset.is_err() {
            color(&mut line, Color::Red)?;
            writeln!(line, "! Error").into_diagnostic()?;
        }
        line.reset().into_diagnostic()?;
        out.print(&line).into_diagnostic()?;

        // Advance the pointer
        offset = new_offset?;
    }

    Ok(())
}

fn disassemble_instruction_at(chunk: &Chunk, offset: usize, line: &mut Buffer) -> Result<usize> {
    match (*chunk.byte(offset)?).try_into()? {
        Opcode::Return => render_simple_instruction("Return", offset, line),
        Opcode::Add => render_simple_instruction("Add", offset, line),
        Opcode::Subtract => render_simple_instruction("Subtract", offset, line),
        Opcode::Multiply => render_simple_instruction("Multiply", offset, line),
        Opcode::Divide => render_simple_instruction("Divide", offset, line),
        Opcode::Negate => render_simple_instruction("Negate", offset, line),
        Opcode::Constant => render_constant("Constant", offset, chunk, line),
    }
}

fn render_simple_instruction(name: &str, offset: usize, line: &mut Buffer) -> Result<usize> {
    color(line, Color::Green)?;
    writeln!(line, "{}", name).into_diagnostic()?;
    Ok(offset + 1)
}

fn render_constant(
    instr_name: &str,
    offset: usize,
    chunk: &Chunk,
    line: &mut Buffer,
) -> Result<usize> {
    color(line, Color::Green)?;
    write!(line, "{:-16}", instr_name).into_diagnostic()?;

    let const_idx = *chunk.byte(offset + 1)?;
    color(line, Color::White)?;
    write!(line, " 0x{:02x} ", const_idx).into_diagnostic()?;
    color(line, Color::Blue)?;
    writeln!(line, "({})", chunk.constant_at(const_idx)?.show()).into_diagnostic()?;

    Ok(offset + 2)
}

fn color(buf: &mut Buffer, color: Color) -> Result<()> {
    buf.set_color(ColorSpec::new().set_fg(Some(color)))
        .into_diagnostic()
}

pub struct DisassemblingTracer {
    out: BufferWriter,
    line_buffer: Buffer,
}

impl DisassemblingTracer {
    pub fn new() -> DisassemblingTracer {
        let out = BufferWriter::stdout(ColorChoice::Auto);
        let line_buffer = out.buffer();
        DisassemblingTracer { out, line_buffer }
    }
}

impl Tracer for DisassemblingTracer {
    fn enter_instruction(&mut self, chunk: &Chunk, offset: usize, stack: Vec<&Value>) -> () {
        color(&mut self.line_buffer, Color::Magenta).unwrap();
        for v in stack {
            write!(self.line_buffer, "[ {} ]", v.show()).unwrap();
        }
        self.line_buffer.reset().unwrap();
        writeln!(self.line_buffer, "").unwrap();
        let res = disassemble_instruction_at(chunk, offset, &mut self.line_buffer);
        if res.is_err() {
            color(&mut self.line_buffer, Color::Red).unwrap();
            writeln!(self.line_buffer, "! Error").unwrap();
        }
        self.line_buffer.reset().unwrap();
        self.out.print(&self.line_buffer).unwrap();
    }

    fn exit_instruction(&mut self) -> () {
        self.line_buffer = self.out.buffer();
    }
}
