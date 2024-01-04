use miette::{IntoDiagnostic, Result};
use std::io::{self, Write};
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
        let location = chunk.location(offset)?;
        if offset > 0 && location.line == chunk.location(offset - 1)?.line {
            write!(line, "   . ").into_diagnostic()?;
        } else {
            write!(line, "{:4} ", location.line).into_diagnostic()?;
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

    let mut footer = out.buffer();
    color(&mut footer, Color::Yellow)?;
    writeln!(
        footer,
        "===={}====",
        std::iter::repeat('=').take(name.len()).collect::<String>()
    )
    .into_diagnostic()?;
    out.print(&footer).into_diagnostic()?;

    Ok(())
}

fn disassemble_instruction_at(chunk: &Chunk, offset: usize, line: &mut Buffer) -> Result<usize> {
    match (*chunk.byte(offset)?).try_into()? {
        Opcode::Print => render_simple_instruction("Print", offset, line),
        Opcode::Return => render_simple_instruction("Return", offset, line),
        Opcode::Pop => render_simple_instruction("Pop", offset, line),
        Opcode::GetGlobal => render_constant("Get Global", offset, chunk, line),
        Opcode::DefineGlobal => render_constant("Define Global", offset, chunk, line),
        Opcode::Equal => render_simple_instruction("Equal", offset, line),
        Opcode::Greater => render_simple_instruction("Greater", offset, line),
        Opcode::Less => render_simple_instruction("Less", offset, line),
        Opcode::Add => render_simple_instruction("Add", offset, line),
        Opcode::Subtract => render_simple_instruction("Subtract", offset, line),
        Opcode::Multiply => render_simple_instruction("Multiply", offset, line),
        Opcode::Divide => render_simple_instruction("Divide", offset, line),
        Opcode::Not => render_simple_instruction("Not", offset, line),
        Opcode::Negate => render_simple_instruction("Negate", offset, line),
        Opcode::Constant => render_constant("Constant", offset, chunk, line),
        Opcode::Nil => render_simple_instruction("Nil", offset, line),
        Opcode::True => render_simple_instruction("True", offset, line),
        Opcode::False => render_simple_instruction("False", offset, line),
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
        if stack.is_empty() {
            write!(self.line_buffer, "ø").unwrap();
        } else {
            for v in stack {
                write!(self.line_buffer, "[ {} ]", v.show()).unwrap();
            }
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
        io::stdout().flush().unwrap();
    }

    fn exit_instruction(&mut self) -> () {
        self.line_buffer = self.out.buffer();
    }
}
