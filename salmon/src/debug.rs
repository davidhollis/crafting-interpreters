use miette::{IntoDiagnostic, Result};
use std::io::Write;
use termcolor::{Buffer, BufferWriter, Color, ColorChoice, ColorSpec, WriteColor};

use crate::chunk::{Chunk, Opcode};

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
        let line_number = chunk.line(offset)?;
        if offset > 0 && line_number == chunk.line(offset - 1)? {
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
        out.print(&line).into_diagnostic()?;

        // Advance the pointer
        offset = new_offset?;
    }

    Ok(())
}

fn disassemble_instruction_at(chunk: &Chunk, offset: usize, line: &mut Buffer) -> Result<usize> {
    match (*chunk.byte(offset)?).try_into()? {
        Opcode::Return => render_simple_instruction("Return", offset, line),
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
