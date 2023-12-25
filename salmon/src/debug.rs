use miette::{IntoDiagnostic, Result};
use std::io::Write;
use termcolor::{Buffer, BufferWriter, Color, ColorChoice, ColorSpec, WriteColor};

use crate::chunk::{Chunk, Opcode};

pub fn disassemble_chunk(name: &str, chunk: Chunk) -> Result<()> {
    let mut offset = 0usize;
    let out = BufferWriter::stdout(ColorChoice::Auto);

    let mut header = out.buffer();
    color(&mut header, Color::Yellow)?;
    writeln!(header, "=== {} ===", name).into_diagnostic()?;
    out.print(&header).into_diagnostic()?;

    while offset < chunk.len() {
        let mut line = out.buffer();
        line.set_color(ColorSpec::new().set_fg(Some(Color::White)).set_dimmed(true))
            .into_diagnostic()?;
        write!(line, "{:04} : ", offset).into_diagnostic()?;
        let new_offset = disassemble_instruction_at(chunk, offset, &mut line);
        if new_offset.is_err() {
            color(&mut line, Color::Red)?;
            writeln!(line, "! Error").into_diagnostic()?;
        }
        out.print(&line).into_diagnostic()?;
        offset = new_offset?;
    }

    Ok(())
}

fn disassemble_instruction_at(chunk: Chunk, offset: usize, line: &mut Buffer) -> Result<usize> {
    match chunk[offset].try_into()? {
        Opcode::Return => render_simple_instruction("Return", offset, line),
    }
}

fn render_simple_instruction(name: &str, offset: usize, line: &mut Buffer) -> Result<usize> {
    color(line, Color::Green)?;
    writeln!(line, "{}", name).into_diagnostic()?;
    Ok(offset + 1)
}

fn color(buf: &mut Buffer, color: Color) -> Result<()> {
    buf.set_color(ColorSpec::new().set_fg(Some(color)))
        .into_diagnostic()
}
