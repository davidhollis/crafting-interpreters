use miette::{miette, IntoDiagnostic, Result};
use std::io::{self, Write};
use termcolor::{Buffer, BufferWriter, Color, ColorChoice, ColorSpec, WriteColor};

use crate::{
    chunk::{Chunk, Opcode},
    object::Object,
    value::Value,
    vm::tracing::Tracer,
};

pub fn disassemble_chunk(name: &str, chunk: &Chunk) -> Result<()> {
    disassemble_chunk_indent(name, chunk, "".to_string())
}

pub fn disassemble_chunk_indent(name: &str, chunk: &Chunk, indent: String) -> Result<()> {
    let mut offset = 0usize;
    let out = BufferWriter::stdout(ColorChoice::Auto);

    let mut header = out.buffer();
    color(&mut header, Color::Yellow)?;
    writeln!(header, "{}=== {} ===", indent, name).into_diagnostic()?;
    out.print(&header).into_diagnostic()?;

    let mut debug_symbols = chunk.debug_symbols.iter().peekable();

    while offset < chunk.len() {
        let mut line = out.buffer();

        // Write out the chunk offset and line number
        line.set_color(ColorSpec::new().set_fg(Some(Color::White)).set_dimmed(true))
            .into_diagnostic()?;
        write!(line, "{}{:04} ", indent, offset).into_diagnostic()?;
        let location = chunk.location(offset)?;
        if offset > 0 && location.line == chunk.location(offset - 1)?.line {
            write!(line, "   . ").into_diagnostic()?;
        } else {
            write!(line, "{:4} ", location.line).into_diagnostic()?;
        }

        // Write out the instruction
        let new_offset = disassemble_instruction_at(chunk, offset, &mut line, &indent);
        if let Ok(new_offset) = new_offset {
            let mut color_set = false;
            while let Some(debug_symbol) = debug_symbols.next_if(|sym| sym.offset < new_offset) {
                if !color_set {
                    color(&mut line, Color::Cyan)?;
                    color_set = true;
                }
                write!(line, " {}", debug_symbol.symbol.to_string()).into_diagnostic()?;
            }
            line.reset().into_diagnostic()?;
            writeln!(line, "").into_diagnostic()?;
            out.print(&line).into_diagnostic()?;
            offset = new_offset;
        } else {
            color(&mut line, Color::Red)?;
            writeln!(line, "\n! Error").into_diagnostic()?;
            out.print(&line).into_diagnostic()?;
            return new_offset.map(|_| ());
        }
    }

    io::stdout().flush().into_diagnostic()?;
    for const_id in 0..chunk.num_constants() {
        match chunk.constant_at(const_id as u8)? {
            Value::Object(Object::Function(inner_function)) => {
                let name = inner_function.debug_name();
                disassemble_chunk_indent(&name, &inner_function.chunk, indent.clone() + "    ")?;
            }
            _ => (),
        }
    }

    let mut footer = out.buffer();
    color(&mut footer, Color::Yellow)?;
    writeln!(
        footer,
        "{}===={}====",
        indent,
        std::iter::repeat('=').take(name.len()).collect::<String>()
    )
    .into_diagnostic()?;
    out.print(&footer).into_diagnostic()?;

    Ok(())
}

fn disassemble_instruction_at(
    chunk: &Chunk,
    offset: usize,
    line: &mut Buffer,
    indent: &str,
) -> Result<usize> {
    match (*chunk.byte(offset)?).try_into()? {
        Opcode::Print => render_simple_instruction("Print", offset, line),
        Opcode::ReplPrint => render_simple_instruction("REPL Print", offset, line),
        Opcode::Jump => render_jump_instruction("Jump", true, offset, chunk, line),
        Opcode::JumpIfFalse => render_jump_instruction("Jump If False", true, offset, chunk, line),
        Opcode::Loop => render_jump_instruction("Loop", false, offset, chunk, line),
        Opcode::Call => render_call_instruction("Call", offset, chunk, line),
        Opcode::Invoke => render_invoke_instruction("Invoke", offset, chunk, line),
        Opcode::InvokeSuper => render_invoke_instruction("Invoke Super", offset, chunk, line),
        Opcode::Closure => render_closure_instruction("Closure", offset, chunk, line, indent),
        Opcode::CloseUpvalue => render_simple_instruction("Close Upvalue", offset, line),
        Opcode::Return => render_simple_instruction("Return", offset, line),
        Opcode::Class => render_constant("Define Class", offset, chunk, line),
        Opcode::Inherit => render_simple_instruction("Inherit Methods", offset, line),
        Opcode::Method => render_constant("Define Method", offset, chunk, line),
        Opcode::Pop => render_simple_instruction("Pop", offset, line),
        Opcode::Swap => render_simple_instruction("Swap", offset, line),
        Opcode::GetLocal => render_local_var_instruction("Get Local", offset, chunk, line),
        Opcode::SetLocal => render_local_var_instruction("Set Local", offset, chunk, line),
        Opcode::GetGlobal => render_constant("Get Global", offset, chunk, line),
        Opcode::DefineGlobal => render_constant("Define Global", offset, chunk, line),
        Opcode::SetGlobal => render_constant("Set Global", offset, chunk, line),
        Opcode::GetUpvalue => render_local_var_instruction("Get Upvalue", offset, chunk, line),
        Opcode::SetUpvalue => render_local_var_instruction("Set Upvalue", offset, chunk, line),
        Opcode::GetProperty => render_constant("Get Property", offset, chunk, line),
        Opcode::SetProperty => render_constant("Set Property", offset, chunk, line),
        Opcode::GetSuper => render_constant("Get Super", offset, chunk, line),
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
    write!(line, "{}", name).into_diagnostic()?;
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
    write!(line, "  0x{:02x} ", const_idx).into_diagnostic()?;
    color(line, Color::Blue)?;
    write!(line, "({})", chunk.constant_at(const_idx)?.show()).into_diagnostic()?;

    Ok(offset + 2)
}

fn render_local_var_instruction(
    instr_name: &str,
    offset: usize,
    chunk: &Chunk,
    line: &mut Buffer,
) -> Result<usize> {
    color(line, Color::Green)?;
    write!(line, "{:-16}", instr_name).into_diagnostic()?;

    let stack_idx = *chunk.byte(offset + 1)?;
    color(line, Color::White)?;
    write!(line, "  0x{:02x}", stack_idx).into_diagnostic()?;

    Ok(offset + 2)
}

fn render_call_instruction(
    instr_name: &str,
    offset: usize,
    chunk: &Chunk,
    line: &mut Buffer,
) -> Result<usize> {
    color(line, Color::Green)?;
    write!(line, "{:-16}", instr_name).into_diagnostic()?;

    let arg_count = *chunk.byte(offset + 1)?;
    color(line, Color::White)?;
    write!(line, "  /{} ", arg_count).into_diagnostic()?;

    Ok(offset + 2)
}

fn render_invoke_instruction(
    instr_name: &str,
    offset: usize,
    chunk: &Chunk,
    line: &mut Buffer,
) -> Result<usize> {
    color(line, Color::Green)?;
    write!(line, "{:-16}", instr_name).into_diagnostic()?;

    let method_name_idx = *chunk.byte(offset + 1)?;
    let arg_count = *chunk.byte(offset + 2)?;
    color(line, Color::White)?;
    write!(line, "  0x{:02x} /{} ", method_name_idx, arg_count).into_diagnostic()?;
    color(line, Color::Blue)?;
    write!(line, "(.{})", chunk.constant_at(method_name_idx)?.print()).into_diagnostic()?;

    Ok(offset + 3)
}

fn render_jump_instruction(
    instr_name: &str,
    forward: bool,
    offset: usize,
    chunk: &Chunk,
    line: &mut Buffer,
) -> Result<usize> {
    let high_byte = *chunk.byte(offset + 1)? as usize;
    let low_byte = *chunk.byte(offset + 2)? as usize;
    let jump = (high_byte << 8) | low_byte;
    let destination = if forward {
        offset + 3 + jump
    } else {
        offset + 3 - jump
    };

    color(line, Color::Green)?;
    write!(line, "{:-16}", instr_name).into_diagnostic()?;

    color(line, Color::White)?;
    write!(line, " {:5} ", jump).into_diagnostic()?;
    color(line, Color::Blue)?;
    write!(line, "(-> {})", destination).into_diagnostic()?;

    Ok(offset + 3)
}

fn render_closure_instruction(
    instr_name: &str,
    offset: usize,
    chunk: &Chunk,
    line: &mut Buffer,
    indent: &str,
) -> Result<usize> {
    color(line, Color::Green)?;
    write!(line, "{:-16}", instr_name).into_diagnostic()?;

    let const_idx = *chunk.byte(offset + 1)?;
    color(line, Color::White)?;
    write!(line, "  0x{:02x} ", const_idx).into_diagnostic()?;
    color(line, Color::Blue)?;
    let function = chunk.constant_at(const_idx)?;
    write!(line, "({})", function.show()).into_diagnostic()?;

    if let Value::Object(Object::Function(function_data)) = function {
        let upvalue_count = function_data.upvalue_count;
        let mut offset = offset + 2;
        for _ in 0..upvalue_count {
            let is_local = chunk.byte(offset)?;
            let index = chunk.byte(offset + 1)?;

            line.set_color(ColorSpec::new().set_fg(Some(Color::White)).set_dimmed(true))
                .into_diagnostic()?;
            write!(line, "\n{}{:04}      ", indent, offset).into_diagnostic()?;
            color(line, Color::Green)?;
            write!(line, "|                      ").into_diagnostic()?;
            color(line, Color::Blue)?;
            write!(
                line,
                "{:-7} @{}",
                if *is_local > 0 { "local" } else { "upvalue" },
                index
            )
            .into_diagnostic()?;

            offset += 2;
        }

        Ok(offset)
    } else {
        Err(miette!(
            "First {} operand was not a function. This is a compiler bug.",
            instr_name
        ))
    }
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
            write!(self.line_buffer, "[]").unwrap();
        } else {
            for v in stack {
                if let Value::Object(Object::Function(function_data)) = v {
                    if function_data.debug_name() == "<toplevel>" {
                        write!(self.line_buffer, "[]").unwrap();
                        continue;
                    }
                }
                write!(self.line_buffer, "[ {} ]", v.show()).unwrap();
            }
        }

        // Write out the chunk offset and line number
        self.line_buffer
            .set_color(ColorSpec::new().set_fg(Some(Color::White)).set_dimmed(true))
            .unwrap();
        write!(self.line_buffer, "\n{:04}      ", offset).unwrap();
        let res = disassemble_instruction_at(chunk, offset, &mut self.line_buffer, "");
        if res.is_err() {
            color(&mut self.line_buffer, Color::Red).unwrap();
            writeln!(self.line_buffer, "\n! Error").unwrap();
        } else {
            writeln!(self.line_buffer, "").unwrap();
        }
        self.line_buffer.reset().unwrap();
        self.out.print(&self.line_buffer).unwrap();
        io::stdout().flush().unwrap();
    }

    fn exit_instruction(&mut self) -> () {
        self.line_buffer = self.out.buffer();
    }
}
