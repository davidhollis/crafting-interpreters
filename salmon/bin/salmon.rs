use std::{fs, path::PathBuf};

use clap::Parser;
use miette::{IntoDiagnostic, NamedSource, Result};
use rustyline::error::ReadlineError;
use salmon::{
    compiler::{compile, compile_repl, ParseError},
    debug::{self, DisassemblingTracer},
    native,
    vm::{Stopped, VM},
};

const VERSION: &str = env!("CARGO_PKG_VERSION");
const CONTINUATION_PROMPT: &str = "           | ";

/// A lox interpreter in rust
#[derive(Parser)]
#[command(author, version, long_about = None)]
struct Salmon {
    /// A script file to run. If no script file is provided, enter the REPL.
    #[arg(value_name = "SCRIPT")]
    script_path: Option<PathBuf>,

    /// Trace execution, outputting VM state as the program runs.
    #[arg(long)]
    debug: bool,
}

fn main() -> Result<()> {
    let opts = Salmon::parse();

    match &opts.script_path {
        Some(path) => run_file(path, &opts),
        None => run_repl(&opts),
    }
}

fn run_file(path: &PathBuf, opts: &Salmon) -> Result<()> {
    let file_name = path
        .file_name()
        .and_then(|osstr| osstr.to_str())
        .unwrap_or("input file");
    let script_file = fs::read_to_string(path.clone()).into_diagnostic()?;
    let mut vm = salmon::vm::new();
    install_stdlib(&mut vm);
    let bytecode = compile(&script_file, &vm.strings)
        .map_err(|err| err.with_source_code(NamedSource::new(file_name, script_file.clone())))?;

    let vm = if opts.debug {
        debug::disassemble_chunk(file_name, &bytecode.chunk)?;
        vm.trace(bytecode, &mut debug::DisassemblingTracer::new())
    } else {
        vm.interpret(bytecode)
    };

    vm.finish()
        .map_err(|err| err.with_source_code(NamedSource::new(file_name, script_file)))
}

fn run_repl(opts: &Salmon) -> Result<()> {
    println!("Welcome to Salmon v{}.", VERSION);

    let mut rl = rustyline::DefaultEditor::new().into_diagnostic()?;
    let mut vm = salmon::vm::new();
    let mut cell_number: usize = 1;
    let mut tracer = DisassemblingTracer::new();
    let mut previous_source = String::new();
    let mut current_segment = String::new();

    install_stdlib(&mut vm);

    loop {
        let base_prompt = format!("salmon:{:04}> ", cell_number);
        match rl.readline(if current_segment.is_empty() {
            &base_prompt
        } else {
            CONTINUATION_PROMPT
        }) {
            Ok(line) => {
                if current_segment.is_empty() {
                    current_segment = line;
                } else {
                    current_segment.push('\n');
                    current_segment.push_str(&line);
                }
                let full_source = previous_source.clone() + &current_segment + "\n";
                match compile_repl(&full_source, previous_source.len(), &vm.strings) {
                    Ok(code) => {
                        // We compiled! Commit the full source code so that runtime errors point at
                        // the right offsets, and clear the current segment since previous_source
                        // now includes it.
                        previous_source = full_source;
                        let _ = rl.add_history_entry(&current_segment);
                        current_segment.clear();

                        // Execute the compiled code.
                        vm = if opts.debug {
                            debug::disassemble_chunk(
                                &format!("repl cell {}", cell_number),
                                &code.chunk,
                            )?;
                            vm.trace(code, &mut tracer)
                        } else {
                            vm.interpret(code)
                        };

                        // Report the error, if any.
                        let line_result;
                        (vm, line_result) = vm.extract_result();
                        if let Err(runtime_error) = line_result {
                            println!(
                                "{:?}",
                                runtime_error.with_source_code(previous_source.clone())
                            );
                        }

                        cell_number += 1;
                    }
                    Err(compile_error) => {
                        match compile_error.downcast_ref::<ParseError>() {
                            // If we hit a continuable error (generally, some sort of "unexpected EOF"),
                            // preserve current_segment, skip reporting the error, and keep reading
                            // another line.
                            Some(parse_error) if parse_error.can_continue() => {
                                if current_segment.ends_with("\n\n") {
                                    // Special case: if the user enters two blank lines, record a
                                    // history entry but discard the input without trying to
                                    // evaluate it.
                                    println!("I just saw two blank lines. Clearing this cell.");
                                    let _ = rl.add_history_entry(&current_segment);
                                    current_segment.clear();
                                }
                            }
                            _ => {
                                // If we hit a non-continuable error, report it and clear current_segment
                                current_segment.clear();
                                println!(
                                    "{:?}",
                                    compile_error.with_source_code(full_source.clone())
                                );
                            }
                        }
                    }
                }
            }
            Err(ReadlineError::Eof) => {
                println!("Bye!");
                return Ok(());
            }
            Err(e) => return Err(e).into_diagnostic(),
        }
    }
}

fn install_stdlib(vm: &mut VM<Stopped>) -> () {
    vm.register_native("clock", native::clock);
    vm.register_native("debug_closure", native::debug_closure);
    vm.register_native("debug_object", native::debug_object);
}
