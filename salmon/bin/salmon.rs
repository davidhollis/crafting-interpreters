use std::{fs, path::PathBuf};

use clap::Parser;
use miette::{IntoDiagnostic, NamedSource, Result};
use rustyline::error::ReadlineError;
use salmon::{
    compiler::{compile, compile_repl},
    debug::{self, DisassemblingTracer},
};

const VERSION: &str = env!("CARGO_PKG_VERSION");

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
    let bytecode = compile(&script_file)
        .map_err(|err| err.with_source_code(NamedSource::new(file_name, script_file.clone())))?;
    let vm = salmon::vm::new();

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
    let mut line_number: usize = 1;
    let mut tracer = DisassemblingTracer::new();
    let mut previous_source = String::new();

    loop {
        match rl.readline(&format!("salmon:{:04}> ", line_number)) {
            Ok(line) => {
                let _ = rl.add_history_entry(&line);
                let full_source = previous_source.clone() + &line + "\n";
                match compile_repl(
                    &full_source,
                    line_number,
                    previous_source.len(),
                    &vm.strings,
                ) {
                    Ok(code) => {
                        previous_source = full_source;
                        line_number += 1;
                        vm = if opts.debug {
                            debug::disassemble_chunk(
                                &format!("repl line {}", line_number),
                                &code.chunk,
                            )?;
                            vm.trace(code, &mut tracer)
                        } else {
                            vm.interpret(code)
                        };
                        let line_result;
                        (vm, line_result) = vm.extract_result();
                        if let Err(runtime_error) = line_result {
                            println!(
                                "{:?}",
                                runtime_error.with_source_code(previous_source.clone())
                            );
                        }
                    }
                    Err(compile_error) => {
                        // TODO(hollis): If we got some kind of unexpected EOF, do some kind of continuation
                        //     Maybe storing the current offset into the "full code", and if the current
                        //     offset is less than previous_source.len(), it's a continuation with a different
                        //     prompt
                        println!("{:?}", compile_error.with_source_code(full_source.clone()))
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
