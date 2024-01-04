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

    println!("Welcome to Salmon v{}.", VERSION);

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
        debug::disassemble_chunk(file_name, &bytecode)?;
        vm.trace(&bytecode, &mut debug::DisassemblingTracer::new())
    } else {
        vm.interpret(&bytecode)
    };

    vm.finish()
        .map_err(|err| err.with_source_code(NamedSource::new(file_name, script_file)))
}

fn run_repl(opts: &Salmon) -> Result<()> {
    let mut rl = rustyline::DefaultEditor::new().into_diagnostic()?;
    let mut vm = salmon::vm::new();
    let mut line_number: usize = 1;
    let mut tracer = DisassemblingTracer::new();

    loop {
        match rl.readline(&format!("salmon:{:04}> ", line_number)) {
            Ok(line) => {
                let _ = rl.add_history_entry(&line);
                match compile_repl(&line, line_number, &vm.strings) {
                    Ok(code) => {
                        vm = if opts.debug {
                            debug::disassemble_chunk(&format!("repl line {}", line_number), &code)?;
                            vm.trace(&code, &mut tracer)
                        } else {
                            vm.interpret(&code)
                        };
                        let line_result;
                        (vm, line_result) = vm.extract_result();
                        if let Err(runtime_error) = line_result {
                            println!("{:?}", runtime_error.with_source_code(line.clone()));
                        }
                    }
                    Err(compile_error) => {
                        println!("{:?}", compile_error.with_source_code(line.clone()))
                    }
                }
            }
            Err(ReadlineError::Eof) => {
                println!("Bye!");
                return Ok(());
            }
            Err(e) => return Err(e).into_diagnostic(),
        }

        line_number += 1;
    }
}
