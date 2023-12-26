use miette::{Diagnostic, Result, SourceOffset};
use thiserror::Error;

use crate::chunk::{Chunk, Opcode};

pub trait VMState {}

pub struct VM<S: VMState> {
    state: S,
}

pub fn new() -> VM<Initialized> {
    VM { state: Initialized }
}

pub struct Initialized;

impl VMState for Initialized {}

impl VM<Initialized> {
    pub fn interpret(self, chunk: &Chunk) -> VM<Stopped> {
        let running = VM {
            state: Running::new(chunk),
        };
        running.run()
    }

    pub fn trace<T: tracing::Tracer>(self, chunk: &Chunk, tracer: &mut T) -> VM<Stopped> {
        let running = VM {
            state: Running::new(chunk),
        };
        running.trace(tracer)
    }
}

pub struct Running<'a> {
    chunk: &'a Chunk,
    ip: usize,
}

impl<'a> Running<'a> {
    fn new(chunk: &'a Chunk) -> Running<'a> {
        Running { chunk, ip: 0 }
    }
}

impl<'a> VMState for Running<'a> {}

impl<'a> VM<Running<'a>> {
    fn run(mut self) -> VM<Stopped> {
        loop {
            match self.next_opcode() {
                Ok(op) => match self.execute_instruction(op) {
                    Ok(RuntimeAction::Continue) => continue,
                    Ok(RuntimeAction::Halt) => return self.halt(Ok(())),
                    Err(e) => return self.halt(Err(e)),
                },
                Err(e) => return self.halt(Err(e)),
            }
        }
    }

    fn trace<T: tracing::Tracer>(mut self, tracer: &mut T) -> VM<Stopped> {
        loop {
            tracer.enter_instruction(self.state.chunk, self.state.ip);
            match self.next_opcode() {
                Ok(op) => {
                    let execution_result = self.execute_instruction(op);
                    tracer.exit_instruction();
                    match execution_result {
                        Ok(RuntimeAction::Continue) => continue,
                        Ok(RuntimeAction::Halt) => return self.halt(Ok(())),
                        Err(e) => return self.halt(Err(e)),
                    }
                }
                Err(e) => return self.halt(Err(e)),
            }
        }
    }

    fn execute_instruction(&mut self, op: Opcode) -> Result<RuntimeAction> {
        match op {
            Opcode::Return => Ok(RuntimeAction::Halt),
            Opcode::Constant => {
                let const_idx = self.next_byte()?;
                let constant = self.state.chunk.constant_at(const_idx)?;
                println!("Constant: {}", constant.show());
                Ok(RuntimeAction::Continue)
            }
        }
    }

    fn next_byte(&mut self) -> Result<u8> {
        let current_byte = self.state.chunk.byte(self.state.ip)?;
        self.state.ip += 1;
        Ok(*current_byte)
    }

    fn next_opcode(&mut self) -> Result<Opcode> {
        let current_byte = self.state.chunk.byte(self.state.ip)?;
        self.state.ip += 1;
        (*current_byte).try_into()
    }

    fn halt(self, result: Result<()>) -> VM<Stopped> {
        VM {
            state: Stopped { result },
        }
    }
}

enum RuntimeAction {
    Continue,
    Halt,
}

pub struct Stopped {
    result: Result<()>,
}

impl VMState for Stopped {}

impl VM<Stopped> {
    pub fn result(self) -> Result<()> {
        self.state.result
    }

    // pub fn result_ref(&self) -> Result<&()> {
    //     self.state.result.as_ref()
    // }
}

#[derive(Error, Debug, Diagnostic)]
pub enum RuntimeError {
    #[error("{desc}")]
    #[diagnostic(code(runtime::general))]
    GeneralError {
        desc: String,
        #[label]
        location: Option<SourceOffset>,
        #[source]
        #[diagnostic_source]
        cause: Box<dyn Diagnostic>,
    },
}

pub mod tracing {
    use crate::chunk::Chunk;

    pub trait Tracer {
        fn enter_instruction(&mut self, chunk: &Chunk, offset: usize) -> ();
        fn exit_instruction(&mut self) -> ();
    }
}
