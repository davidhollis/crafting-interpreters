use miette::{Diagnostic, Result, SourceOffset};
use thiserror::Error;

use crate::{
    chunk::{Chunk, Opcode},
    value::Value,
};

const STACK_SIZE: usize = 255;

pub trait VMState {}

pub struct VM<S: VMState> {
    state: S,
}

pub fn new() -> VM<Stopped> {
    VM {
        state: Stopped { result: Ok(()) },
    }
}

pub struct Running<'a> {
    chunk: &'a Chunk,
    ip: usize,
    stack: [Value; STACK_SIZE],
    stack_offset: usize,
}

impl<'a> Running<'a> {
    fn new(chunk: &'a Chunk) -> Running<'a> {
        Running {
            chunk,
            ip: 0,
            stack: [Value(0.0); STACK_SIZE],
            stack_offset: 0,
        }
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
            tracer.enter_instruction(
                self.state.chunk,
                self.state.ip,
                self.state
                    .stack
                    .iter()
                    .take(self.state.stack_offset)
                    .collect(),
            );
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
            Opcode::Return => {
                println!("{}", self.pop().show());
                Ok(RuntimeAction::Halt)
            }
            Opcode::Add => {
                match (self.pop(), self.pop()) {
                    (Value(right), Value(left)) => self.push(Value(left + right)),
                }
                Ok(RuntimeAction::Continue)
            }
            Opcode::Subtract => {
                match (self.pop(), self.pop()) {
                    (Value(right), Value(left)) => self.push(Value(left - right)),
                }
                Ok(RuntimeAction::Continue)
            }
            Opcode::Multiply => {
                match (self.pop(), self.pop()) {
                    (Value(right), Value(left)) => self.push(Value(left * right)),
                }
                Ok(RuntimeAction::Continue)
            }
            Opcode::Divide => {
                match (self.pop(), self.pop()) {
                    (Value(right), Value(left)) => self.push(Value(left / right)),
                }
                Ok(RuntimeAction::Continue)
            }
            Opcode::Negate => {
                match self.pop() {
                    Value(v) => self.push(Value(-v)),
                }
                Ok(RuntimeAction::Continue)
            }
            Opcode::Constant => {
                let const_idx = self.next_byte()?;
                let constant = self.state.chunk.constant_at(const_idx)?;
                self.push(constant.clone());
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

    fn push(&mut self, value: Value) -> () {
        self.state.stack[self.state.stack_offset] = value;
        self.state.stack_offset += 1;
    }

    fn pop(&mut self) -> Value {
        self.state.stack_offset -= 1;
        self.state.stack[self.state.stack_offset]
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

    pub fn extract_result(mut self) -> (Self, Result<()>) {
        let result = self.state.result;
        self.state.result = Ok(());
        (self, result)
    }

    pub fn finish(self) -> Result<()> {
        self.state.result
    }
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
    use crate::{chunk::Chunk, value::Value};

    pub trait Tracer {
        fn enter_instruction(&mut self, chunk: &Chunk, offset: usize, stack: Vec<&Value>) -> ();
        fn exit_instruction(&mut self) -> ();
    }
}
