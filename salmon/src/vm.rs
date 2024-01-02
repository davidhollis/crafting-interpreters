use miette::{Diagnostic, Result};
use thiserror::Error;

use crate::{
    chunk::{Chunk, Opcode},
    scanner::SourceLocation,
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
            stack: [Value::Nil; STACK_SIZE],
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
                println!("{}", self.pop()?.show());
                Ok(RuntimeAction::Halt)
            }
            Opcode::Equal => {
                let right = self.pop()?;
                let left = self.pop()?;
                self.push(Value::Boolean(left == right))?;
                Ok(RuntimeAction::Continue)
            }
            Opcode::Greater => {
                self.numeric_binary_operation(Opcode::Greater, |left, right| {
                    Value::Boolean(left > right)
                })?;
                Ok(RuntimeAction::Continue)
            }
            Opcode::Less => {
                self.numeric_binary_operation(Opcode::Less, |left, right| {
                    Value::Boolean(left < right)
                })?;
                Ok(RuntimeAction::Continue)
            }
            Opcode::Add => {
                self.numeric_binary_operation(Opcode::Add, |left, right| {
                    Value::Number(left + right)
                })?;
                Ok(RuntimeAction::Continue)
            }
            Opcode::Subtract => {
                self.numeric_binary_operation(Opcode::Subtract, |left, right| {
                    Value::Number(left - right)
                })?;
                Ok(RuntimeAction::Continue)
            }
            Opcode::Multiply => {
                self.numeric_binary_operation(Opcode::Multiply, |left, right| {
                    Value::Number(left * right)
                })?;
                Ok(RuntimeAction::Continue)
            }
            Opcode::Divide => {
                self.numeric_binary_operation(Opcode::Divide, |left, right| {
                    Value::Number(left / right)
                })?;
                Ok(RuntimeAction::Continue)
            }
            Opcode::Not => {
                let result = self.pop()?.is_falsy();
                self.push(Value::Boolean(result))?;
                Ok(RuntimeAction::Continue)
            }
            Opcode::Negate => match self.peek(0) {
                Value::Number(v) => {
                    self.pop_ignore()?;
                    self.push(Value::Number(-v))?;
                    Ok(RuntimeAction::Continue)
                }
                bad_value => {
                    let operator_location = self.previous_opcode_location()?;
                    Err(RuntimeError::TypeErrorUnary {
                        operator: Opcode::Negate,
                        value: bad_value,
                        span: operator_location.span,
                        line: operator_location.line,
                    }
                    .into())
                }
            },
            Opcode::Constant => {
                let const_idx = self.next_byte()?;
                let constant = self.state.chunk.constant_at(const_idx)?;
                self.push(constant.clone())?;
                Ok(RuntimeAction::Continue)
            }
            Opcode::True => self
                .push(Value::Boolean(true))
                .map(|_| RuntimeAction::Continue),
            Opcode::False => self
                .push(Value::Boolean(false))
                .map(|_| RuntimeAction::Continue),
            Opcode::Nil => self.push(Value::Nil).map(|_| RuntimeAction::Continue),
        }
    }

    fn numeric_binary_operation<F>(&mut self, opcode: Opcode, op: F) -> Result<()>
    where
        F: FnOnce(f64, f64) -> Value,
    {
        match (self.peek(0), self.peek(1)) {
            (Value::Number(right), Value::Number(left)) => {
                self.pop_ignore()?;
                self.pop_ignore()?;
                self.push(op(left, right))
            }
            (bad_right, bad_left) => {
                let operator_location = self.previous_opcode_location()?;
                return Err(RuntimeError::TypeErrorBinary {
                    operator: opcode,
                    lvalue: bad_left,
                    rvalue: bad_right,
                    span: operator_location.span,
                    line: operator_location.line,
                }
                .into());
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

    fn previous_opcode_location(&mut self) -> Result<&SourceLocation> {
        self.state.chunk.location(self.state.ip - 1)
    }

    fn push(&mut self, value: Value) -> Result<()> {
        if self.state.stack_offset < STACK_SIZE {
            self.state.stack[self.state.stack_offset] = value;
            self.state.stack_offset += 1;
            Ok(())
        } else {
            Err(RuntimeError::StackOverflow {
                span: self.previous_opcode_location()?.span,
            }
            .into())
        }
    }

    fn pop(&mut self) -> Result<Value> {
        if self.state.stack_offset > 0 {
            self.state.stack_offset -= 1;
            Ok(self.state.stack[self.state.stack_offset])
        } else {
            Err(RuntimeError::StackUnderflow {
                span: self.previous_opcode_location()?.span,
            }
            .into())
        }
    }

    fn pop_ignore(&mut self) -> Result<()> {
        if self.state.stack_offset > 0 {
            self.state.stack_offset -= 1;
            Ok(())
        } else {
            Err(RuntimeError::StackUnderflow {
                span: self.previous_opcode_location()?.span,
            }
            .into())
        }
    }

    fn peek(&self, depth: usize) -> Value {
        if depth <= (self.state.stack_offset - 1) {
            self.state.stack[self.state.stack_offset - depth - 1]
        } else {
            Value::Nil
        }
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
    #[error("type error on line {line}: cannot apply operator {operator:?} to value: {value:?}")]
    #[diagnostic(code(runtime::type_error))]
    TypeErrorUnary {
        operator: Opcode,
        value: Value,
        #[label("here")]
        span: (usize, usize),
        line: usize,
    },
    #[error("type error on line {line}: cannot apply operator {operator:?} to values: {lvalue:?} and {rvalue:?}")]
    #[diagnostic(code(runtime::type_error))]
    TypeErrorBinary {
        operator: Opcode,
        lvalue: Value,
        rvalue: Value,
        #[label("here")]
        span: (usize, usize),
        line: usize,
    },
    #[error("attempted to pop value from empty stack")]
    #[diagnostic(
        code(runtime::internal::stack_underflow),
        help("this is an internal error that was likely caused by a bug in the compiler")
    )]
    StackUnderflow {
        #[label("occurred here")]
        span: (usize, usize),
    },
    #[error("stack overflow")]
    #[diagnostic(
        code(runtime::stack_overflow),
        help("salmon was intentionally built with a rather small stack")
    )]
    StackOverflow {
        #[label("occurred here")]
        span: (usize, usize),
    },
}

pub mod tracing {
    use crate::{chunk::Chunk, value::Value};

    pub trait Tracer {
        fn enter_instruction(&mut self, chunk: &Chunk, offset: usize, stack: Vec<&Value>) -> ();
        fn exit_instruction(&mut self) -> ();
    }
}
