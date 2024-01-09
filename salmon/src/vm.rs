use miette::{Diagnostic, Result};
use thiserror::Error;

use crate::{
    chunk::{Chunk, Opcode},
    object,
    scanner::SourceLocation,
    table::Table,
    value::{DataType, Value},
};

pub(crate) const STACK_SIZE: usize = 255;

pub trait VMState {}

pub struct VM<S: VMState> {
    state: S,
    pub strings: Table,
    globals: Table,
}

pub fn new() -> VM<Stopped> {
    VM {
        state: Stopped { result: Ok(()) },
        strings: Table::new(),
        globals: Table::new(),
    }
}

pub struct Running<'a> {
    chunk: &'a Chunk,
    ip: usize,
    stack: Box<[Value]>,
    stack_offset: usize,
}

impl<'a> Running<'a> {
    fn new(chunk: &'a Chunk) -> Running<'a> {
        Running {
            chunk,
            ip: 0,
            stack: std::iter::repeat(Value::Nil).take(STACK_SIZE).collect(),
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
            Opcode::Print => {
                println!("{}", self.pop()?.print());
                Ok(RuntimeAction::Continue)
            }
            Opcode::Jump => {
                let distance = self.next_u16()?;
                self.state.ip += distance as usize;
                Ok(RuntimeAction::Continue)
            }
            Opcode::JumpIfFalse => {
                let distance = self.next_u16()?;
                if self.peek(0).is_falsy() {
                    self.state.ip += distance as usize;
                }
                Ok(RuntimeAction::Continue)
            }
            Opcode::Return => Ok(RuntimeAction::Halt),
            Opcode::Pop => {
                let _ = self.pop()?;
                Ok(RuntimeAction::Continue)
            }
            Opcode::GetLocal => {
                let stack_idx = self.next_byte()?;
                let value = self.state.stack[stack_idx as usize].clone();
                self.push(value)?;
                Ok(RuntimeAction::Continue)
            }
            Opcode::SetLocal => {
                let stack_idx = self.next_byte()?;
                let new_value = self.peek(0);
                self.state.stack[stack_idx as usize] = new_value;
                Ok(RuntimeAction::Continue)
            }
            Opcode::GetGlobal => {
                let name_idx = self.next_byte()?;
                let global_name = self.state.chunk.constant_at(name_idx)?;
                if let Value::Object(name_ref) = global_name {
                    if let Some(value) = self.globals.get(name_ref) {
                        self.push(value)?;
                        Ok(RuntimeAction::Continue)
                    } else {
                        let location = self.previous_opcode_location()?;
                        Err(RuntimeError::UndefinedVariable {
                            name: global_name.print(),
                            span: location.span,
                            line: location.line,
                        }
                        .into())
                    }
                } else {
                    Err(RuntimeError::Bug {
                        message:
                            "GetGlobal instruction somehow points at something other than a string",
                        span: self.previous_opcode_location()?.span,
                    }
                    .into())
                }
            }
            Opcode::DefineGlobal => {
                let name_idx = self.next_byte()?;
                let global_name = self.state.chunk.constant_at(name_idx)?;
                if let Value::Object(name_ref) = global_name {
                    let _ = self.globals.set(name_ref.clone(), self.peek(0));
                    self.pop()?;
                    Ok(RuntimeAction::Continue)
                } else {
                    Err(RuntimeError::Bug {
                        message:
                            "DefineGlobal instruction somehow points at something other than a string",
                        span: self.previous_opcode_location()?.span,
                    }
                    .into())
                }
            }
            Opcode::SetGlobal => {
                let name_idx = self.next_byte()?;
                let global_name = self.state.chunk.constant_at(name_idx)?;
                if let Value::Object(name_ref) = global_name {
                    if self.globals.set(name_ref.clone(), self.peek(0)) {
                        // If we inserted a new value, then the variable wasn't declared, so we
                        // take it back out and return an error
                        self.globals.delete(name_ref);

                        let location = self.previous_opcode_location()?;
                        Err(RuntimeError::UndefinedVariable {
                            name: name_ref.print(),
                            span: location.span,
                            line: location.line,
                        }
                        .into())
                    } else {
                        // If we overwrote an existing value, all's well
                        Ok(RuntimeAction::Continue)
                    }
                } else {
                    Err(RuntimeError::Bug {
                        message:
                            "SetGlobal instruction somehow points at something other than a string",
                        span: self.previous_opcode_location()?.span,
                    }
                    .into())
                }
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
                let (right_view, left_view) = (self.peek(0), self.peek(1));
                if left_view.is(DataType::Number) && right_view.is(DataType::Number) {
                    if let (Value::Number(right_num), Value::Number(left_num)) =
                        (self.pop()?, self.pop()?)
                    {
                        self.push(Value::Number(left_num + right_num))?;
                        Ok(RuntimeAction::Continue)
                    } else {
                        Err(RuntimeError::Bug {
                            message: "in Add: top two values on stack should've been numbers",
                            span: self.previous_opcode_location()?.span,
                        }
                        .into())
                    }
                } else if left_view.is(DataType::String) && right_view.is(DataType::String) {
                    if let (Value::Object(right_str), Value::Object(left_str)) =
                        (self.pop()?, self.pop()?)
                    {
                        let combined_string = object::string::concatenate(
                            left_str.as_ref(),
                            right_str.as_ref(),
                            &mut self.strings,
                        );
                        self.push(Value::Object(combined_string))?;
                        Ok(RuntimeAction::Continue)
                    } else {
                        Err(RuntimeError::Bug {
                            message: "in Add: top two values on stack should've been strings",
                            span: self.previous_opcode_location()?.span,
                        }
                        .into())
                    }
                } else {
                    let opcode_location = self.previous_opcode_location()?;
                    Err(RuntimeError::TypeErrorBinary {
                        operator: Opcode::Add,
                        lvalue: left_view,
                        rvalue: right_view,
                        span: opcode_location.span,
                        line: opcode_location.line,
                    }
                    .into())
                }
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
                        value: bad_value.clone(),
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
                    lvalue: bad_left.clone(),
                    rvalue: bad_right.clone(),
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

    fn next_u16(&mut self) -> Result<u16> {
        let high_byte = self.state.chunk.byte(self.state.ip)?;
        let low_byte = self.state.chunk.byte(self.state.ip + 1)?;
        self.state.ip += 2;

        Ok(((*high_byte as u16) << 8) | (*low_byte as u16))
    }

    fn next_opcode(&mut self) -> Result<Opcode> {
        let current_byte = self.state.chunk.byte(self.state.ip)?;
        self.state.ip += 1;
        (*current_byte).try_into()
    }

    fn previous_opcode_location(&self) -> Result<&SourceLocation> {
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
            Ok(std::mem::replace(
                &mut self.state.stack[self.state.stack_offset],
                Value::Nil,
            ))
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
            self.state.stack[self.state.stack_offset - depth - 1].clone()
        } else {
            Value::Nil
        }
    }

    fn halt(self, result: Result<()>) -> VM<Stopped> {
        VM {
            state: Stopped { result },
            strings: self.strings,
            globals: self.globals,
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
    pub fn interpret(mut self, chunk: &Chunk) -> VM<Stopped> {
        self.strings.add_all(&chunk.strings);
        let running = VM {
            state: Running::new(chunk),
            strings: self.strings,
            globals: self.globals,
        };
        running.run()
    }

    pub fn trace<T: tracing::Tracer>(mut self, chunk: &Chunk, tracer: &mut T) -> VM<Stopped> {
        self.strings.add_all(&chunk.strings);
        let running = VM {
            state: Running::new(chunk),
            strings: self.strings,
            globals: self.globals,
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
    #[error("undefined variable '{name}' on line {line}")]
    #[diagnostic(
        code(runtime::undefined_variable),
        help("variables must be declared before use")
    )]
    UndefinedVariable {
        name: String,
        #[label("referenced here")]
        span: (usize, usize),
        line: usize,
    },
    #[error("bug in VM: {message}")]
    #[diagnostic(code(runtime::internal::bug))]
    Bug {
        message: &'static str,
        #[label("here-ish")]
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
