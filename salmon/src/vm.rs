use std::{sync::Arc, u8};

use miette::{Context, Diagnostic, Result};
use thiserror::Error;

use crate::{
    chunk::{Chunk, Opcode},
    object::{FunctionData, Object},
    scanner::SourceLocation,
    table::Table,
    value::{DataType, Value},
};

pub(crate) const MAX_FRAME_COUNT: usize = 64;
pub(crate) const STACK_SIZE: usize = MAX_FRAME_COUNT * (u8::MAX as usize);

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

struct CallFrame {
    function: Arc<FunctionData>,
    ip: usize,
    stack_offset: usize,
}

impl CallFrame {
    fn trace_line(&self) -> TraceLine {
        let location = self.function.chunk.location(self.ip - 1).unwrap();
        TraceLine {
            function_name: self.function.debug_name(),
            span: location.span,
            line: location.line,
        }
    }
}

struct FrameView<'a> {
    frame: &'a mut CallFrame,
    stack: &'a mut [Value],
    vm_stack_offset: &'a mut usize,
    globals: &'a mut Table,
    strings: &'a mut Table,
}

impl<'a> FrameView<'a> {
    fn chunk(&'a self) -> &'a Chunk {
        &self.frame.function.chunk
    }

    fn next_byte(&mut self) -> Result<u8> {
        let current_byte = self.frame.function.chunk.byte(self.frame.ip)?;
        self.frame.ip += 1;
        Ok(*current_byte)
    }

    fn next_u16(&mut self) -> Result<u16> {
        let high_byte = self.frame.function.chunk.byte(self.frame.ip)?;
        let low_byte = self.frame.function.chunk.byte(self.frame.ip + 1)?;
        self.frame.ip += 2;

        Ok(((*high_byte as u16) << 8) | (*low_byte as u16))
    }

    fn previous_opcode_location(&self) -> Result<&SourceLocation> {
        self.frame.function.chunk.location(self.frame.ip - 1)
    }

    fn jump_forward(&mut self, distance: usize) -> () {
        self.frame.ip += distance
    }

    fn jump_backward(&mut self, distance: usize) -> () {
        self.frame.ip -= distance
    }

    fn push(&mut self, value: Value) -> Result<()> {
        if *self.vm_stack_offset < STACK_SIZE {
            self.stack[*self.vm_stack_offset - self.frame.stack_offset] = value;
            *self.vm_stack_offset += 1;
            Ok(())
        } else {
            Err(RuntimeError::StackOverflow {
                span: self.previous_opcode_location()?.span,
            }
            .into())
        }
    }

    fn pop(&mut self) -> Result<Value> {
        if *self.vm_stack_offset > 0 {
            *self.vm_stack_offset -= 1;
            Ok(std::mem::replace(
                &mut self.stack[*self.vm_stack_offset - self.frame.stack_offset],
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
        if *self.vm_stack_offset > 0 {
            *self.vm_stack_offset -= 1;
            self.stack[*self.vm_stack_offset - self.frame.stack_offset] = Value::Nil;
            Ok(())
        } else {
            Err(RuntimeError::StackUnderflow {
                span: self.previous_opcode_location()?.span,
            }
            .into())
        }
    }

    fn peek(&self, depth: usize) -> Value {
        let highest_index = *self.vm_stack_offset - self.frame.stack_offset - 1;
        if depth <= highest_index {
            self.stack[highest_index - depth].clone()
        } else {
            Value::Nil
        }
    }
}

pub struct Running {
    frames: Vec<CallFrame>,
    stack: Box<[Value]>,
    stack_offset: usize,
}

impl Running {
    fn new(toplevel_function: Arc<FunctionData>) -> Running {
        let mut running_state = Running {
            frames: vec![],
            stack: std::iter::repeat(Value::Nil).take(STACK_SIZE).collect(),
            stack_offset: 0,
        };

        running_state.stack[running_state.stack_offset] =
            Value::Object(Object::Function(Arc::clone(&toplevel_function)));
        running_state.stack_offset += 1;

        let toplevel_frame = CallFrame {
            function: toplevel_function,
            ip: 0,
            stack_offset: 0,
        };
        running_state.frames.push(toplevel_frame);
        running_state
    }
}

impl<'a> VMState for Running {}

impl VM<Running> {
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
            let frame = self.current_frame();
            tracer.enter_instruction(
                &frame.function.chunk,
                frame.ip,
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
        let mut frame = self.current_frame_view();
        match op {
            Opcode::Print => {
                println!("{}", frame.pop()?.print());
                Ok(RuntimeAction::Continue)
            }
            Opcode::Jump => {
                let distance = frame.next_u16()?;
                frame.jump_forward(distance as usize);
                Ok(RuntimeAction::Continue)
            }
            Opcode::JumpIfFalse => {
                let distance = frame.next_u16()?;
                if frame.peek(0).is_falsy() {
                    frame.jump_forward(distance as usize);
                }
                Ok(RuntimeAction::Continue)
            }
            Opcode::Loop => {
                let distance = frame.next_u16()?;
                frame.jump_backward(distance as usize);
                Ok(RuntimeAction::Continue)
            }
            Opcode::Call => {
                let arg_count = frame.next_byte()?;
                let function = frame.peek(arg_count as usize);
                self.call_value(function, arg_count)?;
                Ok(RuntimeAction::Continue)
            }
            Opcode::Return => Ok(RuntimeAction::Halt),
            Opcode::Pop => {
                let _ = frame.pop()?;
                Ok(RuntimeAction::Continue)
            }
            Opcode::GetLocal => {
                let slot = frame.next_byte()?;
                let value = frame.stack[slot as usize].clone();
                frame.push(value)?;
                Ok(RuntimeAction::Continue)
            }
            Opcode::SetLocal => {
                let slot = frame.next_byte()?;
                let new_value = frame.peek(0);
                frame.stack[slot as usize] = new_value;
                Ok(RuntimeAction::Continue)
            }
            Opcode::GetGlobal => {
                let name_idx = frame.next_byte()?;
                let global_name = frame.chunk().constant_at(name_idx)?;
                if let Value::Object(Object::String(name_ref)) = global_name {
                    if let Some(value) = frame.globals.get(name_ref) {
                        frame.push(value)?;
                        Ok(RuntimeAction::Continue)
                    } else {
                        Err(RuntimeError::UndefinedVariable {
                            name: global_name.print(),
                            span: frame.previous_opcode_location()?.span,
                        }
                        .into())
                    }
                } else {
                    Err(RuntimeError::Bug {
                        message:
                            "GetGlobal instruction somehow points at something other than a string",
                        span: frame.previous_opcode_location()?.span,
                    }
                    .into())
                }
            }
            Opcode::DefineGlobal => {
                let name_idx = frame.next_byte()?;
                let global_name = frame.chunk().constant_at(name_idx)?;
                if let Value::Object(Object::String(name_ref)) = global_name {
                    let _ = frame.globals.set(Arc::clone(name_ref), frame.peek(0));
                    frame.pop()?;
                    Ok(RuntimeAction::Continue)
                } else {
                    Err(RuntimeError::Bug {
                        message:
                            "DefineGlobal instruction somehow points at something other than a string",
                        span: frame.previous_opcode_location()?.span,
                    }
                    .into())
                }
            }
            Opcode::SetGlobal => {
                let name_idx = frame.next_byte()?;
                let global_name = frame.chunk().constant_at(name_idx)?.clone();
                if let Value::Object(Object::String(name_ref)) = global_name {
                    if frame.globals.set(Arc::clone(&name_ref), frame.peek(0)) {
                        // If we inserted a new value, then the variable wasn't declared, so we
                        // take it back out and return an error
                        frame.globals.delete(&name_ref);

                        Err(RuntimeError::UndefinedVariable {
                            name: name_ref.to_string(),
                            span: frame.previous_opcode_location()?.span,
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
                        span: frame.previous_opcode_location()?.span,
                    }
                    .into())
                }
            }
            Opcode::Equal => {
                let right = frame.pop()?;
                let left = frame.pop()?;
                frame.push(Value::Boolean(left == right))?;
                Ok(RuntimeAction::Continue)
            }
            Opcode::Greater => {
                numeric_binary_operation(&mut frame, Opcode::Greater, |left, right| {
                    Value::Boolean(left > right)
                })?;
                Ok(RuntimeAction::Continue)
            }
            Opcode::Less => {
                numeric_binary_operation(&mut frame, Opcode::Less, |left, right| {
                    Value::Boolean(left < right)
                })?;
                Ok(RuntimeAction::Continue)
            }
            Opcode::Add => {
                let (right_view, left_view) = (frame.peek(0), frame.peek(1));
                if left_view.is(DataType::Number) && right_view.is(DataType::Number) {
                    if let (Value::Number(right_num), Value::Number(left_num)) =
                        (frame.pop()?, frame.pop()?)
                    {
                        frame.push(Value::Number(left_num + right_num))?;
                        Ok(RuntimeAction::Continue)
                    } else {
                        Err(RuntimeError::Bug {
                            message: "in Add: top two values on stack should've been numbers",
                            span: frame.previous_opcode_location()?.span,
                        }
                        .into())
                    }
                } else if left_view.is(DataType::String) && right_view.is(DataType::String) {
                    if let (
                        Value::Object(Object::String(right_str)),
                        Value::Object(Object::String(left_str)),
                    ) = (frame.pop()?, frame.pop()?)
                    {
                        let combined_string =
                            left_str.concatenate(right_str.as_ref(), &mut frame.strings);
                        frame.push(Value::Object(Object::String(combined_string)))?;
                        Ok(RuntimeAction::Continue)
                    } else {
                        Err(RuntimeError::Bug {
                            message: "in Add: top two values on stack should've been strings",
                            span: frame.previous_opcode_location()?.span,
                        }
                        .into())
                    }
                } else {
                    Err(RuntimeError::TypeErrorBinary {
                        op_symbol: Opcode::Add.op_symbol(),
                        lvalue: left_view,
                        rvalue: right_view,
                        span: frame.previous_opcode_location()?.span,
                    }
                    .into())
                }
            }
            Opcode::Subtract => {
                numeric_binary_operation(&mut frame, Opcode::Subtract, |left, right| {
                    Value::Number(left - right)
                })?;
                Ok(RuntimeAction::Continue)
            }
            Opcode::Multiply => {
                numeric_binary_operation(&mut frame, Opcode::Multiply, |left, right| {
                    Value::Number(left * right)
                })?;
                Ok(RuntimeAction::Continue)
            }
            Opcode::Divide => {
                numeric_binary_operation(&mut frame, Opcode::Divide, |left, right| {
                    Value::Number(left / right)
                })?;
                Ok(RuntimeAction::Continue)
            }
            Opcode::Not => {
                let result = frame.pop()?.is_falsy();
                frame.push(Value::Boolean(result))?;
                Ok(RuntimeAction::Continue)
            }
            Opcode::Negate => match frame.peek(0) {
                Value::Number(v) => {
                    frame.pop_ignore()?;
                    frame.push(Value::Number(-v))?;
                    Ok(RuntimeAction::Continue)
                }
                bad_value => Err(RuntimeError::TypeErrorUnary {
                    op_symbol: Opcode::Negate.op_symbol(),
                    value: bad_value.clone(),
                    span: frame.previous_opcode_location()?.span,
                }
                .into()),
            },
            Opcode::Constant => {
                let const_idx = frame.next_byte()?;
                let constant = frame.chunk().constant_at(const_idx)?;
                frame.push(constant.clone())?;
                Ok(RuntimeAction::Continue)
            }
            Opcode::True => frame
                .push(Value::Boolean(true))
                .map(|_| RuntimeAction::Continue),
            Opcode::False => frame
                .push(Value::Boolean(false))
                .map(|_| RuntimeAction::Continue),
            Opcode::Nil => frame.push(Value::Nil).map(|_| RuntimeAction::Continue),
        }
    }

    fn current_frame(&self) -> &CallFrame {
        self.state.frames.last().unwrap()
    }

    fn current_frame_view<'a>(&'a mut self) -> FrameView<'a> {
        let frame = self.state.frames.last_mut().unwrap();
        let frame_stack_offset = frame.stack_offset;
        FrameView {
            frame,
            stack: &mut self.state.stack[frame_stack_offset..],
            vm_stack_offset: &mut self.state.stack_offset,
            globals: &mut self.globals,
            strings: &mut self.strings,
        }
    }

    fn call_value(&mut self, value: Value, arg_count: u8) -> Result<()> {
        match value {
            Value::Object(Object::Function(function)) => self.call_function(function, arg_count),
            _ => {
                let call_location = self
                    .current_frame_view()
                    .previous_opcode_location()?
                    .clone();
                Err(RuntimeError::BadCall {
                    value,
                    call_span: call_location.span,
                }
                .into())
            }
        }
    }

    fn call_function(&mut self, function: Arc<FunctionData>, arg_count: u8) -> Result<()> {
        if function.arity != arg_count as usize {
            return Err(RuntimeError::WrongNumberOfArguments {
                name: function.debug_name(),
                got: arg_count,
                expected: function.arity,
                call_span: self.current_frame_view().previous_opcode_location()?.span,
            }
            .into());
        }

        if self.state.frames.len() == MAX_FRAME_COUNT {
            return Err(RuntimeError::StackOverflow {
                span: self.current_frame_view().previous_opcode_location()?.span,
            }
            .into());
        }

        let new_frame = CallFrame {
            function,
            ip: 0,
            stack_offset: self.state.stack_offset - (arg_count as usize) - 1,
        };
        Ok(self.state.frames.push(new_frame))
    }

    fn next_opcode(&mut self) -> Result<Opcode> {
        let current_frame = self.state.frames.last_mut().unwrap();
        let current_byte = current_frame.function.chunk.byte(current_frame.ip)?;
        current_frame.ip += 1;
        (*current_byte).try_into()
    }

    fn halt(self, result: Result<()>) -> VM<Stopped> {
        let mut wrapped_result = result;

        for frame in self.state.frames.iter().rev() {
            wrapped_result = wrapped_result.context(frame.trace_line());
        }

        VM {
            state: Stopped {
                result: wrapped_result.context("Runtime Error"),
            },
            strings: self.strings,
            globals: self.globals,
        }
    }
}

fn numeric_binary_operation<F>(frame: &mut FrameView<'_>, opcode: Opcode, op: F) -> Result<()>
where
    F: FnOnce(f64, f64) -> Value,
{
    match (frame.peek(0), frame.peek(1)) {
        (Value::Number(right), Value::Number(left)) => {
            frame.pop_ignore()?;
            frame.pop_ignore()?;
            frame.push(op(left, right))
        }
        (bad_right, bad_left) => {
            return Err(RuntimeError::TypeErrorBinary {
                op_symbol: opcode.op_symbol(),
                lvalue: bad_left.clone(),
                rvalue: bad_right.clone(),
                span: frame.previous_opcode_location()?.span,
            }
            .into());
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
    pub fn interpret(mut self, toplevel_function: Arc<FunctionData>) -> VM<Stopped> {
        self.strings.add_all(&toplevel_function.chunk.strings);
        let running = VM {
            state: Running::new(toplevel_function),
            strings: self.strings,
            globals: self.globals,
        };
        running.run()
    }

    pub fn trace<T: tracing::Tracer>(
        mut self,
        toplevel_function: Arc<FunctionData>,
        tracer: &mut T,
    ) -> VM<Stopped> {
        self.strings.add_all(&toplevel_function.chunk.strings);
        let running = VM {
            state: Running::new(toplevel_function),
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
    #[error("type error: cannot apply unary operator '{op_symbol}' to value: {value:?}")]
    #[diagnostic(code(runtime::type_error))]
    TypeErrorUnary {
        op_symbol: &'static str,
        value: Value,
        #[label("here")]
        span: (usize, usize),
    },
    #[error(
        "type error: cannot apply operator '{op_symbol}' to values: {lvalue:?} and {rvalue:?}"
    )]
    #[diagnostic(code(runtime::type_error))]
    TypeErrorBinary {
        op_symbol: &'static str,
        lvalue: Value,
        rvalue: Value,
        #[label("here")]
        span: (usize, usize),
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
    #[error("undefined variable '{name}'")]
    #[diagnostic(
        code(runtime::undefined_variable),
        help("variables must be declared before use")
    )]
    UndefinedVariable {
        name: String,
        #[label("referenced here")]
        span: (usize, usize),
    },
    #[error("attempted to call uncallable value {value:?}")]
    #[diagnostic(
        code(runtime::bad_call),
        help("only functions, methods, and classes are callable")
    )]
    BadCall {
        value: Value,
        #[label("call site")]
        call_span: (usize, usize),
    },
    #[error("attempted to call '{name}' with the wrong number of arguments (got {got}; expected {expected})")]
    #[diagnostic(code(runtime::wrong_number_of_arguments))]
    WrongNumberOfArguments {
        name: String,
        got: u8,
        expected: usize,
        #[label("call site")]
        call_span: (usize, usize),
    },
    #[error("bug in VM: {message}")]
    #[diagnostic(code(runtime::internal::bug))]
    Bug {
        message: &'static str,
        #[label("here-ish")]
        span: (usize, usize),
    },
}

#[derive(Debug, Error, Diagnostic)]
#[error("in {function_name} on line {line}")]
#[diagnostic()]
pub struct TraceLine {
    function_name: String,
    #[label]
    span: (usize, usize),
    line: usize,
}

pub mod tracing {
    use crate::{chunk::Chunk, value::Value};

    pub trait Tracer {
        fn enter_instruction(&mut self, chunk: &Chunk, offset: usize, stack: Vec<&Value>) -> ();
        fn exit_instruction(&mut self) -> ();
    }
}
