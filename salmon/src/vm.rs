use std::{collections::BinaryHeap, sync::Arc, u8};

use miette::{Context, Diagnostic, Report, Result};
use thiserror::Error;

use crate::{
    chunk::{Chunk, Opcode},
    object::{
        BoundMethodData, ClassData, ClosureData, FunctionData, InstanceData, NativeData, NativeFn,
        Object, StringData, UpvalueData,
    },
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
    init_string: Arc<StringData>,
    globals: Table,
    open_upvalues: BinaryHeap<Arc<UpvalueData>>,
}

pub fn new() -> VM<Stopped> {
    let mut strings = Table::new();
    let init_string = strings.intern_string("init");
    VM {
        state: Stopped { result: Ok(()) },
        strings,
        init_string,
        globals: Table::new(),
        open_upvalues: BinaryHeap::new(),
    }
}

impl<T: VMState> VM<T> {
    pub fn register_native(&mut self, name: &str, function: NativeFn) -> () {
        let function_name = self.strings.intern_string(name);
        let function_value = Value::Object(Object::Native(NativeData::new(
            Arc::clone(&function_name),
            function,
        )));
        let _ = self.globals.set(function_name, function_value);
    }
}

struct CallFrame {
    closure: Arc<ClosureData>,
    ip: usize,
    stack_offset: usize,
}

impl CallFrame {
    fn trace_line(&self) -> TraceLine {
        let location = self.closure.function.chunk.location(self.ip - 1).unwrap();
        TraceLine {
            function_name: self.closure.function.debug_name(),
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
    open_upvalues: &'a mut BinaryHeap<Arc<UpvalueData>>,
}

impl<'a> FrameView<'a> {
    fn chunk(&'a self) -> &'a Chunk {
        &self.frame.closure.function.chunk
    }

    fn next_byte(&mut self) -> Result<u8> {
        let current_byte = self.frame.closure.function.chunk.byte(self.frame.ip)?;
        self.frame.ip += 1;
        Ok(*current_byte)
    }

    fn next_u16(&mut self) -> Result<u16> {
        let high_byte = self.frame.closure.function.chunk.byte(self.frame.ip)?;
        let low_byte = self.frame.closure.function.chunk.byte(self.frame.ip + 1)?;
        self.frame.ip += 2;

        Ok(((*high_byte as u16) << 8) | (*low_byte as u16))
    }

    fn next_string_ref(&mut self, op: &'static str) -> Result<Arc<StringData>> {
        let string_idx = self.next_byte()?;
        let string = self.chunk().constant_at(string_idx)?;
        if let Value::Object(Object::String(string)) = string {
            Ok(Arc::clone(&string))
        } else {
            Err(RuntimeError::Bug {
                message: format!(
                    "{op} instruction somehow points at something other than a string"
                ),
                span: self.previous_opcode_location()?.span,
            }
            .into())
        }
    }

    fn previous_opcode_location(&self) -> Result<&SourceLocation> {
        self.frame
            .closure
            .function
            .chunk
            .location(self.frame.ip - 1)
    }

    fn jump_forward(&mut self, distance: usize) -> () {
        self.frame.ip += distance
    }

    fn jump_backward(&mut self, distance: usize) -> () {
        self.frame.ip -= distance
    }

    fn push(&mut self, value: Value) -> Result<()> {
        if *self.vm_stack_offset < STACK_SIZE {
            self.stack[*self.vm_stack_offset] = value;
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
                &mut self.stack[*self.vm_stack_offset],
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
            self.stack[*self.vm_stack_offset] = Value::Nil;
            Ok(())
        } else {
            Err(RuntimeError::StackUnderflow {
                span: self.previous_opcode_location()?.span,
            }
            .into())
        }
    }

    fn peek(&self, depth: usize) -> Value {
        let highest_index = *self.vm_stack_offset - 1;
        if depth <= highest_index {
            self.stack[highest_index - depth].clone()
        } else {
            Value::Nil
        }
    }

    fn local_offset(&self, local_slot: usize) -> usize {
        local_slot + self.frame.stack_offset
    }

    fn upvalue(&self, id: u8) -> Arc<UpvalueData> {
        Arc::clone(&self.frame.closure.upvalues[id as usize])
    }

    fn upvalue_ref(&self, id: u8) -> &Arc<UpvalueData> {
        &self.frame.closure.upvalues[id as usize]
    }

    fn capture_upvalue(&mut self, slot: usize) -> Arc<UpvalueData> {
        for existing_upvalue in self.open_upvalues.iter() {
            if existing_upvalue.points_to_slot(slot) {
                return Arc::clone(&existing_upvalue);
            }
        }

        let new_upvalue = UpvalueData::capture(slot);
        self.open_upvalues.push(Arc::clone(&new_upvalue));

        new_upvalue
    }

    fn close_upvalues_beyond(&mut self, last_slot: usize) -> () {
        while let Some(upvalue) = self.open_upvalues.peek() {
            if upvalue.slot >= last_slot {
                upvalue.close(&self.stack);
                let _ = self.open_upvalues.pop();
            } else {
                break;
            }
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
            closure: ClosureData::new(toplevel_function),
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
                &frame.closure.function.chunk,
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
            Opcode::Invoke => {
                let method = frame.next_string_ref("Invoke")?;
                let arg_count = frame.next_byte()?;
                let receiver = frame.peek(arg_count as usize);
                self.invoke(receiver, method, arg_count)?;
                Ok(RuntimeAction::Continue)
            }
            Opcode::InvokeSuper => {
                let method_name = frame.next_string_ref("InvokeSuper")?;
                let arg_count = frame.next_byte()?;
                let superclass = frame.pop()?;

                if let Value::Object(Object::Class(ref superclass)) = superclass {
                    self.invoke_from_class(superclass, method_name, arg_count)?;
                    Ok(RuntimeAction::Continue)
                } else {
                    Err(RuntimeError::Bug {
                        message: "local variable 'super' somehow pointed at something that wasn't a class".to_string(),
                        span: frame.previous_opcode_location()?.span,
                    }.into())
                }
            }
            Opcode::Closure => {
                let const_idx = frame.next_byte()?;
                match frame.chunk().constant_at(const_idx)? {
                    Value::Object(Object::Function(function)) => {
                        let opcode_location = frame.previous_opcode_location()?.clone();
                        let mut closure_ref = ClosureData::new(Arc::clone(function));
                        let closure =
                            Arc::get_mut(&mut closure_ref).ok_or_else(|| RuntimeError::Bug {
                                message: "Somehow we couldn't get an exclusive reference to a closure we just created".to_string(),
                                span: opcode_location.span,
                            })?;
                        for _ in 0..(function.upvalue_count) {
                            let locality = frame.next_byte()?;
                            let index = frame.next_byte()?;
                            if locality > 0 {
                                // Local upvalue (capture it from the current stack frame)
                                closure.upvalues.push(
                                    frame.capture_upvalue(frame.local_offset(index as usize)),
                                );
                            } else {
                                // Indirect upvalue (refer to an upvalue in the current frame)
                                closure.upvalues.push(frame.upvalue(index));
                            }
                        }
                        frame.push(Value::Object(Object::Closure(closure_ref)))?;
                    }
                    v => {
                        return Err(RuntimeError::TypeErrorUnary {
                            op_symbol: "<closure>",
                            value: v.clone(),
                            span: frame.previous_opcode_location()?.span,
                        }
                        .into())
                    }
                }
                Ok(RuntimeAction::Continue)
            }
            Opcode::CloseUpvalue => {
                frame.close_upvalues_beyond(*frame.vm_stack_offset - 1);
                frame.pop_ignore()?;
                Ok(RuntimeAction::Continue)
            }
            Opcode::Return => {
                let return_value = frame.pop()?;
                self.pop_frame()?;
                if self.has_frames_remaining() {
                    self.current_frame_view().push(return_value)?;
                    Ok(RuntimeAction::Continue)
                } else {
                    Ok(RuntimeAction::Halt)
                }
            }
            Opcode::Class => {
                let class_name = frame.next_string_ref("Class")?;
                frame.push(Value::Object(Object::Class(ClassData::new(&class_name))))?;
                Ok(RuntimeAction::Continue)
            }
            Opcode::Inherit => {
                let superclass = frame.peek(1);
                let subclass = frame.peek(0);

                if let (
                    Value::Object(Object::Class(ref superclass_data)),
                    Value::Object(Object::Class(ref subclass_data)),
                ) = (&superclass, &subclass)
                {
                    subclass_data.inherit_from(superclass_data);
                    Ok(RuntimeAction::Continue)
                } else {
                    Err(RuntimeError::TypeErrorBinary {
                        op_symbol: "<inherit>",
                        lvalue: superclass,
                        rvalue: subclass,
                        span: frame.previous_opcode_location()?.span,
                    }
                    .into())
                }
            }
            Opcode::Method => {
                let method_name = frame.next_string_ref("Method")?;
                let method_impl = frame.peek(0);
                let class = frame.peek(1);

                if let Value::Object(Object::Class(ref class_data)) = class {
                    if method_impl.is(DataType::Closure) {
                        class_data.add_method(method_name, method_impl);
                        frame.pop_ignore()?;
                        Ok(RuntimeAction::Continue)
                    } else {
                        Err(RuntimeError::TypeErrorBinary {
                            op_symbol: "<method>",
                            lvalue: class,
                            rvalue: method_impl,
                            span: frame.previous_opcode_location()?.span,
                        }
                        .into())
                    }
                } else {
                    Err(RuntimeError::TypeErrorBinary {
                        op_symbol: "<method>",
                        lvalue: class,
                        rvalue: method_impl,
                        span: frame.previous_opcode_location()?.span,
                    }
                    .into())
                }
            }
            Opcode::Pop => {
                frame.pop_ignore()?;
                Ok(RuntimeAction::Continue)
            }
            Opcode::Swap => {
                let first = frame.pop()?;
                let second = frame.pop()?;
                frame.push(first)?;
                frame.push(second)?;
                Ok(RuntimeAction::Continue)
            }
            Opcode::GetLocal => {
                let local_slot = frame.next_byte()?;
                let value = frame.stack[frame.local_offset(local_slot as usize)].clone();
                frame.push(value)?;
                Ok(RuntimeAction::Continue)
            }
            Opcode::SetLocal => {
                let local_slot = frame.next_byte()?;
                let new_value = frame.peek(0);
                frame.stack[frame.local_offset(local_slot as usize)] = new_value;
                Ok(RuntimeAction::Continue)
            }
            Opcode::GetGlobal => {
                let name = frame.next_string_ref("GetGlobal")?;
                if let Some(value) = frame.globals.get(&name) {
                    frame.push(value)?;
                    Ok(RuntimeAction::Continue)
                } else {
                    Err(RuntimeError::UndefinedVariable {
                        name: name.to_string(),
                        span: frame.previous_opcode_location()?.span,
                    }
                    .into())
                }
            }
            Opcode::DefineGlobal => {
                let name = frame.next_string_ref("DefineGlobal")?;
                let _ = frame.globals.set(name, frame.peek(0));
                frame.pop()?;
                Ok(RuntimeAction::Continue)
            }
            Opcode::SetGlobal => {
                let name = frame.next_string_ref("SetGlobal")?;
                if frame.globals.set(Arc::clone(&name), frame.peek(0)) {
                    // If we inserted a new value, then the variable wasn't declared, so we
                    // take it back out and return an error
                    frame.globals.delete(&name);

                    Err(RuntimeError::UndefinedVariable {
                        name: name.to_string(),
                        span: frame.previous_opcode_location()?.span,
                    }
                    .into())
                } else {
                    // If we overwrote an existing value, all's well
                    Ok(RuntimeAction::Continue)
                }
            }
            Opcode::GetUpvalue => {
                let upvalue_id = frame.next_byte()?;
                frame.push(frame.upvalue_ref(upvalue_id).read(&frame.stack))?;
                Ok(RuntimeAction::Continue)
            }
            Opcode::SetUpvalue => {
                let upvalue_id = frame.next_byte()?;
                let new_value = frame.peek(0);
                frame.upvalue(upvalue_id).write(&mut frame.stack, new_value);
                Ok(RuntimeAction::Continue)
            }
            Opcode::GetProperty => {
                let property_name = frame.next_string_ref("GetProperty")?;
                let receiver = frame.peek(0);
                if let Value::Object(Object::Instance(ref instance_ref)) = receiver {
                    if let Some(value) = instance_ref.get_field(&property_name) {
                        frame.pop_ignore()?; // pop the receiver
                        frame.push(value)?;
                        Ok(RuntimeAction::Continue)
                    } else {
                        if let Some(method_impl) = instance_ref.class.lookup_method(&property_name)
                        {
                            frame.pop_ignore()?;
                            frame.push(Value::Object(Object::BoundMethod(
                                BoundMethodData::bind(receiver, method_impl),
                            )))?;
                            Ok(RuntimeAction::Continue)
                        } else {
                            Err(RuntimeError::NoSuchProperty {
                                receiver,
                                property: property_name.to_string(),
                                access_span: frame.previous_opcode_location()?.span,
                            }
                            .into())
                        }
                    }
                } else {
                    Err(RuntimeError::BadPropertyAccess {
                        receiver,
                        property: property_name.to_string(),
                        operation: "get",
                        access_span: frame.previous_opcode_location()?.span,
                    }
                    .into())
                }
            }
            Opcode::SetProperty => {
                let property_name = frame.next_string_ref("SetProperty")?;
                let receiver = frame.peek(1);
                if let Value::Object(Object::Instance(ref instance_ref)) = receiver {
                    let _ = instance_ref.set_field(property_name, frame.peek(0));
                    let new_value = frame.pop()?;
                    frame.pop_ignore()?; // pop the receiver
                    frame.push(new_value)?;
                    Ok(RuntimeAction::Continue)
                } else {
                    Err(RuntimeError::BadPropertyAccess {
                        receiver,
                        property: property_name.to_string(),
                        operation: "get",
                        access_span: frame.previous_opcode_location()?.span,
                    }
                    .into())
                }
            }
            Opcode::GetSuper => {
                let method_name = frame.next_string_ref("GetSuper")?;
                let superclass = frame.pop()?;
                let receiver = frame.peek(0);

                if let Value::Object(Object::Class(superclass_data)) = &superclass {
                    if let Value::Object(Object::Instance(_)) = &receiver {
                        if let Some(method_impl) = superclass_data.lookup_method(&method_name) {
                            frame.pop_ignore()?;
                            frame.push(Value::Object(Object::BoundMethod(
                                BoundMethodData::bind(receiver, method_impl),
                            )))?;
                            Ok(RuntimeAction::Continue)
                        } else {
                            Err(RuntimeError::NoSuchProperty {
                                receiver: superclass,
                                property: method_name.to_string(),
                                access_span: frame.previous_opcode_location()?.span,
                            }
                            .into())
                        }
                    } else {
                        Err(RuntimeError::BadPropertyAccess {
                            receiver,
                            property: method_name.to_string(),
                            operation: "get",
                            access_span: frame.previous_opcode_location()?.span,
                        }
                        .into())
                    }
                } else {
                    Err(RuntimeError::Bug {
                        message: "local variable 'super' somehow pointed at something that wasn't a class".to_string(),
                        span: frame.previous_opcode_location()?.span,
                    }.into())
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
                            message: "in Add: top two values on stack should've been numbers"
                                .to_string(),
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
                        let combined_string = left_str.concatenate(&right_str, &mut frame.strings);
                        frame.push(Value::Object(Object::String(combined_string)))?;
                        Ok(RuntimeAction::Continue)
                    } else {
                        Err(RuntimeError::Bug {
                            message: "in Add: top two values on stack should've been strings"
                                .to_string(),
                            span: frame.previous_opcode_location()?.span,
                        }
                        .into())
                    }
                } else if left_view.is(DataType::String) {
                    if let (right_value, Value::Object(Object::String(left_str))) =
                        (frame.pop()?, frame.pop()?)
                    {
                        let right_string_raw = right_value.print();
                        let right_str = StringData::new(&right_string_raw);
                        let combined_string = left_str.concatenate(&right_str, &mut frame.strings);
                        frame.push(Value::Object(Object::String(combined_string)))?;
                        Ok(RuntimeAction::Continue)
                    } else {
                        Err(RuntimeError::Bug {
                            message: "in Add: the second value from the top of the stack should've been a string"
                                .to_string(),
                            span: frame.previous_opcode_location()?.span,
                        }
                        .into())
                    }
                } else if right_view.is(DataType::String) {
                    if let (Value::Object(Object::String(right_str)), left_value) =
                        (frame.pop()?, frame.pop()?)
                    {
                        let left_string_raw = left_value.print();
                        let left_str = StringData::new(&left_string_raw);
                        let combined_string = left_str.concatenate(&right_str, &mut frame.strings);
                        frame.push(Value::Object(Object::String(combined_string)))?;
                        Ok(RuntimeAction::Continue)
                    } else {
                        Err(RuntimeError::Bug {
                            message:
                                "in Add: the value on top of the stack should've been a string"
                                    .to_string(),
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

        FrameView {
            frame,
            stack: &mut self.state.stack[..],
            vm_stack_offset: &mut self.state.stack_offset,
            globals: &mut self.globals,
            strings: &mut self.strings,
            open_upvalues: &mut self.open_upvalues,
        }
    }

    fn pop_frame(&mut self) -> Result<()> {
        let mut popped_frame =
            self.state.frames.pop().ok_or_else(|| {
                Into::<Report>::into(RuntimeError::StackUnderflow { span: (0, 0) })
            })?;
        let popped_frame_stack_offset = popped_frame.stack_offset;

        FrameView {
            frame: &mut popped_frame,
            stack: &mut self.state.stack[..],
            vm_stack_offset: &mut self.state.stack_offset,
            globals: &mut self.globals,
            strings: &mut self.strings,
            open_upvalues: &mut self.open_upvalues,
        }
        .close_upvalues_beyond(popped_frame_stack_offset);

        // Pop the function and its arguments from the stack.
        while self.state.stack_offset > popped_frame.stack_offset {
            self.state.stack_offset -= 1;
            self.state.stack[self.state.stack_offset] = Value::Nil;
        }

        Ok(())
    }

    fn has_frames_remaining(&self) -> bool {
        !self.state.frames.is_empty()
    }

    fn call_value(&mut self, value: Value, arg_count: u8) -> Result<()> {
        match value {
            Value::Object(Object::Closure(closure)) => self.call_function(closure, arg_count),
            Value::Object(Object::BoundMethod(bound_method)) => {
                self.state.stack[self.state.stack_offset - (arg_count as usize) - 1] =
                    bound_method.receiver.clone();
                self.call_function(Arc::clone(&bound_method.method), arg_count)
            }
            Value::Object(Object::Class(class)) => {
                let new_instance = Value::Object(Object::Instance(InstanceData::of_class(&class)));
                self.state.stack[self.state.stack_offset - (arg_count as usize) - 1] = new_instance;

                if let Some(initializer) = class.lookup_method(&self.init_string) {
                    self.call_function(initializer, arg_count)
                } else if arg_count != 0 {
                    Err(RuntimeError::WrongNumberOfArguments {
                        name: format!("{}#init/0", class.name.to_string()),
                        got: arg_count,
                        expected: 0,
                        call_span: self.current_frame_view().previous_opcode_location()?.span,
                    }
                    .into())
                } else {
                    Ok(())
                }
            }
            Value::Object(Object::Native(native)) => {
                let arg_count = arg_count as usize;
                let native_frame_offset = self.state.stack_offset - arg_count - 1;
                let args = &self.state.stack[(native_frame_offset + 1)..self.state.stack_offset];
                let return_value = (native.function)(arg_count, args)?;

                // TODO(hollis): factor this "pop all the arguments and push the return value" behavior
                while self.state.stack_offset > native_frame_offset {
                    self.state.stack_offset -= 1;
                    self.state.stack[self.state.stack_offset] = Value::Nil;
                }

                self.state.stack[self.state.stack_offset] = return_value;
                self.state.stack_offset += 1;

                Ok(())
            }
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

    fn invoke(
        &mut self,
        receiver: Value,
        method_name: Arc<StringData>,
        arg_count: u8,
    ) -> Result<()> {
        if let Value::Object(Object::Instance(receiver)) = receiver {
            if let Some(field) = receiver.get_field(&method_name) {
                // We have a field with the given name, so try to call that instead of looking in
                // the method table
                self.state.stack[self.state.stack_offset - (arg_count as usize) - 1] =
                    field.clone();
                self.call_value(field, arg_count)
            } else {
                self.invoke_from_class(&receiver.class, method_name, arg_count)
            }
        } else {
            let invoke_location = self
                .current_frame_view()
                .previous_opcode_location()?
                .clone();
            Err(RuntimeError::BadPropertyAccess {
                receiver,
                property: method_name.to_string(),
                operation: "invoke",
                access_span: invoke_location.span,
            }
            .into())
        }
    }

    fn invoke_from_class(
        &mut self,
        class: &Arc<ClassData>,
        method_name: Arc<StringData>,
        arg_count: u8,
    ) -> Result<()> {
        if let Some(method_impl) = class.lookup_method(&method_name) {
            self.call_function(method_impl, arg_count)
        } else {
            Err(RuntimeError::NoSuchMethod {
                class_name: class.name.to_string(),
                method_name: method_name.to_string(),
                call_span: self.current_frame_view().previous_opcode_location()?.span,
            }
            .into())
        }
    }

    fn call_function(&mut self, closure: Arc<ClosureData>, arg_count: u8) -> Result<()> {
        if closure.function.arity != arg_count as usize {
            return Err(RuntimeError::WrongNumberOfArguments {
                name: closure.function.debug_name(),
                got: arg_count,
                expected: closure.function.arity,
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
            closure,
            ip: 0,
            stack_offset: self.state.stack_offset - (arg_count as usize) - 1,
        };
        Ok(self.state.frames.push(new_frame))
    }

    fn next_opcode(&mut self) -> Result<Opcode> {
        let current_frame = self.state.frames.last_mut().unwrap();
        let current_byte = current_frame
            .closure
            .function
            .chunk
            .byte(current_frame.ip)?;
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
            init_string: self.init_string,
            globals: self.globals,
            open_upvalues: self.open_upvalues,
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
            init_string: self.init_string,
            globals: self.globals,
            open_upvalues: self.open_upvalues,
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
            init_string: self.init_string,
            globals: self.globals,
            open_upvalues: self.open_upvalues,
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
    #[error("cannot {operation} property '{property}' of {receiver:?}")]
    #[diagnostic(
        code(runtime::bad_property_access),
        help("only instances have properties")
    )]
    BadPropertyAccess {
        receiver: Value,
        property: String,
        operation: &'static str,
        #[label("here")]
        access_span: (usize, usize),
    },
    #[error("instance {receiver:?} has no property '{property}'")]
    #[diagnostic(
        code(runtime::no_such_property),
        help("instance properties must be set before they can be read")
    )]
    NoSuchProperty {
        receiver: Value,
        property: String,
        #[label("accessed here")]
        access_span: (usize, usize),
    },
    #[error("class {class_name} has no method '{method_name}'")]
    #[diagnostic(code(runtime::no_such_method))]
    NoSuchMethod {
        class_name: String,
        method_name: String,
        #[label("called here")]
        call_span: (usize, usize),
    },
    #[error("bug in VM: {message}")]
    #[diagnostic(code(runtime::internal::bug))]
    Bug {
        message: String,
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
