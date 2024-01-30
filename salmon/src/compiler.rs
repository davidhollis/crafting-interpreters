use std::sync::Arc;

use miette::{Diagnostic, Report, Result};
use thiserror::Error;

use crate::{
    chunk::Opcode,
    object::{FunctionData, Object, StringData},
    scanner::{Scanner, SourceLocation, Token, TokenType},
    table::Table,
    value::Value,
    vm,
};

pub fn compile(source_code: &str, vm_strings: &Table) -> Result<Arc<FunctionData>> {
    let mut parser = Parser::new_with_strings(Scanner::new(source_code), vm_strings);

    parser.parse();
    parser.finalize()
}

pub fn compile_repl(
    source_code: &str,
    line: usize,
    starting_at: usize,
    vm_strings: &Table,
) -> Result<Arc<FunctionData>> {
    let mut parser = Parser::new_with_strings(
        Scanner::new_repl(source_code, line, starting_at),
        vm_strings,
    );

    parser.parse();
    parser.finalize()
}

struct LocalVariable<'a> {
    name: Token<'a>,
    depth: Option<u8>,
    is_captured: bool,
}

#[derive(PartialEq, Eq)]
enum UnitType {
    Function,
    Script,
}

struct Upvalue {
    index: u8,
    is_local: bool,
}

impl Upvalue {
    fn matches(&self, index: u8, is_local: bool) -> bool {
        self.index == index && self.is_local == is_local
    }
}

struct CompilerState<'a> {
    enclosing: Option<Box<CompilerState<'a>>>,
    compilation_unit: FunctionData,
    unit_type: UnitType,
    locals: Vec<LocalVariable<'a>>,
    scope_depth: u8,
    upvalues: Vec<Upvalue>,
}

impl<'a> CompilerState<'a> {
    fn new_toplevel(unit_type: UnitType) -> CompilerState<'static> {
        let mut compilation_unit = FunctionData::undefined();
        compilation_unit.name = Some(StringData::new("<toplevel>"));
        CompilerState {
            enclosing: None,
            compilation_unit,
            unit_type,
            locals: vec![LocalVariable {
                name: Token::blank_name(),
                depth: Some(0),
                is_captured: false,
            }],
            scope_depth: 0,
            upvalues: vec![],
        }
    }

    fn blank() -> CompilerState<'static> {
        CompilerState {
            enclosing: None,
            compilation_unit: FunctionData::undefined(),
            unit_type: UnitType::Script,
            locals: vec![],
            scope_depth: 0,
            upvalues: vec![],
        }
    }

    fn new_inner(self, name: Arc<StringData>, unit_type: UnitType) -> CompilerState<'a> {
        let mut compilation_unit = FunctionData::undefined();
        compilation_unit.name = Some(name);
        CompilerState {
            enclosing: Some(Box::new(self)),
            compilation_unit,
            unit_type,
            locals: vec![LocalVariable {
                name: Token::blank_name(),
                depth: Some(0),
                is_captured: false,
            }],
            scope_depth: 0,
            upvalues: vec![],
        }
    }

    fn finalize(
        mut self,
        source_location: SourceLocation,
    ) -> (CompilerState<'a>, Arc<FunctionData>, Vec<Upvalue>) {
        self.compilation_unit
            .chunk
            .write_byte(Opcode::Nil as u8, source_location.clone())
            .write_byte(Opcode::Return as u8, source_location);
        let finished_function = self.compilation_unit.finalize();
        let parent = self
            .enclosing
            .map(|outer| *outer)
            .unwrap_or_else(|| CompilerState::new_toplevel(UnitType::Script));

        (parent, finished_function, self.upvalues)
    }

    fn add_upvalue(
        &mut self,
        index: u8,
        is_local: bool,
        location: SourceLocation,
    ) -> Result<Option<u8>> {
        // If we've already captured this upvalue, just return the id of the existing one
        if let Some((upvalue_id, _)) = self
            .upvalues
            .iter()
            .enumerate()
            .find(|(_, u)| u.matches(index, is_local))
        {
            return Ok(Some(upvalue_id as u8));
        }

        // If not, add a new one and return its id
        if self.upvalues.len() >= u8::MAX as usize {
            return Err(ParseError::TooManyLocalVariables {
                token_span: location.span,
            }
            .into());
        }
        self.upvalues.push(Upvalue { index, is_local });
        self.compilation_unit.upvalue_count += 1;
        Ok(Some((self.compilation_unit.upvalue_count - 1) as u8))
    }
}

struct Parser<'a> {
    scanner: Scanner<'a>,
    previous: Token<'a>,
    current: Token<'a>,
    compiler: CompilerState<'a>,
    strings: Table,
    errors: Vec<ParseError>,
    panic_mode: bool,
}

impl<'a> Parser<'a> {
    fn new_with_strings(scanner: Scanner<'a>, preloaded_strings: &Table) -> Parser<'a> {
        let mut strings = Table::new();
        strings.add_all(preloaded_strings);
        Parser {
            scanner,
            previous: Token::empty(),
            current: Token::empty(),
            compiler: CompilerState::new_toplevel(UnitType::Script),
            strings,
            errors: vec![],
            panic_mode: false,
        }
    }

    fn parse(&mut self) -> () {
        self.advance();

        while !self.match_token(TokenType::EOF) {
            match declaration(self) {
                Ok(()) => (),
                Err(err) => self.report_and_continue(err),
            }

            if self.panic_mode {
                self.synchronize();
            }
        }
    }

    fn advance(&mut self) -> () {
        loop {
            match self.scanner.next_token() {
                Ok(tok) => {
                    self.previous = std::mem::replace(&mut self.current, tok);
                    break;
                }
                Err(err) => self.report_and_continue(err),
            }
        }
    }

    fn consume(&mut self, token_type: TokenType, err_message: &str) -> Result<()> {
        if self.current.tpe == token_type {
            self.advance();
            Ok(())
        } else {
            Err(ParseError::UnexpectedToken {
                message: err_message.to_string(),
                token_span: self.current.error_span(),
                line: self.current.line,
            }
            .into())
        }
    }

    fn match_token(&mut self, token_type: TokenType) -> bool {
        if self.current.tpe == token_type {
            self.advance();
            true
        } else {
            false
        }
    }

    fn check_token(&self, token_type: TokenType) -> bool {
        self.current.tpe == token_type
    }

    fn finalize(mut self) -> Result<Arc<FunctionData>> {
        if self.errors.is_empty() {
            self.compiler
                .compilation_unit
                .chunk
                .strings
                .add_all(&self.strings);
            let (_, function, _) = self.compiler.finalize(self.previous.location());
            Ok(function)
        } else {
            Err(ParseError::NoBytecode(self.errors).into())
        }
    }

    fn emit_byte(&mut self, byte: u8) -> () {
        let _ = self
            .compiler
            .compilation_unit
            .chunk
            .write_byte(byte, self.previous.location());
    }

    fn emit_byte_at(&mut self, byte: u8, location: SourceLocation) -> () {
        let _ = self
            .compiler
            .compilation_unit
            .chunk
            .write_byte(byte, location);
    }

    fn emit_bytes(&mut self, bytes: &[u8]) -> () {
        for byte in bytes {
            let _ = self
                .compiler
                .compilation_unit
                .chunk
                .write_byte(*byte, self.previous.location());
        }
    }

    fn emit_bytes_at(&mut self, bytes: &[u8], location: SourceLocation) -> () {
        for byte in bytes {
            let _ = self
                .compiler
                .compilation_unit
                .chunk
                .write_byte(*byte, location.clone());
        }
    }

    fn emit_jump(&mut self, jump_op: Opcode, location: SourceLocation) -> usize {
        self.emit_bytes_at(&[jump_op as u8, 0xFF, 0xFF], location);
        self.compiler.compilation_unit.chunk.len() - 2
    }

    fn patch_jump(&mut self, jump_arg_offset: usize) -> Result<()> {
        // forwards jump distance = (current chunk length) - (address of first jump argument byte) - (size of jump argument)
        let jump_distance = self.compiler.compilation_unit.chunk.len() - jump_arg_offset - 2;

        if jump_distance > u16::MAX as usize {
            return Err(ParseError::JumpTooLong {
                approx_jump_location: self
                    .compiler
                    .compilation_unit
                    .chunk
                    .location(jump_arg_offset)
                    .unwrap()
                    .span,
            }
            .into());
        }

        self.compiler
            .compilation_unit
            .chunk
            .patch(jump_arg_offset, ((jump_distance >> 8) & 0xFF) as u8)?; // most significant 8 bits
        self.compiler
            .compilation_unit
            .chunk
            .patch(jump_arg_offset + 1, (jump_distance & 0xFF) as u8) // least significant 8 bits
    }

    fn mark(&self) -> (usize, SourceLocation) {
        (
            self.compiler.compilation_unit.chunk.len(),
            self.previous.location(),
        )
    }

    fn emit_loop(&mut self, mark: (usize, SourceLocation)) -> Result<()> {
        // backwards jump distance = (loop instruction location) + (size of loop instruction) - (address of loop condition)
        let jump_distance = self.compiler.compilation_unit.chunk.len() + 3 - mark.0;

        if jump_distance > u16::MAX as usize {
            return Err(ParseError::JumpTooLong {
                approx_jump_location: mark.1.span,
            }
            .into());
        }

        let high_byte = ((jump_distance >> 8) & 0xFF) as u8;
        let low_byte = (jump_distance & 0xFF) as u8;

        self.emit_bytes_at(&[Opcode::Loop as u8, high_byte, low_byte], mark.1);

        Ok(())
    }

    fn report_and_continue(&mut self, err: Report) -> () {
        if !self.panic_mode {
            self.panic_mode = true;
            if err.is::<ParseError>() {
                self.errors.push(err.downcast().unwrap());
            } else {
                println!("{:?}", err);
            }
        }
    }

    fn synchronize(&mut self) -> () {
        self.panic_mode = false;

        loop {
            if self.previous.tpe == TokenType::Semicolon {
                // We're synchronized if the preceeding token was a semicolon
                return;
            }
            match self.current.tpe {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::If
                | TokenType::For
                | TokenType::While
                | TokenType::Print
                | TokenType::Return
                | TokenType::EOF => {
                    // We're synchronized if the next token is a reserved word that can start a statement
                    return;
                }
                // Otherwise, keep scanning to find a synchronization point
                _ => (),
            }

            self.advance();
        }
    }

    fn make_constant(&mut self, value: Value) -> Result<u8> {
        let const_id = self.compiler.compilation_unit.chunk.add_constant(value);
        if const_id > (u8::MAX as usize) {
            Err(ParseError::TooManyConstants {
                token_span: self.previous.error_span(),
            }
            .into())
        } else {
            Ok(const_id as u8)
        }
    }

    fn identifier_constant(&mut self, name_token: &Token) -> Result<u8> {
        let identifier_name = self.strings.intern_string(&name_token.lexeme);
        self.make_constant(Value::Object(Object::String(identifier_name)))
    }

    fn is_global_scope(&self) -> bool {
        self.compiler.scope_depth == 0
    }

    fn begin_scope(&mut self) -> () {
        self.compiler.scope_depth += 1;
    }

    fn end_scope(&mut self) -> () {
        self.compiler.scope_depth -= 1;

        // Clear out the locals from this scope
        while let Some(local) = self.compiler.locals.last() {
            if local.depth.unwrap_or(u8::MAX) <= self.compiler.scope_depth {
                // Once we hit a local that isn't from an inner scope, stop popping
                break;
            }

            if local.is_captured {
                self.emit_byte(Opcode::CloseUpvalue as u8);
            } else {
                self.emit_byte(Opcode::Pop as u8);
            }
            let _ = self.compiler.locals.pop();
        }
    }

    fn add_local_variable(&mut self, name_token: Token<'a>) -> Result<()> {
        if self.compiler.locals.len() == vm::STACK_SIZE {
            return Err(ParseError::TooManyLocalVariables {
                token_span: self.previous.error_span(),
            }
            .into());
        }
        self.compiler.locals.push(LocalVariable {
            name: name_token,
            depth: None,
            is_captured: false,
        });
        Ok(())
    }

    fn mark_last_initialized(&mut self) -> () {
        if let Some(last_local) = self.compiler.locals.last_mut() {
            last_local.depth = Some(self.compiler.scope_depth);
        }
    }

    fn new_compilation_unit(&mut self, name: Arc<StringData>, unit_type: UnitType) -> () {
        let current_compiler = std::mem::replace(&mut self.compiler, CompilerState::blank());
        self.compiler = current_compiler.new_inner(name, unit_type);
    }

    fn finish_compilation_unit(
        &mut self,
        source_location: SourceLocation,
    ) -> (Arc<FunctionData>, Vec<Upvalue>) {
        let current_compiler = std::mem::replace(&mut self.compiler, CompilerState::blank());
        let resulting_function: Arc<FunctionData>;
        let upvalues: Vec<Upvalue>;

        (self.compiler, resulting_function, upvalues) = current_compiler.finalize(source_location);

        (resulting_function, upvalues)
    }
}

// TODO(hollis): I'm not happy with the organization of this module
//               Some of these need to be separate functions so they can be included in the table,
//               but others could very realistically be associated functions on Parser

fn declaration(parser: &mut Parser) -> Result<()> {
    if parser.match_token(TokenType::Class) {
        class_declaration(parser)
    } else if parser.match_token(TokenType::Fun) {
        function_declaration(parser)
    } else if parser.match_token(TokenType::Var) {
        var_declaration(parser)
    } else {
        statement(parser)
    }
}

fn class_declaration(parser: &mut Parser) -> Result<()> {
    parser.consume(TokenType::Identifier, "expected a class name")?;
    let name_token = parser.previous.clone();
    let global_id = parser.identifier_constant(&name_token)?;
    let name_location = parser.previous.location();

    if !parser.is_global_scope() {
        declare_local_variable(parser, name_token)?;
        parser.mark_last_initialized();
    }

    parser.emit_bytes_at(&[Opcode::Class as u8, global_id], name_location.clone());

    if parser.is_global_scope() {
        define_global_variable(parser, global_id, name_location);
    }

    parser.consume(TokenType::LeftBrace, "expected '{' before a class body")?;
    parser.consume(TokenType::RightBrace, "expected '}' after a class body")?;

    Ok(())
}

fn function_declaration(parser: &mut Parser) -> Result<()> {
    let global_id = consume_variable_name(parser, "fun")?;
    let name_location = parser.previous.location();

    if !parser.is_global_scope() {
        parser.mark_last_initialized();
    }

    build_function(parser, UnitType::Function)?;

    if parser.is_global_scope() {
        define_global_variable(parser, global_id, name_location);
    }

    Ok(())
}

fn var_declaration(parser: &mut Parser) -> Result<()> {
    let var_location = parser.previous.location();
    let global_id = consume_variable_name(parser, "var")?;
    let name_location = parser.previous.location();

    if parser.match_token(TokenType::Equal) {
        expression(parser)?;
    } else {
        parser.emit_byte_at(Opcode::Nil as u8, var_location);
    }

    parser.consume(
        TokenType::Semicolon,
        "expected a ';' at the end of a var declaration",
    )?;

    if parser.is_global_scope() {
        // If we're in the global scope, emit an instruction to define a global with the value on
        // top of the stack.
        define_global_variable(parser, global_id, name_location);
    } else {
        // If we're not in the global scope, mark the local we just declared as initialized
        parser.mark_last_initialized();
    }

    Ok(())
}

fn consume_variable_name(parser: &mut Parser, keyword: &str) -> Result<u8> {
    parser.consume(
        TokenType::Identifier,
        &format!("expected identifier after '{}'", keyword),
    )?;
    let name_token = parser.previous.clone();

    if parser.is_global_scope() {
        parser.identifier_constant(&name_token)
    } else {
        declare_local_variable(parser, name_token)?;
        Ok(0)
    }
}

fn declare_local_variable<'a>(parser: &mut Parser<'a>, name_token: Token<'a>) -> Result<()> {
    // Scan for existing bindings in the same scope
    for existing_local in parser.compiler.locals.iter().rev() {
        match existing_local.depth {
            Some(d) if d < parser.compiler.scope_depth => {
                // Bindings are strictly ordered, so if we find a binding from an outer scope,
                // we can stop scanning.
                break;
            }
            _ => (),
        }

        if name_token.lexeme == existing_local.name.lexeme {
            return Err(ParseError::InvalidRedeclaration {
                name: name_token.lexeme.to_string(),
                duplicate_decl_span: name_token.error_span(),
                original_decl_span: existing_local.name.error_span(),
                line: name_token.line,
            }
            .into());
        }
    }

    parser.add_local_variable(name_token)
}

fn define_global_variable(
    parser: &mut Parser,
    global_id: u8,
    source_location: SourceLocation,
) -> () {
    parser.emit_bytes_at(&[Opcode::DefineGlobal as u8, global_id], source_location);
}

fn build_function(parser: &mut Parser, unit_type: UnitType) -> Result<()> {
    let function_name_location = parser.previous.location();
    let function_name_raw = parser.previous.lexeme;
    let function_name = parser.strings.intern_string(function_name_raw);
    parser.new_compilation_unit(function_name, unit_type);
    parser.begin_scope();

    parser.consume(
        TokenType::LeftParen,
        "expected a '(' after the name of a function declaration",
    )?;

    // Argument list
    if !parser.check_token(TokenType::RightParen) {
        loop {
            parser.compiler.compilation_unit.arity += 1;
            if parser.compiler.compilation_unit.arity > 255 {
                return Err(ParseError::TooManyArguments {
                    token_span: parser.current.error_span(),
                }
                .into());
            }
            let _ = consume_variable_name(parser, "argument list")?;
            parser.mark_last_initialized();

            if !parser.match_token(TokenType::Comma) {
                break;
            }
        }
    }

    parser.consume(
        TokenType::RightParen,
        "expected a ')' after the argument list of a function declaration",
    )?;
    parser.consume(
        TokenType::LeftBrace,
        "expected a '{' before a function body",
    )?;
    block_statement(parser)?;

    let (function, upvalues) = parser.finish_compilation_unit(parser.previous.location());
    let const_idx = parser.make_constant(Value::Object(Object::Function(function)))?;

    // Emit a closure instruction with upvalue indicators
    parser.emit_bytes_at(
        &[Opcode::Closure as u8, const_idx],
        function_name_location.clone(),
    );
    for upvalue in upvalues {
        parser.emit_byte_at(
            if upvalue.is_local { 1 } else { 0 },
            function_name_location.clone(),
        );
        parser.emit_byte_at(upvalue.index, function_name_location.clone());
    }

    Ok(())
}

fn statement(parser: &mut Parser) -> Result<()> {
    if parser.match_token(TokenType::Print) {
        print_statement(parser)
    } else if parser.match_token(TokenType::If) {
        if_statement(parser)
    } else if parser.match_token(TokenType::Return) {
        return_statement(parser)
    } else if parser.match_token(TokenType::While) {
        while_statement(parser)
    } else if parser.match_token(TokenType::For) {
        for_statement(parser)
    } else if parser.match_token(TokenType::LeftBrace) {
        parser.begin_scope();
        block_statement(parser)?;
        parser.end_scope();
        Ok(())
    } else {
        expression_statement(parser)
    }
}

fn print_statement(parser: &mut Parser) -> Result<()> {
    let print_location = parser.previous.location();
    expression(parser)?;
    parser.consume(
        TokenType::Semicolon,
        "expected a ';' at the end of a print statement",
    )?;
    parser.emit_byte_at(Opcode::Print as u8, print_location);
    Ok(())
}

fn if_statement(parser: &mut Parser) -> Result<()> {
    let if_location = parser.previous.location();
    parser.consume(TokenType::LeftParen, "expected a '(' after 'if'")?;
    expression(parser)?;
    parser.consume(
        TokenType::RightParen,
        "expected a ')' after the condition of an if statement",
    )?;

    let then_jump_offset = parser.emit_jump(Opcode::JumpIfFalse, if_location.clone());
    let rparen_location = parser.previous.location();
    parser.emit_byte(Opcode::Pop as u8);
    statement(parser)?;
    let else_jump_offset = parser.emit_jump(Opcode::Jump, if_location);
    parser.patch_jump(then_jump_offset)?;
    parser.emit_byte_at(Opcode::Pop as u8, rparen_location);

    if parser.match_token(TokenType::Else) {
        statement(parser)?;
    }

    parser.patch_jump(else_jump_offset)
}

fn return_statement(parser: &mut Parser) -> Result<()> {
    if parser.compiler.unit_type == UnitType::Script {
        return Err(ParseError::BadReturn {
            bad_return_span: parser.previous.error_span(),
        }
        .into());
    }

    if parser.match_token(TokenType::Semicolon) {
        Ok(parser.emit_bytes(&[Opcode::Nil as u8, Opcode::Return as u8]))
    } else {
        let return_location = parser.previous.location();
        expression(parser)?;
        parser.consume(
            TokenType::Semicolon,
            "expected a ';' at the end of a return statement",
        )?;
        Ok(parser.emit_byte_at(Opcode::Return as u8, return_location))
    }
}

fn while_statement(parser: &mut Parser) -> Result<()> {
    let loop_start_mark = parser.mark();

    parser.consume(TokenType::LeftParen, "expected a '(' after 'while'")?;
    expression(parser)?;
    parser.consume(
        TokenType::RightParen,
        "expected a ')' after the condition of a while loop",
    )?;
    let rparen_location = parser.previous.location();

    let done_jump_offset = parser.emit_jump(Opcode::JumpIfFalse, loop_start_mark.1.clone());
    parser.emit_byte(Opcode::Pop as u8);
    statement(parser)?;
    parser.emit_loop(loop_start_mark)?;
    parser.patch_jump(done_jump_offset)?;
    Ok(parser.emit_byte_at(Opcode::Pop as u8, rparen_location))
}

fn for_statement(parser: &mut Parser) -> Result<()> {
    let for_location = parser.previous.location();

    parser.begin_scope();

    // == Initializer expression ==
    parser.consume(TokenType::LeftParen, "expected a '(' after 'for'")?;
    if parser.match_token(TokenType::Semicolon) {
        // Empty initializer expression. Emit no bytecode.
    } else if parser.match_token(TokenType::Var) {
        // Bind a local variable in the initializer.
        var_declaration(parser)?;
    } else {
        // Just execute an expression, pop, and consume the ;
        expression_statement(parser)?;
    }

    // == Condition expression ==
    let (loop_start_addr, _) = parser.mark();
    let loop_start_mark = (loop_start_addr, for_location.clone());
    let done_jump_offset = if parser.match_token(TokenType::Semicolon) {
        // No condition expression, so we don't emit a conditional jump
        None
    } else {
        expression(parser)?;
        parser.consume(
            TokenType::Semicolon,
            "expected a ';' after a for loop's condition expression",
        )?;
        let offset = parser.emit_jump(Opcode::JumpIfFalse, for_location.clone());
        parser.emit_byte_at(Opcode::Pop as u8, for_location.clone());
        Some(offset)
    };

    // == Increment expression ==
    let increment_start_mark = if parser.match_token(TokenType::RightParen) {
        // No increment expression, so the end of the body will just jump straight to the head of the loop
        None
    } else {
        // We have an increment expression, so:

        // 1. If the condition is true, we jump over this segment and into the body
        let body_jump_offset = parser.emit_jump(Opcode::Jump, parser.previous.location());
        // 2. At the end of the body, we jump here (the head of the increment)
        let increment_start = parser.mark();
        expression(parser)?;
        parser.consume(
            TokenType::RightParen,
            "expected a ')' after a for loop's increment expression",
        )?;
        parser.emit_byte(Opcode::Pop as u8);
        // 3. After the increment expression has run, we unconditionally jump back to the head of the condition
        parser.emit_loop(loop_start_mark.clone())?;
        parser.patch_jump(body_jump_offset)?;
        Some(increment_start)
    };

    // == Loop body ==
    statement(parser)?;
    parser.emit_loop(increment_start_mark.unwrap_or(loop_start_mark))?;

    // == Post-loop cleanup ==
    if let Some(done_jump_offset) = done_jump_offset {
        parser.patch_jump(done_jump_offset)?;
        parser.emit_byte_at(Opcode::Pop as u8, for_location)
    }
    Ok(parser.end_scope())
}

fn block_statement(parser: &mut Parser) -> Result<()> {
    while !parser.check_token(TokenType::RightBrace) && !parser.check_token(TokenType::EOF) {
        declaration(parser)?;
    }

    parser.consume(TokenType::RightBrace, "expected '}' at the end of a block")
}

fn expression_statement(parser: &mut Parser) -> Result<()> {
    expression(parser)?;
    parser.consume(
        TokenType::Semicolon,
        "expected a ';' at the end of an expression statement",
    )?;
    parser.emit_byte(Opcode::Pop as u8);
    Ok(())
}

fn expression(parser: &mut Parser) -> Result<()> {
    parse_precedence(parser, Precedence::Assignment)
}

fn number(parser: &mut Parser, _can_assign: bool) -> Result<()> {
    match parser.previous.lexeme.parse() {
        Ok(num) => {
            let const_id = parser.make_constant(Value::Number(num))?;
            Ok(parser.emit_bytes(&[Opcode::Constant as u8, const_id]))
        }
        Err(_err) => Err(ParseError::BadLiteral {
            kind: "number",
            token_span: parser.previous.error_span(),
            line: parser.previous.line,
        }
        .into()),
    }
}

fn string(parser: &mut Parser, _can_assign: bool) -> Result<()> {
    let string_literal = parser
        .strings
        .intern_string(&parser.previous.lexeme[1..(parser.previous.lexeme.len() - 1)]);
    let const_id = parser.make_constant(Value::Object(Object::String(string_literal)))?;
    Ok(parser.emit_bytes(&[Opcode::Constant as u8, const_id]))
}

fn variable(parser: &mut Parser, can_assign: bool) -> Result<()> {
    resolve_variable_expression(parser, can_assign)
}

fn resolve_variable_expression(parser: &mut Parser, can_assign: bool) -> Result<()> {
    let name_token = parser.previous.clone();

    let (get_op, set_op, idx) = match resolve_local(&parser.compiler, &name_token)? {
        Some(stack_index) => (Opcode::GetLocal as u8, Opcode::SetLocal as u8, stack_index),
        None => match resolve_upvalue(&mut parser.compiler, &name_token)? {
            Some(upvalue_id) => (
                Opcode::GetUpvalue as u8,
                Opcode::SetUpvalue as u8,
                upvalue_id,
            ),
            None => (
                Opcode::GetGlobal as u8,
                Opcode::SetGlobal as u8,
                parser.identifier_constant(&name_token)?,
            ),
        },
    };

    if can_assign && parser.match_token(TokenType::Equal) {
        let equal_location = parser.previous.location();
        expression(parser)?;
        Ok(parser.emit_bytes_at(&[set_op, idx], equal_location))
    } else {
        Ok(parser.emit_bytes(&[get_op, idx]))
    }
}

fn resolve_local(compiler_state: &CompilerState, name_token: &Token) -> Result<Option<u8>> {
    for (stack_idx, local) in compiler_state.locals.iter().enumerate().rev() {
        if local.name.lexeme == name_token.lexeme {
            // Found a local binding for this variable
            if local.depth.is_none() {
                return Err(ParseError::UninitializedRead {
                    name: name_token.lexeme.to_string(),
                    bad_use_span: name_token.error_span(),
                    line: name_token.line,
                }
                .into());
            } else {
                return Ok(Some(stack_idx as u8));
            }
        }
    }

    // We didn't find a local binding, so the variable must belong to an enclosing function scope
    Ok(None)
}

fn resolve_upvalue(compiler_state: &mut CompilerState, name_token: &Token) -> Result<Option<u8>> {
    // Check the enclosing function scope
    if let Some(ref mut enclosing_compiler_state) = &mut compiler_state.enclosing {
        // If the enclosing function had a local variable with this name, create an upvalue pointing to it
        if let Some(local_idx) = resolve_local(&enclosing_compiler_state, name_token)? {
            enclosing_compiler_state.locals[local_idx as usize].is_captured = true;
            return compiler_state.add_upvalue(local_idx, true, name_token.location());
        }

        // If the enclosing function has (or could have) an upvalue pointing at a local variable even
        // further up, create an upvalue pointing indirectly at it
        if let Some(upvalue_id) = resolve_upvalue(enclosing_compiler_state, name_token)? {
            return compiler_state.add_upvalue(upvalue_id, false, name_token.location());
        }
    }

    // If there is no enclosign scope or we didn't find a matching local or upvalue, then we
    // couldn't resolve it. It must be a global.
    Ok(None)
}

fn grouping(parser: &mut Parser, _can_assign: bool) -> Result<()> {
    expression(parser)?;
    parser.consume(
        TokenType::RightParen,
        "expected ')' after grouped expression",
    )?;
    Ok(())
}

fn call(parser: &mut Parser, _can_assign: bool) -> Result<()> {
    let open_paren_location = parser.previous.location();
    let arg_count = argument_list(parser)?;
    Ok(parser.emit_bytes_at(&[Opcode::Call as u8, arg_count], open_paren_location))
}

fn argument_list(parser: &mut Parser) -> Result<u8> {
    let mut arg_count = 0;
    if !parser.check_token(TokenType::RightParen) {
        loop {
            expression(parser)?;
            if arg_count == u8::MAX {
                return Err(ParseError::TooManyArguments {
                    token_span: parser.previous.error_span(),
                }
                .into());
            }
            arg_count += 1;
            if !parser.match_token(TokenType::Comma) {
                break;
            }
        }
    }

    parser.consume(
        TokenType::RightParen,
        "expected a ')' after the argument list in a call expression",
    )?;

    Ok(arg_count)
}

fn unary_op(parser: &mut Parser, _can_assign: bool) -> Result<()> {
    let operator_type = parser.previous.tpe;
    let operator_location = parser.previous.location();

    parse_precedence(parser, Precedence::Unary)?;

    match operator_type {
        TokenType::Minus => Ok(parser.emit_byte_at(Opcode::Negate as u8, operator_location)),
        TokenType::Bang => Ok(parser.emit_byte_at(Opcode::Not as u8, operator_location)),
        _ => Err(ParseError::Bug("unary_op(unrecognized unary operator)").into()),
    }
}

fn binary_op(parser: &mut Parser, _can_assign: bool) -> Result<()> {
    let operator_type = parser.previous.tpe;
    let operator_location = parser.previous.location();
    let rule = ParseRule::rule_for(operator_type);

    // We've already parsed the lefthand side, so now do the righthand side
    parse_precedence(parser, rule.precedence.next())?;

    match operator_type {
        TokenType::BangEqual => {
            Ok(parser.emit_bytes_at(&[Opcode::Equal as u8, Opcode::Not as u8], operator_location))
        }
        TokenType::EqualEqual => Ok(parser.emit_byte_at(Opcode::Equal as u8, operator_location)),
        TokenType::Greater => Ok(parser.emit_byte_at(Opcode::Greater as u8, operator_location)),
        TokenType::GreaterEqual => {
            Ok(parser.emit_bytes_at(&[Opcode::Less as u8, Opcode::Not as u8], operator_location))
        }
        TokenType::Less => Ok(parser.emit_byte_at(Opcode::Less as u8, operator_location)),
        TokenType::LessEqual => Ok(parser.emit_bytes_at(
            &[Opcode::Greater as u8, Opcode::Not as u8],
            operator_location,
        )),
        TokenType::Plus => Ok(parser.emit_byte_at(Opcode::Add as u8, operator_location)),
        TokenType::Minus => Ok(parser.emit_byte_at(Opcode::Subtract as u8, operator_location)),
        TokenType::Star => Ok(parser.emit_byte_at(Opcode::Multiply as u8, operator_location)),
        TokenType::Slash => Ok(parser.emit_byte_at(Opcode::Divide as u8, operator_location)),
        _ => Err(ParseError::Bug("binary_op(unrecognized binary operator)").into()),
    }
}

fn logical_and(parser: &mut Parser, _can_assign: bool) -> Result<()> {
    let short_circuit_jump_offset =
        parser.emit_jump(Opcode::JumpIfFalse, parser.previous.location());
    parser.emit_byte(Opcode::Pop as u8);

    parse_precedence(parser, Precedence::And)?;

    parser.patch_jump(short_circuit_jump_offset)
}

fn logical_or(parser: &mut Parser, _can_assign: bool) -> Result<()> {
    let evaluate_jump_offset = parser.emit_jump(Opcode::JumpIfFalse, parser.previous.location());
    let short_circuit_jump_offset = parser.emit_jump(Opcode::Jump, parser.previous.location());

    parser.patch_jump(evaluate_jump_offset)?;
    parser.emit_byte(Opcode::Pop as u8);

    parse_precedence(parser, Precedence::Or)?;
    parser.patch_jump(short_circuit_jump_offset)
}

fn literal(parser: &mut Parser, _can_assign: bool) -> Result<()> {
    match parser.previous.tpe {
        TokenType::True => Ok(parser.emit_byte(Opcode::True as u8)),
        TokenType::False => Ok(parser.emit_byte(Opcode::False as u8)),
        TokenType::Nil => Ok(parser.emit_byte(Opcode::Nil as u8)),
        _ => Err(ParseError::Bug("literal(unrecognized literal type").into()),
    }
}

fn parse_precedence(parser: &mut Parser, precedence: Precedence) -> Result<()> {
    let can_assign = precedence <= Precedence::Assignment;

    parser.advance();

    match ParseRule::rule_for(parser.previous.tpe).prefix {
        Some(prefix_action) => prefix_action(parser, can_assign)?,
        None => return Err(ParseError::vanilla_unexpected(&parser.previous).into()),
    }

    while precedence <= ParseRule::rule_for(parser.current.tpe).precedence {
        parser.advance();
        match ParseRule::rule_for(parser.previous.tpe).infix {
            Some(infix_action) => infix_action(parser, can_assign)?,
            None => return Err(ParseError::vanilla_unexpected(&parser.previous).into()),
        }
    }

    if can_assign && parser.match_token(TokenType::Equal) {
        let equal_location = parser.previous.location();
        return Err(ParseError::InvalidAssignmentTarget {
            equal_token_span: equal_location.span,
            line: equal_location.line,
        }
        .into());
    }

    Ok(())
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Clone, Copy)]
#[repr(usize)]
enum Precedence {
    None = 0,
    Assignment, // =
    Or,         // or
    And,        // and
    Equality,   // ==  !=
    Comparison, // <  >  <=  >=
    Term,       // +  -
    Factor,     // *  /
    Unary,      // !  -
    Call,       // .  ()
    Primary,
}

impl Precedence {
    fn next(&self) -> Precedence {
        (*self as usize + 1).into()
    }
}

impl From<usize> for Precedence {
    fn from(value: usize) -> Self {
        match value {
            x if x == Precedence::Assignment as usize => Precedence::Assignment,
            x if x == Precedence::Or as usize => Precedence::Or,
            x if x == Precedence::And as usize => Precedence::And,
            x if x == Precedence::Equality as usize => Precedence::Equality,
            x if x == Precedence::Comparison as usize => Precedence::Comparison,
            x if x == Precedence::Term as usize => Precedence::Term,
            x if x == Precedence::Factor as usize => Precedence::Factor,
            x if x == Precedence::Unary as usize => Precedence::Unary,
            x if x == Precedence::Call as usize => Precedence::Call,
            x if x == Precedence::Primary as usize => Precedence::Primary,
            _ => Precedence::None,
        }
    }
}

type Action = fn(&mut Parser, bool) -> Result<()>;

struct ParseRule {
    prefix: Option<Action>,
    infix: Option<Action>,
    precedence: Precedence,
}

impl ParseRule {
    fn rule_for(token_type: TokenType) -> &'static ParseRule {
        &RULES[token_type as usize]
    }
}

#[rustfmt::skip]
const RULES: [ParseRule; 39] = [
    // TokenType::LeftParen
    ParseRule { prefix: Some(grouping), infix: Some(call), precedence: Precedence::Call },
    // TokenType::RightParen
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
    // TokenType::LeftBrace
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
    // TokenType::RightBrace
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
    // TokenType::Comma
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
    // TokenType::Dot
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
    // TokenType::Minus
    ParseRule { prefix: Some(unary_op), infix: Some(binary_op), precedence: Precedence::Term },
    // TokenType::Plus
    ParseRule { prefix: None, infix: Some(binary_op), precedence: Precedence::Term },
    // TokenType::Semicolon
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
    // TokenType::Slash
    ParseRule { prefix: None, infix: Some(binary_op), precedence: Precedence::Factor },
    // TokenType::Star
    ParseRule { prefix: None, infix: Some(binary_op), precedence: Precedence::Factor },
    // TokenType::Bang
    ParseRule { prefix: Some(unary_op), infix: None, precedence: Precedence::None },
    // TokenType::BangEqual
    ParseRule { prefix: None, infix: Some(binary_op), precedence: Precedence::Equality },
    // TokenType::Equal
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
    // TokenType::EqualEqual
    ParseRule { prefix: None, infix: Some(binary_op), precedence: Precedence::Equality },
    // TokenType::Greater
    ParseRule { prefix: None, infix: Some(binary_op), precedence: Precedence::Equality },
    // TokenType::GreaterEqual
    ParseRule { prefix: None, infix: Some(binary_op), precedence: Precedence::Equality },
    // TokenType::Less
    ParseRule { prefix: None, infix: Some(binary_op), precedence: Precedence::Equality },
    // TokenType::LessEqual
    ParseRule { prefix: None, infix: Some(binary_op), precedence: Precedence::Equality },
    // TokenType::Identifier
    ParseRule { prefix: Some(variable), infix: None, precedence: Precedence::None },
    // TokenType::String
    ParseRule { prefix: Some(string), infix: None, precedence: Precedence::None },
    // TokenType::Number
    ParseRule { prefix: Some(number), infix: None, precedence: Precedence::None },
    // TokenType::And
    ParseRule { prefix: None, infix: Some(logical_and), precedence: Precedence::And },
    // TokenType::Class
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
    // TokenType::Else
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
    // TokenType::False
    ParseRule { prefix: Some(literal), infix: None, precedence: Precedence::None },
    // TokenType::For
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
    // TokenType::Fun
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
    // TokenType::If
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
    // TokenType::Nil
    ParseRule { prefix: Some(literal), infix: None, precedence: Precedence::None },
    // TokenType::Or
    ParseRule { prefix: None, infix: Some(logical_or), precedence: Precedence::Or },
    // TokenType::Print
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
    // TokenType::Return
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
    // TokenType::Super
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
    // TokenType::This
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
    // TokenType::True
    ParseRule { prefix: Some(literal), infix: None, precedence: Precedence::None },
    // TokenType::Var
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
    // TokenType::While
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
    // TokenType::EOF
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
];

#[derive(Error, Debug, Diagnostic)]
pub enum ParseError {
    #[error("{message} at line {line}")]
    #[diagnostic(code(parser::unexpected_token))]
    UnexpectedToken {
        message: String,
        #[label("here")]
        token_span: (usize, usize),
        line: usize,
    },
    #[error("bad {kind} literal at line {line}")]
    #[diagnostic(code(parser::bad_literal))]
    BadLiteral {
        kind: &'static str,
        #[label("here")]
        token_span: (usize, usize),
        line: usize,
    },
    #[error("too many constants in one compilation unit")]
    #[diagnostic(
        code(compiler::too_many_constants),
        help("try wrapping some constants in a function definition")
    )]
    TooManyConstants {
        #[label("the 256th constant")]
        token_span: (usize, usize),
    },
    #[error("too many local variables in one compilation unit")]
    #[diagnostic(code(compiler::too_many_locals), help("this is a compiler limitation"))]
    TooManyLocalVariables {
        #[label("last variable defined here")]
        token_span: (usize, usize),
    },
    #[error("too many arguments to one function or method")]
    #[diagnostic(code(compiler::too_many_args), help("this is a compiler limitation"))]
    TooManyArguments {
        #[label("last argument defined here")]
        token_span: (usize, usize),
    },
    #[error("invalid assignment target on line {line}")]
    #[diagnostic(
        code(compiler::invalid_assignment_target),
        help("The expression to the left of this '=' cannot be assigned to. Only object fields and variables can be assigned to.")
    )]
    InvalidAssignmentTarget {
        #[label("this assignment expression")]
        equal_token_span: (usize, usize),
        line: usize,
    },
    #[error("attempted to redeclare variable '{name}' on line {line}")]
    #[diagnostic(
        code(compiler::invalid_redeclaration),
        help("Variables cannot be redeclared in the same scope. If you want to reassign {name}, try removing the 'var' from the second assignment.")
    )]
    InvalidRedeclaration {
        name: String,
        #[label("attempt to redeclare here")]
        duplicate_decl_span: (usize, usize),
        #[label("original declaration here")]
        original_decl_span: (usize, usize),
        line: usize,
    },
    #[error("attempted to use variable '{name}' in its own initializer")]
    #[diagnostic(
        code(compiler::uninitialized_read),
        help("If you're intentionally trying to shadow another variable, consider explicitly doing something like `var _{name} = {name}; var {name} = _{name};`")
    )]
    UninitializedRead {
        name: String,
        #[label("here")]
        bad_use_span: (usize, usize),
        line: usize,
    },
    #[error("attempted to jump over more than 65535 bytes")]
    #[diagnostic(
        code(compiler::internal::jump_too_long),
        help("This is due to an internal limitation.")
    )]
    JumpTooLong {
        approx_jump_location: (usize, usize),
    },
    #[error("attempted to return from a toplevel scope")]
    #[diagnostic(
        code(compiler::bad_return),
        help("Return statements are only valid with function or method bodies.")
    )]
    BadReturn {
        #[label("here")]
        bad_return_span: (usize, usize),
    },
    #[error("failed to produce bytecode due to the following error(s)")]
    #[diagnostic(code(compiler::no_bytecode))]
    NoBytecode(#[related] Vec<ParseError>),
    #[error("bug in compiler at {0}. Condition should be unreachable.")]
    #[diagnostic(code(compiler::bug))]
    Bug(&'static str),
}

impl ParseError {
    fn vanilla_unexpected(token: &Token) -> ParseError {
        ParseError::UnexpectedToken {
            message: format!("unexpected {:?}", token.tpe),
            token_span: token.error_span(),
            line: token.line,
        }
    }
}
