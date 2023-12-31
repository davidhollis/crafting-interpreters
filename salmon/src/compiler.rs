use miette::{Diagnostic, Report, Result};
use thiserror::Error;

use crate::{
    chunk::{Chunk, Opcode},
    scanner::{Scanner, SourceLocation, Token, TokenType},
    table::Table,
    value::Value,
    vm,
};

pub fn compile(source_code: &str) -> Result<Chunk> {
    let mut parser = Parser::new(Scanner::new(source_code), Chunk::new());

    parser.parse();
    parser.finalize()
}

pub fn compile_repl(source_code: &str, line: usize, vm_strings: &Table) -> Result<Chunk> {
    // TODO(hollis): We should handle "unexpected EOF" differently here, preserving the parser
    //               state so that we can continue parsing across another line.
    let mut parser = Parser::new(
        Scanner::new_line(source_code, line),
        Chunk::new_with_strings(vm_strings),
    );

    parser.parse();
    parser.finalize()
}

struct LocalVariable<'a> {
    name: Token<'a>,
    depth: Option<u8>,
}

struct Parser<'a> {
    scanner: Scanner<'a>,
    previous: Token<'a>,
    current: Token<'a>,
    chunk: Chunk,
    locals: Vec<LocalVariable<'a>>,
    scope_depth: u8,
    errors: Vec<ParseError>,
    panic_mode: bool,
}

impl<'a> Parser<'a> {
    fn new(scanner: Scanner<'a>, chunk: Chunk) -> Parser<'a> {
        Parser {
            scanner,
            previous: Token::empty(),
            current: Token::empty(),
            chunk,
            locals: vec![],
            scope_depth: 0,
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

        self.emit_byte(Opcode::Return as u8);
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

    fn finalize(self) -> Result<Chunk> {
        if self.errors.is_empty() {
            Ok(self.chunk)
        } else {
            Err(ParseError::NoBytecode(self.errors).into())
        }
    }

    fn emit_byte(&mut self, byte: u8) -> () {
        let _ = self.chunk.write_byte(byte, self.previous.location());
    }

    fn emit_byte_at(&mut self, byte: u8, location: SourceLocation) -> () {
        let _ = self.chunk.write_byte(byte, location);
    }

    fn emit_bytes(&mut self, bytes: &[u8]) -> () {
        for byte in bytes {
            let _ = self.chunk.write_byte(*byte, self.previous.location());
        }
    }

    fn emit_bytes_at(&mut self, bytes: &[u8], location: SourceLocation) -> () {
        for byte in bytes {
            let _ = self.chunk.write_byte(*byte, location.clone());
        }
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
        let const_id = self.chunk.add_constant(value);
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
        let identifier_name = self.chunk.strings.intern_string(&name_token.lexeme);
        self.make_constant(Value::Object(identifier_name))
    }

    fn begin_scope(&mut self) -> () {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) -> () {
        self.scope_depth -= 1;

        // Clear out the locals from this scope
        while let Some(local) = self.locals.last() {
            if local.depth.unwrap_or(u8::MAX) <= self.scope_depth {
                // Once we hit a local that isn't from an inner scope, stop popping
                break;
            }

            self.emit_byte(Opcode::Pop as u8);
            let _ = self.locals.pop();
        }
    }

    fn add_local_variable(&mut self, name_token: Token<'a>) -> Result<()> {
        if self.locals.len() == vm::STACK_SIZE {
            return Err(ParseError::TooManyLocalVariables {
                token_span: self.previous.error_span(),
            }
            .into());
        }
        self.locals.push(LocalVariable {
            name: name_token,
            depth: None,
        });
        Ok(())
    }

    fn mark_last_initialized(&mut self) -> () {
        if let Some(last_local) = self.locals.last_mut() {
            last_local.depth = Some(self.scope_depth);
        }
    }
}

// TODO(hollis): I'm not happy with the organization of this module
//               Some of these need to be separate functions so they can be included in the table,
//               but others could very realistically be associated functions on Parser

fn declaration(parser: &mut Parser) -> Result<()> {
    if parser.match_token(TokenType::Var) {
        var_declaration(parser)
    } else {
        statement(parser)
    }
}

fn var_declaration(parser: &mut Parser) -> Result<()> {
    let var_location = parser.previous.location();
    let global_id = consume_variable_name(parser)?;
    let name_location = parser.previous.location();

    if parser.match_token(TokenType::Equal) {
        expression(parser)?;
    } else {
        parser.emit_byte_at(Opcode::Nil as u8, var_location);
    }

    parser.consume(
        TokenType::Semicolon,
        "expected a semicolon at the end of a var declaration",
    )?;

    if parser.scope_depth == 0 {
        // If we're in the global scope, emit an instruction to define a global with the value on
        // top of the stack.
        define_global_variable(parser, global_id, name_location);
    } else {
        // If we're not in the global scope, mark the local we just declared as initialized
        parser.mark_last_initialized();
    }

    Ok(())
}

fn consume_variable_name(parser: &mut Parser) -> Result<u8> {
    parser.consume(TokenType::Identifier, "expected identifier after 'var'")?;
    let name_token = parser.previous.clone();

    if parser.scope_depth > 0 {
        declare_local_variable(parser, name_token)?;
        Ok(0)
    } else {
        parser.identifier_constant(&name_token)
    }
}

fn declare_local_variable<'a>(parser: &mut Parser<'a>, name_token: Token<'a>) -> Result<()> {
    // Scan for existing bindings in the same scope
    for existing_local in parser.locals.iter().rev() {
        match existing_local.depth {
            Some(d) if d < parser.scope_depth => {
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

fn statement(parser: &mut Parser) -> Result<()> {
    if parser.match_token(TokenType::Print) {
        print_statement(parser)
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
        "expected a semicolon at the end of a print statement",
    )?;
    parser.emit_byte_at(Opcode::Print as u8, print_location);
    Ok(())
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
        "expected a semicolon at the end of an expression statement",
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
        .chunk
        .strings
        .intern_string(&parser.previous.lexeme[1..(parser.previous.lexeme.len() - 1)]);
    let const_id = parser.make_constant(Value::Object(string_literal))?;
    Ok(parser.emit_bytes(&[Opcode::Constant as u8, const_id]))
}

fn variable(parser: &mut Parser, can_assign: bool) -> Result<()> {
    resolve_variable_expression(parser, can_assign)
}

fn resolve_variable_expression(parser: &mut Parser, can_assign: bool) -> Result<()> {
    let name_token = parser.previous.clone();

    let (get_op, set_op, idx) = match resolve_local(parser, &name_token)? {
        Some(stack_index) => (Opcode::GetLocal as u8, Opcode::SetLocal as u8, stack_index),
        None => (
            Opcode::GetGlobal as u8,
            Opcode::SetGlobal as u8,
            parser.identifier_constant(&name_token)?,
        ),
    };

    if can_assign && parser.match_token(TokenType::Equal) {
        let equal_location = parser.previous.location();
        expression(parser)?;
        Ok(parser.emit_bytes_at(&[set_op, idx], equal_location))
    } else {
        Ok(parser.emit_bytes(&[get_op, idx]))
    }
}

fn resolve_local(parser: &Parser, name_token: &Token) -> Result<Option<u8>> {
    for (stack_idx, local) in parser.locals.iter().enumerate().rev() {
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

    // We didn't find a local binding, so the variable must be a global
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
    ParseRule { prefix: Some(grouping), infix: None, precedence: Precedence::None },
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
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
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
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
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
