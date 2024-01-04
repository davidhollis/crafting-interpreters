use miette::{Diagnostic, Report, Result};
use thiserror::Error;

use crate::{
    chunk::{Chunk, Opcode},
    scanner::{Scanner, SourceLocation, Token, TokenType},
    table::Table,
    value::Value,
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

struct Parser<'a> {
    scanner: Scanner<'a>,
    previous: Token<'a>,
    current: Token<'a>,
    chunk: Chunk,
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
}

fn declaration(parser: &mut Parser) -> Result<()> {
    if parser.match_token(TokenType::Var) {
        var_declaration(parser)
    } else {
        statement(parser)
    }
}

fn var_declaration(parser: &mut Parser) -> Result<()> {
    let var_location = parser.previous.location();
    parser.consume(TokenType::Identifier, "expected identifier after 'var'")?;
    let name_location = parser.previous.location();
    let global_id = variable_name(parser)?;

    if parser.match_token(TokenType::Equal) {
        expression(parser)?;
    } else {
        parser.emit_byte_at(Opcode::Nil as u8, var_location);
    }

    parser.consume(
        TokenType::Semicolon,
        "expected a semicolon at the end of a var declaration",
    )?;

    parser.emit_bytes_at(&[Opcode::DefineGlobal as u8, global_id], name_location);
    Ok(())
}

fn statement(parser: &mut Parser) -> Result<()> {
    if parser.match_token(TokenType::Print) {
        print_statement(parser)
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

fn number(parser: &mut Parser) -> Result<()> {
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

fn string(parser: &mut Parser) -> Result<()> {
    let string_literal = parser
        .chunk
        .strings
        .intern_string(&parser.previous.lexeme[1..(parser.previous.lexeme.len() - 1)]);
    let const_id = parser.make_constant(Value::Object(string_literal))?;
    Ok(parser.emit_bytes(&[Opcode::Constant as u8, const_id]))
}

fn variable_name(parser: &mut Parser) -> Result<u8> {
    let identifier_name = parser.chunk.strings.intern_string(&parser.previous.lexeme);
    parser.make_constant(Value::Object(identifier_name))
}

fn grouping(parser: &mut Parser) -> Result<()> {
    expression(parser)?;
    parser.consume(
        TokenType::RightParen,
        "expected ')' after grouped expression",
    )?;
    Ok(())
}

fn unary_op(parser: &mut Parser) -> Result<()> {
    let operator_type = parser.previous.tpe;
    let operator_location = parser.previous.location();

    parse_precedence(parser, Precedence::Unary)?;

    match operator_type {
        TokenType::Minus => Ok(parser.emit_byte_at(Opcode::Negate as u8, operator_location)),
        TokenType::Bang => Ok(parser.emit_byte_at(Opcode::Not as u8, operator_location)),
        _ => Err(ParseError::Bug("unary_op(unrecognized unary operator)").into()),
    }
}

fn binary_op(parser: &mut Parser) -> Result<()> {
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

fn literal(parser: &mut Parser) -> Result<()> {
    match parser.previous.tpe {
        TokenType::True => Ok(parser.emit_byte(Opcode::True as u8)),
        TokenType::False => Ok(parser.emit_byte(Opcode::False as u8)),
        TokenType::Nil => Ok(parser.emit_byte(Opcode::Nil as u8)),
        _ => Err(ParseError::Bug("literal(unrecognized literal type").into()),
    }
}

fn parse_precedence(parser: &mut Parser, precedence: Precedence) -> Result<()> {
    parser.advance();

    match ParseRule::rule_for(parser.previous.tpe).prefix {
        Some(prefix_action) => prefix_action(parser)?,
        None => return Err(ParseError::vanilla_unexpected(&parser.previous).into()),
    }

    while precedence <= ParseRule::rule_for(parser.current.tpe).precedence {
        parser.advance();
        match ParseRule::rule_for(parser.previous.tpe).infix {
            Some(infix_action) => infix_action(parser)?,
            None => return Err(ParseError::vanilla_unexpected(&parser.previous).into()),
        }
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

type Action = fn(&mut Parser) -> Result<()>;

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
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
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
