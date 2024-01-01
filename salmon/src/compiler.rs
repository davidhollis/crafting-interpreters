use miette::{Diagnostic, Report, Result};
use thiserror::Error;

use crate::{
    chunk::{Chunk, Opcode},
    scanner::{Scanner, Token, TokenType},
    value::Value,
};

pub fn compile(source_code: &str) -> Result<Chunk> {
    let mut parser = Parser::new(Scanner::new(source_code), Chunk::new());

    if let Err(err) = parser.parse() {
        return Err(err.with_source_code(source_code.to_string()));
    }

    parser.finalize()
}

pub fn compile_repl(source_code: &str, line: usize) -> Result<Chunk> {
    let mut parser = Parser::new(Scanner::new_line(source_code, line), Chunk::new());

    if let Err(err) = parser.parse() {
        return Err(err.with_source_code(source_code.to_string()));
    }

    parser.finalize()
}

struct Parser<'a> {
    scanner: Scanner<'a>,
    previous: Token<'a>,
    current: Token<'a>,
    chunk: Chunk,
    had_error: bool,
    panic_mode: bool,
}

impl<'a> Parser<'a> {
    fn new(scanner: Scanner<'a>, chunk: Chunk) -> Parser<'a> {
        Parser {
            scanner,
            previous: Token::empty(),
            current: Token::empty(),
            chunk,
            had_error: false,
            panic_mode: false,
        }
    }

    fn parse(&mut self) -> Result<()> {
        self.advance();
        expression(self)?;
        self.consume(TokenType::EOF, "expected end of expression")?;
        self.emit_byte(Opcode::Return as u8);
        Ok(())
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

    fn finalize(self) -> Result<Chunk> {
        if self.had_error {
            Err(ParseError::NoBytecode.into())
        } else {
            Ok(self.chunk)
        }
    }

    fn emit_byte(&mut self, byte: u8) -> () {
        let _ = self.chunk.write_byte(byte, self.previous.line);
    }

    fn emit_bytes(&mut self, bytes: &[u8]) -> () {
        for byte in bytes {
            let _ = self.chunk.write_byte(*byte, self.previous.line);
        }
    }

    fn report_and_continue(&mut self, err: Report) -> () {
        if !self.panic_mode {
            self.panic_mode = true;
            self.had_error = true;
            println!("{:?}", err);
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

fn expression(parser: &mut Parser) -> Result<()> {
    parse_precedence(parser, Precedence::Assignment)
}

fn number(parser: &mut Parser) -> Result<()> {
    match parser.previous.lexeme.parse() {
        Ok(num) => {
            let const_id = parser.make_constant(Value(num))?;
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
    // TODO(hollis): also save the token span for runtime error reporting...

    parse_precedence(parser, Precedence::Unary)?;

    match operator_type {
        TokenType::Minus => Ok(parser.emit_byte(Opcode::Negate as u8)), // TODO(hollis): ... and pass it in here
        _ => Err(ParseError::Bug("unary_op(unrecognized unary operator)").into()),
    }
}

fn binary_op(parser: &mut Parser) -> Result<()> {
    let operator_type = parser.previous.tpe;
    // TODO(hollis): save the span in a manner matching unary_op
    let rule = ParseRule::rule_for(operator_type);

    // We've already parsed the lefthand side, so now do the righthand side
    parse_precedence(parser, rule.precedence.next())?;

    match operator_type {
        TokenType::Plus => Ok(parser.emit_byte(Opcode::Add as u8)),
        TokenType::Minus => Ok(parser.emit_byte(Opcode::Subtract as u8)),
        TokenType::Star => Ok(parser.emit_byte(Opcode::Multiply as u8)),
        TokenType::Slash => Ok(parser.emit_byte(Opcode::Divide as u8)),
        _ => Err(ParseError::Bug("binary_op(unrecognized binary operator)").into()),
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
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
    // TokenType::BangEqual
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
    // TokenType::Equal
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
    // TokenType::EqualEqual
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
    // TokenType::Greater
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
    // TokenType::GreaterEqual
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
    // TokenType::Less
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
    // TokenType::LessEqual
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
    // TokenType::Identifier
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
    // TokenType::String
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
    // TokenType::Number
    ParseRule { prefix: Some(number), infix: None, precedence: Precedence::None },
    // TokenType::And
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
    // TokenType::Class
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
    // TokenType::Else
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
    // TokenType::False
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
    // TokenType::For
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
    // TokenType::Fun
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
    // TokenType::If
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
    // TokenType::Nil
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
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
    ParseRule { prefix: None, infix: None, precedence: Precedence::None },
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
    #[error("failed to produce bytecode due to previous error(s)")]
    #[diagnostic(code(compiler::no_bytecode), help("see above for more details"))]
    NoBytecode,
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
