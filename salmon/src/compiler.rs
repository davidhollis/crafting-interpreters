use miette::Result;

use crate::{
    chunk::Chunk,
    scanner::{Scanner, TokenType},
};

pub fn compile(source_code: &str) -> Result<Chunk> {
    let mut line = 0;
    let mut scanner = Scanner::new(source_code);

    loop {
        match scanner.next_token() {
            Ok(token) => {
                if token.line != line {
                    print!("{:4} ", token.line);
                    line = token.line;
                } else {
                    print!("   | ");
                }

                println!("{:<12} '{}'", format!("{:?}", token.tpe), token.lexeme);

                if token.tpe == TokenType::EOF {
                    break;
                }
            }
            Err(scanner_error) => {
                return Err(scanner_error.with_source_code(source_code.to_string()))
            }
        }
    }

    let empty_chunk = Chunk::new();
    Ok(empty_chunk)
}

pub fn compile_repl(_source_code: &str, _line: usize) -> Result<Chunk> {
    let empty_chunk = Chunk::new();
    Ok(empty_chunk)
}
