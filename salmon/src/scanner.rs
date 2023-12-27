use std::{
    iter::Peekable,
    str::{CharIndices, Chars},
};

use miette::{Diagnostic, Result};
use thiserror::Error;

#[derive(Debug, PartialEq, Eq)]
pub enum TokenType {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    // One- or two-character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Literals
    Identifier,
    String,
    Number,
    // Keywords
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    // Zero-width tokens
    EOF,
}

impl TokenType {
    pub fn classify(identifier: &str) -> TokenType {
        let mut id_iter = identifier.chars();

        match id_iter.next() {
            Some('a') => TokenType::check_keyword(id_iter, "nd", TokenType::And),
            Some('c') => TokenType::check_keyword(id_iter, "lass", TokenType::Class),
            Some('e') => TokenType::check_keyword(id_iter, "lse", TokenType::Else),
            Some('f') => match id_iter.next() {
                Some('a') => TokenType::check_keyword(id_iter, "lse", TokenType::False),
                Some('o') => TokenType::check_keyword(id_iter, "r", TokenType::For),
                Some('u') => TokenType::check_keyword(id_iter, "n", TokenType::Fun),
                _ => TokenType::Identifier,
            },
            Some('i') => TokenType::check_keyword(id_iter, "f", TokenType::If),
            Some('n') => TokenType::check_keyword(id_iter, "il", TokenType::Nil),
            Some('o') => TokenType::check_keyword(id_iter, "r", TokenType::Or),
            Some('p') => TokenType::check_keyword(id_iter, "rint", TokenType::Print),
            Some('r') => TokenType::check_keyword(id_iter, "eturn", TokenType::Return),
            Some('s') => TokenType::check_keyword(id_iter, "uper", TokenType::Super),
            Some('t') => match id_iter.next() {
                Some('h') => TokenType::check_keyword(id_iter, "is", TokenType::This),
                Some('r') => TokenType::check_keyword(id_iter, "ue", TokenType::True),
                _ => TokenType::Identifier,
            },
            Some('v') => TokenType::check_keyword(id_iter, "ar", TokenType::Var),
            Some('w') => TokenType::check_keyword(id_iter, "hile", TokenType::While),
            _ => TokenType::Identifier,
        }
    }

    fn check_keyword(source: Chars<'_>, target: &str, token_type: TokenType) -> TokenType {
        if source.as_str() == target {
            token_type
        } else {
            TokenType::Identifier
        }
    }
}

pub struct Token<'a> {
    pub tpe: TokenType,
    pub lexeme: &'a str,
    pub source_offset: usize,
    pub line: usize,
}

pub struct Scanner<'a> {
    source: &'a str,
    current_character: Peekable<CharIndices<'a>>,
    current_token_start_offset: usize,
    line: usize,
}

impl Scanner<'_> {
    pub fn new(source: &str) -> Scanner {
        Scanner {
            source,
            current_character: source.char_indices().peekable(),
            current_token_start_offset: 0,
            line: 1,
        }
    }

    pub fn new_line(source: &str, line: usize) -> Scanner {
        Scanner {
            source,
            current_character: source.char_indices().peekable(),
            current_token_start_offset: 0,
            line,
        }
    }
}

impl<'a> Scanner<'a> {
    pub fn next_token(&mut self) -> Result<Token<'a>> {
        self.skip_whitespace();

        if let Some((offset, c)) = self.current_character.next() {
            self.current_token_start_offset = offset;
            match c {
                '(' => Ok(self.cut_token(TokenType::LeftParen, offset + 1)),
                ')' => Ok(self.cut_token(TokenType::RightParen, offset + 1)),
                '{' => Ok(self.cut_token(TokenType::LeftBrace, offset + 1)),
                '}' => Ok(self.cut_token(TokenType::RightBrace, offset + 1)),
                ';' => Ok(self.cut_token(TokenType::Semicolon, offset + 1)),
                ',' => Ok(self.cut_token(TokenType::Comma, offset + 1)),
                '.' => Ok(self.cut_token(TokenType::Dot, offset + 1)),
                '-' => Ok(self.cut_token(TokenType::Minus, offset + 1)),
                '+' => Ok(self.cut_token(TokenType::Plus, offset + 1)),
                '/' => {
                    if self.try_match('/') {
                        self.skip_until_eol();
                        self.next_token()
                    } else {
                        Ok(self.cut_token(TokenType::Slash, offset + 1))
                    }
                }
                '*' => Ok(self.cut_token(TokenType::Star, offset + 1)),
                '!' => {
                    if self.try_match('=') {
                        Ok(self.cut_token(TokenType::BangEqual, offset + 2))
                    } else {
                        Ok(self.cut_token(TokenType::Bang, offset + 1))
                    }
                }
                '=' => {
                    if self.try_match('=') {
                        Ok(self.cut_token(TokenType::EqualEqual, offset + 2))
                    } else {
                        Ok(self.cut_token(TokenType::Equal, offset + 1))
                    }
                }
                '<' => {
                    if self.try_match('=') {
                        Ok(self.cut_token(TokenType::LessEqual, offset + 2))
                    } else {
                        Ok(self.cut_token(TokenType::Less, offset + 1))
                    }
                }
                '>' => {
                    if self.try_match('=') {
                        Ok(self.cut_token(TokenType::GreaterEqual, offset + 2))
                    } else {
                        Ok(self.cut_token(TokenType::Greater, offset + 1))
                    }
                }
                '"' => self.scan_string(),
                c if c.is_ascii_digit() => self.scan_number(),
                c if c.is_ascii_alphabetic() => self.scan_identifier(),
                c => Err(ScannerError::UnexpectedCharacter {
                    character: c,
                    offset,
                    line: self.line,
                }
                .into()),
            }
        } else {
            Ok(self.cut_token(TokenType::EOF, self.current_token_start_offset))
        }
    }

    fn skip_whitespace(&mut self) -> () {
        while let Some((_, c)) = self.current_character.next_if(|(_, nc)| nc.is_whitespace()) {
            if c == '\n' {
                self.line += 1;
            }
        }
    }

    fn skip_until_eol(&mut self) -> () {
        while let Some(_) = self.current_character.next_if(|(_, nc)| *nc != '\n') {}
    }

    fn try_match(&mut self, c: char) -> bool {
        self.current_character.next_if(|(_, nc)| *nc == c).is_some()
    }

    fn scan_string(&mut self) -> Result<Token<'a>> {
        while let Some((_, c)) = self.current_character.next_if(|(_, nc)| *nc != '"') {
            // TODO(hollis): support escape sequences
            if c == '\n' {
                self.line += 1;
            }
        }

        match self.current_character.next() {
            Some((end_quote_offset, '"')) => {
                Ok(self.cut_token(TokenType::String, end_quote_offset + 1))
            }
            Some((invalid_offset, c)) => Err(ScannerError::UnexpectedCharacter {
                character: c,
                offset: invalid_offset,
                line: self.line,
            }
            .into()),
            None => Err(ScannerError::UnexpectedEOFInString {
                string_start_offset: self.current_token_start_offset,
            }
            .into()),
        }
    }

    fn scan_number(&mut self) -> Result<Token<'a>> {
        let mut last_char_offset = self.current_token_start_offset;

        while let Some((offset, _)) = self
            .current_character
            .next_if(|(_, nc)| nc.is_ascii_digit())
        {
            last_char_offset = offset;
        }

        if self.try_match('.') {
            last_char_offset += 1;

            while let Some((offset, _)) = self
                .current_character
                .next_if(|(_, nc)| nc.is_ascii_digit())
            {
                last_char_offset = offset;
            }
        }

        Ok(self.cut_token(TokenType::Number, last_char_offset + 1))
    }

    fn scan_identifier(&mut self) -> Result<Token<'a>> {
        let mut last_char_offset = self.current_token_start_offset;

        while let Some((offset, _)) = self
            .current_character
            .next_if(|(_, nc)| nc.is_ascii_alphanumeric())
        {
            last_char_offset = offset;
        }

        Ok(self.cut_token(
            TokenType::classify(
                &self.source[self.current_token_start_offset..(last_char_offset + 1)],
            ),
            last_char_offset + 1,
        ))
    }

    fn cut_token(&mut self, token_type: TokenType, end_offset: usize) -> Token<'a> {
        let token = Token {
            tpe: token_type,
            lexeme: &self.source[self.current_token_start_offset..end_offset],
            source_offset: self.current_token_start_offset,
            line: self.line,
        };
        self.current_token_start_offset = end_offset;
        token
    }
}

#[derive(Error, Debug, Diagnostic)]
pub enum ScannerError {
    #[error("unexpected character '{character}' on line {line}")]
    #[diagnostic(code(scanner::unexpected))]
    UnexpectedCharacter {
        character: char,
        #[label("here")]
        offset: usize,
        line: usize,
    },
    #[error("unexpected end of file on line {line}")]
    #[diagnostic(code(scanner::unexpected_eof))]
    UnexpectedEOF {
        #[label("here")]
        offset: usize,
        line: usize,
    },
    #[error("unexpected end of file inside a string")]
    #[diagnostic(
        code(scanner::unexpected_eof_string),
        help("did you miss a closing quote?")
    )]
    UnexpectedEOFInString {
        #[label("string starts here")]
        string_start_offset: usize,
    },
}
