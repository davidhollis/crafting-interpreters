use std::{
    iter::Peekable,
    str::{CharIndices, Chars},
};

use miette::{Diagnostic, Result};
use thiserror::Error;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[repr(usize)]
pub enum TokenType {
    // Single-character tokens
    LeftParen = 0,
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

#[derive(Clone)]
pub struct Token<'a> {
    pub tpe: TokenType,
    pub lexeme: &'a str,
    pub source_offset: usize,
    pub line: usize,
}

impl Token<'_> {
    pub fn empty_token() -> Token<'static> {
        Token {
            tpe: TokenType::EOF,
            lexeme: "",
            source_offset: 0,
            line: 0,
        }
    }

    pub fn blank_name_token() -> Token<'static> {
        Token {
            tpe: TokenType::Identifier,
            lexeme: "",
            source_offset: 0,
            line: 0,
        }
    }

    pub fn this_token() -> Token<'static> {
        Token {
            tpe: TokenType::This,
            lexeme: "this",
            source_offset: 0,
            line: 0,
        }
    }

    pub fn super_token() -> Token<'static> {
        Token {
            tpe: TokenType::Super,
            lexeme: "super",
            source_offset: 0,
            line: 0,
        }
    }

    pub fn error_span(&self) -> (usize, usize) {
        (self.source_offset, self.lexeme.len())
    }

    pub fn location(&self) -> SourceLocation {
        SourceLocation {
            span: self.error_span(),
            line: self.line,
        }
    }
}

impl<'a> Token<'a> {
    pub fn at_location(self, location: &SourceLocation) -> Self {
        Token {
            tpe: self.tpe,
            lexeme: self.lexeme,
            source_offset: location.span.0,
            line: location.line,
        }
    }
}

#[derive(Clone)]
pub struct SourceLocation {
    pub span: (usize, usize),
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

    pub fn new_repl(source: &str, mut starting_at: usize) -> Scanner {
        let mut character_iter = source.char_indices().peekable();
        let mut line = 1;

        while starting_at > 0 {
            if let Some((_, '\n')) = character_iter.next() {
                line += 1;
            }
            starting_at -= 1;
        }

        Scanner {
            source,
            current_character: character_iter,
            current_token_start_offset: starting_at,
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
                c if c.is_ascii_alphabetic() || c == '_' => self.scan_identifier(),
                c => Err(ScannerError::UnexpectedCharacter {
                    character: c,
                    offset,
                    line: self.line,
                    source_code: self.source.to_string(),
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
        let mut escaping = false;
        while let Some((_, c)) = self.current_character.next_if(|(_, nc)| {
            if escaping {
                // If we were escaping, unconditionally grab the next character and we're no longer
                // escaping. While it's true that some escape sequences span multiple characters,
                // the only one we care about when looking for the end of the string is \".
                escaping = false;
                true
            } else if *nc == '\\' {
                // If we're not escaping and we see a backslash, start escaping and keep scanning
                // the string literal.
                escaping = true;
                true
            } else {
                // If we're not escaping and not looking at a backslash, keep going if and only if
                // we're not looking at a quote.
                *nc != '"'
            }
        }) {
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
                source_code: self.source.to_string(),
            }
            .into()),
            None => Err(ScannerError::UnexpectedEOFInString {
                string_start_offset: self.current_token_start_offset,
                source_code: self.source.to_string(),
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
            .next_if(|(_, nc)| nc.is_ascii_alphanumeric() || *nc == '_')
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

pub struct StringLiteralScanner<'a> {
    literal: &'a str,
    current_character: Peekable<CharIndices<'a>>,
    literal_offset: usize,
}

impl<'a> StringLiteralScanner<'a> {
    pub fn from_token(token: Token<'a>) -> StringLiteralScanner<'a> {
        let literal = &token.lexeme[1..(token.lexeme.len() - 1)];
        StringLiteralScanner {
            literal,
            current_character: literal.char_indices().peekable(),
            literal_offset: token.source_offset + 1,
        }
    }

    fn char_from_hex_str(
        &self,
        hex_str: &str,
        offset: usize,
        full_sequence: String,
    ) -> Result<char> {
        u32::from_str_radix(hex_str, 16)
            .ok()
            .and_then(char::from_u32)
            .ok_or_else(|| {
                let len = full_sequence.len();
                ScannerError::InvalidEscapeSequence {
                    sequence: full_sequence,
                    span: (offset + self.literal_offset, len),
                }
                .into()
            })
    }

    pub fn scan(self) -> Result<String> {
        self.collect()
    }
}

impl Iterator for StringLiteralScanner<'_> {
    type Item = Result<char>;

    fn next(&mut self) -> Option<Self::Item> {
        let (current_offset, current_char) = self.current_character.next()?;

        if current_char == '\\' {
            // An escape sequence!
            match self.current_character.next() {
                Some((_, 'n')) => Some(Ok('\n')),
                Some((_, 'r')) => Some(Ok('\r')),
                Some((_, 't')) => Some(Ok('\t')),
                Some((_, '"')) => Some(Ok('"')),
                Some((_, '\\')) => Some(Ok('\\')),
                Some((_, '0')) => Some(Ok('\0')),
                // \xHH
                Some((_, 'x')) => {
                    match (self.current_character.next(), self.current_character.next()) {
                        (Some((high_offset, _)), Some((low_offset, _))) => {
                            let hex_str = &self.literal[high_offset..=low_offset];
                            Some(self.char_from_hex_str(
                                hex_str,
                                current_offset,
                                format!("\\x{hex_str}"),
                            ))
                        }
                        (Some((_, ch)), None) => Some(Err(ScannerError::InvalidEscapeSequence {
                            sequence: format!("\\x{ch}"),
                            span: (current_offset + self.literal_offset, 3),
                        }
                        .into())),
                        _ => Some(Err(ScannerError::InvalidEscapeSequence {
                            sequence: "\\x".to_string(),
                            span: (current_offset + self.literal_offset, 2),
                        }
                        .into())),
                    }
                }
                // \u{H+}
                Some((_, 'u')) => {
                    if let Some((_, '{')) = self.current_character.next() {
                    } else {
                        return Some(Err(ScannerError::InvalidEscapeSequence {
                            sequence: "\\u".to_string(),
                            span: (current_offset + self.literal_offset, 2),
                        }
                        .into()));
                    }

                    let mut hex_str = String::new();
                    while let Some((_, hex_digit)) = self
                        .current_character
                        .next_if(|(_, c)| c.is_ascii_hexdigit())
                    {
                        hex_str.push(hex_digit);
                    }

                    if let Some((_, '}')) = self.current_character.next() {
                    } else {
                        let sequence = format!("\\u{{{hex_str}");
                        let len = sequence.len();
                        return Some(Err(ScannerError::InvalidEscapeSequence {
                            sequence,
                            span: (current_offset + self.literal_offset, len),
                        }
                        .into()));
                    }

                    Some(self.char_from_hex_str(
                        &hex_str,
                        current_offset,
                        format!("\\u{{{hex_str}}}"),
                    ))
                }
                Some((_, ch)) => {
                    let sequence = format!("\\{ch}");
                    let len = sequence.len();
                    Some(Err(ScannerError::InvalidEscapeSequence {
                        sequence,
                        span: (current_offset + self.literal_offset, len),
                    }
                    .into()))
                }
                None => {
                    let sequence = format!("\\[EOF]");
                    Some(Err(ScannerError::InvalidEscapeSequence {
                        sequence,
                        span: (current_offset + self.literal_offset, 1),
                    }
                    .into()))
                }
            }
        } else {
            Some(Ok(current_char))
        }
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
        #[source_code]
        source_code: String,
    },
    #[error("unexpected end of file inside a string")]
    #[diagnostic(
        code(scanner::unexpected_eof_string),
        help("did you miss a closing quote?")
    )]
    UnexpectedEOFInString {
        #[label("string starts here")]
        string_start_offset: usize,
        #[source_code]
        source_code: String,
    },
    #[error("invalid escape sequence '{sequence}'")]
    #[diagnostic(code(scanner::invalid_escape_sequence))]
    InvalidEscapeSequence {
        sequence: String,
        #[label("here")]
        span: (usize, usize),
    },
}

#[cfg(test)]
mod tests {
    use miette::Result;

    use super::{StringLiteralScanner, Token, TokenType};

    #[test]
    fn it_handles_simple_escape_sequences() -> Result<()> {
        let expected = "abc\ndef\rghi\tjkl\"mno\0pqr";
        let test_token = Token {
            tpe: TokenType::String,
            lexeme: "\"abc\\ndef\\rghi\\tjkl\\\"mno\\0pqr\"",
            source_offset: 0,
            line: 1,
        };

        let actual = StringLiteralScanner::from_token(test_token).scan()?;

        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn it_handles_x_escapes() -> Result<()> {
        let expected = "abc\x7Edef";
        let test_token = Token {
            tpe: TokenType::String,
            lexeme: "\"abc\\x7Edef\"",
            source_offset: 0,
            line: 1,
        };

        let actual = StringLiteralScanner::from_token(test_token).scan()?;

        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn it_handles_u_escapes() -> Result<()> {
        let expected = "abc\u{1F9C4}def";
        let test_token = Token {
            tpe: TokenType::String,
            lexeme: "\"abc\\u{1F9C4}def\"",
            source_offset: 0,
            line: 1,
        };

        let actual = StringLiteralScanner::from_token(test_token).scan()?;

        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn it_correctly_fails_invalid_sequences() -> () {
        let invalid_escapes = vec![
            "\\",              // empty sequence
            "\\q",             // not a valid character
            "\\xA",            // only one hex character
            "\\xZZ",           // not a valid hex numeral
            "\\u!",            // character right after \u isn't '{'
            "\\u{abc!",        // wrong closing character for a \u{...} sequence
            "\\u{AAAAAAAAAA}", // not a u32
            "\\u{FFFFFFFF}",   // not an actual unicode character
        ];

        for invalid_example in invalid_escapes {
            let invalid_token = Token {
                tpe: TokenType::String,
                lexeme: &format!("\"{invalid_example}\""),
                source_offset: 0,
                line: 1,
            };

            let result = StringLiteralScanner::from_token(invalid_token).scan();

            assert!(
                result.is_err(),
                "expected {:?} to contain an invalid escape sequence",
                invalid_example,
            );
        }
    }
}
