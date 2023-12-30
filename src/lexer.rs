use std::str::FromStr;

use crate::token::{match_identifier, Token, TokenType};

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    character: char,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut lexer = Lexer {
            input: String::from_str(input).unwrap(),
            position: 0,
            read_position: 0,
            character: '\0',
        };

        lexer.read_char();
        lexer
    }

    pub fn tokenize(mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        let mut token = self.next_token();
        while token.kind != TokenType::Eof {
            tokens.push(token);
            token = self.next_token();
        }
        tokens.push(self.next_token());
        tokens
    }

    pub fn next_token(&mut self) -> Token {
        let mut skip_read_char = false;

        self.skip_whitespace();

        let token = match self.character {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::from_str(TokenType::Equal, "==")
                } else {
                    Token::from_char(TokenType::Assign, self.character)
                }
            }
            '+' => Token::from_char(TokenType::Plus, self.character),
            '-' => Token::from_char(TokenType::Minus, self.character),
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::from_str(TokenType::NotEqual, "!=")
                } else {
                    Token::from_char(TokenType::Bang, self.character)
                }
            }
            '/' => Token::from_char(TokenType::Slash, self.character),
            '*' => Token::from_char(TokenType::Asterisk, self.character),
            '<' => Token::from_char(TokenType::LessThan, self.character),
            '>' => Token::from_char(TokenType::GreaterThan, self.character),
            ',' => Token::from_char(TokenType::Comma, self.character),
            ';' => Token::from_char(TokenType::Semicolon, self.character),
            ':' => Token::from_char(TokenType::Colon, self.character),
            '(' => Token::from_char(TokenType::LParen, self.character),
            ')' => Token::from_char(TokenType::RParen, self.character),
            '{' => Token::from_char(TokenType::LBrace, self.character),
            '}' => Token::from_char(TokenType::RBrace, self.character),
            '[' => Token::from_char(TokenType::LBracket, self.character),
            ']' => Token::from_char(TokenType::RBracket, self.character),
            '"' => Token::from_str(TokenType::String, &self.read_string()),
            '\0' => Token::from_char(TokenType::Eof, self.character),
            ch if ch.is_ascii_alphabetic() => {
                let position = self.position;
                while self.character.is_ascii_alphabetic() {
                    self.read_char();
                }
                skip_read_char = true;
                match_identifier(self.input.get(position..self.position).unwrap())
            }
            ch if ch.is_ascii_digit() => {
                let position = self.position;
                while self.character.is_ascii_digit() {
                    self.read_char();
                }
                skip_read_char = true;
                Token::from_str(
                    TokenType::Integer,
                    self.input.get(position..self.position).unwrap(),
                )
            }
            _ => Token::from_char(TokenType::Unknown, '\0'),
        };

        if !skip_read_char {
            self.read_char();
        }
        token
    }

    fn read_string(&mut self) -> String {
        let position = self.position + 1;
        loop {
            self.read_char();
            if self.character == '"' || self.character == '\0' {
                break;
            }
        }
        self.input[position..self.position].to_string()
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.chars().count() {
            self.character = '\0';
        } else {
            self.character = self.input.chars().nth(self.read_position).unwrap();
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.chars().count() {
            '\0'
        } else {
            self.input.chars().nth(self.read_position).unwrap()
        }
    }

    fn skip_whitespace(&mut self) {
        while self.character.is_ascii_whitespace() {
            self.read_char();
        }
    }
}
