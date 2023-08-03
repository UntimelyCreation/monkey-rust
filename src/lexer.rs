use crate::token::{match_identifier, Token};

pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
    read_position: usize,
    character: char,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer {
            input,
            position: 0,
            read_position: 0,
            character: '\0',
        };

        lexer.read_char();
        lexer
    }

    pub fn tokenize(mut self) -> Vec<Token<'a>> {
        let mut tokens = Vec::new();

        let mut token = self.next_token();
        while token != Token::Eof {
            tokens.push(token);
            token = self.next_token();
        }
        tokens.push(self.next_token());
        tokens
    }

    fn next_token(&mut self) -> Token<'a> {
        let mut skip_read_char = false;

        self.skip_whitespace();

        let token = match self.character {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::Equal
                } else {
                    Token::Assign
                }
            }
            '+' => Token::Plus,
            '-' => Token::Minus,
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::NotEqual
                } else {
                    Token::Bang
                }
            }
            '/' => Token::Slash,
            '*' => Token::Asterisk,
            '<' => Token::LessThan,
            '>' => Token::GreaterThan,
            ',' => Token::Comma,
            ';' => Token::Semicolon,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            '\0' => Token::Eof,
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
                Token::Integer(
                    self.input
                        .get(position..self.position)
                        .unwrap()
                        .parse()
                        .unwrap(),
                )
            }
            _ => Token::Unknown,
        };

        if !skip_read_char {
            self.read_char();
        }
        token
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
