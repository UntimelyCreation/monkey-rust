use token::{match_identifier, Token};

mod test_lexer;
pub mod token;

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    character: char,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut lexer = Lexer {
            input: input.to_string(),
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
        while token != Token::Eof {
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
            ':' => Token::Colon,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            '[' => Token::LBracket,
            ']' => Token::RBracket,
            '"' => Token::String(self.read_string()),
            '\0' => Token::Eof,
            ch if ch.is_ascii_alphabetic() => {
                let position = self.position;
                while self.character.is_ascii_alphabetic() {
                    self.read_char();
                }
                skip_read_char = true;
                match_identifier(&self.input[position..self.position])
            }
            ch if ch.is_ascii_digit() => {
                let position = self.position;
                while self.character.is_ascii_digit() {
                    self.read_char();
                }
                skip_read_char = true;
                match self.input[position..self.position].parse() {
                    Ok(integer) => Token::Integer(integer),
                    Err(_) => Token::Unknown,
                }
            }
            _ => Token::Unknown,
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
        self.character = self.peek_char();
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&self) -> char {
        self.input.chars().nth(self.read_position).unwrap_or('\0')
    }

    fn skip_whitespace(&mut self) {
        while self.character.is_ascii_whitespace() {
            self.read_char();
        }
    }
}
