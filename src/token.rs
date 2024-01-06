#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub struct Token {
    pub kind: TokenType,
    pub literal: String,
}

impl Token {
    pub fn from_char(kind: TokenType, ch: char) -> Self {
        Token {
            kind,
            literal: String::from(ch),
        }
    }

    pub fn from_str(kind: TokenType, literal: &str) -> Self {
        Token {
            kind,
            literal: literal.to_string(),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Copy, Clone)]
pub enum TokenType {
    Unknown,
    Eof,

    // Identifiers
    Identifier,
    Integer,
    String,

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    LessThan,
    GreaterThan,

    Equal,
    NotEqual,

    // Delimiters
    Comma,
    Semicolon,
    Colon,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    // Keywords
    Let,
    Function,
    True,
    False,
    If,
    Else,
    Return,
}

pub fn match_identifier(identifier: &str) -> Token {
    match identifier {
        "let" => Token::from_str(TokenType::Let, identifier),
        "fn" => Token::from_str(TokenType::Function, identifier),
        "true" => Token::from_str(TokenType::True, identifier),
        "false" => Token::from_str(TokenType::False, identifier),
        "if" => Token::from_str(TokenType::If, identifier),
        "else" => Token::from_str(TokenType::Else, identifier),
        "return" => Token::from_str(TokenType::Return, identifier),
        _ => Token::from_str(TokenType::Identifier, identifier),
    }
}
