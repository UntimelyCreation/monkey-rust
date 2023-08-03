#[derive(PartialEq, Debug)]
pub enum Token<'a> {
    Unknown,
    Eof,

    // Identifiers
    Identifier(&'a str),
    Integer(i32),

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

    LParen,
    RParen,
    LBrace,
    RBrace,

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
        "let" => Token::Let,
        "fn" => Token::Function,
        "true" => Token::True,
        "false" => Token::False,
        "if" => Token::If,
        "else" => Token::Else,
        "return" => Token::Return,
        _ => Token::Identifier(identifier),
    }
}
