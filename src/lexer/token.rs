#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub enum Token {
    Unknown,
    Eof,

    // Identifiers
    Identifier(String),
    Integer(i32),
    Boolean(bool),
    String(String),

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
    If,
    Else,
    Return,
}

impl Token {
    pub fn get_literal(&self) -> String {
        match self {
            Self::Identifier(identifier) => identifier.to_owned(),
            Self::Integer(integer) => integer.to_string(),
            Self::Boolean(boolean) => boolean.to_string(),
            Self::String(string) => string.to_owned(),
            Self::Assign => '='.to_string(),
            Self::Plus => '+'.to_string(),
            Self::Minus => '-'.to_string(),
            Self::Bang => '!'.to_string(),
            Self::Asterisk => '*'.to_string(),
            Self::Slash => '/'.to_string(),
            Self::LessThan => '<'.to_string(),
            Self::GreaterThan => '>'.to_string(),
            Self::Equal => "==".to_string(),
            Self::NotEqual => "!=".to_string(),
            Self::Comma => ','.to_string(),
            Self::Semicolon => ';'.to_string(),
            Self::Colon => ':'.to_string(),
            Self::LParen => '('.to_string(),
            Self::RParen => ')'.to_string(),
            Self::LBrace => '{'.to_string(),
            Self::RBrace => '}'.to_string(),
            Self::LBracket => '['.to_string(),
            Self::RBracket => ']'.to_string(),
            Self::Let => "let".to_string(),
            Self::Function => "fn".to_string(),
            Self::If => "if".to_string(),
            Self::Else => "else".to_string(),
            Self::Return => "return".to_string(),
            Self::Eof => "Eof".to_string(),
            _ => '\0'.to_string(),
        }
    }
}

pub fn match_identifier(identifier: &str) -> Token {
    match identifier {
        "let" => Token::Let,
        "fn" => Token::Function,
        "true" => Token::Boolean(true),
        "false" => Token::Boolean(false),
        "if" => Token::If,
        "else" => Token::Else,
        "return" => Token::Return,
        _ => Token::Identifier(identifier.to_string()),
    }
}
