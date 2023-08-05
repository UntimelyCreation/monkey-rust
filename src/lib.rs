mod lexer;
pub mod repl;
mod token;

#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, token::Token};

    #[test]
    fn test_next_token() {
        let input = "let five = 5;
        let ten = 10;

        let add = fn(x, y) {
            x + y;
        };

        let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;

        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 10;
        10 != 9;";

        let expected = vec![
            Token::Let,
            Token::Identifier("five"),
            Token::Assign,
            Token::Integer(5),
            Token::Semicolon,
            Token::Let,
            Token::Identifier("ten"),
            Token::Assign,
            Token::Integer(10),
            Token::Semicolon,
            Token::Let,
            Token::Identifier("add"),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Identifier("x"),
            Token::Comma,
            Token::Identifier("y"),
            Token::RParen,
            Token::LBrace,
            Token::Identifier("x"),
            Token::Plus,
            Token::Identifier("y"),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Identifier("result"),
            Token::Assign,
            Token::Identifier("add"),
            Token::LParen,
            Token::Identifier("five"),
            Token::Comma,
            Token::Identifier("ten"),
            Token::RParen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Integer(5),
            Token::Semicolon,
            Token::Integer(5),
            Token::LessThan,
            Token::Integer(10),
            Token::GreaterThan,
            Token::Integer(5),
            Token::Semicolon,
            Token::If,
            Token::LParen,
            Token::Integer(5),
            Token::LessThan,
            Token::Integer(10),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RBrace,
            Token::Integer(10),
            Token::Equal,
            Token::Integer(10),
            Token::Semicolon,
            Token::Integer(10),
            Token::NotEqual,
            Token::Integer(9),
            Token::Semicolon,
            Token::Eof,
        ];
        let lexer = Lexer::new(input);
        assert_eq!(lexer.tokenize(), expected);
    }
}
