#[cfg(test)]
mod tests {
    use crate::lexer::{token::Token, Lexer};

    fn test_lexing(input: &str, expected: &[Token]) {
        let lexer = Lexer::new(input);
        assert_eq!(lexer.tokenize(), expected);
    }

    #[test]
    fn test_lexer_let() {
        let input = "let five=5;
let ten = 10;";
        let expected = vec![
            Token::Let,
            Token::Identifier("five".to_string()),
            Token::Assign,
            Token::Integer(5),
            Token::Semicolon,
            Token::Let,
            Token::Identifier("ten".to_string()),
            Token::Assign,
            Token::Integer(10),
            Token::Semicolon,
            Token::Eof,
        ];
        test_lexing(input, &expected);
    }

    #[test]
    fn test_lexer_function() {
        let input = "let add = fn(x, y) {
    x + y;
};

let result = add(five, ten);";
        let expected = vec![
            Token::Let,
            Token::Identifier("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Identifier("x".to_string()),
            Token::Comma,
            Token::Identifier("y".to_string()),
            Token::RParen,
            Token::LBrace,
            Token::Identifier("x".to_string()),
            Token::Plus,
            Token::Identifier("y".to_string()),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Identifier("result".to_string()),
            Token::Assign,
            Token::Identifier("add".to_string()),
            Token::LParen,
            Token::Identifier("five".to_string()),
            Token::Comma,
            Token::Identifier("ten".to_string()),
            Token::RParen,
            Token::Semicolon,
            Token::Eof,
        ];
        test_lexing(input, &expected);
    }

    #[test]
    fn test_lexer_symbols() {
        let input = "!*-/5;
5 < 10 > 5;
10 == 10;
10 != 9;";
        let expected = vec![
            Token::Bang,
            Token::Asterisk,
            Token::Minus,
            Token::Slash,
            Token::Integer(5),
            Token::Semicolon,
            Token::Integer(5),
            Token::LessThan,
            Token::Integer(10),
            Token::GreaterThan,
            Token::Integer(5),
            Token::Semicolon,
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
        test_lexing(input, &expected);
    }

    #[test]
    fn test_lexer_if_else() {
        let input = "if (5 < 10) {
    return true;
} else {
    return false;
};";
        let expected = vec![
            Token::If,
            Token::LParen,
            Token::Integer(5),
            Token::LessThan,
            Token::Integer(10),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::Boolean(true),
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::Boolean(false),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Eof,
        ];
        test_lexing(input, &expected);
    }

    #[test]
    fn test_lexer_string() {
        let input = "\"foobar\";
\"foo bar\";";
        let expected = vec![
            Token::String("foobar".to_string()),
            Token::Semicolon,
            Token::String("foo bar".to_string()),
            Token::Semicolon,
            Token::Eof,
        ];
        test_lexing(input, &expected);
    }

    #[test]
    fn test_lexer_array() {
        let input = "[1, 2];";
        let expected = vec![
            Token::LBracket,
            Token::Integer(1),
            Token::Comma,
            Token::Integer(2),
            Token::RBracket,
            Token::Semicolon,
            Token::Eof,
        ];
        test_lexing(input, &expected);
    }

    #[test]
    fn test_lexer_hash() {
        let input = "{\"foo\": \"bar\"};";
        let expected = vec![
            Token::LBrace,
            Token::String("foo".to_string()),
            Token::Colon,
            Token::String("bar".to_string()),
            Token::RBrace,
            Token::Semicolon,
            Token::Eof,
        ];
        test_lexing(input, &expected);
    }
}
