use std::collections::BTreeMap;

use crate::ast::{
    ArrayLiteralExpression, HashLiteralExpression, IndexExpression, Node, StringExpression,
};
use crate::{
    ast::{
        BlockStatement, BooleanExpression, CallExpression, Expression, ExpressionStatement,
        FnLiteralExpression, IdentifierExpression, IfExpression, InfixExpression,
        IntegerExpression, LetStatement, PrefixExpression, Program, ReturnStatement, Statement,
    },
    lexer::Lexer,
    token::Token,
};

const LOWEST: usize = 1;
const EQUALS: usize = 2;
const LESSGREATER: usize = 3;
const SUM: usize = 4;
const PRODUCT: usize = 5;
const PREFIX: usize = 6;
const CALL: usize = 7;
const INDEX: usize = 8;

fn get_precedence(token: &Token) -> usize {
    match token {
        Token::Equal | Token::NotEqual => EQUALS,
        Token::LessThan | Token::GreaterThan => LESSGREATER,
        Token::Plus | Token::Minus => SUM,
        Token::Asterisk | Token::Slash => PRODUCT,
        Token::LParen => CALL,
        Token::LBracket => INDEX,
        _ => LOWEST,
    }
}

type PrefixParseFn = for<'a> fn(&'a mut Parser) -> Option<Expression>;
type InfixParseFn = for<'a> fn(&'a mut Parser, Box<Expression>) -> Option<Expression>;

fn get_prefix_fn(token: &Token) -> Option<PrefixParseFn> {
    match token {
        Token::Identifier(_) => Some(Parser::parse_identifier_expression),
        Token::Integer(_) => Some(Parser::parse_integer_expression),
        Token::Boolean(_) => Some(Parser::parse_boolean_expression),
        Token::String(_) => Some(Parser::parse_string_literal_expression),
        Token::Bang => Some(Parser::parse_prefix_expression),
        Token::Minus => Some(Parser::parse_prefix_expression),
        Token::LParen => Some(Parser::parse_grouped_expression),
        Token::If => Some(Parser::parse_if_expression),
        Token::Function => Some(Parser::parse_fn_literal_expression),
        Token::LBracket => Some(Parser::parse_array_literal_expression),
        Token::LBrace => Some(Parser::parse_hash_literal_expression),
        _ => None,
    }
}

fn get_infix_fn(token: &Token) -> Option<InfixParseFn> {
    match token {
        Token::Plus => Some(Parser::parse_infix_expression),
        Token::Minus => Some(Parser::parse_infix_expression),
        Token::Asterisk => Some(Parser::parse_infix_expression),
        Token::Slash => Some(Parser::parse_infix_expression),
        Token::Equal => Some(Parser::parse_infix_expression),
        Token::NotEqual => Some(Parser::parse_infix_expression),
        Token::LessThan => Some(Parser::parse_infix_expression),
        Token::GreaterThan => Some(Parser::parse_infix_expression),
        Token::LParen => Some(Parser::parse_call_expression),
        Token::LBracket => Some(Parser::parse_index_expression),
        _ => None,
    }
}

type ParseError = String;

fn fmt_token_error(expected: &Token, result: &Token) -> ParseError {
    format!(
        "parse error: expected {}, found {}",
        expected.get_literal(),
        result.get_literal()
    )
}

fn fmt_expression_error(expr: &str) -> ParseError {
    format!("parse error: no valid expression found for {}", expr)
}

pub fn parse(input: &str) -> Option<Node> {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    parser.parse_program().map(Node::Program)
}

pub struct Parser {
    lexer: Lexer,
    curr_token: Token,
    peek_token: Token,
    errors: Vec<ParseError>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            curr_token: Token::Unknown,
            peek_token: Token::Unknown,
            errors: Vec::new(),
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    pub fn parse_program(&mut self) -> Option<Program> {
        let mut program: Program = Program(Vec::new());

        while self.curr_token != Token::Eof {
            if let Some(stmt) = self.parse_statement() {
                program.0.push(stmt);
            }
            self.next_token();
        }

        if !self.errors.is_empty() {
            self.eprint_errors();
            return None;
        }

        Some(program)
    }

    pub fn eprint_errors(&self) {
        for err in self.errors.iter() {
            eprintln!("{}", err);
        }
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.curr_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression(&mut self, precedence: usize) -> Option<Expression> {
        match get_prefix_fn(&self.curr_token) {
            Some(prefix_fn) => match prefix_fn(self) {
                Some(mut lhs) => {
                    while self.peek_token != Token::Semicolon
                        && precedence < get_precedence(&self.peek_token)
                    {
                        if let Some(infix_fn) = get_infix_fn(&self.peek_token) {
                            self.next_token();
                            match infix_fn(self, Box::new(lhs.clone())) {
                                Some(expr) => {
                                    lhs = expr;
                                }
                                None => {
                                    self.errors.push(fmt_expression_error("operand"));
                                    return None;
                                }
                            }
                        } else {
                            return Some(lhs);
                        }
                    }

                    Some(lhs)
                }
                None => {
                    self.errors.push(fmt_expression_error("operand"));
                    None
                }
            },
            None => {
                self.errors.push(format!(
                    "parse error: no prefix function found for {}",
                    self.curr_token.get_literal()
                ));
                None
            }
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        match &self.peek_token {
            Token::Identifier(_) => {
                self.next_token();
                let identifier = self.curr_token.get_literal();

                if !self.expect_peek(&Token::Assign) {
                    return None;
                }

                self.next_token();

                match self.parse_expression(LOWEST) {
                    Some(value) => {
                        if self.peek_token == Token::Semicolon {
                            self.next_token();
                        }

                        Some(Statement::Let(LetStatement {
                            identifier: IdentifierExpression { name: identifier },
                            value,
                        }))
                    }
                    None => {
                        self.errors.push(fmt_expression_error("value"));
                        None
                    }
                }
            }
            _ => {
                self.errors.push("identifier not found".to_string());
                None
            }
        }
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();

        match self.parse_expression(LOWEST) {
            Some(value) => {
                if self.peek_token == Token::Semicolon {
                    self.next_token();
                }

                Some(Statement::Return(ReturnStatement { value }))
            }
            None => {
                self.errors.push(fmt_expression_error("return value"));
                None
            }
        }
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let expression = self.parse_expression(LOWEST);

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        expression.map(|expr| Statement::Expression(ExpressionStatement { expr }))
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let mut statements = Vec::new();

        self.next_token();

        while self.curr_token != Token::RBrace && self.curr_token != Token::Eof {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            }
            self.next_token();
        }

        BlockStatement { statements }
    }

    fn parse_identifier_expression(&mut self) -> Option<Expression> {
        Some(Expression::Identifier(IdentifierExpression {
            name: self.curr_token.get_literal(),
        }))
    }

    fn parse_integer_expression(&mut self) -> Option<Expression> {
        match self.curr_token.get_literal().parse() {
            Ok(int) => Some(Expression::Integer(IntegerExpression { value: int })),
            Err(_) => {
                self.errors.push(format!(
                    "parse error: expected integer, found {}",
                    self.curr_token.get_literal()
                ));
                None
            }
        }
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let prefix = self.curr_token.clone();
        self.next_token();
        match self.parse_expression(PREFIX) {
            Some(expr) => Some(Expression::Prefix(PrefixExpression {
                prefix,
                operand: Box::new(expr),
            })),
            None => {
                self.errors.push(fmt_expression_error("prefix operand"));
                None
            }
        }
    }

    fn parse_infix_expression(&mut self, lhs: Box<Expression>) -> Option<Expression> {
        let operator = self.curr_token.clone();
        let precedence = get_precedence(&self.curr_token);
        self.next_token();
        match self.parse_expression(precedence) {
            Some(rhs) => Some(Expression::Infix(InfixExpression {
                operator,
                lhs,
                rhs: Box::new(rhs),
            })),
            None => {
                self.errors.push(fmt_expression_error("infix operand"));
                None
            }
        }
    }

    fn parse_boolean_expression(&mut self) -> Option<Expression> {
        match self.curr_token.get_literal().parse() {
            Ok(boolean) => Some(Expression::Boolean(BooleanExpression { value: boolean })),
            Err(_) => {
                self.errors.push(format!(
                    "parse error: expected boolean, found {}",
                    self.curr_token.get_literal()
                ));
                None
            }
        }
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();
        let expr = self.parse_expression(LOWEST);
        if !self.expect_peek(&Token::RParen) {
            return None;
        };
        expr
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        if !self.expect_peek(&Token::LParen) {
            return None;
        };

        self.next_token();
        match self.parse_expression(LOWEST) {
            Some(expr) => {
                let condition = Box::new(expr);

                if !self.expect_peek(&Token::RParen) {
                    return None;
                };

                if !self.expect_peek(&Token::LBrace) {
                    return None;
                };

                let consequence = self.parse_block_statement();

                let alternative = if self.peek_token == Token::Else {
                    self.next_token();
                    if !self.expect_peek(&Token::LBrace) {
                        return None;
                    }

                    Some(self.parse_block_statement())
                } else {
                    None
                };

                Some(Expression::If(IfExpression {
                    condition,
                    consequence,
                    alternative,
                }))
            }
            None => {
                self.errors.push(fmt_expression_error("condition"));
                None
            }
        }
    }

    fn parse_fn_literal_expression(&mut self) -> Option<Expression> {
        if !self.expect_peek(&Token::LParen) {
            return None;
        };

        let parameters = self.parse_function_parameters();

        if !self.expect_peek(&Token::LBrace) {
            return None;
        };

        let body = self.parse_block_statement();

        Some(Expression::FnLiteral(FnLiteralExpression {
            parameters,
            body,
        }))
    }

    fn parse_function_parameters(&mut self) -> Vec<IdentifierExpression> {
        let mut identifiers = Vec::new();

        if self.peek_token == Token::RParen {
            self.next_token();
            return identifiers;
        };

        self.next_token();

        let identifier = IdentifierExpression {
            name: self.curr_token.get_literal(),
        };
        identifiers.push(identifier);

        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();
            let identifier = IdentifierExpression {
                name: self.curr_token.get_literal(),
            };
            identifiers.push(identifier);
        }

        if !self.expect_peek(&Token::RParen) {
            return Vec::new();
        };

        identifiers
    }

    fn parse_string_literal_expression(&mut self) -> Option<Expression> {
        Some(Expression::String(StringExpression {
            value: self.curr_token.get_literal(),
        }))
    }

    fn parse_call_expression(&mut self, lhs: Box<Expression>) -> Option<Expression> {
        Some(Expression::Call(CallExpression {
            function: lhs,
            arguments: self.parse_expression_list(Token::RParen),
        }))
    }

    fn parse_array_literal_expression(&mut self) -> Option<Expression> {
        Some(Expression::ArrayLiteral(ArrayLiteralExpression {
            elements: self.parse_expression_list(Token::RBracket),
        }))
    }

    fn parse_expression_list(&mut self, end: Token) -> Vec<Expression> {
        let mut exprs = Vec::new();

        if self.peek_token == end {
            self.next_token();
            return exprs;
        }

        self.next_token();
        if let Some(expr) = self.parse_expression(LOWEST) {
            exprs.push(expr);
        }

        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();
            if let Some(expr) = self.parse_expression(LOWEST) {
                exprs.push(expr);
            }
        }

        if !self.expect_peek(&end) {
            return Vec::new();
        }

        exprs
    }

    fn parse_index_expression(&mut self, lhs: Box<Expression>) -> Option<Expression> {
        self.next_token();
        match self.parse_expression(LOWEST) {
            Some(index) => {
                if !self.expect_peek(&Token::RBracket) {
                    return None;
                };

                Some(Expression::Index(IndexExpression {
                    identifier: lhs,
                    index: Box::new(index),
                }))
            }
            None => {
                self.errors.push(fmt_expression_error("index"));
                None
            }
        }
    }

    fn parse_hash_literal_expression(&mut self) -> Option<Expression> {
        let mut pairs = BTreeMap::new();

        while self.peek_token != Token::RBrace {
            self.next_token();
            match self.parse_expression(LOWEST) {
                Some(key) => {
                    if !self.expect_peek(&Token::Colon) {
                        return None;
                    }

                    self.next_token();
                    match self.parse_expression(LOWEST) {
                        Some(value) => {
                            pairs.insert(key, value);

                            if self.peek_token != Token::RBrace && !self.expect_peek(&Token::Comma)
                            {
                                return None;
                            }
                        }
                        None => {
                            self.errors.push(fmt_expression_error("hash value"));
                            return None;
                        }
                    }
                }
                None => {
                    self.errors.push(fmt_expression_error("hash key"));
                    return None;
                }
            }
        }

        if !self.expect_peek(&Token::RBrace) {
            return None;
        }

        Some(Expression::HashLiteral(HashLiteralExpression { pairs }))
    }

    fn next_token(&mut self) {
        self.curr_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn expect_peek(&mut self, expected: &Token) -> bool {
        match &self.peek_token {
            token if token == expected => {
                self.next_token();
                true
            }
            _ => {
                self.errors
                    .push(fmt_token_error(expected, &self.peek_token));
                false
            }
        }
    }
}
