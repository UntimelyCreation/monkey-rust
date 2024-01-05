use std::collections::BTreeMap;
use std::fmt::Display;
use std::{collections::HashMap, error::Error};

use crate::ast::{
    ArrayLiteralExpression, HashLiteralExpression, IndexExpression, StringExpression,
};
use crate::{
    ast::{
        BlockStatement, BooleanExpression, CallExpression, Expression, ExpressionStatement,
        FnLiteralExpression, IdentifierExpression, IfExpression, InfixExpression,
        IntegerExpression, LetStatement, PrefixExpression, Program, ReturnStatement, Statement,
    },
    lexer::Lexer,
    token::{Token, TokenType},
};

const LOWEST: usize = 1;
const EQUALS: usize = 2;
const LESSGREATER: usize = 3;
const SUM: usize = 4;
const PRODUCT: usize = 5;
const PREFIX: usize = 6;
const CALL: usize = 7;
const INDEX: usize = 8;

type PrefixParseFn = for<'a> fn(&'a mut Parser) -> Option<Expression>;
type InfixParseFn = for<'a> fn(&'a mut Parser, Box<Expression>) -> Option<Expression>;

#[derive(Debug)]
enum ParseError {
    Token(TokenType, TokenType),
    PrefixParseFn(TokenType),
    Expression(String),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::Token(expected, result) => {
                write!(
                    f,
                    "parse error: expected {:?}, found {:?}",
                    expected, result
                )
            }
            ParseError::PrefixParseFn(token_type) => {
                write!(
                    f,
                    "parse error: no prefix function found for {:?}",
                    token_type
                )
            }
            ParseError::Expression(expr_type) => {
                write!(
                    f,
                    "parse error: no valid expression found for {}",
                    expr_type
                )
            }
        }
    }
}

impl Error for ParseError {}

pub struct Parser {
    lexer: Lexer,
    curr_token: Token,
    peek_token: Token,
    errors: Vec<ParseError>,
    precedences: HashMap<TokenType, usize>,
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            curr_token: Token::from_char(TokenType::Unknown, '\0'),
            peek_token: Token::from_char(TokenType::Unknown, '\0'),
            errors: Vec::new(),
            precedences: HashMap::new(),
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        parser.init_precedences();
        parser.init_parse_fns();

        parser.next_token();
        parser.next_token();

        parser
    }

    fn init_precedences(&mut self) {
        self.precedences.insert(TokenType::Equal, EQUALS);
        self.precedences.insert(TokenType::NotEqual, EQUALS);
        self.precedences.insert(TokenType::LessThan, LESSGREATER);
        self.precedences.insert(TokenType::GreaterThan, LESSGREATER);
        self.precedences.insert(TokenType::Plus, SUM);
        self.precedences.insert(TokenType::Minus, SUM);
        self.precedences.insert(TokenType::Slash, PRODUCT);
        self.precedences.insert(TokenType::Asterisk, PRODUCT);
        self.precedences.insert(TokenType::LParen, CALL);
        self.precedences.insert(TokenType::LBracket, INDEX);
    }

    fn init_parse_fns(&mut self) {
        self.register_prefix(TokenType::Identifier, Parser::parse_identifier_expression);
        self.register_prefix(TokenType::Integer, Parser::parse_integer_expression);
        self.register_prefix(TokenType::Bang, Parser::parse_prefix_expression);
        self.register_prefix(TokenType::Minus, Parser::parse_prefix_expression);
        self.register_prefix(TokenType::True, Parser::parse_boolean_expression);
        self.register_prefix(TokenType::False, Parser::parse_boolean_expression);
        self.register_prefix(TokenType::LParen, Parser::parse_grouped_expression);
        self.register_prefix(TokenType::If, Parser::parse_if_expression);
        self.register_prefix(TokenType::Function, Parser::parse_fn_literal_expression);
        self.register_prefix(TokenType::LBracket, Parser::parse_array_literal_expression);
        self.register_prefix(TokenType::LBrace, Parser::parse_hash_literal_expression);
        self.register_prefix(TokenType::String, Parser::parse_string_literal_expression);

        self.register_infix(TokenType::Plus, Parser::parse_infix_expression);
        self.register_infix(TokenType::Minus, Parser::parse_infix_expression);
        self.register_infix(TokenType::Asterisk, Parser::parse_infix_expression);
        self.register_infix(TokenType::Slash, Parser::parse_infix_expression);
        self.register_infix(TokenType::Equal, Parser::parse_infix_expression);
        self.register_infix(TokenType::NotEqual, Parser::parse_infix_expression);
        self.register_infix(TokenType::LessThan, Parser::parse_infix_expression);
        self.register_infix(TokenType::GreaterThan, Parser::parse_infix_expression);
        self.register_infix(TokenType::LParen, Parser::parse_call_expression);
        self.register_infix(TokenType::LBracket, Parser::parse_index_expression);
    }

    pub fn parse_program(&mut self) -> Option<Program> {
        let mut program: Program = Program(Vec::new());

        while self.curr_token.kind != TokenType::Eof {
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
        match self.curr_token.kind {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression(&mut self, precedence: usize) -> Option<Expression> {
        match self.prefix_parse_fns.get(&self.curr_token.kind) {
            Some(prefix_fn) => match prefix_fn(self) {
                Some(mut lhs) => {
                    while self.peek_token.kind != TokenType::Semicolon
                        && precedence < self.peek_precedence()
                    {
                        let infix_parse_fns = self.infix_parse_fns.clone();
                        if let Some(infix_fn) = infix_parse_fns.get(&self.peek_token.kind) {
                            self.next_token();
                            match infix_fn(self, Box::new(lhs.clone())) {
                                Some(expr) => {
                                    lhs = expr;
                                }
                                None => {
                                    self.errors
                                        .push(ParseError::Expression("operand".to_string()));
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
                    self.errors
                        .push(ParseError::Expression("operand".to_string()));
                    None
                }
            },
            None => {
                self.errors
                    .push(ParseError::PrefixParseFn(self.curr_token.kind));
                None
            }
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        if !self.expect_peek(TokenType::Identifier) {
            return None;
        }
        let let_name = self.curr_token.literal.clone();

        if !self.expect_peek(TokenType::Assign) {
            return None;
        }

        self.next_token();

        match self.parse_expression(LOWEST) {
            Some(value) => {
                if self.peek_token.kind == TokenType::Semicolon {
                    self.next_token();
                }

                Some(Statement::Let(LetStatement {
                    name: IdentifierExpression { value: let_name },
                    value,
                }))
            }
            None => {
                self.errors
                    .push(ParseError::Expression("value".to_string()));
                None
            }
        }
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();

        match self.parse_expression(LOWEST) {
            Some(value) => {
                if self.peek_token.kind == TokenType::Semicolon {
                    self.next_token();
                }

                Some(Statement::Return(ReturnStatement { value }))
            }
            None => {
                self.errors
                    .push(ParseError::Expression("return value".to_string()));
                None
            }
        }
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let expression = self.parse_expression(LOWEST);

        if self.peek_token.kind == TokenType::Semicolon {
            self.next_token();
        }

        expression.map(|expr| Statement::Expression(ExpressionStatement { expr }))
    }

    fn parse_block_statement(&mut self) -> Statement {
        let mut statements = Vec::new();

        self.next_token();

        while self.curr_token.kind != TokenType::RBrace && self.curr_token.kind != TokenType::Eof {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            }
            self.next_token();
        }

        Statement::Block(BlockStatement { statements })
    }

    fn parse_identifier_expression(&mut self) -> Option<Expression> {
        Some(Expression::Identifier(IdentifierExpression {
            value: self.curr_token.literal.clone(),
        }))
    }

    fn parse_integer_expression(&mut self) -> Option<Expression> {
        match self.curr_token.literal.parse() {
            Ok(int) => Some(Expression::Integer(IntegerExpression { value: int })),
            Err(_) => {
                self.errors
                    .push(ParseError::Token(TokenType::Integer, self.curr_token.kind));
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
                expr: Box::new(expr),
            })),
            None => {
                self.errors
                    .push(ParseError::Expression("prefix operand".to_string()));
                None
            }
        }
    }

    fn parse_infix_expression(&mut self, lhs: Box<Expression>) -> Option<Expression> {
        let operator = self.curr_token.clone();
        let precedence = self.curr_precedence();
        self.next_token();
        match self.parse_expression(precedence) {
            Some(rhs) => Some(Expression::Infix(InfixExpression {
                operator,
                lhs,
                rhs: Box::new(rhs),
            })),
            None => {
                self.errors
                    .push(ParseError::Expression("infix operand".to_string()));
                None
            }
        }
    }

    fn parse_boolean_expression(&mut self) -> Option<Expression> {
        match self.curr_token.literal.parse() {
            Ok(boolean) => Some(Expression::Boolean(BooleanExpression { value: boolean })),
            Err(_) => {
                self.errors
                    .push(ParseError::Token(TokenType::True, self.curr_token.kind));
                None
            }
        }
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();
        let expr = self.parse_expression(LOWEST);
        if !self.expect_peek(TokenType::RParen) {
            return None;
        };
        expr
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        if !self.expect_peek(TokenType::LParen) {
            return None;
        };

        self.next_token();
        match self.parse_expression(LOWEST) {
            Some(expr) => {
                let condition = Box::new(expr);

                if !self.expect_peek(TokenType::RParen) {
                    return None;
                };

                if !self.expect_peek(TokenType::LBrace) {
                    return None;
                };

                let consequence = match self.parse_block_statement() {
                    Statement::Block(block_stmt) => block_stmt,
                    _ => unreachable!(),
                };

                let alternative = if self.peek_token.kind == TokenType::Else {
                    self.next_token();
                    if !self.expect_peek(TokenType::LBrace) {
                        return None;
                    }

                    Some(match self.parse_block_statement() {
                        Statement::Block(block_stmt) => block_stmt,
                        _ => unreachable!(),
                    })
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
                self.errors
                    .push(ParseError::Expression("condition".to_string()));
                None
            }
        }
    }

    fn parse_fn_literal_expression(&mut self) -> Option<Expression> {
        if !self.expect_peek(TokenType::LParen) {
            return None;
        };

        let parameters = self.parse_function_parameters();

        if !self.expect_peek(TokenType::LBrace) {
            return None;
        };

        let body = match self.parse_block_statement() {
            Statement::Block(block_stmt) => block_stmt,
            _ => unreachable!(),
        };

        Some(Expression::FnLiteral(FnLiteralExpression {
            parameters,
            body,
        }))
    }

    fn parse_function_parameters(&mut self) -> Vec<IdentifierExpression> {
        let mut identifiers = Vec::new();

        if self.peek_token.kind == TokenType::RParen {
            self.next_token();
            return identifiers;
        };

        self.next_token();

        let identifier = IdentifierExpression {
            value: self.curr_token.literal.clone(),
        };
        identifiers.push(identifier);

        while self.peek_token.kind == TokenType::Comma {
            self.next_token();
            self.next_token();
            let identifier = IdentifierExpression {
                value: self.curr_token.literal.clone(),
            };
            identifiers.push(identifier);
        }

        if !self.expect_peek(TokenType::RParen) {
            return Vec::new();
        };

        identifiers
    }

    fn parse_string_literal_expression(&mut self) -> Option<Expression> {
        Some(Expression::String(StringExpression {
            value: self.curr_token.literal.clone(),
        }))
    }

    fn parse_call_expression(&mut self, lhs: Box<Expression>) -> Option<Expression> {
        Some(Expression::Call(CallExpression {
            function: lhs,
            arguments: self.parse_expression_list(TokenType::RParen),
        }))
    }

    fn parse_array_literal_expression(&mut self) -> Option<Expression> {
        Some(Expression::ArrayLiteral(ArrayLiteralExpression {
            elements: self.parse_expression_list(TokenType::RBracket),
        }))
    }

    fn parse_expression_list(&mut self, end: TokenType) -> Vec<Expression> {
        let mut exprs = Vec::new();

        if self.peek_token.kind == end {
            self.next_token();
            return exprs;
        }

        self.next_token();
        if let Some(expr) = self.parse_expression(LOWEST) {
            exprs.push(expr);
        }

        while self.peek_token.kind == TokenType::Comma {
            self.next_token();
            self.next_token();
            if let Some(expr) = self.parse_expression(LOWEST) {
                exprs.push(expr);
            }
        }

        if !self.expect_peek(end) {
            return Vec::new();
        }

        exprs
    }

    fn parse_index_expression(&mut self, lhs: Box<Expression>) -> Option<Expression> {
        self.next_token();
        match self.parse_expression(LOWEST) {
            Some(index) => {
                if !self.expect_peek(TokenType::RBracket) {
                    return None;
                };

                Some(Expression::Index(IndexExpression {
                    identifier: lhs,
                    index: Box::new(index),
                }))
            }
            None => {
                self.errors
                    .push(ParseError::Expression("index".to_string()));
                None
            }
        }
    }

    fn parse_hash_literal_expression(&mut self) -> Option<Expression> {
        let mut pairs = BTreeMap::new();

        while self.peek_token.kind != TokenType::RBrace {
            self.next_token();
            match self.parse_expression(LOWEST) {
                Some(key) => {
                    if !self.expect_peek(TokenType::Colon) {
                        return None;
                    }

                    self.next_token();
                    match self.parse_expression(LOWEST) {
                        Some(value) => {
                            pairs.insert(key, value);

                            if self.peek_token.kind != TokenType::RBrace
                                && !self.expect_peek(TokenType::Comma)
                            {
                                return None;
                            }
                        }
                        None => {
                            self.errors
                                .push(ParseError::Expression("hash value".to_string()));
                            return None;
                        }
                    }
                }
                None => {
                    self.errors
                        .push(ParseError::Expression("hash key".to_string()));
                    return None;
                }
            }
        }

        if !self.expect_peek(TokenType::RBrace) {
            return None;
        }

        Some(Expression::HashLiteral(HashLiteralExpression { pairs }))
    }

    fn next_token(&mut self) {
        self.curr_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn expect_peek(&mut self, expected: TokenType) -> bool {
        match self.peek_token.kind {
            token_type if token_type == expected => {
                self.next_token();
                true
            }
            _ => {
                self.errors
                    .push(ParseError::Token(expected, self.peek_token.kind));
                false
            }
        }
    }

    fn peek_precedence(&self) -> usize {
        match self.precedences.get(&self.peek_token.kind) {
            Some(precedence) => *precedence,
            None => LOWEST,
        }
    }

    fn curr_precedence(&self) -> usize {
        match self.precedences.get(&self.curr_token.kind) {
            Some(precedence) => *precedence,
            None => LOWEST,
        }
    }

    fn register_prefix(&mut self, token_type: TokenType, prefix_parse_fn: PrefixParseFn) {
        self.prefix_parse_fns.insert(token_type, prefix_parse_fn);
    }

    fn register_infix(&mut self, token_type: TokenType, infix_parse_fn: InfixParseFn) {
        self.infix_parse_fns.insert(token_type, infix_parse_fn);
    }
}
