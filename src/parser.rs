use std::fmt::Display;
use std::{collections::HashMap, error::Error};

use crate::ast::StringExpression;
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

type PrefixParseFn = for<'a> fn(&'a mut Parser) -> Option<Expression>;
type InfixParseFn = for<'a> fn(&'a mut Parser, Box<Expression>) -> Option<Expression>;

#[derive(Debug)]
enum ParseError {
    ParseError(TokenType, TokenType),
    PrefixParseFnError(TokenType),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::ParseError(expected, result) => {
                write!(
                    f,
                    "parse error: expected {:?}, found {:?}",
                    expected, result
                )
            }
            ParseError::PrefixParseFnError(token_type) => {
                write!(
                    f,
                    "parse error: no prefix function found for {:?}",
                    token_type
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
            Some(prefix_fn) => {
                let mut lhs = prefix_fn(self);

                while self.peek_token.kind != TokenType::Semicolon
                    && precedence < self.peek_precedence()
                {
                    let infix_parse_fns = self.infix_parse_fns.clone();
                    if let Some(infix_fn) = infix_parse_fns.get(&self.peek_token.kind) {
                        self.next_token();
                        lhs = infix_fn(self, Box::new(lhs.unwrap()));
                    } else {
                        return lhs;
                    }
                }

                lhs
            }
            None => {
                self.errors
                    .push(ParseError::PrefixParseFnError(self.curr_token.kind));
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

        let value = self.parse_expression(LOWEST).unwrap();

        if self.peek_token.kind == TokenType::Semicolon {
            self.next_token();
        }

        Some(Statement::Let(LetStatement {
            name: IdentifierExpression { value: let_name },
            value,
        }))
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();

        let value = self.parse_expression(LOWEST).unwrap();

        if self.peek_token.kind == TokenType::Semicolon {
            self.next_token();
        }

        Some(Statement::Return(ReturnStatement { value }))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let expression = self.parse_expression(LOWEST);

        if self.peek_token.kind == TokenType::Semicolon {
            self.next_token();
        }

        expression.map(|expr| Statement::Expression(ExpressionStatement { expr }))
    }

    fn parse_block_statement(&mut self) -> Option<Statement> {
        let mut statements = Vec::new();

        self.next_token();

        while !(self.curr_token.kind == TokenType::RBrace)
            && !(self.curr_token.kind == TokenType::Eof)
        {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            }
            self.next_token();
        }

        Some(Statement::Block(BlockStatement { statements }))
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
                self.errors.push(ParseError::ParseError(
                    TokenType::Integer,
                    self.curr_token.kind,
                ));
                None
            }
        }
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let prefix = self.curr_token.clone();
        self.next_token();
        let expr = Box::new(self.parse_expression(PREFIX).unwrap());
        Some(Expression::Prefix(PrefixExpression { prefix, expr }))
    }

    fn parse_infix_expression(&mut self, lhs: Box<Expression>) -> Option<Expression> {
        let operator = self.curr_token.clone();
        let precedence = self.curr_precedence();
        self.next_token();
        let rhs = Box::new(self.parse_expression(precedence).unwrap());
        Some(Expression::Infix(InfixExpression { operator, lhs, rhs }))
    }

    fn parse_boolean_expression(&mut self) -> Option<Expression> {
        match self.curr_token.literal.parse() {
            Ok(boolean) => Some(Expression::Boolean(BooleanExpression { value: boolean })),
            Err(_) => {
                self.errors.push(ParseError::ParseError(
                    TokenType::True,
                    self.curr_token.kind,
                ));
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
        let condition = Box::new(self.parse_expression(LOWEST).unwrap());

        if !self.expect_peek(TokenType::RParen) {
            return None;
        };

        if !self.expect_peek(TokenType::LBrace) {
            return None;
        };

        // HACK: Disgusting enum coercion
        let consequence = self
            .parse_block_statement()
            .map(|stmt| match stmt {
                Statement::Block(block_stmt) => block_stmt,
                _ => unreachable!(),
            })
            .unwrap();

        let alternative = if self.peek_token.kind == TokenType::Else {
            self.next_token();
            if !self.expect_peek(TokenType::LBrace) {
                return None;
            }

            // HACK: Same here
            self.parse_block_statement().map(|stmt| match stmt {
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

    fn parse_fn_literal_expression(&mut self) -> Option<Expression> {
        if !self.expect_peek(TokenType::LParen) {
            return None;
        };

        let parameters = self.parse_function_parameters();

        if !self.expect_peek(TokenType::LBrace) {
            return None;
        };

        let body = self
            .parse_block_statement()
            .map(|stmt| match stmt {
                Statement::Block(block_stmt) => block_stmt,
                _ => unreachable!(),
            })
            .unwrap();

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
            arguments: self.parse_call_arguments(),
        }))
    }

    fn parse_call_arguments(&mut self) -> Vec<Expression> {
        let mut arguments = Vec::new();

        if self.peek_token.kind == TokenType::RParen {
            self.next_token();
            return arguments;
        };

        self.next_token();
        if let Some(expr) = self.parse_expression(LOWEST) {
            arguments.push(expr);
        }

        while self.peek_token.kind == TokenType::Comma {
            self.next_token();
            self.next_token();
            if let Some(expr) = self.parse_expression(LOWEST) {
                arguments.push(expr);
            }
        }

        if !self.expect_peek(TokenType::RParen) {
            return Vec::new();
        };

        arguments
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
                    .push(ParseError::ParseError(expected, self.peek_token.kind));
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
