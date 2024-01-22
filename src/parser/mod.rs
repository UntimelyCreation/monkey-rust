use std::collections::BTreeMap;

use crate::{lexer::token::Token, lexer::Lexer, parser::ast::*};

pub mod ast;
mod test_parser;

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

type PrefixParseFn = for<'a> fn(&'a mut Parser) -> Result<Expression, ParseError>;
type InfixParseFn = for<'a> fn(&'a mut Parser, Box<Expression>) -> Result<Expression, ParseError>;

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
type ParseErrors = Vec<ParseError>;

pub fn eprint_parse_errors(errs: &ParseErrors) {
    for err in errs.iter() {
        eprintln!("parse error: {}", err);
    }
}

fn fmt_token_error(expected: &Token, result: &Token) -> ParseError {
    format!(
        "expected {}, found {}",
        expected.get_literal(),
        result.get_literal()
    )
}

pub fn parse(input: &str) -> Result<Node, ParseErrors> {
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

    pub fn parse_program(&mut self) -> Result<Program, ParseErrors> {
        let mut program: Program = Program(Vec::new());

        while self.curr_token != Token::Eof {
            match self.parse_statement() {
                Ok(stmt) => program.0.push(stmt),
                Err(err) => self.errors.push(err),
            }
            self.next_token();
        }

        if self.errors.is_empty() {
            Ok(program)
        } else {
            Err(self.errors.clone())
        }
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.curr_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParseError> {
        let mut name = String::new();
        match &self.peek_token {
            Token::Identifier(identifier) => {
                name = identifier.to_owned();
                self.next_token();

                self.expect_peek(&Token::Assign)?;

                self.next_token();

                let value = self.parse_expression(LOWEST)?;

                if self.peek_token == Token::Semicolon {
                    self.next_token();
                }

                Ok(Statement::Let(LetStatement {
                    identifier: IdentifierExpression { name },
                    value,
                }))
            }
            token => Err(format!(
                "expected identifier, found {}",
                token.get_literal()
            )),
        }
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParseError> {
        self.next_token();

        let value = self.parse_expression(LOWEST)?;

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        Ok(Statement::Return(ReturnStatement { value }))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParseError> {
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
            if let Ok(stmt) = self.parse_statement() {
                statements.push(stmt);
            }
            self.next_token();
        }

        BlockStatement { statements }
    }

    fn parse_expression(&mut self, precedence: usize) -> Result<Expression, ParseError> {
        match get_prefix_fn(&self.curr_token) {
            Some(prefix_fn) => {
                let mut lhs = prefix_fn(self)?;

                while self.peek_token != Token::Semicolon
                    && precedence < get_precedence(&self.peek_token)
                {
                    if let Some(infix_fn) = get_infix_fn(&self.peek_token) {
                        self.next_token();
                        lhs = infix_fn(self, Box::new(lhs.clone()))?;
                    } else {
                        return Ok(lhs);
                    }
                }

                Ok(lhs)
            }
            None => Err(format!(
                "no prefix function found for {}",
                self.curr_token.get_literal()
            )),
        }
    }

    fn parse_identifier_expression(&mut self) -> Result<Expression, ParseError> {
        Ok(Expression::Identifier(IdentifierExpression {
            name: self.curr_token.get_literal(),
        }))
    }

    fn parse_integer_expression(&mut self) -> Result<Expression, ParseError> {
        match self.curr_token.get_literal().parse() {
            Ok(int) => Ok(Expression::Integer(IntegerExpression { value: int })),
            Err(_) => Err(format!(
                "expected integer, found {}",
                self.curr_token.get_literal()
            )),
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, ParseError> {
        let prefix = self.curr_token.clone();
        self.next_token();
        let operand = self.parse_expression(PREFIX)?;

        Ok(Expression::Prefix(PrefixExpression {
            operator: prefix,
            operand: Box::new(operand),
        }))
    }

    fn parse_infix_expression(&mut self, lhs: Box<Expression>) -> Result<Expression, ParseError> {
        let operator = self.curr_token.clone();
        let precedence = get_precedence(&self.curr_token);
        self.next_token();
        let rhs = self.parse_expression(precedence)?;

        Ok(Expression::Infix(InfixExpression {
            operator,
            lhs,
            rhs: Box::new(rhs),
        }))
    }

    fn parse_boolean_expression(&mut self) -> Result<Expression, ParseError> {
        match self.curr_token.get_literal().parse() {
            Ok(boolean) => Ok(Expression::Boolean(BooleanExpression { value: boolean })),
            Err(_) => Err(format!(
                "expected boolean, found {}",
                self.curr_token.get_literal()
            )),
        }
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, ParseError> {
        self.next_token();
        let expr = self.parse_expression(LOWEST)?;
        self.expect_peek(&Token::RParen)?;

        Ok(expr)
    }

    fn parse_if_expression(&mut self) -> Result<Expression, ParseError> {
        self.expect_peek(&Token::LParen)?;

        self.next_token();
        let expr = self.parse_expression(LOWEST)?;
        let condition = Box::new(expr);

        self.expect_peek(&Token::RParen)?;

        self.expect_peek(&Token::LBrace)?;

        let consequence = self.parse_block_statement();

        let alternative = if self.peek_token == Token::Else {
            self.next_token();
            self.expect_peek(&Token::LBrace)?;

            Some(self.parse_block_statement())
        } else {
            None
        };

        Ok(Expression::If(IfExpression {
            condition,
            consequence,
            alternative,
        }))
    }

    fn parse_fn_literal_expression(&mut self) -> Result<Expression, ParseError> {
        self.expect_peek(&Token::LParen)?;

        let parameters = self.parse_function_parameters()?;

        self.expect_peek(&Token::LBrace)?;

        let body = self.parse_block_statement();

        Ok(Expression::FnLiteral(FnLiteralExpression {
            parameters,
            body,
        }))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<IdentifierExpression>, ParseError> {
        let mut parameters = Vec::new();

        if self.peek_token == Token::RParen {
            self.next_token();
            return Ok(parameters);
        };

        self.next_token();

        match &self.curr_token {
            Token::Identifier(name) => {
                parameters.push(IdentifierExpression {
                    name: name.to_owned(),
                });
            }
            token => {
                return Err(format!(
                    "expected identifier, found {}",
                    token.get_literal()
                ))
            }
        }

        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();
            match &self.curr_token {
                Token::Identifier(identifier) => parameters.push(IdentifierExpression {
                    name: identifier.to_owned(),
                }),
                token => {
                    return Err(format!(
                        "expected identifier, found {}",
                        token.get_literal()
                    ))
                }
            }
        }

        self.expect_peek(&Token::RParen)?;

        Ok(parameters)
    }

    fn parse_string_literal_expression(&mut self) -> Result<Expression, ParseError> {
        Ok(Expression::String(StringExpression {
            value: self.curr_token.get_literal(),
        }))
    }

    fn parse_call_expression(&mut self, lhs: Box<Expression>) -> Result<Expression, ParseError> {
        Ok(Expression::Call(CallExpression {
            function: lhs,
            arguments: self.parse_expression_list(Token::RParen)?,
        }))
    }

    fn parse_array_literal_expression(&mut self) -> Result<Expression, ParseError> {
        Ok(Expression::ArrayLiteral(ArrayLiteralExpression {
            elements: self.parse_expression_list(Token::RBracket)?,
        }))
    }

    fn parse_expression_list(&mut self, end: Token) -> Result<Vec<Expression>, ParseError> {
        let mut exprs = Vec::new();

        if self.peek_token == end {
            self.next_token();
            return Ok(exprs);
        }

        self.next_token();
        if let Ok(expr) = self.parse_expression(LOWEST) {
            exprs.push(expr);
        }

        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();
            if let Ok(expr) = self.parse_expression(LOWEST) {
                exprs.push(expr);
            }
        }

        self.expect_peek(&end)?;

        Ok(exprs)
    }

    fn parse_index_expression(&mut self, lhs: Box<Expression>) -> Result<Expression, ParseError> {
        self.next_token();
        let index = self.parse_expression(LOWEST)?;
        self.expect_peek(&Token::RBracket)?;

        Ok(Expression::Index(IndexExpression {
            identifier: lhs,
            index: Box::new(index),
        }))
    }

    fn parse_hash_literal_expression(&mut self) -> Result<Expression, ParseError> {
        let mut pairs = BTreeMap::new();

        while self.peek_token != Token::RBrace {
            self.next_token();
            let key = self.parse_expression(LOWEST)?;

            self.expect_peek(&Token::Colon)?;

            self.next_token();
            let value = self.parse_expression(LOWEST)?;
            pairs.insert(key, value);

            if self.peek_token != Token::RBrace {
                self.expect_peek(&Token::Comma)?;
            }
        }

        self.expect_peek(&Token::RBrace)?;

        Ok(Expression::HashLiteral(HashLiteralExpression { pairs }))
    }

    fn next_token(&mut self) {
        self.curr_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn expect_peek(&mut self, expected: &Token) -> Result<(), ParseError> {
        match &self.peek_token {
            token if token == expected => {
                self.next_token();
                Ok(())
            }
            _ => Err(fmt_token_error(expected, &self.peek_token)),
        }
    }
}
