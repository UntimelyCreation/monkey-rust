use std::{
    collections::BTreeMap,
    fmt::{Display, Formatter, Result},
};

use crate::token::Token;

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}

impl Display for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Program(prgm) => write!(f, "{}", prgm),
            Self::Statement(stmt) => write!(f, "{}", stmt),
            Self::Expression(expr) => write!(f, "{}", expr),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub struct Program(pub Vec<Statement>);

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", fmt_statements(&self.0, "\n"))
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Let(stmt) => {
                write!(f, "let {} = {};", stmt.identifier, stmt.value)
            }
            Self::Return(stmt) => write!(f, "return {};", stmt.value),
            Self::Expression(stmt) => write!(f, "{};", stmt.expr),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub struct LetStatement {
    pub identifier: IdentifierExpression,
    pub value: Expression,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub struct ReturnStatement {
    pub value: Expression,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub struct ExpressionStatement {
    pub expr: Expression,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", fmt_statements(&self.statements, ""))
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub enum Expression {
    Identifier(IdentifierExpression),
    Integer(IntegerExpression),
    String(StringExpression),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Boolean(BooleanExpression),
    If(IfExpression),
    FnLiteral(FnLiteralExpression),
    ArrayLiteral(ArrayLiteralExpression),
    HashLiteral(HashLiteralExpression),
    Call(CallExpression),
    Index(IndexExpression),
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Expression::Identifier(expr) => write!(f, "{}", expr),
            Expression::Integer(expr) => write!(f, "{}", expr.value),
            Expression::String(expr) => write!(f, "{}", expr.value),
            Expression::Prefix(expr) => {
                write!(f, "({}{})", expr.prefix.get_literal(), expr.operand)
            }
            Expression::Infix(expr) => write!(
                f,
                "({} {} {})",
                expr.lhs,
                expr.operator.get_literal(),
                expr.rhs
            ),
            Expression::Boolean(expr) => write!(f, "{}", expr.value),
            Expression::If(expr) => match &expr.alternative {
                Some(alternative) => write!(
                    f,
                    "if {} {{ {} }} else {{ {} }}",
                    expr.condition, expr.consequence, alternative
                ),
                None => write!(f, "if {} {{ {} }}", expr.condition, expr.consequence),
            },
            Expression::FnLiteral(expr) => write!(
                f,
                "fn({}) {{ {} }}",
                fmt_identifier_expressions(&expr.parameters, ", "),
                expr.body
            ),
            Expression::ArrayLiteral(expr) => {
                write!(f, "[{}]", fmt_expressions(&expr.elements, ", "))
            }
            Expression::HashLiteral(expr) => write!(
                f,
                "{{{}}}",
                expr.pairs
                    .iter()
                    .map(|(k, v)| [k.to_string(), v.to_string()].join(": "))
                    .collect::<Vec<String>>()
                    .join(", "),
            ),
            Expression::Call(expr) => write!(
                f,
                "{}({})",
                expr.function,
                fmt_expressions(&expr.arguments, ", "),
            ),
            Expression::Index(expr) => write!(f, "({}[{}])", expr.identifier, expr.index),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub struct IdentifierExpression {
    pub name: String,
}

impl Display for IdentifierExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.name)
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub struct IntegerExpression {
    pub value: i32,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub struct StringExpression {
    pub value: String,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub struct PrefixExpression {
    pub prefix: Token,
    pub operand: Box<Expression>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub struct InfixExpression {
    pub operator: Token,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub struct BooleanExpression {
    pub value: bool,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub struct IfExpression {
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub struct FnLiteralExpression {
    pub parameters: Vec<IdentifierExpression>,
    pub body: BlockStatement,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub struct ArrayLiteralExpression {
    pub elements: Vec<Expression>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub struct HashLiteralExpression {
    pub pairs: BTreeMap<Expression, Expression>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub struct CallExpression {
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub struct IndexExpression {
    pub identifier: Box<Expression>,
    pub index: Box<Expression>,
}

fn fmt_statements(stmts: &[Statement], separator: &str) -> String {
    stmts
        .iter()
        .map(|stmt| stmt.to_string())
        .collect::<Vec<String>>()
        .join(separator)
}

pub fn fmt_identifier_expressions(exprs: &[IdentifierExpression], separator: &str) -> String {
    exprs
        .iter()
        .map(|stmt| stmt.to_string())
        .collect::<Vec<String>>()
        .join(separator)
}

fn fmt_expressions(exprs: &[Expression], separator: &str) -> String {
    exprs
        .iter()
        .map(|stmt| stmt.to_string())
        .collect::<Vec<String>>()
        .join(separator)
}
