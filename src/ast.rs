use std::fmt::Display;

use crate::token::Token;

trait Node {
    fn to_string(&self) -> String;
}

#[derive(PartialEq, Debug, Clone)]
pub struct Program(pub Vec<Statement>);

impl std::ops::Deref for Program {
    type Target = Vec<Statement>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Node for Program {
    fn to_string(&self) -> String {
        (*self
            .iter()
            .map(|stmt| stmt.to_string())
            .collect::<Vec<String>>()
            .join("\n"))
        .to_string()
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", Node::to_string(self))
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
    Block(BlockStatement),
}

impl Node for Statement {
    fn to_string(&self) -> String {
        match self {
            Self::Let(stmt) => stmt.to_string(),
            Self::Return(stmt) => stmt.to_string(),
            Self::Expression(stmt) => stmt.to_string(),
            Self::Block(stmt) => stmt.to_string(),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct LetStatement {
    pub name: IdentifierExpression,
    pub value: Expression,
}

impl Node for LetStatement {
    fn to_string(&self) -> String {
        [
            "let ".to_string(),
            self.name.to_string(),
            " = ".to_string(),
            self.value.to_string(),
            ";".to_string(),
        ]
        .join("")
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct ReturnStatement {
    pub value: Expression,
}

impl Node for ReturnStatement {
    fn to_string(&self) -> String {
        [
            "return ".to_string(),
            self.value.to_string(),
            ";".to_string(),
        ]
        .join("")
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct ExpressionStatement {
    pub expr: Expression,
}

impl Node for ExpressionStatement {
    fn to_string(&self) -> String {
        [self.expr.to_string(), ";".to_string()].join("")
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

impl Node for BlockStatement {
    fn to_string(&self) -> String {
        self.statements
            .iter()
            .map(|stmt| stmt.to_string())
            .collect::<Vec<String>>()
            .join("")
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    Identifier(IdentifierExpression),
    Integer(IntegerExpression),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Boolean(BooleanExpression),
    If(IfExpression),
    FnLiteral(FnLiteralExpression),
    Call(CallExpression),
}

impl Node for Expression {
    fn to_string(&self) -> String {
        match self {
            Expression::Identifier(expr) => expr.to_string(),
            Expression::Integer(expr) => expr.to_string(),
            Expression::Prefix(expr) => expr.to_string(),
            Expression::Infix(expr) => expr.to_string(),
            Expression::Boolean(expr) => expr.to_string(),
            Expression::If(expr) => expr.to_string(),
            Expression::FnLiteral(expr) => expr.to_string(),
            Expression::Call(expr) => expr.to_string(),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct IdentifierExpression {
    pub value: String,
}

impl Node for IdentifierExpression {
    fn to_string(&self) -> String {
        self.value.clone()
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct IntegerExpression {
    pub value: i32,
}

impl Node for IntegerExpression {
    fn to_string(&self) -> String {
        self.value.to_string()
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct PrefixExpression {
    pub prefix: Token,
    pub expr: Box<Expression>,
}

impl Node for PrefixExpression {
    fn to_string(&self) -> String {
        [
            "(".to_string(),
            self.prefix.literal.clone(),
            self.expr.to_string(),
            ")".to_string(),
        ]
        .join("")
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct InfixExpression {
    pub operator: Token,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

impl Node for InfixExpression {
    fn to_string(&self) -> String {
        [
            "(".to_string(),
            self.lhs.to_string(),
            " ".to_string(),
            self.operator.literal.clone(),
            " ".to_string(),
            self.rhs.to_string(),
            ")".to_string(),
        ]
        .join("")
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct BooleanExpression {
    pub value: bool,
}

impl Node for BooleanExpression {
    fn to_string(&self) -> String {
        self.value.to_string()
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct IfExpression {
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl Node for IfExpression {
    fn to_string(&self) -> String {
        [
            "if ".to_string(),
            self.condition.to_string(),
            " ".to_string(),
            self.consequence.to_string(),
            match &self.alternative {
                Some(stmt) => ["else ".to_string(), stmt.to_string()].join(""),
                None => "".to_string(),
            },
        ]
        .join("")
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct FnLiteralExpression {
    pub parameters: Vec<IdentifierExpression>,
    pub body: BlockStatement,
}

impl Node for FnLiteralExpression {
    fn to_string(&self) -> String {
        [
            "fn(".to_string(),
            self.parameters
                .iter()
                .map(|stmt| stmt.to_string())
                .collect::<Vec<String>>()
                .join(""),
            ") ".to_string(),
            self.body.to_string(),
        ]
        .join("")
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct CallExpression {
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

impl Node for CallExpression {
    fn to_string(&self) -> String {
        [
            self.function.to_string(),
            "(".to_string(),
            self.arguments
                .iter()
                .map(|stmt| stmt.to_string())
                .collect::<Vec<String>>()
                .join(", "),
            ")".to_string(),
        ]
        .join("")
    }
}
