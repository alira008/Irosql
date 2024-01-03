use crate::token::Token;

#[derive(Debug, PartialEq, Clone)]
pub struct Query {
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Literal(Token),
    Binary {
        left: Box<Expression>,
        operator: Token,
        right: Box<Expression>,
    },
    Unary {
        operator: Token,
        right: Box<Expression>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Select(SelectStatement),
}

impl Query {
    pub fn new() -> Self {
        Query {
            statements: Vec::new(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct SelectStatement {
    pub columns: Vec<Expression>,
    pub table: Vec<Expression>,
    pub where_clause: Option<Expression>,
}

impl SelectStatement {
    pub fn new() -> Self {
        SelectStatement {
            columns: vec![],
            table: vec![],
            where_clause: None,
        }
    }
}
