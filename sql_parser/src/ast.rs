use crate::token::{TokenType, Identifier};

pub struct Query {
    pub statements: Vec<Statement>,
}

pub enum Expression {
    Identifier(Identifier),
    Number(f64),
    QuotedString(String),
}

pub enum Statement {
    SelectStatement(SelectStatement),
}

impl Query {
    pub fn new() -> Self {
        Query {
            statements: Vec::new(),
        }
    }
}

pub struct SelectStatement {
    pub token: TokenType, // the token::TokenType::SELECT
    pub columns: Vec<Expression>,
    pub table: Vec<Expression>,
    pub where_clause: Option<Expression>,
}

impl SelectStatement {
    pub fn new(token: TokenType) -> Self {
        SelectStatement {
            token,
            columns: vec![],
            table: vec![],
            where_clause: None,
        }
    }
}

