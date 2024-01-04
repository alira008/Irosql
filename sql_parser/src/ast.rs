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

#[derive(Debug, PartialEq, Clone, Default)]
pub struct SelectStatement {
    pub distinct: bool,
    pub top: Option<TopArg>,
    pub columns: Vec<Expression>,
    pub table: Vec<Expression>,
    pub where_clause: Option<Expression>,
    pub order_by: Vec<OrderByArg>,
    pub offset: Option<OffsetArg>,
    pub fetch: Option<FetchArg>,
}

impl SelectStatement {
    pub fn new() -> Self {
        SelectStatement::default()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TopArg {
    pub with_ties: bool,
    pub percent: bool,
    pub quantity: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub struct OrderByArg {
    pub column: Expression,
    pub asc: Option<bool>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct OffsetArg {
    pub value: Expression,
    // either ROW or ROWS
    pub row: RowOrRows,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum RowOrRows {
    Row,
    Rows,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FetchArg {
    pub value: Expression,
    // either NEXT or FIRST
    pub first: NextOrFirst,
    // either ROW or ROWS
    pub row: RowOrRows,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum NextOrFirst {
    Next,
    First,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum FetchRow {
    Row, Rows
}
