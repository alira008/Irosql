use core::fmt;

use crate::token::Token;

#[derive(Debug, PartialEq, Clone)]
pub struct Query {
    pub statements: Vec<Statement>,
}

impl Query {
    pub fn new() -> Self {
        Query {
            statements: Vec::new(),
        }
    }
}

impl fmt::Display for Query {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for statement in &self.statements {
            write!(f, "{}", statement)?;
        }
        Ok(())
    }
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

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Expression::Literal(token) => write!(f, "{}", token),
            Expression::Binary {
                left,
                operator,
                right,
            } => write!(f, "{} {} {}", left, operator, right),
            Expression::Unary { operator, right } => write!(f, "{} {}", operator, right),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Select(SelectStatement),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Statement::Select(select) => write!(f, "{}", select),
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

impl fmt::Display for SelectStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // SELECT
        f.write_str("SELECT ")?;

        // DISTINCT
        if self.distinct {
            f.write_str("DISTINCT ")?;
        }

        // TOP
        if let Some(top) = &self.top {
            write!(f, "{} ", top)?;
        }

        // COLUMNS
        for (i, col) in self.columns.iter().enumerate() {
            write!(f, "{} ", col)?;

            // only print comma if not last column
            if i < self.columns.len() - 1 {
                f.write_str(", ")?;
            }
        }

        // FROM
        f.write_str("FROM ")?;

        // TABLE
        for (i, table) in self.table.iter().enumerate() {
            write!(f, "{} ", table)?;
            // only print comma if not last table
            if i < self.table.len() - 1 {
                f.write_str(", ")?;
            }
        }

        // WHERE
        if let Some(where_clause) = &self.where_clause {
            write!(f, "WHERE {} ", where_clause)?;
        }

        // ORDER BY
        if !self.order_by.is_empty() {
            f.write_str("ORDER BY ")?;
            for (i, order_by) in self.order_by.iter().enumerate() {
                write!(f, "{} ", order_by)?;
                // only print comma if not last order_by
                if i < self.order_by.len() - 1 {
                    f.write_str(", ")?;
                }
            }
        }

        // OFFSET
        if let Some(offset) = &self.offset {
            write!(f, "{} ", offset)?;
        }

        // FETCH
        if let Some(fetch) = &self.fetch {
            write!(f, "{} ", fetch)?;
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TopArg {
    pub with_ties: bool,
    pub percent: bool,
    pub quantity: Expression,
}

impl fmt::Display for TopArg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match (self.with_ties, self.percent) {
            (true, true) => write!(f, "TOP {} PERCENT WITH TIES", self.quantity),
            (true, false) => write!(f, "TOP {} WITH TIES", self.quantity),
            (false, true) => write!(f, "TOP {} PERCENT", self.quantity),
            (false, false) => write!(f, "TOP {}", self.quantity),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct OrderByArg {
    pub column: Expression,
    pub asc: Option<bool>,
}

impl fmt::Display for OrderByArg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.asc {
            Some(true) => write!(f, "{} ASC", self.column),
            Some(false) => write!(f, "{} DESC", self.column),
            None => write!(f, "{}", self.column),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct OffsetArg {
    pub value: Expression,
    // either ROW or ROWS
    pub row: RowOrRows,
}

impl fmt::Display for OffsetArg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "OFFSET {} {}", self.value, self.row)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum RowOrRows {
    Row,
    Rows,
}

impl fmt::Display for RowOrRows {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RowOrRows::Row => write!(f, "ROW"),
            RowOrRows::Rows => write!(f, "ROWS"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FetchArg {
    pub value: Expression,
    // either NEXT or FIRST
    pub first: NextOrFirst,
    // either ROW or ROWS
    pub row: RowOrRows,
}

impl fmt::Display for FetchArg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "FETCH {} {} {} ONLY", self.first, self.value, self.row)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum NextOrFirst {
    Next,
    First,
}

impl fmt::Display for NextOrFirst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            NextOrFirst::Next => write!(f, "NEXT"),
            NextOrFirst::First => write!(f, "FIRST"),
        }
    }
}
