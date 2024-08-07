mod data_type;
mod expressions;
mod keyword;
mod utils;

use crate::token::Token;
use core::fmt;
pub use data_type::DataType;
pub use data_type::NumericSize;
pub use expressions::*;
pub use keyword::{Keyword, KeywordKind};
use sql_lexer::Span;
pub use utils::*;

#[derive(Debug, PartialEq, Clone)]
pub enum Symbol {
    LeftParen { start: Span, end: Span },
}

impl Default for Symbol {
    fn default() -> Self {
        Self::LeftParen {
            start: Span::default(),
            end: Span::default(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CommonTableExpression {
    pub name: Expression,
    pub columns: Vec<Expression>,
    pub query: SelectStatement,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExecOrExecute {
    Exec,
    Execute,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ProcedureParameter {
    pub name: Option<Token>,
    pub value: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub enum CommonTableExpressionStatement {
    Select(SelectStatement),
    Insert(InsertStatement),
    Update(UpdateStatement),
    Delete(DeleteStatement),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Select(SelectStatement),
    Insert(InsertStatement),
    Update(UpdateStatement),
    Delete(DeleteStatement),
    CTE {
        ctes: Vec<CommonTableExpression>,
        statement: CommonTableExpressionStatement,
    },
    Declare(Vec<LocalVariable>),
    SetLocalVariable {
        name: Token,
        value: Expression,
    },
    Execute {
        keyword: ExecOrExecute,
        procedure_name: Expression,
        parameters: Vec<ProcedureParameter>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct LocalVariable {
    pub name: Token,
    pub is_as: bool,
    pub data_type: DataType,
    pub value: Option<Expression>,
}

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

#[derive(Debug, PartialEq, Clone)]
pub enum SelectItem {
    Wildcard,
    Unnamed(Expression),
    WithAlias {
        expression: Expression,
        as_kw: Option<Keyword>,
        alias: Expression,
    },
    WildcardWithAlias {
        expression: Expression,
        as_kw: Option<Keyword>,
        alias: Expression,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct InsertStatement {
    pub top: Option<Top>,
    pub table: Expression,
    pub columns: Vec<Expression>,
    pub values: Vec<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct UpdateStatement {
    pub top: Option<Top>,
    pub table: Expression,
    pub update_columns: Vec<Expression>,
    pub from: Option<TableArg>,
    pub where_clause: Option<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct DeleteStatement {
    pub top: Option<Top>,
    pub table: TableArg,
    pub where_clause: Option<Expression>,
}

#[derive(Debug, PartialEq, Clone, Default)]
pub struct SelectStatement {
    pub select: Keyword,
    pub distinct: Option<Keyword>,
    pub all: Option<Keyword>,
    pub top: Option<Top>,
    pub columns: Vec<SelectItem>,
    pub into_table: Option<IntoArg>,
    pub table: Option<TableArg>,
    pub where_clause: Option<WhereClause>,
    pub group_by: Option<GroupByClause>,
    pub having: Option<HavingClause>,
    pub order_by: Option<OrderByClause>,
}

impl SelectStatement {
    pub fn new() -> Self {
        SelectStatement::default()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Top {
    pub top: Keyword,
    pub with_ties: Option<Vec<Keyword>>,
    pub percent: Option<Keyword>,
    pub quantity: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub struct WhereClause {
    pub where_kw: Keyword,
    pub expression: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub struct GroupByClause {
    pub group_by_kws: Vec<Keyword>,
    pub expressions: Vec<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct OrderByClause {
    pub order_by_kws: Vec<Keyword>,
    pub expressions: Vec<OrderByArg>,
    pub offset_fetch_clause: Option<OffsetFetchClause>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct OffsetFetchClause {
    pub offset: OffsetArg,
    pub fetch: Option<FetchArg>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct HavingClause {
    pub having_kw: Keyword,
    pub expression: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IntoArg {
    pub table: Expression,
    pub file_group: Option<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TableSource {
    Table {
        name: Expression,
        alias: Option<Expression>,
    },
    Derived {
        query: Expression,
        is_as: bool,
        alias: String,
    },
    Pivot,
    Unpivot,
    TableValuedFunction {
        function: Expression,
        is_as: bool,
        alias: Option<String>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum JoinType {
    Inner,
    Left,
    LeftOuter,
    Right,
    RightOuter,
    Full,
    FullOuter,
    CrossApply,
    OuterApply,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Join {
    pub join: Vec<Keyword>,
    pub join_type: JoinType,
    pub on: Keyword,
    pub table: TableSource,
    pub condition: Option<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TableArg {
    pub from: Keyword,
    pub table: TableSource,
    pub joins: Vec<Join>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct OffsetArg {
    pub offset_kw: Keyword,
    pub value: Expression,
    pub row_or_rows_kw: Keyword,
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
    pub fetch_kw: Keyword,
    pub value: Expression,
    // either NEXT or FIRST
    pub first: NextOrFirst,
    pub first_or_next_kw: Keyword,
    // either ROW or ROWS
    pub row: RowOrRows,
    pub row_or_rows_kw: Keyword,
    pub only_kw: Keyword,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum NextOrFirst {
    Next,
    First,
}

impl fmt::Display for CommonTableExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)?;
        if !self.columns.is_empty() {
            write!(f, "(")?;
            display_list_comma_separated(&self.columns, f)?;
            write!(f, ")")?;
        }
        write!(f, " AS ({})", self.query)
    }
}

impl fmt::Display for ExecOrExecute {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            ExecOrExecute::Exec => write!(f, "EXEC"),
            ExecOrExecute::Execute => write!(f, "EXECUTE"),
        }
    }
}

impl fmt::Display for ProcedureParameter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(name) = &self.name {
            write!(f, "{} = ", name)?;
        }
        write!(f, "{}", &self.value)
    }
}

impl fmt::Display for CommonTableExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            CommonTableExpressionStatement::Select(select) => write!(f, "{}", select),
            CommonTableExpressionStatement::Insert(insert) => write!(f, "{}", insert),
            CommonTableExpressionStatement::Update(update) => write!(f, "{}", update),
            CommonTableExpressionStatement::Delete(delete) => write!(f, "{}", delete),
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Statement::Select(select) => write!(f, "{}", select),
            Statement::CTE { ctes, statement } => {
                write!(f, "WITH ")?;
                display_list_comma_separated(ctes, f)?;
                write!(f, " {}", statement)
            }
            Statement::Declare(local_variables) => {
                write!(f, "DECLARE ")?;
                display_list_comma_separated(local_variables, f)
            }
            Statement::SetLocalVariable { name, value } => write!(f, "SET {} = {}", name, value),
            Statement::Execute {
                keyword,
                procedure_name,
                parameters,
            } => {
                write!(f, "{} {}", keyword, procedure_name)?;
                display_list_comma_separated(parameters, f)
            }
            Statement::Insert(insert) => write!(f, "{}", insert),
            Statement::Update(update) => write!(f, "{}", update),
            Statement::Delete(delete) => write!(f, "{}", delete),
        }
    }
}

impl fmt::Display for LocalVariable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)?;
        if self.is_as {
            write!(f, " AS {}", self.data_type)
        } else {
            write!(f, " {}", self.data_type)
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

impl fmt::Display for SelectItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            SelectItem::Wildcard => write!(f, "*"),
            SelectItem::Unnamed(expr) => write!(f, "{}", expr),
            SelectItem::WithAlias {
                expression,
                as_kw,
                alias,
            } => {
                write!(f, "{}", expression)?;

                if let Some(kw) = as_kw {
                    write!(f, " {} ", kw)?;
                } else {
                    write!(f, " ")?;
                }

                write!(f, "{}", alias)?;
                Ok(())
            }
            SelectItem::WildcardWithAlias {
                expression,
                as_kw,
                alias,
            } => {
                write!(f, "{}", expression)?;

                if let Some(kw) = as_kw {
                    write!(f, " {} ", kw)?;
                } else {
                    write!(f, " ")?;
                }

                write!(f, "{}", alias)?;
                Ok(())
            }
        }
    }
}

impl fmt::Display for InsertStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "INSERT ")?;
        if let Some(top) = &self.top {
            write!(f, "{} ", top)?;
        }
        write!(f, "INTO {} ", self.table)?;
        if !self.columns.is_empty() {
            write!(f, "(")?;
            display_list_comma_separated(&self.columns, f)?;
            write!(f, ") ")?;
        }
        write!(f, "VALUES (")?;
        display_list_comma_separated(&self.values, f)?;
        write!(f, ") ")
    }
}

impl fmt::Display for DeleteStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "DELETE ")?;
        if let Some(top) = &self.top {
            write!(f, "{} ", top)?;
        }
        write!(f, "FROM {} ", self.table)?;
        if let Some(where_clause) = &self.where_clause {
            write!(f, " WHERE {}", where_clause)?;
        }

        Ok(())
    }
}

impl fmt::Display for UpdateStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "UPDATE ")?;
        if let Some(top) = &self.top {
            write!(f, "{} ", top)?;
        }
        write!(f, "{} ", self.table)?;
        f.write_str("SET ")?;

        if !self.update_columns.is_empty() {
            display_list_comma_separated(&self.update_columns, f)?;
        }

        // FROM
        if let Some(from_table) = &self.from {
            write!(f, " FROM {}", from_table)?;
        }

        // WHERE
        if let Some(where_clause) = &self.where_clause {
            write!(f, " WHERE {}", where_clause)?;
        }

        Ok(())
    }
}

impl fmt::Display for SelectStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // SELECT
        write!(f, "{}", self.select)?;

        // DISTINCT
        if let Some(distinct) = &self.distinct {
            write!(f, " {}", distinct)?;
        }

        // ALL
        if let Some(distinct) = &self.all {
            write!(f, " {}", distinct)?;
        }

        // TOP
        if let Some(top) = &self.top {
            write!(f, " {}", top)?;
        }

        // COLUMNS
        if !self.columns.is_empty() {
            f.write_str(" ")?;
            display_list_comma_separated(&self.columns, f)?;
        }

        if let Some(into_table) = &self.into_table {
            write!(f, " INTO {} ", into_table)?;
        }

        // FROM
        if let Some(table) = &self.table {
            write!(f, " {}", table)?;
        }

        // WHERE
        if let Some(where_clause) = &self.where_clause {
            write!(f, " {}", where_clause)?;
        }

        // GROUPING
        if let Some(group_by_clause) = &self.group_by {
            f.write_str(" ")?;
            display_list_delimiter_separated(&group_by_clause.group_by_kws, " ", f)?;
            f.write_str(" ")?;
            display_list_comma_separated(&group_by_clause.expressions, f)?;
        }

        // HAVING
        if let Some(having_clause) = &self.having {
            write!(
                f,
                " {} {}",
                having_clause.having_kw, having_clause.expression
            )?;
        }

        // ORDER BY
        if let Some(order_by_clause) = &self.order_by {
            f.write_str(" ")?;
            display_list_delimiter_separated(&order_by_clause.order_by_kws, " ", f)?;
            f.write_str(" ")?;
            display_list_comma_separated(&order_by_clause.expressions, f)?;
            if let Some(offset_fetch_clause) = &order_by_clause.offset_fetch_clause {
                write!(f, "{}", offset_fetch_clause)?;
            }
        }

        Ok(())
    }
}

impl fmt::Display for Top {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.top, self.quantity)?;
        if let Some(percent) = &self.percent {
            write!(f, " {}", percent)?;
        }
        if let Some(with_ties) = &self.with_ties {
            f.write_str(" ")?;
            display_list_delimiter_separated(with_ties, " ", f)?;
        }

        Ok(())
    }
}

impl fmt::Display for WhereClause {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.where_kw, self.expression)?;

        Ok(())
    }
}

impl fmt::Display for OffsetFetchClause {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", &self.offset)?;

        if let Some(fetch) = &self.fetch {
            write!(f, "{}", fetch)?;
        }

        Ok(())
    }
}

impl fmt::Display for IntoArg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.file_group {
            Some(file_group) => write!(f, "INTO {} ON {}", self.table, file_group),
            None => write!(f, "INTO {}", self.table),
        }
    }
}

impl fmt::Display for TableSource {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            TableSource::Table { name, alias } => match alias {
                Some(alias) => {
                    write!(f, "{} {}", name, alias)
                }
                None => write!(f, "{}", name),
            },
            TableSource::Derived {
                query,
                is_as,
                alias,
            } => {
                if *is_as {
                    write!(f, "{} AS {}", query, alias)
                } else {
                    write!(f, "{} {}", query, alias)
                }
            }
            TableSource::Pivot => write!(f, "PIVOT"),
            TableSource::Unpivot => write!(f, "UNPIVOT"),
            TableSource::TableValuedFunction {
                function,
                is_as,
                alias,
            } => {
                write!(f, "{}", function)?;
                if let Some(alias) = alias {
                    if *is_as {
                        write!(f, " AS {}", alias)?;
                    } else {
                        write!(f, " {}", alias)?;
                    }
                }

                Ok(())
            }
        }
    }
}

impl fmt::Display for JoinType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            JoinType::Inner => write!(f, "INNER JOIN"),
            JoinType::Left => write!(f, "LEFT JOIN"),
            JoinType::LeftOuter => write!(f, "LEFT JOIN OUTER"),
            JoinType::Right => write!(f, "RIGHT JOIN"),
            JoinType::RightOuter => write!(f, "RIGHT JOIN OUTER"),
            JoinType::Full => write!(f, "FULL JOIN "),
            JoinType::FullOuter => write!(f, "FULL JOIN OUTER"),
            JoinType::CrossApply => write!(f, "CROSS APPLY"),
            JoinType::OuterApply => write!(f, "OUTER APPLY"),
        }
    }
}

impl fmt::Display for Join {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.join_type, self.table)?;
        if let Some(condition) = &self.condition {
            write!(f, " ON {}", condition)?;
        }
        Ok(())
    }
}
impl fmt::Display for TableArg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.from, self.table)?;

        for join in &self.joins {
            write!(f, " {}", join)?;
        }
        Ok(())
    }
}

impl fmt::Display for OffsetArg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            " {} {} {}",
            self.offset_kw, self.value, self.row_or_rows_kw
        )
    }
}

impl fmt::Display for RowOrRows {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RowOrRows::Row => write!(f, "ROW"),
            RowOrRows::Rows => write!(f, "ROWS"),
        }
    }
}

impl fmt::Display for FetchArg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            " {} {} {} {} {}",
            self.fetch_kw, self.first_or_next_kw, self.value, self.row_or_rows_kw, self.only_kw
        )
    }
}

impl fmt::Display for NextOrFirst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            NextOrFirst::Next => write!(f, "NEXT"),
            NextOrFirst::First => write!(f, "FIRST"),
        }
    }
}
