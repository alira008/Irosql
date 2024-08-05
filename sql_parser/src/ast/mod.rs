pub mod data_type;
mod keyword;
use crate::token::Token;
use core::fmt;
use sql_lexer::Span;
pub use keyword::{Keyword, KeywordKind};


pub use data_type::DataType;
pub use data_type::NumericSize;

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
pub enum Expression {
    Identifier(String),
    QuotedIdentifier(String),
    StringLiteral(String),
    NumberLiteral(String),
    LocalVariable(String),
    Compound(Vec<Expression>),
    Asterisk,
    Literal(Token),
    CompoundLiteral(Vec<Token>),
    Binary {
        left: Box<Expression>,
        operator: Token,
        right: Box<Expression>,
    },
    Unary {
        operator: Token,
        right: Box<Expression>,
    },
    Grouping(Box<Expression>),
    Subquery(Box<SelectStatement>),
    IsTrue(Box<Expression>),
    IsNotTrue(Box<Expression>),
    IsNull(Box<Expression>),
    IsNotNull(Box<Expression>),
    InList {
        expression: Box<Expression>,
        list: Vec<Expression>,
        not: bool,
    },
    Between {
        not: bool,
        low: Box<Expression>,
        high: Box<Expression>,
    },
    Any {
        left: Box<Expression>,
        operator: Token,
        right: Box<Expression>,
    },
    All {
        left: Box<Expression>,
        operator: Token,
        right: Box<Expression>,
    },
    Some {
        left: Box<Expression>,
        operator: Token,
        right: Box<Expression>,
    },
    Exists(Box<Expression>),
    ExpressionList(Vec<Expression>),
    Function {
        name: Box<FunctionName>,
        args: Option<Vec<Expression>>,
        over: Option<Box<OverClause>>,
    },
    Cast {
        expression: Box<Expression>,
        data_type: DataType,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum FunctionName {
    Builtin(Keyword),
    User(Expression)
}

#[derive(Debug, PartialEq, Clone)]
pub struct OverClause {
    pub partition_by: Vec<Expression>,
    pub order_by: Vec<OrderByArg>,
    pub window_frame: Option<WindowFrame>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum RowsOrRange {
    Rows,
    Range,
}

#[derive(Debug, PartialEq, Clone)]
pub enum WindowFrameBound {
    CurrentRow,
    Preceding(Expression),
    Following(Expression),
    UnboundedPreceding,
    UnboundedFollowing,
}

#[derive(Debug, PartialEq, Clone)]
pub struct WindowFrame {
    pub rows_or_range: RowsOrRange,
    pub start: WindowFrameBound,
    pub end: Option<WindowFrameBound>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum SelectItem {
    Wildcard,
    Unnamed(Expression),
    WithAlias {
        expression: Expression,
        as_kw: Option<Keyword>,
        alias: String,
    },
    WildcardWithAlias {
        expression: Expression,
        as_kw: Option<Keyword>,
        alias: String,
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
    pub where_clause: Option<Expression>,
    pub group_by: Vec<Expression>,
    pub having: Option<Expression>,
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
pub struct Top {
    pub top: Keyword,
    pub with_ties: Option<Vec<Keyword>>,
    pub percent: Option<Keyword>,
    pub quantity: Expression,
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
        is_as: bool,
        alias: Option<String>,
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

fn display_list_comma_separated<T>(list: &[T], f: &mut fmt::Formatter) -> fmt::Result
where
    T: fmt::Display,
{
    display_list_delimiter_separated(list, ", ", f)
}

fn display_list_delimiter_separated<T>(
    list: &[T],
    delimeter: &str,
    f: &mut fmt::Formatter,
) -> fmt::Result
where
    T: fmt::Display,
{
    for (i, item) in list.iter().enumerate() {
        write!(f, "{}", item)?;

        if i < list.len() - 1 {
            write!(f, "{delimeter}")?;
        }
    }
    Ok(())
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

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Expression::Literal(token) => write!(f, "{}", token),
            Expression::CompoundLiteral(tokens) => display_list_delimiter_separated(tokens, ".", f),
            Expression::Binary {
                left,
                operator,
                right,
            } => write!(f, "{} {} {}", left, operator, right),
            Expression::Unary { operator, right } => write!(f, "{} {}", operator, right),
            Expression::Grouping(expr) => write!(f, "({})", expr),
            Expression::Subquery(subquery) => write!(f, "({})", subquery),
            Expression::IsTrue(expr) => write!(f, "IS {}", expr),
            Expression::IsNotTrue(expr) => write!(f, "IS NOT {}", expr),
            Expression::IsNull(expr) => write!(f, "{} IS NULL", expr),
            Expression::IsNotNull(expr) => write!(f, "{} IS NOT NULL", expr),
            Expression::InList {
                expression,
                list,
                not,
            } => {
                write!(f, "{}", expression)?;
                f.write_str(if *not { " NOT IN " } else { " IN " })?;
                f.write_str("( ")?;
                display_list_comma_separated(list, f)?;
                f.write_str(" )")?;
                Ok(())
            }
            Expression::Between { not, low, high } => write!(
                f,
                "{} BETWEEN {} AND {}",
                if *not { "NOT" } else { "" },
                low,
                high
            ),
            Expression::Any {
                left,
                operator,
                right,
            } => write!(f, "{} {} ANY {}", left, operator, right),
            Expression::All {
                left,
                operator,
                right,
            } => write!(f, "{} {} ALL {}", left, operator, right),
            Expression::Some {
                left,
                operator,
                right,
            } => write!(f, "{} {} SOME {}", left, operator, right),
            Expression::Exists(expr) => write!(f, "EXISTS {}", expr),
            Expression::ExpressionList(list) => {
                f.write_str("(")?;
                display_list_comma_separated(list, f)?;
                f.write_str(")")?;

                Ok(())
            }
            Expression::Function { name, args, over } => {
                write!(f, "{}{}", name, args)?;
                if let Some(over_clause) = over {
                    write!(f, " OVER({})", over_clause)?;
                }
                Ok(())
            }
            Expression::Cast {
                expression,
                data_type,
            } => write!(f, "CAST({} as {})", expression, data_type),
            Expression::Identifier(v) => write!(f, "{}", v),
            Expression::QuotedIdentifier(v) => write!(f, "{}", v),
            Expression::StringLiteral(v) => write!(f, "{}", v),
            Expression::NumberLiteral(v) => write!(f, "{}", v),
            Expression::LocalVariable(v) => write!(f, "{}", v),
            Expression::Compound(v) => display_list_delimiter_separated(v, ".", f),
            Expression::Asterisk => write!(f, "*"),
        }
    }
}

impl fmt::Display for WindowFrameBound {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            WindowFrameBound::CurrentRow => write!(f, "CURRENT ROW"),
            WindowFrameBound::Preceding(expr) => write!(f, "{} PRECEDING", expr),
            WindowFrameBound::Following(expr) => write!(f, "{} FOLLOWING", expr),
            WindowFrameBound::UnboundedPreceding => write!(f, "UNBOUNDED PRECEDING"),
            WindowFrameBound::UnboundedFollowing => write!(f, "UNBOUNDED FOLLOWING"),
        }
    }
}

impl fmt::Display for RowsOrRange {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            RowsOrRange::Rows => write!(f, "ROWS"),
            RowsOrRange::Range => write!(f, "RANGE"),
        }
    }
}

impl fmt::Display for WindowFrame {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.rows_or_range)?;
        if let Some(end) = &self.end {
            write!(f, " BETWEEN {}", self.start)?;
            write!(f, " AND {}", end)?;
        } else {
            write!(f, " {}", self.start)?;
        }
        Ok(())
    }
}

impl fmt::Display for OverClause {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if !self.partition_by.is_empty() {
            write!(f, "PARTITION BY ")?;
            display_list_comma_separated(&self.partition_by, f)?;
        }
        if !self.order_by.is_empty() {
            write!(f, "ORDER BY ")?;
            display_list_comma_separated(&self.order_by, f)?;
        }
        if let Some(window_frame) = &self.window_frame {
            write!(f, "{}", window_frame)?;
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
        f.write_str("SELECT ")?;

        // DISTINCT
        if let Some(distinct) = &self.distinct {
            write!(f, "{} ", distinct)?;
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
            write!(f, " FROM {}", table)?;
        }

        // WHERE
        if let Some(where_clause) = &self.where_clause {
            write!(f, " WHERE {}", where_clause)?;
        }

        // GROUPING
        if !self.group_by.is_empty() {
            f.write_str(" GROUP BY ")?;
            display_list_comma_separated(&self.group_by, f)?;
        }

        // HAVING
        if let Some(having_clause) = &self.having {
            write!(f, " HAVING {}", having_clause)?;
        }

        // ORDER BY
        if !self.order_by.is_empty() {
            f.write_str(" ORDER BY ")?;
            display_list_comma_separated(&self.order_by, f)?;
        }

        // OFFSET
        if let Some(offset) = &self.offset {
            write!(f, " {}", offset)?;
        }

        // FETCH
        if let Some(fetch) = &self.fetch {
            write!(f, " {}", fetch)?;
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
            display_list_delimiter_separated(with_ties, "", f)?;
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
            TableSource::Table { name, is_as, alias } => match alias {
                Some(alias) => {
                    if *is_as {
                        write!(f, "{} AS {}", name, alias)
                    } else {
                        write!(f, "{} {}", name, alias)
                    }
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
        write!(f, "{}", self.table)?;

        for join in &self.joins {
            write!(f, " {}", join)?;
        }
        Ok(())
    }
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

impl fmt::Display for OffsetArg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "OFFSET {} {}", self.value, self.row)
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
        write!(f, "FETCH {} {} {} ONLY", self.first, self.value, self.row)
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
