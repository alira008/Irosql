mod data_type;
mod expressions;
mod keyword;
mod utils;

use crate::error::{parse_error, ParseError, ParseErrorType};
use core::fmt;
pub use data_type::{DataType, DataTypeSize, NumericSize};
pub use expressions::*;
pub use keyword::{Keyword, KeywordKind};
use lexer::{Span, Token, TokenKind};
pub use utils::*;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum SymbolKind {
    LeftParen,
    RightParen,
    Asterisk,
    Semicolon,
    EqualSign,
}

impl fmt::Display for SymbolKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SymbolKind::LeftParen => f.write_str("("),
            SymbolKind::RightParen => f.write_str(")"),
            SymbolKind::Asterisk => f.write_str("*"),
            SymbolKind::Semicolon => f.write_str(";"),
            SymbolKind::EqualSign => f.write_str("="),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Symbol {
    pub kind: SymbolKind,
    pub location: Span,
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl<'a> From<Token<'a>> for Symbol {
    fn from(value: Token<'a>) -> Self {
        let kind = match value.kind_as_ref() {
            &TokenKind::Asterisk => SymbolKind::Asterisk,
            &TokenKind::LeftParen => SymbolKind::LeftParen,
            &TokenKind::RightParen => SymbolKind::RightParen,
            &TokenKind::SemiColon => SymbolKind::Semicolon,
            &TokenKind::Equal => SymbolKind::EqualSign,
            _ => unreachable!(),
        };
        Symbol {
            kind,
            location: value.location(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Comment {
    pub content: String,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CommonTableExpression {
    pub name: Expression,
    pub columns: Option<ExpressionList>,
    pub as_kw: Keyword,
    pub left_paren: Symbol,
    pub query: SelectStatement,
    pub right_paren: Symbol,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExpressionList {
    pub left_paren: Symbol,
    pub items: Vec<Expression>,
    pub right_paren: Symbol,
}

#[derive(Debug, PartialEq, Clone)]
pub enum CommonTableExpressionStatement {
    Select(SelectStatement),
    Insert(InsertStatement),
    // Update(UpdateStatement),
    // Delete(DeleteStatement),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Select(SelectStatement),
    Insert(InsertStatement),
    Update(UpdateStatement),
    Delete(DeleteStatement),
    CTE {
        with_kw: Keyword,
        ctes: Vec<CommonTableExpression>,
        statement: CommonTableExpressionStatement,
    },
    Declare {
        declare_kw: Keyword,
        variables: Vec<LocalVariable>,
    },
    SetLocalVariable {
        set_kw: Keyword,
        name: Expression,
        equal_sign: Symbol,
        value: Expression,
    },
    Execute {
        exec_kw: Keyword,
        procedure_name: Expression,
        parameters: Vec<ProcedureParameter>,
    },
    Union {
        select: SelectStatement,
        unions: Vec<Union>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct Union {
    pub union_kw: Keyword,
    pub all_kw: Option<Keyword>,
    pub select: SelectStatement,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ProcedureParameter {
    pub name: Option<(ProcedureParameterName, Symbol)>,
    pub value: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ProcedureParameterName {
    pub location: Span,
    pub content: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LocalVariable {
    pub name: Expression,
    pub data_type: DataType,
    pub value: Option<(Symbol, Expression)>,
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
    Wildcard(Symbol),
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
    ReverseAliasAssign {
        alias: Expression,
        expression: Expression,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum InsertStatement {
    Values {
        insert_kw: Keyword,
        into_kw: Option<Keyword>,
        object: Expression,
        columns: Option<ExpressionList>,
        values_kw: Keyword,
        values: ExpressionList,
    },
    Table {
        insert_kw: Keyword,
        into_kw: Option<Keyword>,
        object: Expression,
        select_kw: Keyword,
        top: Option<Top>,
        columns: Vec<Expression>,
        table: TableArg,
        where_clause: Option<WhereClause>,
    },
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
        alias: Expression,
    },
    TableValuedFunction {
        function: Expression,
        alias: Option<Expression>,
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
}

#[derive(Debug, PartialEq, Clone)]
pub struct Join {
    pub join: Vec<Keyword>,
    pub join_type: JoinType,
    pub table: TableSource,
    pub condition: Option<JoinCondition>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct JoinCondition {
    pub on_kw: Keyword,
    pub condition: Expression,
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

impl fmt::Display for ExpressionList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.left_paren)?;
        display_list_comma_separated(&self.items, f)?;
        write!(f, "{}", self.right_paren)
    }
}

impl fmt::Display for CommonTableExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)?;
        if let Some(columns) = &self.columns {
            write!(f, " {}", columns)?;
        }
        write!(
            f,
            " {} {}{}{}",
            self.as_kw, self.left_paren, self.query, self.right_paren
        )
    }
}

impl fmt::Display for CommonTableExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            CommonTableExpressionStatement::Select(select) => write!(f, "{}", select),
            CommonTableExpressionStatement::Insert(insert) => write!(f, "{}", insert),
            // CommonTableExpressionStatement::Update(update) => write!(f, "{}", update),
            // CommonTableExpressionStatement::Delete(delete) => write!(f, "{}", delete),
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Statement::Select(select) => write!(f, "{}", select),
            Statement::CTE {
                with_kw,
                ctes,
                statement,
            } => {
                write!(f, "{} ", with_kw)?;
                display_list_comma_separated(ctes, f)?;
                write!(f, " {}", statement)
            }
            Statement::Declare {
                declare_kw,
                variables,
            } => {
                write!(f, "{} ", declare_kw)?;
                display_list_comma_separated(variables, f)
            }
            Statement::SetLocalVariable {
                set_kw,
                name,
                equal_sign,
                value,
            } => write!(f, "{} {} {} {}", set_kw, name, equal_sign, value),
            Statement::Execute {
                exec_kw,
                procedure_name,
                parameters,
            } => {
                write!(f, "{} {} ", exec_kw, procedure_name)?;
                display_list_comma_separated(parameters, f)
            }
            Statement::Insert(insert) => write!(f, "{}", insert),
            Statement::Update(update) => write!(f, "{}", update),
            Statement::Delete(delete) => write!(f, "{}", delete),
            Statement::Union { select, unions } => {
                write!(f, "{}", select)?;
                for union in unions.iter() {
                    write!(f, "\n{}", union)?;
                }

                Ok(())
            }
        }
    }
}

impl fmt::Display for Union {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.union_kw)?;
        if let Some(kw) = &self.all_kw {
            write!(f, " {}", kw)?;
        }
        write!(f, "{}", self.select)
    }
}

impl fmt::Display for LocalVariable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.name, self.data_type)?;
        if let Some(value) = &self.value {
            write!(f, " {} {}", value.0, value.1)?;
        }

        Ok(())
    }
}

impl fmt::Display for ProcedureParameter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(name) = &self.name {
            write!(f, "{} {} ", name.0, name.1)?;
        }
        write!(f, "{}", self.value)?;

        Ok(())
    }
}

impl fmt::Display for ProcedureParameterName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "@{}", self.content)
    }
}

impl<'a> TryFrom<Token<'a>> for ProcedureParameterName {
    type Error = ParseError<'a>;

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        let content = match value.kind() {
            TokenKind::LocalVariable(i) => i.to_string(),
            _ => return parse_error(ParseErrorType::ExpectedLocalVariable, value.location()),
        };
        Ok(ProcedureParameterName {
            location: value.location(),
            content,
        })
    }
}

impl<'a> TryFrom<Option<Token<'a>>> for ProcedureParameterName {
    type Error = ParseError<'a>;

    fn try_from(value: Option<Token<'a>>) -> Result<Self, Self::Error> {
        if let Some(token) = value {
            ProcedureParameterName::try_from(token)
        } else {
            unreachable!()
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
            SelectItem::Wildcard(s) => write!(f, "{}", s),
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
            SelectItem::ReverseAliasAssign { alias, expression } => {
                write!(f, "{} = {}", alias, expression)?;
                Ok(())
            }
        }
    }
}

impl fmt::Display for InsertStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            InsertStatement::Values {
                insert_kw,
                into_kw,
                object,
                columns,
                values_kw,
                values,
            } => {
                write!(f, "{}", insert_kw)?;
                if let Some(into_kw) = into_kw {
                    write!(f, " {}", into_kw)?;
                }
                write!(f, " {}", object)?;

                if let Some(columns) = columns {
                    write!(f, " {}", columns)?;
                }

                write!(f, " {}", values_kw)?;

                write!(f, " {}", values)?;
                Ok(())
            }
            InsertStatement::Table {
                insert_kw,
                into_kw,
                object,
                select_kw,
                top,
                columns,
                table,
                where_clause,
            } => {
                write!(f, "{}", insert_kw)?;
                if let Some(into_kw) = into_kw {
                    write!(f, " {}", into_kw)?;
                }
                write!(f, " {} {}", object, select_kw)?;
                if let Some(top) = top {
                    write!(f, " {}", top)?;
                }
                f.write_str(" ")?;
                display_list_comma_separated(&columns, f)?;
                write!(f, " {}", table)?;
                if let Some(where_clause) = where_clause {
                    write!(f, " {}", where_clause)?;
                }

                Ok(())
            }
        }
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
            TableSource::Derived { query, alias } => {
                write!(f, "{} {}", query, alias)
            }
            TableSource::TableValuedFunction { function, alias } => {
                write!(f, "{}", function)?;
                if let Some(alias) = alias {
                    write!(f, " {}", alias)?;
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
        }
    }
}

impl fmt::Display for Join {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        display_list_delimiter_separated(&self.join, " ", f)?;
        write!(f, " {}", self.table)?;
        if let Some(condition) = &self.condition {
            write!(f, " {}", condition)?;
        }
        Ok(())
    }
}

impl fmt::Display for JoinCondition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.on_kw, self.condition)?;
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
