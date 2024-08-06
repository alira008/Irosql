use core::fmt;
use std::fmt::Write;

use sql_lexer::{Span, Token, TokenKind};

use crate::error::{parse_error, ParseError, ParseErrorType};

use super::{display_list_comma_separated, display_list_delimiter_separated, DataType, Keyword};

#[derive(Debug, PartialEq, Clone)]
pub struct ComparisonOperator {
    pub location: Span,
    pub kind: ComparisonOperatorKind,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ArithmeticOperator {
    pub location: Span,
    pub kind: ArithmeticOperatorKind,
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnaryOperator {
    pub location: Span,
    pub kind: UnaryOperatorKind,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Literal {
    pub location: Span,
    pub content: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct OverClause {
    pub partition_by: Vec<Expression>,
    // pub order_by: Vec<OrderByArg>,
    pub window_frame: Option<WindowFrame>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct WindowFrame {
    pub rows_or_range: RowsOrRange,
    pub start: WindowFrameBound,
    pub end: Option<WindowFrameBound>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Asterisk,
    Identifier(Literal),
    QuotedIdentifier(Literal),
    StringLiteral(Literal),
    NumberLiteral(Literal),
    LocalVariable(Literal),
    Keyword(Keyword),
    Compound(Vec<Expression>),
    Arithmetic {
        operator: ArithmeticOperator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Comparison {
        operator: ComparisonOperator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Unary {
        operator: UnaryOperator,
        right: Box<Expression>,
    },
    ExpressionList(Vec<Expression>),
    Function {
        name: Box<FunctionName>,
        args: Option<Vec<Expression>>,
        over: Option<Box<OverClause>>,
    },
    Cast {
        cast_kw: Keyword,
        expression: Box<Expression>,
        as_kw: Keyword,
        data_type: DataType,
    },
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ComparisonOperatorKind {
    Equal,
    NotEqualBang,
    NotEqualArrow,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ArithmeticOperatorKind {
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulus,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOperatorKind {
    Plus,
    Minus,
}

#[derive(Debug, PartialEq, Clone)]
pub enum FunctionName {
    Builtin(Keyword),
    User(Expression),
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

impl ComparisonOperator {
    pub fn new(location: Span, kind: ComparisonOperatorKind) -> Self {
        Self { location, kind }
    }
}

impl fmt::Display for ComparisonOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            ComparisonOperatorKind::Equal => f.write_str("="),
            ComparisonOperatorKind::NotEqualBang => f.write_str("!="),
            ComparisonOperatorKind::NotEqualArrow => f.write_str("<>"),
            ComparisonOperatorKind::GreaterThan => f.write_str(">"),
            ComparisonOperatorKind::GreaterThanEqual => f.write_str(">="),
            ComparisonOperatorKind::LessThan => f.write_str("<="),
            ComparisonOperatorKind::LessThanEqual => f.write_str("<="),
        }
    }
}


impl<'a> TryFrom<Token<'a>> for ComparisonOperator {
    type Error = ParseError<'a>;

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        let kind = match value.kind() {
            TokenKind::Equal => ComparisonOperatorKind::Equal,
            TokenKind::BangEqual => ComparisonOperatorKind::NotEqualBang,
            TokenKind::LessThanGreaterThan => ComparisonOperatorKind::NotEqualArrow,
            TokenKind::GreaterThan => ComparisonOperatorKind::GreaterThan,
            TokenKind::GreaterThanEqual => ComparisonOperatorKind::GreaterThanEqual,
            TokenKind::LessThan => ComparisonOperatorKind::LessThan,
            TokenKind::LessThanEqual => ComparisonOperatorKind::LessThanEqual,
            _ => return parse_error(ParseErrorType::ExpectedKeyword),
        };
        Ok(Self::new(value.location(), kind))
    }
}

impl ArithmeticOperator {
    pub fn new(location: Span, kind: ArithmeticOperatorKind) -> Self {
        Self { location, kind }
    }
}

impl fmt::Display for ArithmeticOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            ArithmeticOperatorKind::Plus => f.write_str("+"),
            ArithmeticOperatorKind::Minus => f.write_str("-"),
            ArithmeticOperatorKind::Multiply => f.write_str("*"),
            ArithmeticOperatorKind::Divide => f.write_str("/"),
            ArithmeticOperatorKind::Modulus => f.write_str("%"),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for ArithmeticOperator {
    type Error = ParseError<'a>;

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        let kind = match value.kind() {
            TokenKind::Plus => ArithmeticOperatorKind::Plus,
            TokenKind::Minus => ArithmeticOperatorKind::Minus,
            TokenKind::Asterisk => ArithmeticOperatorKind::Multiply,
            TokenKind::ForwardSlash => ArithmeticOperatorKind::Divide,
            TokenKind::PercentSign => ArithmeticOperatorKind::Modulus,
            _ => return parse_error(ParseErrorType::ExpectedKeyword),
        };
        Ok(Self::new(value.location(), kind))
    }
}

impl UnaryOperator {
    pub fn new(location: Span, kind: UnaryOperatorKind) -> Self {
        Self { location, kind }
    }
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            UnaryOperatorKind::Plus => f.write_str("+"),
            UnaryOperatorKind::Minus => f.write_str("-"),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for UnaryOperator {
    type Error = ParseError<'a>;

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        let kind = match value.kind() {
            TokenKind::Plus => UnaryOperatorKind::Plus,
            TokenKind::Minus => UnaryOperatorKind::Minus,
            _ => return parse_error(ParseErrorType::ExpectedKeyword),
        };
        Ok(Self::new(value.location(), kind))
    }
}

impl Literal {
    pub fn new(location: Span, content: String) -> Self {
        Self { location, content }
    }
}

impl<'a> TryFrom<Token<'a>> for Literal {
    type Error = ParseError<'a>;

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        let content = match value.kind() {
            TokenKind::Identifier(str)
            | TokenKind::QuotedIdentifier(str)
            | TokenKind::NumberLiteral(str)
            | TokenKind::StringLiteral(str)
            | TokenKind::LocalVariable(str) => str.to_string(),
            _ => return parse_error(ParseErrorType::ExpectedKeyword),
        };
        Ok(Self::new(value.location(), content))
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.content)
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Expression::Arithmetic {
                operator,
                left,
                right,
            } => write!(f, "{} {} {}", left, operator, right),
            Expression::Comparison {
                operator,
                left,
                right,
            } => write!(f, "{} {} {}", left, operator, right),
            Expression::Unary { operator, right } => write!(f, "{} {}", operator, right),
            Expression::ExpressionList(list) => {
                f.write_str("(")?;
                display_list_comma_separated(list, f)?;
                f.write_str(")")?;

                Ok(())
            }
            Expression::Function { name, args, over } => {
                write!(f, "{}", name)?;
                if let Some(over_clause) = over {
                    write!(f, " OVER({})", over_clause)?;
                }
                Ok(())
            }
            Expression::Cast {
                cast_kw,
                expression,
                as_kw,
                data_type,
            } => write!(f, "{}({} {} {})", cast_kw, expression, as_kw, data_type),
            Expression::Identifier(v) => write!(f, "{}", v),
            Expression::QuotedIdentifier(v) => write!(f, "[{}]", v),
            Expression::StringLiteral(v) => write!(f, "'{}'", v),
            Expression::NumberLiteral(v) => write!(f, "{}", v),
            Expression::LocalVariable(v) => write!(f, "@{}", v),
            Expression::Keyword(v) => write!(f, "{}", v),
            Expression::Compound(v) => display_list_delimiter_separated(v, ".", f),
            Expression::Asterisk => write!(f, "*"),
        }
    }
}

impl fmt::Display for FunctionName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            FunctionName::Builtin(e) => write!(f, "{}", e),
            FunctionName::User(e) => write!(f, "{}", e),
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