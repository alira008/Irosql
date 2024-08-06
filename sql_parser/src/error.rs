use sql_lexer::TokenKind;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ParseError<'a> {
    pub error: ParseErrorType<'a>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ParseErrorType<'a> {
    UnexpectedToken { token: TokenKind<'a>, expected: Vec<String> },
    UnrecognizedEof,
    ExpectedKeyword,
    ExpectedFunctionName,
    EmptySelectColumns,
    EmptyPartitionByClause,
    EmptyOrderByArgs,
    MissingRowsOrRangeInWindowFrameClause,
    MissingAliasAfterAsKeyword,
    ExpectedUnboundedPrecedingCurrentRowOrNumberPreceding,
    ExpectedUnboundedPrecedingCurrentRowOrNumberFollowing,
    InvalidOrUnimplementedStatement,
}

pub fn parse_error<T>(error: ParseErrorType) -> Result<T, ParseError> {
    Err(ParseError { error })
}
