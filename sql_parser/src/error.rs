use crate::token_new::Token;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct LexicalError {
    pub error: LexicalErrorType,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum LexicalErrorType {
    UnrecognizedToken,
    UnexpectedStringEnd,
    UnexpectedQuotedIdentifierEnd,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ParseError<'a> {
    pub error: ParseErrorType<'a>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ParseErrorType<'a> {
    UnexpectedToken { token: Token<'a>, expected: Vec<String> },
    UnrecognizedEof,
    ExpectedKeyword,
    InvalidOrUnimplementedStatement,
}

pub fn parse_error<T>(error: ParseErrorType) -> Result<T, ParseError> {
    Err(ParseError { error })
}
