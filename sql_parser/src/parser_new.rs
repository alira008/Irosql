use crate::ast::{self, Span};
use crate::error::{parse_error, LexicalError, ParseError, ParseErrorType};
use crate::keywords::Keyword;
use crate::lexer_new::{Lexer, SpannedToken};
use crate::operator::{get_precedence, Precedence};
use crate::token_new::Token;

#[derive(Debug, Clone)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Option<SpannedToken<'a>>,
    peek_token: Option<SpannedToken<'a>>,
    extra_peek_token: Option<SpannedToken<'a>>,

    lexer_errors: Vec<LexicalError>,
    parse_errors: Vec<ParseError<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut parser = Parser {
            lexer,
            current_token: None,
            peek_token: None,
            extra_peek_token: None,
            lexer_errors: vec![],
            parse_errors: vec![],
        };
        parser.advance();
        parser.advance();
        parser
    }

    pub fn parse(&mut self) -> ast::Query {
        let mut query = ast::Query::new();

        while self.current_token.is_some() {
            match self.parse_statement() {
                Ok(statement) => query.statements.push(statement),
                Err(parse_error) => self.parse_errors.push(parse_error),
            }
            self.advance();
        }

        query
    }

    fn advance(&mut self) {
        self.current_token = self.peek_token;
        self.peek_token = self.extra_peek_token;
        let next_spanned;

        loop {
            match self.lexer.next() {
                Some(r) => match r {
                    Ok(spanned_token) => match spanned_token.1 {
                        Token::Comment(_) => todo!(),
                        _ => {
                            next_spanned = Some(spanned_token);
                            break;
                        }
                    },
                    Err(e) => self.lexer_errors.push(e),
                },
                None => {
                    next_spanned = None;
                    break;
                }
            }
        }

        self.extra_peek_token = next_spanned;
    }

    fn peek_precedence(&self) -> Precedence {
        match self.peek_token {
            Some((_, token, _)) => get_precedence(token),
            None => Precedence::Lowest,
        }
    }

    fn current_precedence(&self) -> Precedence {
        match self.current_token {
            Some((_, token, _)) => get_precedence(token),
            None => Precedence::Lowest,
        }
    }

    fn maybe_current_token(&mut self, token: &Token) -> Option<SpannedToken> {
        match self.current_token.take() {
            Some((s, t, e)) if t == *token => {
                self.advance();
                Some((s, t, e))
            }
            t => {
                self.current_token = t;
                None
            }
        }
    }

    fn maybe_peek_token(&mut self, token: &Token) -> Option<SpannedToken> {
        match self.current_token.take() {
            Some((s, t, e)) if t == *token => {
                self.advance();
                Some((s, t, e))
            }
            t => {
                self.current_token = t;
                None
            }
        }
    }

    fn expect_peek_token(&mut self, expected_token: &Token) -> Result<(Span, Span), ParseError> {
        match self.maybe_peek_token(expected_token) {
            Some((s, _, e)) => Ok((s, e)),
            None => self.unexpected_peek_token(vec![expected_token.to_string()]),
        }
    }

    fn unexpected_peek_token<A>(&mut self, expected: Vec<String>) -> Result<A, ParseError> {
        match self.peek_token {
            Some((_, t, _)) => parse_error(ParseErrorType::UnexpectedToken { token: t, expected }),
            None => parse_error(ParseErrorType::UnrecognizedEof),
        }
    }
}

impl<'a> Parser<'a> {
    fn parse_statement(&mut self) -> Result<ast::Statement, ParseError<'a>> {
        match self.current_token {
            Some((_, t, _)) => match t {
                Token::Keyword(keyword) => match keyword {
                    Keyword::SELECT => {
                        return Ok(ast::Statement::Select(self.parse_select_statement()?))
                    }
                    // Keyword::INSERT => {
                    //     return Ok(ast::Statement::Insert(self.parse_insert_statement()?))
                    // }
                    // Keyword::UPDATE => {
                    //     return Ok(ast::Statement::Update(self.parse_update_statement()?))
                    // }
                    // Keyword::DELETE => {
                    //     return Ok(ast::Statement::Delete(self.parse_delete_statement()?))
                    // }
                    // Keyword::WITH => return self.parse_cte_statement(),
                    // Keyword::DECLARE => return self.parse_declare_statement(),
                    // Keyword::SET => return self.parse_set_local_variable_statement(),
                    // Keyword::EXEC | Keyword::EXECUTE => return self.parse_execute_statement(),
                    _ => return parse_error(ParseErrorType::InvalidOrUnimplementedStatement),
                },
                _ => return parse_error(ParseErrorType::ExpectedKeyword),
            },
            None => todo!(),
        }
    }

    fn parse_select_statement(&mut self) -> Result<ast::SelectStatement, ParseError<'a>> {
        let select_statement = ast::SelectStatement::default();

        return Ok(select_statement);
    }
}
