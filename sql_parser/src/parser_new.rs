use crate::ast::{self, KeywordDef, Span};
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
        parser.advance();
        parser
    }

    fn advance(&mut self) {
        let _ = self.next_token();
    }

    fn next_token(&mut self) -> Option<SpannedToken> {
        let token = self.current_token.take();
        let mut next_spanned;

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

        self.current_token = self.peek_token.take();
        self.peek_token = self.extra_peek_token.take();
        self.extra_peek_token = next_spanned.take();
        token
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
            Some((s, t, e)) if t.shallow_eq_token(token) => {
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
        match self.peek_token.take() {
            Some((s, t, e)) if t.shallow_eq_token(token) => {
                self.advance();
                self.advance();
                Some((s, t, e))
            }
            t => {
                self.peek_token = t;
                None
            }
        }
    }

    fn maybe_current_token_many(&mut self, tokens: &[Token]) -> Option<SpannedToken> {
        for token in tokens {
            match self.current_token.take() {
                Some((s, t, e)) if t.shallow_eq_token(token) => {
                    self.advance();
                    return Some((s, t, e));
                }
                t => {
                    self.current_token = t;
                }
            }
        }

        None
    }

    fn maybe_peek_token_many(&mut self, tokens: &[Token]) -> Option<SpannedToken> {
        for token in tokens {
            match self.peek_token.take() {
                Some((s, t, e)) if t.shallow_eq_token(token) => {
                    self.advance();
                    self.advance();
                    return Some((s, t, e));
                }
                t => {
                    self.peek_token = t;
                }
            }
        }
        None
    }

    fn maybe_current_keyword(&mut self, keyword: Keyword) -> Option<(Span, Keyword, Span)> {
        match self.current_token.take() {
            Some((s, Token::Keyword(k), e)) if k == keyword => {
                self.advance();
                Some((s, keyword, e))
            }
            t => {
                self.current_token = t;
                None
            }
        }
    }

    fn maybe_peek_keyword(&mut self, keyword: Keyword) -> Option<(Span, Keyword, Span)> {
        match self.peek_token.take() {
            Some((s, Token::Keyword(k), e)) if k == keyword => {
                self.advance();
                self.advance();
                Some((s, keyword, e))
            }
            t => {
                self.peek_token = t;
                None
            }
        }
    }

    fn expect_peek_token(
        &mut self,
        expected_token: &Token,
    ) -> Result<(Span, Span), ParseError<'a>> {
        match self.maybe_peek_token(expected_token) {
            Some((s, _, e)) => Ok((s, e)),
            None => self.unexpected_peek_token(vec![expected_token.to_string()]),
        }
    }

    fn expect_current_token(
        &mut self,
        expected_token: &Token,
    ) -> Result<(Span, Span), ParseError<'a>> {
        match self.maybe_current_token(expected_token) {
            Some((s, _, e)) => Ok((s, e)),
            None => self.unexpected_current_token(vec![expected_token.to_string()]),
        }
    }

    fn expect_peek_token_many(
        &mut self,
        expected_tokens: &[Token],
    ) -> Result<(Span, Span), ParseError<'a>> {
        match self.maybe_peek_token_many(expected_tokens) {
            Some((s, _, e)) => Ok((s, e)),
            None => {
                self.unexpected_peek_token(expected_tokens.iter().map(|t| t.to_string()).collect())
            }
        }
    }

    fn expect_current_token_many(
        &mut self,
        expected_tokens: &[Token],
    ) -> Result<(Span, Span), ParseError<'a>> {
        match self.maybe_current_token_many(expected_tokens) {
            Some((s, _, e)) => Ok((s, e)),
            None => self
                .unexpected_current_token(expected_tokens.iter().map(|t| t.to_string()).collect()),
        }
    }

    fn expect_peek_keyword(
        &mut self,
        expected_keyword: Keyword,
    ) -> Result<(Span, Keyword, Span), ParseError<'a>> {
        match self.maybe_peek_keyword(expected_keyword) {
            Some((s, k, e)) => Ok((s, k, e)),
            None => self.unexpected_peek_token(vec![expected_keyword.to_string()]),
        }
    }

    fn expect_current_keyword(
        &mut self,
        expected_keyword: Keyword,
    ) -> Result<(Span, Keyword, Span), ParseError<'a>> {
        match self.maybe_current_keyword(expected_keyword) {
            Some((s, k, e)) => Ok((s, k, e)),
            None => self.unexpected_current_token(vec![expected_keyword.to_string()]),
        }
    }

    fn unexpected_peek_token<A>(&mut self, expected: Vec<String>) -> Result<A, ParseError<'a>> {
        match self.peek_token {
            Some((_, t, _)) => parse_error(ParseErrorType::UnexpectedToken { token: t, expected }),
            None => parse_error(ParseErrorType::UnrecognizedEof),
        }
    }

    fn unexpected_current_token<A>(&mut self, expected: Vec<String>) -> Result<A, ParseError<'a>> {
        match self.current_token {
            Some((_, t, _)) => parse_error(ParseErrorType::UnexpectedToken { token: t, expected }),
            None => parse_error(ParseErrorType::UnrecognizedEof),
        }
    }

}

impl<'a> Parser<'a> {
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
        let mut select_statement = ast::SelectStatement::default();

        let select_kw = self.expect_current_keyword(Keyword::SELECT)?;
        select_statement.select = KeywordDef::new_single(select_kw);

        if let Some(kw) = self.maybe_current_keyword(Keyword::DISTINCT) {
            select_statement.distinct = Some(KeywordDef::new_single(kw));
        }

        if let Some(kw) = self.maybe_current_keyword(Keyword::ALL) {
            select_statement.all = Some(KeywordDef::new_single(kw));
        }

        return Ok(select_statement);
    }

    fn parse_select_items(&mut self) -> Result<Vec<ast::SelectItem>, String> {
        // check if the next token is an identifier
        // return an error if the next token is not an identifier or number
        // self.expect_many_kind(
        //     &[
        //         Kind::Ident,
        //         Kind::Number,
        //         Kind::Asterisk,
        //         Kind::LeftParen,
        //         Kind::LocalVariable,
        //     ],
        //     &self.peek_token,
        // )?;
        //
        // // get the columns to select
        let columns: Vec<ast::SelectItem> = vec![];
        // while let Some(token_w_span) = self.maybe_current_token_many(&[Token::Identifier("")]){
        //
        // }
        // while !self.peek_token_is(Kind::Keyword(Keyword::FROM))
        //     && !self.peek_token_is(Kind::Keyword(Keyword::INTO))
        //     && !self.peek_token_is(Kind::Keyword(Keyword::WITH))
        //     && !self.peek_token_is(Kind::Keyword(Keyword::EXEC))
        //     && !self.peek_token_is(Kind::Keyword(Keyword::EXECUTE))
        //     && !self.peek_token_is(Kind::Keyword(Keyword::DECLARE))
        //     && !self.peek_token_is(Kind::Keyword(Keyword::SET))
        //     && !self.peek_token_is(Kind::Eof)
        // {
        //     self.next_token();
        //
        //     if columns.len() > 0 {
        //         // expect a COMMA before the next GROUP BY expression
        //         self.expect_kind(Kind::Comma, &self.current_token)?;
        //
        //         // consume the COMMA
        //         self.next_token();
        //     }
        //
        //     // parse the expression
        //     let expression = self.parse_expression(PRECEDENCE_LOWEST)?;
        //
        //     // check if it is a compounded identifier
        //     // check if the next token is the keyword AS
        //     let mut is_as = false;
        //     if self.peek_token_is(Kind::Keyword(Keyword::AS)) {
        //         // skip the AS keyword
        //         self.next_token();
        //         is_as = true;
        //
        //         if !self.peek_token_is(Kind::Ident) {
        //             self.expected(
        //                 "token to either be a quoted string or identifier after AS keyword",
        //                 &self.peek_token,
        //             )?;
        //         }
        //     }
        //
        //     if self.peek_token_is(Kind::Ident) {
        //         self.next_token();
        //
        //         // check if expression was a wildcard
        //         // add to the columns
        //         if matches!(expression, ast::Expression::Literal(ref token) if token.kind() == Kind::Asterisk)
        //         {
        //             columns.push(ast::SelectItem::WildcardWithAlias {
        //                 expression,
        //                 as_token: is_as,
        //                 alias: self.current_token.to_string(),
        //             });
        //         } else {
        //             columns.push(ast::SelectItem::WithAlias {
        //                 expression,
        //                 as_token: is_as,
        //                 alias: self.current_token.to_string(),
        //             });
        //         }
        //     } else {
        //         // add to the columns
        //         // check if expression was a wildcard
        //         if matches!(expression, ast::Expression::Literal(ref token) if token.kind() == Kind::Asterisk)
        //         {
        //             columns.push(ast::SelectItem::Wildcard);
        //         } else {
        //             columns.push(ast::SelectItem::Unnamed(expression));
        //         }
        //     }
        // }
        //
        // if columns.len() == 0 {
        //     self.expected("SELECT items in SELECT expression", &self.peek_token)?;
        // }

        Ok(columns)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn basic_select_statement_new() {
        let input = "SELECT distInct all *, name, firstname, lastname, [first], dob FROM users;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let query = parser.parse();

        println!("parsing");
        let mut select_statement = ast::SelectStatement::default();
        select_statement.select =
            KeywordDef::new_single((Span::new(1, 1), Keyword::SELECT, Span::new(1, 6)));
        select_statement.distinct = Some(KeywordDef::new_single((
            Span::new(1, 8),
            Keyword::DISTINCT,
            Span::new(1, 15),
        )));
        select_statement.all = Some(KeywordDef::new_single((
            Span::new(1, 17),
            Keyword::ALL,
            Span::new(1, 19),
        )));
        let expected_query = ast::Query {
            statements: vec![ast::Statement::Select(select_statement)],
        };

        assert_eq!(expected_query, query);
    }
}
