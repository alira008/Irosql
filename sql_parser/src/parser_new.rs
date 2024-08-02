use crate::ast::{self, KeywordDef, Span};
use crate::error::{parse_error, LexicalError, ParseError, ParseErrorType};
use crate::keywords::Keyword;
use crate::lexer_new::{Lexer, SpannedKeyword, SpannedToken};
use crate::operator::{get_precedence, Precedence};
use crate::token_new::Token;

const SELECT_ITEM_TYPE: &'static [Token<'static>] = &[
    Token::Identifier(""),
    Token::QuotedIdentifier(""),
    Token::NumberLiteral(""),
    Token::LocalVariable(""),
    Token::LeftParen,
    Token::Asterisk,
    Token::Minus,
    Token::Plus,
];

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
            Some((_, token)) => get_precedence(token),
            None => Precedence::Lowest,
        }
    }

    fn current_precedence(&self) -> Precedence {
        match self.current_token {
            Some((_, token)) => get_precedence(token),
            None => Precedence::Lowest,
        }
    }

    fn maybe_token(&mut self, token: &Token, current: bool) -> Option<SpannedToken> {
        let tok = if current {
            self.current_token.take()
        } else {
            self.peek_token.take()
        };
        match tok {
            Some((s, t)) if t.shallow_eq_token(token) => {
                if current {
                    self.advance();
                } else {
                    self.advance();
                    self.advance();
                }
                Some((s, t))
            }
            t => {
                if current {
                    self.current_token = t;
                } else {
                    self.peek_token = t;
                };
                None
            }
        }
    }

    fn maybe_token_many(&mut self, tokens: &[Token], current: bool) -> Option<SpannedToken> {
        for token in tokens {
            if let Some(spanned_token) = self.maybe_token(token, current) {
                Some(spanned_token);
            }
        }

        None
    }

    fn token_is_keyword(&mut self, keyword: &Keyword, current: bool) -> bool {
        let compare_token = if current {
            self.current_token
        } else {
            self.peek_token
        };
        if compare_token.is_some_and(|(_, t)| matches!(t, Token::Keyword(kw) if kw == *keyword)) {
            return true;
        }
        false
    }

    fn token_is_any_keyword(&mut self, keywords: &[Keyword], current: bool) -> bool {
        let compare_token = if current {
            self.current_token
        } else {
            self.peek_token
        };
        // let ret_spanned_token;
        for keyword in keywords {
            if compare_token.is_some_and(|(_, t)| matches!(t, Token::Keyword(kw) if kw == *keyword))
            {
                return true;
            }
        }
        false
    }

    fn token_is(&mut self, token: &Token, current: bool) -> bool {
        let compare_token = if current {
            self.current_token
        } else {
            self.peek_token
        };
        if compare_token.is_some_and(|(_, t)| t.shallow_eq_token(token)) {
            return true;
        }
        false
    }

    fn token_is_any(&mut self, tokens: &[Token], current: bool) -> bool {
        let compare_token = if current {
            self.current_token
        } else {
            self.peek_token
        };
        // let ret_spanned_token;
        for token in tokens {
            if compare_token.is_some_and(|(_, t)| t.shallow_eq_token(token)) {
                return true;
            }
        }
        false
    }

    fn maybe_keyword(&mut self, keyword: Keyword, current: bool) -> Option<SpannedKeyword> {
        let tok = if current {
            self.current_token.take()
        } else {
            self.peek_token.take()
        };
        match tok {
            Some((s, Token::Keyword(k))) if k == keyword => {
                if current {
                    self.advance();
                } else {
                    self.advance();
                    self.advance();
                }
                Some((s, keyword))
            }
            t => {
                if current {
                    self.current_token = t;
                } else {
                    self.peek_token = t;
                };
                None
            }
        }
    }

    fn expect_token(
        &mut self,
        expected_token: &Token,
        current: bool,
    ) -> Result<Span, ParseError<'a>> {
        match self.maybe_token(expected_token, current) {
            Some((s, _)) => Ok(s),
            None => self.unexpected_token(vec![expected_token.to_string()], current),
        }
    }

    fn expect_token_many(
        &mut self,
        expected_tokens: &[Token],
        current: bool,
    ) -> Result<Span, ParseError<'a>> {
        match self.maybe_token_many(expected_tokens, current) {
            Some((s, _)) => Ok(s),
            None => self.unexpected_token(
                expected_tokens.iter().map(|t| t.to_string()).collect(),
                current,
            ),
        }
    }

    fn expect_keyword(
        &mut self,
        expected_keyword: Keyword,
        current: bool,
    ) -> Result<SpannedKeyword, ParseError<'a>> {
        match self.maybe_keyword(expected_keyword, current) {
            Some((s, k)) => Ok((s, k)),
            None => self.unexpected_token(vec![expected_keyword.to_string()], current),
        }
    }

    fn unexpected_token<A>(
        &self,
        expected: Vec<String>,
        current: bool,
    ) -> Result<A, ParseError<'a>> {
        let tok = if current {
            self.current_token
        } else {
            self.peek_token
        };
        match tok {
            Some((_, t)) => parse_error(ParseErrorType::UnexpectedToken { token: t, expected }),
            None => parse_error(ParseErrorType::UnrecognizedEof),
        }
    }

    fn expect_identifier(&mut self, current: bool) -> Result<Span, ParseError<'a>> {
        let spanned_tok = if current {
            self.current_token
        } else {
            self.peek_token
        };
        if let Some((span, tok)) = spanned_tok {
            if matches!(tok, Token::Identifier(..)) {
                if current {
                    self.advance();
                } else {
                    self.advance();
                    self.advance();
                };
                return Ok(span);
            }
        }
        self.unexpected_token(vec!["Identifier".to_string()], current)
    }

    fn maybe_identifier(&mut self) -> Option<(Span, &'a str)> {
        match self.current_token.take() {
            Some((s, Token::Identifier(val))) => {
                self.advance();
                Some((s, val))
            }
            t => {
                self.current_token = t;
                None
            }
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
            Some((_, t)) => match t {
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

        let select_kw = self.expect_keyword(Keyword::SELECT, true)?;
        select_statement.select = KeywordDef::new_single(select_kw);

        if let Some(kw) = self.maybe_keyword(Keyword::DISTINCT, true) {
            select_statement.distinct = Some(KeywordDef::new_single(kw));
        }

        if let Some(kw) = self.maybe_keyword(Keyword::ALL, true) {
            select_statement.all = Some(KeywordDef::new_single(kw));
        }

        select_statement.columns = self.parse_select_items()?;

        if let Some(kw) = self.maybe_keyword(Keyword::FROM, true) {
            let _ = self.parse_table_arg(KeywordDef::new_single(kw))?;
        }

        return Ok(select_statement);
    }

    fn parse_select_items(&mut self) -> Result<Vec<ast::SelectItem>, ParseError<'a>> {
        // check if the next token is an identifier
        // return an error if the next token is not an identifier or number
        // get the columns to select
        let mut columns: Vec<ast::SelectItem> = vec![];
        while self.token_is_any(SELECT_ITEM_TYPE, true) {
            println!("current: {:#?}", self.current_token);
            if !columns.is_empty() {
                let _ = self.expect_token(&Token::Comma, true);
            }

            let expression = self.parse_expression(Precedence::Lowest)?;
            match expression {
                ast::Expression::Identifier(_)
                | ast::Expression::QuotedIdentifier(_)
                | ast::Expression::StringLiteral(_)
                | ast::Expression::NumberLiteral(_)
                | ast::Expression::LocalVariable(_)
                | ast::Expression::Compound(_)
                | ast::Expression::Asterisk => (),
                _ => return self.unexpected_token(vec!["select items".to_string()], true),
            }
            println!("peek: {:#?}", self.peek_token);

            if let Some(spanned_kw) = self.maybe_keyword(Keyword::AS, true) {
                let as_keyword = ast::KeywordDef::new_single(spanned_kw);
                if matches!(expression, ast::Expression::Asterisk) {
                    columns.push(ast::SelectItem::Wildcard);
                    let select_item = ast::SelectItem::WildcardWithAlias {
                        expression,
                        as_token: true,
                        alias: "".to_string(),
                    };
                    columns.push(select_item);
                } else {
                    let select_item = ast::SelectItem::WithAlias {
                        expression,
                        as_token: true,
                        alias: "".to_string(),
                    };
                    columns.push(select_item);
                }
            } else {
                if matches!(expression, ast::Expression::Asterisk) {
                    columns.push(ast::SelectItem::Wildcard);
                } else {
                    columns.push(ast::SelectItem::Unnamed(expression));
                }
            }

            if !self.token_is(&Token::Comma, false) {
                break;
            }

            self.advance();
            self.advance();
        }

        if columns.is_empty() {
            return parse_error(ParseErrorType::EmptySelectColumns);
        }

        Ok(columns)
    }

    fn parse_compound_identifier(
        &mut self,
        expr: ast::Expression,
    ) -> Result<Option<ast::Expression>, ParseError<'a>> {
        if !self.token_is(&Token::Period, false) {
            return Ok(None);
        }

        let mut compound = vec![expr];
        // go to period
        self.advance();
        loop {
            let new_expr;
            if let Some((s, Token::Identifier(val))) =
                self.maybe_token(&Token::Identifier(""), false)
            {
                new_expr = ast::Expression::Identifier(val.to_string());
            } else if let Some((s, Token::QuotedIdentifier(val))) =
                self.maybe_token(&Token::QuotedIdentifier(""), false)
            {
                new_expr = ast::Expression::QuotedIdentifier(val.to_string());
            } else if let Some((s, _)) = self.maybe_token(&Token::Asterisk, false) {
                new_expr = ast::Expression::Asterisk;
                break;
            } else {
                return self.unexpected_token(vec!["Identifier".to_string()], false);
            }

            compound.push(new_expr);
            if !self.token_is(&Token::Period, false) {
                break;
            }

            self.advance();
        }

        let compound_expr = ast::Expression::Compound(compound);
        Ok(Some(compound_expr))
    }

    fn parse_table_arg(&mut self, keyword: KeywordDef) -> Result<ast::TableArg, ParseError<'a>> {
        let table_source = self.parse_table_source()?;
        // check if we have joins

        let mut joins = vec![];
        if self.token_is_any_keyword(
            &[Keyword::INNER, Keyword::LEFT, Keyword::RIGHT, Keyword::FULL],
            false,
        ) {
            joins = self.parse_table_joins()?;
        }

        Ok(ast::TableArg {
            from: keyword,
            table: table_source,
            joins,
        })
    }

    fn parse_table_source(&mut self) -> Result<ast::TableSource, ParseError<'a>> {
        let _ = self.expect_token_many(
            &[
                Token::Identifier(""),
                Token::QuotedIdentifier(""),
                Token::LocalVariable(""),
                Token::LeftParen,
            ],
            true,
        )?;

        let expr = self.parse_expression(Precedence::Lowest)?;
        match expr {
            ast::Expression::Identifier(_)
            | ast::Expression::QuotedIdentifier(_)
            | ast::Expression::LocalVariable(_)|ast::Expression::Compound(_) => {
                return Ok(ast::TableSource::Table {
                    name: expr,
                    is_as: false,
                    alias: None,
                })
            }
            _ => return self.unexpected_token(vec!["select items".to_string()], true),
        }
        self.unexpected_token(vec![], true)
    }

    fn parse_table_joins(&mut self) -> Result<Vec<ast::Join>, ParseError<'a>> {
        self.unexpected_token(vec![], true)
    }

    fn parse_expression(
        &mut self,
        precedence: Precedence,
    ) -> Result<ast::Expression, ParseError<'a>> {
        // check if the current token is an identifier
        // or if it is a prefix operator
        let mut left_expression = self.parse_prefix_expression()?;

        // parse the infix expression
        while precedence < self.peek_precedence() {
            // move to the next token
            self.next_token();

            left_expression = self.parse_infix_expression(left_expression)?;
        }

        Ok(left_expression)
    }

    fn parse_prefix_expression(&mut self) -> Result<ast::Expression, ParseError<'a>> {
        let looking_at_current = true;
        if self.token_is_any(&SELECT_ITEM_TYPE, looking_at_current) {
            let mut expr = match self.current_token {
                Some((s, Token::Identifier(val))) => ast::Expression::Identifier(val.to_string()),
                Some((s, Token::QuotedIdentifier(val))) => {
                    ast::Expression::QuotedIdentifier(val.to_string())
                }
                Some((s, Token::StringLiteral(val))) => {
                    ast::Expression::StringLiteral(val.to_string())
                }
                Some((s, Token::NumberLiteral(val))) => {
                    ast::Expression::NumberLiteral(val.to_string())
                }
                Some((s, Token::LocalVariable(val))) => {
                    ast::Expression::LocalVariable(val.to_string())
                }
                Some((s, Token::Asterisk)) => ast::Expression::Asterisk,
                _ => unreachable!(),
            };

            if self.token_is_any(&[Token::Identifier(""), Token::QuotedIdentifier("")], true) {
                if let Some(compound) = self.parse_compound_identifier(expr.clone())? {
                    expr = compound;
                }
            }

            // self.advance();
            return Ok(expr);
        }

        self.unexpected_token(vec!["expression".to_string()], looking_at_current)
    }

    fn parse_infix_expression(
        &mut self,
        left: ast::Expression,
    ) -> Result<ast::Expression, ParseError<'a>> {
        let looking_at_current = true;

        self.unexpected_token(vec!["expression".to_string()], looking_at_current)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn basic_select_statement_new() {
        let input = "SELECT distInct all name, firstname from";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let query = parser.parse();

        let mut select_statement = ast::SelectStatement::default();
        select_statement.select = KeywordDef::new_single((Span::new(0, 5), Keyword::SELECT));
        select_statement.distinct = Some(KeywordDef::new_single((
            Span::new(7, 14),
            Keyword::DISTINCT,
        )));
        select_statement.columns = vec![
            ast::SelectItem::Unnamed(ast::Expression::Identifier("name".to_string())),
            ast::SelectItem::Unnamed(ast::Expression::Identifier("firstname".to_string())),
        ];
        select_statement.all = Some(KeywordDef::new_single((Span::new(16, 18), Keyword::ALL)));
        let expected_query = ast::Query {
            statements: vec![ast::Statement::Select(select_statement)],
        };

        assert_eq!(expected_query, query);
    }
}
