use crate::ast::{self, Keyword};
use crate::error::{parse_error, ParseError, ParseErrorType};
use crate::operator::{get_precedence, Precedence};
use sql_lexer::{Lexer, LexicalError, Span, Token, TokenKind};

const SELECT_ITEM_TYPE_START: &'static [TokenKind<'static>] = &[
    TokenKind::Identifier(""),
    TokenKind::QuotedIdentifier(""),
    TokenKind::NumberLiteral(""),
    TokenKind::LocalVariable(""),
    TokenKind::LeftParen,
    TokenKind::Asterisk,
    TokenKind::Minus,
    TokenKind::Plus,
];

const FUNCTION_ARGS_START: &'static [TokenKind<'static>] = &[
    TokenKind::Identifier(""),
    TokenKind::QuotedIdentifier(""),
    TokenKind::NumberLiteral(""),
    TokenKind::StringLiteral(""),
    TokenKind::LocalVariable(""),
];

const TABLE_SOURCE_START: &'static [TokenKind<'static>] = &[
    TokenKind::Identifier(""),
    TokenKind::QuotedIdentifier(""),
    TokenKind::LocalVariable(""),
    TokenKind::LeftParen,
];

#[derive(Debug, Clone)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Option<Token<'a>>,
    peek_token: Option<Token<'a>>,
    extra_peek_token: Option<Token<'a>>,

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
        // parser.advance();
        parser
    }

    fn advance(&mut self) {
        let _ = self.next_token();
    }

    fn next_token(&mut self) -> Option<Token<'a>> {
        let token = self.current_token.take();
        let mut next_tok;

        loop {
            match self.lexer.next() {
                Some(r) => match r {
                    Ok(token) => match token.kind_as_ref() {
                        TokenKind::Comment(_) => todo!(),
                        _ => {
                            next_tok = Some(token);
                            break;
                        }
                    },
                    Err(e) => self.lexer_errors.push(e),
                },
                None => {
                    next_tok = None;
                    break;
                }
            }
        }

        self.current_token = self.peek_token.take();
        self.peek_token = self.extra_peek_token.take();
        self.extra_peek_token = next_tok.take();
        token
    }

    fn peek_precedence(&self) -> Precedence {
        match self.peek_token {
            Some(token) => get_precedence(token.kind_as_ref()),
            None => Precedence::Lowest,
        }
    }

    fn token_is(&mut self, token_kind: &TokenKind) -> bool {
        if self
            .peek_token
            .is_some_and(|t| t.shallow_eq_token_kind(token_kind))
        {
            return true;
        }
        false
    }

    fn expect_token(&mut self, token_kind: &TokenKind) -> Result<Token, ParseError<'a>> {
        if self
            .peek_token
            .is_some_and(|t| t.shallow_eq_token_kind(token_kind))
        {
            self.advance();
            return self
                .peek_token
                .ok_or(self.unexpected_token(vec![token_kind.to_string()])?);
        }
        parse_error(ParseErrorType::UnrecognizedEof)
    }

    fn token_is_any(&mut self, token_kinds: &[TokenKind]) -> bool {
        // let ret_spanned_token;
        for token in token_kinds {
            if self
                .peek_token
                .is_some_and(|t| t.shallow_eq_token_kind(token))
            {
                return true;
            }
        }
        false
    }

    fn unexpected_token<A>(&self, expected: Vec<String>) -> Result<A, ParseError<'a>> {
        match self.peek_token {
            Some(t) => parse_error(ParseErrorType::UnexpectedToken {
                token: *t.kind_as_ref(),
                expected,
            }),
            None => parse_error(ParseErrorType::UnrecognizedEof),
        }
    }

    fn expect_function_args_start(&mut self) -> Result<(), ParseError<'a>> {
        if let Some(token) = self.peek_token {
            for start_token in FUNCTION_ARGS_START {
                if start_token.shallow_eq_token(token.kind_as_ref()) {
                    return Ok(());
                }
            }
        }
        self.unexpected_token(FUNCTION_ARGS_START.iter().map(|s| s.to_string()).collect())
    }

    fn expect_select_item_start(&mut self) -> Result<(), ParseError<'a>> {
        if let Some(token) = self.peek_token {
            for start_token in SELECT_ITEM_TYPE_START {
                if start_token.shallow_eq_token(token.kind_as_ref()) {
                    return Ok(());
                }
            }
        }
        self.unexpected_token(
            SELECT_ITEM_TYPE_START
                .iter()
                .map(|s| s.to_string())
                .collect(),
        )
    }

    fn expect_table_source_start(&mut self) -> Result<(), ParseError<'a>> {
        if let Some(token) = self.peek_token {
            for start_token in TABLE_SOURCE_START {
                if start_token.shallow_eq_token(token.kind_as_ref()) {
                    return Ok(());
                }
            }
        }
        self.unexpected_token(TABLE_SOURCE_START.iter().map(|s| s.to_string()).collect())
    }

    fn maybe_keyword(&mut self, kind: TokenKind) -> Option<Keyword> {
        if let Some(token) = self.peek_token {
            if token.shallow_eq_token_kind(&kind) {
                self.advance();
                return Keyword::try_from(token).ok();
            }
        }
        None
    }

    fn consume_keyword(&mut self, kind: TokenKind) -> Result<Keyword, ParseError<'a>> {
        if let Some(token) = self.peek_token {
            if token.shallow_eq_token_kind(&kind) {
                let keyword = Keyword::try_from(token)?;
                self.advance();
                return Ok(keyword);
            }
        }
        parse_error(ParseErrorType::ExpectedKeyword)
    }

    fn consume_keyword_many(
        &mut self,
        kinds: &[TokenKind],
    ) -> Result<Vec<Keyword>, ParseError<'a>> {
        let mut keywords = vec![];
        for kind in kinds {
            let keyword = self.consume_keyword(*kind)?;
            keywords.push(keyword);
        }

        Ok(keywords)
    }

    // fn maybe_identifier(&mut self) -> Option<(Span, &'a str)> {
    //     match &self.current_token {
    //         Some(token) if matches!(token.kind_as_ref(), TokenKind::Identifier(..)) => {
    //             self.advance();
    //             Some((token.location(), val))
    //         }
    //         t => {
    //             self.current_token = t;
    //             None
    //         }
    //     }
    // }
}

impl<'a> Parser<'a> {
    pub fn parse(&mut self) -> ast::Query {
        let mut query = ast::Query::new();

        while self.peek_token.is_some() {
            match self.parse_statement() {
                Ok(statement) => query.statements.push(statement),
                Err(parse_error) => self.parse_errors.push(parse_error),
            }
            self.advance();
        }

        query
    }

    fn parse_statement(&mut self) -> Result<ast::Statement, ParseError<'a>> {
        match self.peek_token {
            Some(maybe_token) => match maybe_token.kind_as_ref() {
                TokenKind::Select => {
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
                _ => return parse_error(ParseErrorType::ExpectedKeyword),
            },
            None => todo!(),
        }
    }

    fn parse_select_statement(&mut self) -> Result<ast::SelectStatement, ParseError<'a>> {
        let mut select_statement = ast::SelectStatement::default();
        dbg!(self.current_token);
        dbg!(self.peek_token);

        select_statement.select = self.consume_keyword(TokenKind::Select)?;
        select_statement.distinct = self.maybe_keyword(TokenKind::Distinct);
        select_statement.all = self.maybe_keyword(TokenKind::All);

        select_statement.columns = self.parse_select_items()?;

        if let Some(kw) = self.maybe_keyword(TokenKind::From) {
            let _ = self.parse_table_arg(kw)?;
        }

        return Ok(select_statement);
    }

    fn parse_select_items(&mut self) -> Result<Vec<ast::SelectItem>, ParseError<'a>> {
        // check if the next token is an identifier
        // return an error if the next token is not an identifier or number
        // get the columns to select
        let mut columns: Vec<ast::SelectItem> = vec![];
        while self.token_is_any(&SELECT_ITEM_TYPE_START) {
            let expression = self.parse_expression(Precedence::Lowest)?;
            match expression {
                ast::Expression::Identifier(_)
                | ast::Expression::QuotedIdentifier(_)
                | ast::Expression::StringLiteral(_)
                | ast::Expression::NumberLiteral(_)
                | ast::Expression::LocalVariable(_)
                | ast::Expression::Compound(_)
                | ast::Expression::Asterisk => (),
                _ => return self.unexpected_token(vec!["select items".to_string()]),
            }

            if let Some(kw) = self.maybe_keyword(TokenKind::As) {
                if matches!(expression, ast::Expression::Asterisk) {
                    columns.push(ast::SelectItem::Wildcard);
                    let select_item = ast::SelectItem::WildcardWithAlias {
                        expression,
                        as_kw: Some(kw),
                        alias: "".to_string(),
                    };
                    columns.push(select_item);
                } else {
                    let select_item = ast::SelectItem::WithAlias {
                        expression,
                        as_kw: Some(kw),
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

            self.advance();
            if !self.token_is(&TokenKind::Comma) {
                break;
            }
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
        if !self.token_is(&TokenKind::Period) {
            return Ok(None);
        }

        let mut compound = vec![expr];
        // go to period
        self.advance();
        loop {
            self.expect_select_item_start()?;
            if let Some(token) = self.peek_token {
                let new_expr = match token.kind_as_ref() {
                    TokenKind::Identifier(i) => ast::Expression::Identifier(i.to_string()),
                    TokenKind::QuotedIdentifier(q) => {
                        ast::Expression::QuotedIdentifier(q.to_string())
                    }
                    TokenKind::Asterisk => {
                        compound.push(ast::Expression::Asterisk);
                        break;
                    }
                    _ => unreachable!(),
                };

                compound.push(new_expr);
            }

            if !self.token_is(&TokenKind::Period) {
                break;
            }

            self.advance();
        }

        let compound_expr = ast::Expression::Compound(compound);
        Ok(Some(compound_expr))
    }

    fn parse_table_arg(&mut self, keyword: Keyword) -> Result<ast::TableArg, ParseError<'a>> {
        let table_source = self.parse_table_source()?;
        // check if we have joins

        let mut joins = vec![];
        if self.token_is_any(&[
            TokenKind::Inner,
            TokenKind::Left,
            TokenKind::Right,
            TokenKind::Full,
        ]) {
            joins = self.parse_table_joins()?;
        }

        Ok(ast::TableArg {
            from: keyword,
            table: table_source,
            joins,
        })
    }

    fn parse_table_source(&mut self) -> Result<ast::TableSource, ParseError<'a>> {
        self.expect_table_source_start()?;

        let expr = self.parse_expression(Precedence::Lowest)?;
        match expr {
            ast::Expression::Identifier(_)
            | ast::Expression::QuotedIdentifier(_)
            | ast::Expression::LocalVariable(_)
            | ast::Expression::Compound(_) => {
                return Ok(ast::TableSource::Table {
                    name: expr,
                    is_as: false,
                    alias: None,
                })
            }
            _ => return self.unexpected_token(vec!["select items".to_string()]),
        }
    }

    fn parse_table_joins(&mut self) -> Result<Vec<ast::Join>, ParseError<'a>> {
        let mut joins = vec![];
        loop {
            let join_type;
            let join_keyword;
            if let Some(kw) = self.maybe_keyword(TokenKind::Inner) {
                join_keyword = vec![kw, self.consume_keyword(TokenKind::Join)?];
                join_type = ast::JoinType::Inner;
            } else if let Some(kw) = self.maybe_keyword(TokenKind::Left) {
                if let Some(outer) = self.maybe_keyword(TokenKind::Outer) {
                    join_keyword = vec![kw, outer, self.consume_keyword(TokenKind::Join)?];
                    join_type = ast::JoinType::LeftOuter;
                } else {
                    join_keyword = vec![kw, self.consume_keyword(TokenKind::Join)?];
                    join_type = ast::JoinType::Left;
                }
            } else if let Some(kw) = self.maybe_keyword(TokenKind::Right) {
                if let Some(outer) = self.maybe_keyword(TokenKind::Outer) {
                    join_keyword = vec![kw, outer, self.consume_keyword(TokenKind::Join)?];
                    join_type = ast::JoinType::RightOuter;
                } else {
                    join_keyword = vec![kw, self.consume_keyword(TokenKind::Join)?];
                    join_type = ast::JoinType::Right;
                }
            } else if let Some(kw) = self.maybe_keyword(TokenKind::Full) {
                if let Some(outer) = self.maybe_keyword(TokenKind::Outer) {
                    join_keyword = vec![kw, outer, self.consume_keyword(TokenKind::Join)?];
                    join_type = ast::JoinType::FullOuter;
                } else {
                    join_keyword = vec![kw, self.consume_keyword(TokenKind::Join)?];
                    join_type = ast::JoinType::Full;
                }
            } else {
                break;
            }

            let table_source = self.parse_table_source()?;
            let on_kw = self.consume_keyword(TokenKind::On)?;
            let search_condition = self.parse_expression(Precedence::Lowest)?;

            let join = ast::Join {
                join: join_keyword,
                join_type,
                on: on_kw,
                table: table_source,
                condition: Some(search_condition),
            };
            joins.push(join);
        }

        Ok(joins)
    }

    fn parse_function(
        &mut self,
        fn_keyword: Option<Keyword>,
    ) -> Result<ast::Expression, ParseError<'a>> {
        let mut function_name;
        if let Some(kw) = fn_keyword {
            function_name = ast::FunctionName::Builtin(kw);
        } else {
            function_name = ast::FunctionName::User(self.parse_function_name()?);
        }

        let _ = self.expect_token(&TokenKind::LeftParen)?;
        let mut args = None;
        if !self.token_is(&TokenKind::RightParen) {
            args = Some(self.parse_function_args()?);
        }
        let _ = self.expect_token(&TokenKind::RightParen)?;

        if let Some(kw) = self.maybe_keyword(TokenKind::Over) {
            return Ok(ast::Expression::Function {
                name: Box::new(function_name),
                args,
                over: None,
            });
        }

        parse_error(ParseErrorType::ExpectedKeyword)
    }

    fn parse_function_name(&mut self) -> Result<ast::Expression, ParseError<'a>> {
        let function_name = match self.peek_token {
            Some(token) => match token.kind_as_ref() {
                TokenKind::Identifier(val) => ast::Expression::Identifier(val.to_string()),
                TokenKind::QuotedIdentifier(val) => {
                    ast::Expression::QuotedIdentifier(val.to_string())
                }
                TokenKind::LocalVariable(val) => ast::Expression::LocalVariable(val.to_string()),
                _ => parse_error(ParseErrorType::ExpectedFunctionName)?,
            },
            _ => parse_error(ParseErrorType::UnrecognizedEof)?,
        };

        if self.token_is_any(&[TokenKind::Identifier(""), TokenKind::QuotedIdentifier("")]) {
            if let Some(compound) = self.parse_compound_identifier(function_name)? {
                return Ok(compound);
            }
        }

        Ok(function_name)
    }

    fn parse_function_args(&mut self) -> Result<Vec<ast::Expression>, ParseError<'a>> {
        let args = vec![];

        loop {
            self.expect_function_args_start()?;

            if let Some(token) = self.peek_token {
                let expr = match token.kind_as_ref() {
                    TokenKind::LocalVariable(str) => {
                        ast::Expression::LocalVariable(str.to_string())
                    }
                    TokenKind::StringLiteral(str) => {
                        ast::Expression::StringLiteral(str.to_string())
                    }
                    TokenKind::NumberLiteral(str) => {
                        ast::Expression::NumberLiteral(str.to_string())
                    }
                    // TokenKind::QuotedIdentifier(str) | TokenKind::Identifier(str) => {
                    // if let Some(compound) = self.parse_compound_identifier(token.kind_as_ref())? {
                    //     return ast::Expression::
                    // }
                    // }
                    _ => unreachable!(),
                };
            } else {
                unreachable!()
            }

            if self.token_is(&TokenKind::RightParen) {
                break;
            }
            self.advance()
        }

        Ok(args)
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
        if self.token_is_any(&SELECT_ITEM_TYPE_START) {
            let expr = match self.peek_token {
                Some(token) => match token.kind_as_ref() {
                    TokenKind::Identifier(val) => ast::Expression::Identifier(val.to_string()),
                    TokenKind::QuotedIdentifier(val) => {
                        ast::Expression::QuotedIdentifier(val.to_string())
                    }
                    TokenKind::StringLiteral(val) => {
                        ast::Expression::StringLiteral(val.to_string())
                    }
                    TokenKind::NumberLiteral(val) => {
                        ast::Expression::NumberLiteral(val.to_string())
                    }
                    TokenKind::LocalVariable(val) => {
                        ast::Expression::LocalVariable(val.to_string())
                    }
                    TokenKind::Asterisk => ast::Expression::Asterisk,
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            };

            if self.token_is_any(&[TokenKind::Identifier(""), TokenKind::QuotedIdentifier("")]) {
                if let Some(compound) = self.parse_compound_identifier(expr)? {
                    return Ok(compound);
                }
            }

            // parse user defined function
            if self.token_is(&TokenKind::LeftParen) {
                return Ok(self.parse_function(None)?);
            }

            // self.advance();
            return Ok(expr);
        }

        self.unexpected_token(vec!["expression".to_string()])
    }

    fn parse_infix_expression(
        &mut self,
        left: ast::Expression,
    ) -> Result<ast::Expression, ParseError<'a>> {
        self.unexpected_token(vec!["expression".to_string()])
    }
}
