use crate::ast::{self, Keyword};
use crate::error::{parse_error, ParseError, ParseErrorType};
use crate::operator::{get_precedence, Precedence};
use sql_lexer::{Lexer, LexicalError, Token, TokenKind};

const SELECT_ITEM_TYPE_START: &'static [TokenKind<'static>] = &[
    TokenKind::Identifier(""),
    TokenKind::QuotedIdentifier(""),
    TokenKind::NumberLiteral(""),
    TokenKind::StringLiteral(""),
    TokenKind::LocalVariable(""),
    TokenKind::LeftParen,
    TokenKind::Asterisk,
    TokenKind::Minus,
    TokenKind::Plus,
];

const GROUP_BY_START: &'static [TokenKind<'static>] =
    &[TokenKind::Identifier(""), TokenKind::QuotedIdentifier("")];

const EXPRESSION_LIST_START: &'static [TokenKind<'static>] = &[
    TokenKind::Identifier(""),
    TokenKind::QuotedIdentifier(""),
    TokenKind::NumberLiteral(""),
    TokenKind::StringLiteral(""),
    TokenKind::LocalVariable(""),
];

const BUILTIN_FN_START: &'static [TokenKind<'static>] = &[
    TokenKind::Abs,
    TokenKind::Acos,
    TokenKind::Asin,
    TokenKind::Atan,
    TokenKind::Avg,
    // TokenKind::Cast,
    TokenKind::Ceil,
    TokenKind::Ceiling,
    TokenKind::Cos,
    TokenKind::Cot,
    TokenKind::Count,
    TokenKind::Degrees,
    TokenKind::DenseRank,
    TokenKind::Exp,
    TokenKind::Floor,
    TokenKind::Getdate,
    TokenKind::Log,
    TokenKind::Log10,
    TokenKind::Max,
    TokenKind::Min,
    TokenKind::Nullif,
    TokenKind::Pi,
    TokenKind::Power,
    TokenKind::Radians,
    TokenKind::Rank,
    TokenKind::Round,
    TokenKind::RowNumber,
    TokenKind::Sqrt,
    TokenKind::Square,
    TokenKind::Stage,
    TokenKind::Stdev,
    TokenKind::Stdevp,
    TokenKind::Sum,
    TokenKind::Tan,
    TokenKind::Var,
    TokenKind::Varp,
];

const ORDER_BY_ARGS_START: &'static [TokenKind<'static>] = &[
    TokenKind::Identifier(""),
    TokenKind::QuotedIdentifier(""),
    TokenKind::NumberLiteral(""),
    TokenKind::LocalVariable(""),
];

const PARTITION_BY_START: &'static [TokenKind<'static>] =
    &[TokenKind::Identifier(""), TokenKind::QuotedIdentifier("")];

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

    fn expect_token(&mut self, token_kind: &TokenKind) -> Result<Token<'a>, ParseError<'a>> {
        if self.token_is(token_kind) {
            let tok = self.peek_token.unwrap();
            self.advance();
            return Ok(tok);
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
            if token.kind_as_ref().builtin_fn() {
                return Ok(());
            }
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

    fn expect_group_by_expression_start(&mut self) -> Result<(), ParseError<'a>> {
        if let Some(token) = self.peek_token {
            if token.kind_as_ref().builtin_fn() {
                return Ok(());
            }
            for start_token in GROUP_BY_START {
                if start_token.shallow_eq_token(token.kind_as_ref()) {
                    return Ok(());
                }
            }
        }
        self.unexpected_token(GROUP_BY_START.iter().map(|s| s.to_string()).collect())
    }

    fn expect_expression_list_start(&mut self) -> Result<(), ParseError<'a>> {
        if let Some(token) = self.peek_token {
            if token.kind_as_ref().builtin_fn() {
                return Ok(());
            }
            for start_token in EXPRESSION_LIST_START {
                if start_token.shallow_eq_token(token.kind_as_ref()) {
                    return Ok(());
                }
            }
        }
        self.unexpected_token(
            EXPRESSION_LIST_START
                .iter()
                .map(|s| s.to_string())
                .collect(),
        )
    }

    fn expect_order_by_args_start(&mut self) -> Result<(), ParseError<'a>> {
        if let Some(token) = self.peek_token {
            for start_token in ORDER_BY_ARGS_START {
                if start_token.shallow_eq_token(token.kind_as_ref()) {
                    return Ok(());
                }
            }
        }
        self.unexpected_token(ORDER_BY_ARGS_START.iter().map(|s| s.to_string()).collect())
    }

    fn expect_partition_by_start(&mut self) -> Result<(), ParseError<'a>> {
        if let Some(token) = self.peek_token {
            for start_token in PARTITION_BY_START {
                if start_token.shallow_eq_token(token.kind_as_ref()) {
                    return Ok(());
                }
            }
        }
        self.unexpected_token(PARTITION_BY_START.iter().map(|s| s.to_string()).collect())
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

        select_statement.select = self.consume_keyword(TokenKind::Select)?;
        select_statement.distinct = self.maybe_keyword(TokenKind::Distinct);
        select_statement.all = self.maybe_keyword(TokenKind::All);

        if let Some(kw) = self.maybe_keyword(TokenKind::Top) {
            select_statement.top = Some(self.parse_top_clause(kw)?);
        }
        dbg!(self.peek_token);

        select_statement.columns = self.parse_select_items()?;

        if let Some(kw) = self.maybe_keyword(TokenKind::From) {
            select_statement.table = Some(self.parse_table_arg(kw)?);
        }

        if let Some(kw) = self.maybe_keyword(TokenKind::Where) {
            select_statement.where_clause = Some(self.parse_where_clause(kw)?);
        }

        if let Some(kw) = self.maybe_keyword(TokenKind::Group) {
            let group_by_kws = vec![kw, self.consume_keyword(TokenKind::By)?];
            select_statement.group_by = Some(self.parse_group_by_clause(group_by_kws)?);
        }

        if let Some(kw) = self.maybe_keyword(TokenKind::Having) {
            select_statement.having = Some(self.parse_having_clause(kw)?);
        }

        if let Some(kw) = self.maybe_keyword(TokenKind::Order) {
            let order_by_kws = vec![kw, self.consume_keyword(TokenKind::By)?];
            select_statement.order_by = Some(self.parse_order_by_clause(order_by_kws)?);
        }

        return Ok(select_statement);
    }

    fn parse_select_items(&mut self) -> Result<Vec<ast::SelectItem>, ParseError<'a>> {
        // check if the next token is an identifier
        // return an error if the next token is not an identifier or number
        // get the columns to select
        let mut columns: Vec<ast::SelectItem> = vec![];
        // while self.token_is_any(&SELECT_ITEM_TYPE_START) {
        loop {
            self.expect_select_item_start()?;
            dbg!(self.peek_token);
            let expression = self.parse_expression(Precedence::Lowest)?;
            dbg!(&expression);

            let as_kw = self.maybe_keyword(TokenKind::As);

            // check for alias
            if self.token_is_any(&[
                TokenKind::Identifier(""),
                TokenKind::QuotedIdentifier(""),
                TokenKind::StringLiteral(""),
            ]) {
                let alias = ast::Expression::try_from(self.peek_token)?;
                self.advance();

                if matches!(expression, ast::Expression::Asterisk) {
                    let select_item = ast::SelectItem::WildcardWithAlias {
                        expression,
                        as_kw,
                        alias,
                    };
                    columns.push(select_item);
                } else {
                    let select_item = ast::SelectItem::WithAlias {
                        expression,
                        as_kw,
                        alias,
                    };
                    columns.push(select_item);
                }
            } else if as_kw.is_none() {
                if matches!(expression, ast::Expression::Asterisk) {
                    columns.push(ast::SelectItem::Wildcard);
                } else {
                    columns.push(ast::SelectItem::Unnamed(expression));
                }
            } else {
                return parse_error(ParseErrorType::MissingAliasAfterAsKeyword);
            }
            dbg!(self.peek_token);

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

    fn parse_top_clause(&mut self, top_kw: Keyword) -> Result<ast::Top, ParseError<'a>> {
        dbg!(self.peek_token);
        let top_expr = ast::Expression::try_from(self.peek_token)?;
        match top_expr {
            ast::Expression::NumberLiteral(_) => {}
            _ => return self.unexpected_token(vec!["numeric literal".to_string()]),
        }

        dbg!(self.peek_token);
        self.advance();
        let percent_kw = self.maybe_keyword(TokenKind::Percent);

        dbg!(self.peek_token);
        let with_ties_kw = if let Some(with_kw) = self.maybe_keyword(TokenKind::With) {
            let ties_kw = self.consume_keyword(TokenKind::Ties)?;
            Some(vec![with_kw, ties_kw])
        } else {
            None
        };

        Ok(ast::Top {
            top: top_kw,
            with_ties: with_ties_kw,
            percent: percent_kw,
            quantity: top_expr,
        })
    }

    fn parse_where_clause(
        &mut self,
        where_kw: Keyword,
    ) -> Result<ast::WhereClause, ParseError<'a>> {
        let expression = self.parse_expression(Precedence::Lowest)?;

        Ok(ast::WhereClause {
            where_kw,
            expression,
        })
    }

    fn parse_compound_identifier(
        &mut self,
        expr: ast::Expression,
    ) -> Result<ast::Expression, ParseError<'a>> {
        let mut compound = vec![expr];
        // go to period
        self.advance();
        loop {
            self.expect_select_item_start()?;
            if let Some(token) = self.peek_token {
                let new_expr = ast::Expression::try_from(token)?;
                compound.push(new_expr);
            }

            self.advance();
            if !self.token_is(&TokenKind::Period) {
                break;
            }

            self.advance();
        }

        let compound_expr = ast::Expression::Compound(compound);
        Ok(compound_expr)
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

                // return Ok(ast::TableSource::Table {
                //     name: expr,
                //     is_as: false,
                //     alias: None,
                // })
            }
            _ => return self.unexpected_token(vec!["select items".to_string()]),
        }

        // check for alias
        if self.token_is_any(&[
            TokenKind::Identifier(""),
            TokenKind::QuotedIdentifier(""),
            TokenKind::StringLiteral(""),
        ]) {
            let alias = ast::Expression::try_from(self.peek_token)?;
            self.advance();
            return Ok(ast::TableSource::Table {
                name: expr,
                alias: Some(alias),
            });
        }

        Ok(ast::TableSource::Table {
            name: expr,
            alias: None,
        })
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

    fn parse_function(&mut self, name: ast::Expression) -> Result<ast::Expression, ParseError<'a>> {
        let function_name = match name {
            ast::Expression::Keyword(kw) => ast::FunctionName::Builtin(kw),
            ast::Expression::Compound(_)
            | ast::Expression::Identifier(_)
            | ast::Expression::QuotedIdentifier(_) => ast::FunctionName::User(name),
            _ => unreachable!(),
        };

        let _ = self.expect_token(&TokenKind::LeftParen)?;
        let mut args = None;
        if !self.token_is(&TokenKind::RightParen) {
            args = Some(self.parse_function_args()?);
        }
        let _ = self.expect_token(&TokenKind::RightParen)?;

        if let Some(kw) = self.maybe_keyword(TokenKind::Over) {
            let over_clause = self.parse_function_over_clause(kw)?;
            dbg!(&over_clause);
            return Ok(ast::Expression::Function {
                name: Box::new(function_name),
                args,
                over: Some(Box::new(over_clause)),
            });
        }

        Ok(ast::Expression::Function {
            name: Box::new(function_name),
            args,
            over: None,
        })
    }

    fn parse_function_args(&mut self) -> Result<Vec<ast::Expression>, ParseError<'a>> {
        let mut args = vec![];

        dbg!(self.peek_token);
        loop {
            self.expect_function_args_start()?;
            let expr = self.parse_expression(Precedence::Lowest)?;
            args.push(expr);

            dbg!(self.peek_token);
            if self.token_is(&TokenKind::RightParen) {
                break;
            }
            self.advance()
        }

        Ok(args)
    }

    fn parse_function_over_clause(
        &mut self,
        over_kw: Keyword,
    ) -> Result<ast::OverClause, ParseError<'a>> {
        let _ = self.expect_token(&TokenKind::LeftParen)?;

        let mut partition_by_kws = None;
        if let Some(partition_kw) = self.maybe_keyword(TokenKind::Partition) {
            let by_kw = self.consume_keyword(TokenKind::By)?;
            partition_by_kws = Some(vec![partition_kw, by_kw]);
        }
        let partition_by_clause = self.parse_function_partition_clause()?;
        dbg!(&partition_by_clause);

        let mut order_by_kws = None;
        if let Some(order_kw) = self.maybe_keyword(TokenKind::Order) {
            let by_kw = self.consume_keyword(TokenKind::By)?;
            order_by_kws = Some(vec![order_kw, by_kw]);
        }
        let order_by_args = self.parse_order_by_args()?;
        dbg!(&order_by_args);

        let mut window_frame_clause = None;
        if self.token_is_any(&[TokenKind::Rows, TokenKind::Range]) {
            window_frame_clause = Some(self.parse_function_window_frame_clause()?);
        }
        dbg!(&window_frame_clause);

        let _ = self.expect_token(&TokenKind::RightParen)?;

        Ok(ast::OverClause {
            over_kw,
            partition_by_kws,
            partition_by: partition_by_clause,
            order_by_kws,
            order_by: order_by_args,
            window_frame: window_frame_clause,
        })
    }

    fn parse_function_partition_clause(&mut self) -> Result<Vec<ast::Expression>, ParseError<'a>> {
        let mut args = vec![];

        loop {
            self.expect_partition_by_start()?;
            let expr = self.parse_expression(Precedence::Lowest)?;
            args.push(expr);

            if !self.token_is(&TokenKind::Comma) {
                break;
            }
            self.advance();
        }

        if args.is_empty() {
            return parse_error(ParseErrorType::EmptyPartitionByClause);
        }

        Ok(args)
    }

    fn parse_order_by_args(&mut self) -> Result<Vec<ast::OrderByArg>, ParseError<'a>> {
        let mut items = vec![];

        loop {
            self.expect_order_by_args_start()?;
            let expr = self.parse_expression(Precedence::Lowest)?;

            let item = if let Some(kw) = self.maybe_keyword(TokenKind::Asc) {
                ast::OrderByArg {
                    column: expr,
                    order_kw: Some(kw),
                }
            } else if let Some(kw) = self.maybe_keyword(TokenKind::Desc) {
                ast::OrderByArg {
                    column: expr,
                    order_kw: Some(kw),
                }
            } else {
                ast::OrderByArg {
                    column: expr,
                    order_kw: None,
                }
            };

            items.push(item);
            if !self.token_is(&TokenKind::Comma) {
                break;
            }
            self.advance();
        }

        if items.is_empty() {
            return parse_error(ParseErrorType::EmptyOrderByArgs);
        }

        Ok(items)
    }

    fn parse_function_window_frame_clause(&mut self) -> Result<ast::WindowFrame, ParseError<'a>> {
        let rows_or_range_kw;
        let rows_or_range;
        if let Some(kw) = self.maybe_keyword(TokenKind::Rows) {
            rows_or_range_kw = kw;
            rows_or_range = ast::RowsOrRange::Rows;
        } else if let Some(kw) = self.maybe_keyword(TokenKind::Range) {
            rows_or_range_kw = kw;
            rows_or_range = ast::RowsOrRange::Range;
        } else {
            return parse_error(ParseErrorType::MissingRowsOrRangeInWindowFrameClause);
        }

        let between_kw = self.maybe_keyword(TokenKind::Between);

        let start_bound_keywords;
        let window_frame_bound_start;
        if let Some(unbounded_kw) = self.maybe_keyword(TokenKind::Unbounded) {
            let preceding_kw = self.consume_keyword(TokenKind::Preceding)?;
            start_bound_keywords = vec![unbounded_kw, preceding_kw];
            window_frame_bound_start = ast::WindowFrameBound::UnboundedPreceding;
        } else if let Some(current_kw) = self.maybe_keyword(TokenKind::Current) {
            let row_kw = self.consume_keyword(TokenKind::Row)?;
            start_bound_keywords = vec![current_kw, row_kw];
            window_frame_bound_start = ast::WindowFrameBound::CurrentRow;
        } else if self.token_is(&TokenKind::NumberLiteral("")) {
            let expr = self.parse_expression(Precedence::Lowest)?;
            let preceding_kw = self.consume_keyword(TokenKind::Preceding)?;
            start_bound_keywords = vec![preceding_kw];
            window_frame_bound_start = ast::WindowFrameBound::Preceding(expr);
        } else {
            return parse_error(
                ParseErrorType::ExpectedUnboundedPrecedingCurrentRowOrNumberPreceding,
            );
        }

        if between_kw.is_none() {
            return Ok(ast::WindowFrame {
                rows_or_range,
                rows_or_range_kw,
                start_bound_keywords,
                start: window_frame_bound_start,
                between_kw,
                and_kw: None,
                end_bound_keywords: None,
                end: None,
            });
        }

        let and_kw = self.consume_keyword(TokenKind::And)?;

        let end_bound_keywords;
        let window_frame_bound_end;
        if let Some(unbounded_kw) = self.maybe_keyword(TokenKind::Unbounded) {
            let following_kw = self.consume_keyword(TokenKind::Following)?;
            end_bound_keywords = vec![unbounded_kw, following_kw];
            window_frame_bound_end = ast::WindowFrameBound::UnboundedFollowing;
        } else if let Some(current_kw) = self.maybe_keyword(TokenKind::Current) {
            let row_kw = self.consume_keyword(TokenKind::Row)?;
            end_bound_keywords = vec![current_kw, row_kw];
            window_frame_bound_end = ast::WindowFrameBound::CurrentRow;
        } else if self.token_is(&TokenKind::NumberLiteral("")) {
            let expr = self.parse_expression(Precedence::Lowest)?;
            let following_kw = self.consume_keyword(TokenKind::Following)?;
            end_bound_keywords = vec![following_kw];
            window_frame_bound_end = ast::WindowFrameBound::Preceding(expr);
        } else {
            return parse_error(
                ParseErrorType::ExpectedUnboundedPrecedingCurrentRowOrNumberFollowing,
            );
        }

        return Ok(ast::WindowFrame {
            rows_or_range,
            rows_or_range_kw,
            start_bound_keywords,
            start: window_frame_bound_start,
            between_kw,
            and_kw: Some(and_kw),
            end_bound_keywords: Some(end_bound_keywords),
            end: Some(window_frame_bound_end),
        });
    }

    fn parse_group_by_clause(
        &mut self,
        group_by_kws: Vec<Keyword>,
    ) -> Result<ast::GroupByClause, ParseError<'a>> {
        let mut expressions: Vec<ast::Expression> = vec![];
        loop {
            self.expect_group_by_expression_start()?;
            dbg!(self.peek_token);
            let expression = self.parse_expression(Precedence::Lowest)?;
            dbg!(self.peek_token);

            expressions.push(expression);
            if !self.token_is(&TokenKind::Comma) {
                break;
            }
            self.advance();
        }

        if expressions.is_empty() {
            return parse_error(ParseErrorType::EmptyGroupByClause);
        }

        Ok(ast::GroupByClause {
            expressions,
            group_by_kws,
        })
    }

    fn parse_having_clause(
        &mut self,
        having_kw: Keyword,
    ) -> Result<ast::HavingClause, ParseError<'a>> {
        let expression = self.parse_expression(Precedence::Lowest)?;
        // todo: add check for expression type

        Ok(ast::HavingClause {
            having_kw,
            expression,
        })
    }

    fn parse_order_by_clause(
        &mut self,
        order_by_kws: Vec<Keyword>,
    ) -> Result<ast::OrderByClause, ParseError<'a>> {
        let args = self.parse_order_by_args()?;

        let offset_fetch_clause = if let Some(kw) = self.maybe_keyword(TokenKind::Offset) {
            Some(self.parse_offset_fetch_clause(kw)?)
        } else {
            None
        };
        // todo: add check for expression type

        Ok(ast::OrderByClause {
            order_by_kws,
            expressions: args,
            offset_fetch_clause,
        })
    }

    fn parse_offset_fetch_clause(
        &mut self,
        offset_kw: Keyword,
    ) -> Result<ast::OffsetFetchClause, ParseError<'a>> {
        let offset = self.parse_offset_clause(offset_kw)?;

        let fetch = if let Some(kw) = self.maybe_keyword(TokenKind::Fetch) {
            Some(self.parse_fetch_clause(kw)?)
        } else {
            None
        };

        Ok(ast::OffsetFetchClause { offset, fetch })
    }

    fn parse_offset_clause(
        &mut self,
        offset_kw: Keyword,
    ) -> Result<ast::OffsetArg, ParseError<'a>> {
        let offset = self.parse_expression(Precedence::Lowest)?;

        let row_or_rows;
        let row_or_rows_kw = if let Some(kw) = self.maybe_keyword(TokenKind::Row) {
            row_or_rows = ast::RowOrRows::Row;
            kw
        } else {
            let kw = self.consume_keyword(TokenKind::Rows)?;
            row_or_rows = ast::RowOrRows::Rows;
            kw
        };

        Ok(ast::OffsetArg {
            offset_kw,
            value: offset,
            row_or_rows_kw,
            row: row_or_rows,
        })
    }

    fn parse_fetch_clause(&mut self, fetch_kw: Keyword) -> Result<ast::FetchArg, ParseError<'a>> {
        let next_or_first;
        let next_or_first_kw = if let Some(kw) = self.maybe_keyword(TokenKind::First) {
            next_or_first = ast::NextOrFirst::First;
            kw
        } else {
            let kw = self.consume_keyword(TokenKind::Next)?;
            next_or_first = ast::NextOrFirst::Next;
            kw
        };

        let fetch = self.parse_expression(Precedence::Lowest)?;

        let row_or_rows;
        let row_or_rows_kw = if let Some(kw) = self.maybe_keyword(TokenKind::Row) {
            row_or_rows = ast::RowOrRows::Row;
            kw
        } else {
            let kw = self.consume_keyword(TokenKind::Rows)?;
            row_or_rows = ast::RowOrRows::Rows;
            kw
        };

        let only_kw = self.consume_keyword(TokenKind::Only)?;

        Ok(ast::FetchArg {
            fetch_kw,
            value: fetch,
            first_or_next_kw: next_or_first_kw,
            first: next_or_first,
            row_or_rows_kw,
            row: row_or_rows,
            only_kw,
        })
    }

    fn parse_cast_expression(&mut self) -> Result<ast::Expression, ParseError<'a>> {
        let cast_kw = self.consume_keyword(TokenKind::Cast)?;
        let _ = self.expect_token(&TokenKind::LeftParen)?;

        dbg!(&cast_kw);
        let expression = self.parse_expression(Precedence::Lowest)?;
        dbg!(&expression);

        let as_kw = self.consume_keyword(TokenKind::As)?;
        dbg!(&as_kw);
        let data_type = self.parse_data_type()?;
        dbg!(&data_type);
        dbg!(self.peek_token);
        let _ = self.expect_token(&TokenKind::RightParen)?;

        Ok(ast::Expression::Cast {
            cast_kw,
            expression: Box::new(expression),
            as_kw,
            data_type,
        })
    }

    fn parse_data_type(&mut self) -> Result<ast::DataType, ParseError<'a>> {
        let data_type = if self.token_is(&TokenKind::Int) {
            let keyword = Keyword::try_from(self.peek_token)?;
            self.advance();
            ast::DataType::Int(keyword)
        } else if self.token_is(&TokenKind::Bigint) {
            let keyword = Keyword::try_from(self.peek_token)?;
            self.advance();
            ast::DataType::BigInt(keyword)
        } else if self.token_is(&TokenKind::Tinyint) {
            let keyword = Keyword::try_from(self.peek_token)?;
            self.advance();
            ast::DataType::TinyInt(keyword)
        } else if self.token_is(&TokenKind::Smallint) {
            let keyword = Keyword::try_from(self.peek_token)?;
            self.advance();
            ast::DataType::SmallInt(keyword)
        } else if self.token_is(&TokenKind::Bit) {
            let keyword = Keyword::try_from(self.peek_token)?;
            self.advance();
            ast::DataType::Bit(keyword)
        } else if self.token_is(&TokenKind::Real) {
            let keyword = Keyword::try_from(self.peek_token)?;
            self.advance();
            ast::DataType::Real(keyword)
        } else if self.token_is(&TokenKind::Date) {
            let keyword = Keyword::try_from(self.peek_token)?;
            self.advance();
            ast::DataType::Date(keyword)
        } else if self.token_is(&TokenKind::Datetime) {
            let keyword = Keyword::try_from(self.peek_token)?;
            self.advance();
            ast::DataType::Datetime(keyword)
        } else if self.token_is(&TokenKind::Time) {
            let keyword = Keyword::try_from(self.peek_token)?;
            self.advance();
            ast::DataType::Time(keyword)
        } else if self.token_is(&TokenKind::Float) {
            let keyword = Keyword::try_from(self.peek_token)?;
            self.advance();
            let float_precision = self.parse_float_precision()?;
            ast::DataType::Float(keyword, float_precision)
        } else if self.token_is(&TokenKind::Decimal) {
            let keyword = Keyword::try_from(self.peek_token)?;
            self.advance();
            let numeric_size = self.parse_numeric_size()?;
            ast::DataType::Decimal(keyword, numeric_size)
        } else if self.token_is(&TokenKind::Numeric) {
            let keyword = Keyword::try_from(self.peek_token)?;
            self.advance();
            let numeric_size = self.parse_numeric_size()?;
            ast::DataType::Numeric(keyword, numeric_size)
        } else if self.token_is(&TokenKind::Varchar) {
            let keyword = Keyword::try_from(self.peek_token)?;
            self.advance();
            let float_precision = self.parse_float_precision()?;
            ast::DataType::Varchar(keyword, float_precision)
        } else {
            return parse_error(ParseErrorType::ExpectedDataType);
        };

        Ok(data_type)
    }

    fn parse_float_precision(&mut self) -> Result<Option<u32>, ParseError<'a>> {
        if self.token_is(&TokenKind::LeftParen) {
            let _ = self.expect_token(&TokenKind::LeftParen)?;
            let numeric_literal = match self.peek_token {
                Some(token) => ast::Expression::try_from(token)?,
                _ => unreachable!(),
            };
            let size: u32 = match numeric_literal {
                ast::Expression::NumberLiteral(n) => match n.content.parse() {
                    Ok(n) => n,
                    Err(_) => return parse_error(ParseErrorType::ExpectedFloatPrecision),
                },
                _ => return parse_error(ParseErrorType::ExpectedFloatPrecision),
            };
            let _ = self.expect_token(&TokenKind::RightParen)?;
            Ok(Some(size))
        } else {
            Ok(None)
        }
    }

    fn parse_numeric_size(&mut self) -> Result<Option<ast::NumericSize>, ParseError<'a>> {
        let _ = self.expect_token(&TokenKind::LeftParen)?;
        if self.token_is(&TokenKind::RightParen) {
            return Ok(None);
        }

        let float_precision = match self.parse_float_precision()? {
            Some(p) => p,
            None => return parse_error(ParseErrorType::ExpectedFloatPrecision),
        };

        let scale = if self.token_is(&TokenKind::Comma) {
            let _ = self.expect_token(&TokenKind::Comma)?;
            Some(self.parse_numeric_scale()?)
        } else {
            None
        };

        let _ = self.expect_token(&TokenKind::RightParen)?;

        Ok(Some(ast::NumericSize {
            precision: float_precision,
            scale,
        }))
    }

    fn parse_numeric_scale(&mut self) -> Result<u32, ParseError<'a>> {
        let numeric_literal = match self.peek_token {
            Some(token) => ast::Expression::try_from(token)?,
            _ => unreachable!(),
        };
        let size: u32 = match numeric_literal {
            ast::Expression::NumberLiteral(n) => match n.content.parse() {
                Ok(n) => n,
                Err(_) => return parse_error(ParseErrorType::ExpectedFloatPrecision),
            },
            _ => return parse_error(ParseErrorType::ExpectedFloatPrecision),
        };

        Ok(size)
    }

    fn parse_expression_list(&mut self) -> Result<Vec<ast::Expression>, ParseError<'a>> {
        let mut expressions = vec![];

        loop {
            self.expect_expression_list_start()?;
            dbg!(self.peek_token);
            let expression = self.parse_expression(Precedence::Lowest)?;
            dbg!(self.peek_token);

            expressions.push(expression);
            if !self.token_is(&TokenKind::Comma) {
                break;
            }
            self.advance();
        }

        Ok(expressions)
    }

    fn parse_subquery(&mut self) -> Result<ast::Expression, ParseError<'a>> {
        let _ = self.expect_token(&TokenKind::LeftParen)?;
        let select_statement = self.parse_select_statement()?;
        let _ = self.expect_token(&TokenKind::RightParen)?;

        Ok(ast::Expression::Subquery(Box::new(select_statement)))
    }

    fn parse_in_expression(
        &mut self,
        test_expression: ast::Expression,
        in_kw: Keyword,
        not_kw: Option<Keyword>,
    ) -> Result<ast::Expression, ParseError<'a>> {
        let _ = self.expect_token(&TokenKind::LeftParen)?;
        let expr = if self.token_is(&TokenKind::Select) {
            dbg!(self.peek_token);
            let subquery = ast::Expression::Subquery(Box::new(self.parse_select_statement()?));
            ast::Expression::InSubquery {
                test_expression: Box::new(test_expression),
                in_kw,
                not_kw,
                subquery: Box::new(subquery),
            }
        } else if self.token_is_any(&EXPRESSION_LIST_START) {
            let list = self.parse_expression_list()?;
            ast::Expression::InExpressionList {
                test_expression: Box::new(test_expression),
                in_kw,
                not_kw,
                list,
            }
        } else {
            return parse_error(ParseErrorType::ExpectedSubqueryOrExpressionList);
        };

        let _ = self.expect_token(&TokenKind::RightParen)?;

        Ok(expr)
    }

    fn parse_between_expression(
        &mut self,
        test_expression: ast::Expression,
        not_kw: Option<Keyword>,
        between_kw: Keyword,
    ) -> Result<ast::Expression, ParseError<'a>> {
        let begin = self.parse_prefix_expression()?;
        let and_kw = self.consume_keyword(TokenKind::And)?;
        let end = self.parse_prefix_expression()?;

        Ok(ast::Expression::Between {
            test_expression: Box::new(test_expression),
            not_kw,
            between_kw,
            begin: Box::new(begin),
            and_kw,
            end: Box::new(end),
        })
    }

    fn parse_expression(
        &mut self,
        precedence: Precedence,
    ) -> Result<ast::Expression, ParseError<'a>> {
        // check if the current token is an identifier
        // or if it is a prefix operator
        let mut left_expression = self.parse_prefix_expression()?;
        dbg!(&left_expression);

        // parse the infix expression
        while precedence < self.peek_precedence() {
            left_expression = self.parse_infix_expression(left_expression)?;
        }

        Ok(left_expression)
    }

    fn parse_prefix_expression(&mut self) -> Result<ast::Expression, ParseError<'a>> {
        if self.token_is_any(&[
            TokenKind::Identifier(""),
            TokenKind::QuotedIdentifier(""),
            TokenKind::NumberLiteral(""),
            TokenKind::StringLiteral(""),
            TokenKind::LocalVariable(""),
            TokenKind::Asterisk,
        ]) {
            let mut expr = ast::Expression::try_from(self.peek_token)?;

            let mut could_be_compound = false;
            if self.token_is_any(&[TokenKind::Identifier(""), TokenKind::QuotedIdentifier("")]) {
                could_be_compound = true;
            }

            self.advance();

            if could_be_compound && self.token_is(&TokenKind::Period) {
                expr = self.parse_compound_identifier(expr)?;
            }

            // parse user defined function
            if self.token_is(&TokenKind::LeftParen) {
                return Ok(self.parse_function(expr)?);
            }

            // self.advance();
            return Ok(expr);
        } else if self.token_is_any(BUILTIN_FN_START) {
            let fn_name = ast::Expression::Keyword(ast::Keyword::try_from(self.peek_token)?);

            self.advance();
            // parse user defined function
            if self.token_is(&TokenKind::LeftParen) {
                return Ok(self.parse_function(fn_name)?);
            }

            unreachable!();
        } else if self.token_is_any(&[TokenKind::Minus, TokenKind::Plus]) {
            let unary_op = ast::UnaryOperator::try_from(self.peek_token)?;

            self.advance();
            let right_expr = ast::Expression::try_from(self.peek_token)?;
            match right_expr {
                ast::Expression::NumberLiteral(_) => {}
                _ => return self.unexpected_token(vec!["numeric literal".to_string()]),
            }

            return Ok(ast::Expression::Unary {
                operator: unary_op,
                right: Box::new(right_expr),
            });
        } else if self.token_is(&TokenKind::Cast) {
            let expr = self.parse_cast_expression()?;
            dbg!(&expr);
            return Ok(expr);
        } else if self.token_is(&TokenKind::LeftParen) {
            let subquery = self.parse_subquery()?;
            return Ok(subquery);
        } else if self.token_is(&TokenKind::Not) {
            let not_kw = self.consume_keyword(TokenKind::Not)?;
            let expression = self.parse_expression(Precedence::Lowest)?;
            return Ok(ast::Expression::Not {
                not_kw,
                expression: Box::new(expression),
            });
        }

        self.unexpected_token(vec!["expression".to_string()])
    }

    fn parse_infix_expression(
        &mut self,
        left: ast::Expression,
    ) -> Result<ast::Expression, ParseError<'a>> {
        if self.token_is(&TokenKind::And) {
            let precedence = self.peek_precedence();
            let and_kw = self.consume_keyword(TokenKind::And)?;
            let right = self.parse_expression(precedence)?;

            return Ok(ast::Expression::And {
                and_kw,
                left: Box::new(left),
                right: Box::new(right),
            });
        } else if self.token_is(&TokenKind::Or) {
            let precedence = self.peek_precedence();
            let or_kw = self.consume_keyword(TokenKind::Or)?;
            let right = self.parse_expression(precedence)?;

            return Ok(ast::Expression::Or {
                or_kw,
                left: Box::new(left),
                right: Box::new(right),
            });
        } else if self.token_is_any(&[
            TokenKind::Equal,
            TokenKind::BangEqual,
            TokenKind::LessThanGreaterThan,
            TokenKind::GreaterThan,
            TokenKind::GreaterThanEqual,
            TokenKind::LessThan,
            TokenKind::LessThanEqual,
        ]) {
            let op = ast::ComparisonOperator::try_from(self.peek_token)?;
            let precedence = self.peek_precedence();

            self.advance();
            dbg!(self.peek_token);
            let right = self.parse_expression(precedence)?;

            dbg!(&left);
            dbg!(&right);
            return Ok(ast::Expression::Comparison {
                operator: op,
                left: Box::new(left),
                right: Box::new(right),
            });
        } else if self.token_is_any(&[
            TokenKind::Plus,
            TokenKind::Minus,
            TokenKind::Asterisk,
            TokenKind::ForwardSlash,
            TokenKind::PercentSign,
        ]) {
            let op = ast::ArithmeticOperator::try_from(self.peek_token)?;
            let precedence = self.peek_precedence();

            self.advance();
            dbg!(self.peek_token);
            let right = self.parse_expression(precedence)?;

            dbg!(&left);
            dbg!(&right);
            return Ok(ast::Expression::Arithmetic {
                operator: op,
                left: Box::new(left),
                right: Box::new(right),
            });
        } else if self.token_is(&TokenKind::In) {
            let in_kw = self.consume_keyword(TokenKind::In)?;
            return Ok(self.parse_in_expression(left, in_kw, None)?);
        } else if self.token_is(&TokenKind::Between) {
            let between_kw = self.consume_keyword(TokenKind::Between)?;
            return Ok(self.parse_between_expression(left, None, between_kw)?);
        } else if self.token_is(&TokenKind::Not) {
            let not_kw = self.consume_keyword(TokenKind::Not)?;
            if let Some(in_kw) = self.maybe_keyword(TokenKind::In) {
                return Ok(self.parse_in_expression(left, in_kw, Some(not_kw))?);
            } else if let Some(between_kw) = self.maybe_keyword(TokenKind::Between) {
                return Ok(self.parse_between_expression(left, Some(not_kw), between_kw)?);
            } else {
                return parse_error(ParseErrorType::ExpectedSubqueryOrExpressionList);
            }
        }

        self.unexpected_token(vec!["expression".to_string()])
    }
}
