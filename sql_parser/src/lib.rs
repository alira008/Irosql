pub mod ast;
mod keywords;
pub mod lexer;
pub mod token;
use keywords::Keyword;
use token::{Kind, Literal, Token};

#[derive(Debug, Clone)]
pub struct Parser<'a> {
    lexer: lexer::Lexer<'a>,
    current_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

// create a precedence table
// this will be used to determine the precedence of operators
const PRECEDENCE_HIGHEST: u8 = 8;
const PRECEDENCE_PRODUCT: u8 = 7;
const PRECEDENCE_SUM: u8 = 6;
const PRECEDENCE_COMPARISON: u8 = 5;
const PRECEDENCE_NOT: u8 = 4;
const PRECEDENCE_AND: u8 = 3;
const PRECEDENCE_OTHER_LOGICALS: u8 = 2;
const PRECEDENCE_LOWEST: u8 = 1;

impl<'a> Parser<'a> {
    pub fn new(lexer: lexer::Lexer<'a>) -> Self {
        let mut parser = Parser {
            lexer,
            current_token: Token::wrap(Kind::Eof, Literal::new_string("")),
            peek_token: Token::wrap(Kind::Eof, Literal::new_string("")),
            errors: vec![],
        };
        parser.next_token();
        parser.next_token();
        parser
    }

    pub fn errors(&self) -> Vec<String> {
        self.errors.clone()
    }

    pub fn parse(&mut self) -> ast::Query {
        let mut query = ast::Query::new();

        while self.current_token.kind() != Kind::Eof {
            if let Some(statement) = self.parse_statement() {
                query.statements.push(statement);
            }

            self.next_token();
        }

        query
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn parse_statement(&mut self) -> Option<ast::Statement> {
        match self.current_token.kind() {
            Kind::Keyword(keyword) => match keyword {
                Keyword::SELECT => {
                    let select_statement = self.parse_select_statement();
                    select_statement
                }
                _ => None,
            },
            _ => None,
        }
    }

    fn parse_select_statement(&mut self) -> Option<ast::Statement> {
        let mut statement = ast::SelectStatement::new();

        // check if the next token is a DISTINCT keyword
        if self.peek_token_is(Kind::Keyword(Keyword::DISTINCT)) {
            self.next_token();
            statement.distinct = true;
        }

        // check if the optional all keyword is present
        if self.peek_token_is(Kind::Keyword(Keyword::ALL)) {
            self.next_token();
        }

        // check if the next token is a TOP keyword
        if self.peek_token_is(Kind::Keyword(Keyword::TOP)) {
            self.next_token();

            // skip TOP keyword
            self.next_token();

            if let Some(expression) = self.parse_expression(PRECEDENCE_LOWEST) {
                // check if the next token is PERCENT
                let mut is_percent = false;
                if self.peek_token_is(Kind::Keyword(Keyword::PERCENT)) {
                    self.next_token();
                    is_percent = true;
                }

                // check if the next token is WITH TIES
                let mut is_with_ties = false;
                if self.peek_token_is(Kind::Keyword(Keyword::WITH)) {
                    self.next_token();
                    if !self.expect_peek(Kind::Keyword(Keyword::TIES)) {
                        // TODO: error handling
                        return None;
                    }
                    is_with_ties = true;
                }

                statement.top = Some(ast::TopArg {
                    with_ties: is_with_ties,
                    percent: is_percent,
                    quantity: expression,
                });
            } else {
                self.current_msg_error("expected expression after TOP keyword");
                return None;
            }
        }

        // check for columns
        if let Some(select_items) = self.parse_select_items() {
            statement.columns = select_items;
        } else {
            return None;
        }

        // check if we have a INTO keyword
        if self.peek_token_is(Kind::Keyword(Keyword::INTO)) {
            // go to the INTO keyword
            self.next_token();

            // check if the next token is an identifier
            if !self.expect_peek(Kind::Ident) {
                return None;
            }

            let into_table = ast::Expression::Literal(self.current_token.clone());
            let mut file_group: Option<ast::Expression> = None;

            // check if we ON keyword
            if self.peek_token_is(Kind::Keyword(Keyword::ON)) {
                // skip the ON keyword
                self.next_token();

                // check if the next token is an identifier
                if !self.expect_peek(Kind::Ident) {
                    return None;
                }

                file_group = Some(ast::Expression::Literal(self.current_token.clone()));
            }
            statement.into_table = Some(ast::IntoArg {
                table: into_table,
                file_group,
            });
        }

        // two cases:
        // normal query where we select from a table
        // or a query where we select numbers|quoted string (this one doesn't require FROM keyword)
        // Note: we should have one or more columns by the time we get here
        // check if we have a quoted_strings or numbers only
        let number_of_non_literal_tokens = statement
            .columns
            .iter()
            .filter(|ex| !match ex {
                ast::SelectItem::Unnamed(expression)
                | ast::SelectItem::WithAlias { expression, .. } => match expression {
                    ast::Expression::Literal(token) => {
                        matches!(token.kind(), Kind::Number)
                    }
                    ast::Expression::Subquery(_) => true,
                    _ => false,
                },
                _ => false,
            })
            .count();

        if number_of_non_literal_tokens > 0 {
            // at this point we should have a FROM keyword
            // but we should make sure
            if !self.expect_peek(Kind::Keyword(Keyword::FROM)) {
                return None;
            }

            statement.table = self.parse_table_arg();
            dbg!(&statement.table);
        } else {
            // check if we have a FROM keyword
            if self.peek_token_is(Kind::Keyword(Keyword::FROM)) {
                // go to the FROM keyword
                self.next_token();

                statement.table = self.parse_table_arg();
            }
        }

        // check if we have any where clause
        if self.peek_token_is(Kind::Keyword(Keyword::WHERE)) {
            // skip the WHERE keyword
            self.next_token();
            self.next_token();

            let expression = self.parse_expression(PRECEDENCE_LOWEST);
            if expression.as_ref().is_some_and(|ex| {
                !matches!(
                    *ex,
                    ast::Expression::Binary { .. }
                        | ast::Expression::Between { .. }
                        | ast::Expression::Unary { .. }
                )
            }) {
                self.current_msg_error("expected expression after WHERE keyword");
            }
            if expression.is_none() {
                self.current_msg_error("expected expression after WHERE keyword");
                return None;
            }

            statement.where_clause = expression;
        }

        // check if we have any GROUP BY clause
        if self.peek_token_is(Kind::Keyword(Keyword::GROUP)) {
            // skip the GROUP keyword
            self.next_token();

            if let Some(expression) = self.parse_group_by_args() {
                statement.group_by = expression;
            } else {
                return None;
            }
        }

        // check if we have any having clause
        if self.peek_token_is(Kind::Keyword(Keyword::HAVING)) {
            // skip the having keyword
            self.next_token();
            self.next_token();

            let expression = self.parse_expression(PRECEDENCE_LOWEST);
            if expression
                .as_ref()
                .is_some_and(|ex| !matches!(*ex, ast::Expression::Binary { .. }))
            {
                self.current_msg_error("expected expression after HAVING keyword");
            }
            if expression.is_none() {
                self.current_msg_error("expected expression after HAVING keyword");
                return None;
            }

            statement.having = expression;
        }

        // order by expression
        if self.peek_token_is(Kind::Keyword(Keyword::ORDER)) {
            // go to order keyword
            self.next_token();

            if let Some(args) = self.parse_order_by_args() {
                statement.order_by = args;
            } else {
                return None;
            }

            if self.peek_token_is(Kind::Keyword(Keyword::OFFSET)) {
                // go to offset keyword
                self.next_token();

                let offset = self.parse_offset();
                if offset.is_none() {
                    return None;
                }

                statement.offset = offset;

                // check if we have a FETCH keyword
                if self.peek_token_is(Kind::Keyword(Keyword::FETCH)) {
                    // go to fetch keyword
                    self.next_token();

                    let fetch = self.parse_fetch();
                    if fetch.is_none() {
                        return None;
                    }

                    statement.fetch = fetch;
                    self.next_token();
                }
            }
        }

        Some(ast::Statement::Select(Box::new(statement)))
    }

    fn parse_table_source(&mut self) -> Option<ast::TableSource> {
        // check if the next token is an identifier
        if !self.expect_peek(Kind::Ident) {
            dbg!(self.current_token.clone());
            return None;
        }
        let table_name = ast::Expression::Literal(self.current_token.clone());

        // check if we have an alias
        let is_an;
        let mut alias = None;
        if self.peek_token_is(Kind::Keyword(Keyword::AS)) {
            // skip the AS keyword
            self.next_token();
            // check if the next token is an identifier
            if !self.peek_token_is(Kind::Ident) {
                return None;
            }
            is_an = true;
        } else {
            is_an = false;
        }

        if self.peek_token_is(Kind::Ident) {
            self.next_token();
            alias = Some(self.current_token.to_string());
        }

        Some(ast::TableSource::Table {
            name: table_name,
            is_an,
            alias,
        })
    }

    fn parse_table_arg(&mut self) -> Option<ast::TableArg> {
        if let Some(table_source) = self.parse_table_source() {
            // check if we have joins
            let mut joins = vec![];
            loop {
                let join = if self.peek_token_is(Kind::Keyword(Keyword::INNER)) {
                    // skip the INNER keyword
                    self.next_token();

                    // skip the JOIN keyword
                    if !self.expect_peek(Kind::Keyword(Keyword::JOIN)) {
                        return None;
                    }

                    // parse the table source
                    if let Some(table_source) = self.parse_table_source() {
                        // check if we have an ON keyword
                        if !self.expect_peek(Kind::Keyword(Keyword::ON)) {
                            return None;
                        }

                        // skip the ON keyword
                        self.next_token();

                        // parse the search condition
                        if let Some(expression) = self.parse_expression(PRECEDENCE_LOWEST) {
                            ast::Join {
                                join_type: ast::JoinType::Inner,
                                table: table_source,
                                condition: Some(expression),
                            }
                        } else {
                            self.current_msg_error("expected search condition after ON keyword");
                            return None;
                        }
                    } else {
                        self.current_msg_error("expected TABLE SOURCE after JOIN keyword");
                        return None;
                    }
                } else if self.peek_token_is(Kind::Keyword(Keyword::LEFT))
                    | self.peek_token_is(Kind::Keyword(Keyword::RIGHT))
                    | self.peek_token_is(Kind::Keyword(Keyword::FULL))
                {
                    self.next_token();
                    let join_type = match (self.current_token.kind(), self.peek_token.kind()) {
                        (Kind::Keyword(Keyword::LEFT), Kind::Keyword(Keyword::OUTER)) => {
                            self.next_token();
                            ast::JoinType::LeftOuter
                        }
                        (Kind::Keyword(Keyword::LEFT), _) => ast::JoinType::Left,
                        (Kind::Keyword(Keyword::RIGHT), Kind::Keyword(Keyword::OUTER)) => {
                            self.next_token();
                            ast::JoinType::RightOuter
                        }
                        (Kind::Keyword(Keyword::RIGHT), _) => ast::JoinType::Right,
                        (Kind::Keyword(Keyword::FULL), Kind::Keyword(Keyword::OUTER)) => {
                            self.next_token();
                            ast::JoinType::FullOuter
                        }
                        (Kind::Keyword(Keyword::FULL), _) => ast::JoinType::Full,
                        _ => unreachable!(),
                    };

                    // skip the JOIN keyword
                    if !self.expect_peek(Kind::Keyword(Keyword::JOIN)) {
                        return None;
                    }

                    // get the table source
                    if let Some(table_source) = self.parse_table_source() {
                        // check if we have an ON keyword
                        if !self.expect_peek(Kind::Keyword(Keyword::ON)) {
                            return None;
                        }

                        // skip the ON keyword
                        self.next_token();

                        // parse the search condition
                        if let Some(expression) = self.parse_expression(PRECEDENCE_LOWEST) {
                            ast::Join {
                                join_type,
                                table: table_source,
                                condition: Some(expression),
                            }
                        } else {
                            self.current_msg_error("expected search condition after ON keyword");
                            return None;
                        }
                    } else {
                        self.current_msg_error("expected TABLE SOURCE after JOIN keyword");
                        return None;
                    }
                } else {
                    break;
                };

                joins.push(join);
            }

            dbg!(&joins);
            Some(ast::TableArg {
                table: table_source,
                joins,
            })
        } else {
            None
        }
    }

    fn parse_select_items(&mut self) -> Option<Vec<ast::SelectItem>> {
        // check if the next token is an identifier
        // return an error if the next token is not an identifier or number
        if !self.peek_token_is(Kind::Ident)
            && !self.peek_token_is(Kind::Number)
            && !self.peek_token_is(Kind::Asterisk)
            && !self.peek_token_is(Kind::LeftParen)
        {
            self.peek_error(Kind::Ident);
            return None;
        }

        // get the columns to select
        let mut columns: Vec<ast::SelectItem> = vec![];
        while !self.peek_token_is(Kind::Keyword(Keyword::FROM))
            && !self.peek_token_is(Kind::Keyword(Keyword::INTO))
            && !self.peek_token_is(Kind::Eof)
        {
            self.next_token();

            if columns.len() > 0 {
                // expect a COMMA before the next GROUP BY expression
                if !self.expect_current(Kind::Comma) {
                    self.current_msg_error("expected COMMA before next ORDER BY expression");
                    return None;
                }

                // consume the COMMA
                self.next_token();
            }

            // parse the expression
            if let Some(expression) = self.parse_expression(PRECEDENCE_LOWEST) {
                // check if it is a compounded identifier
                // check if the next token is the keyword AS
                let mut is_as = false;
                if self.peek_token_is(Kind::Keyword(Keyword::AS)) {
                    // skip the AS keyword
                    self.next_token();
                    is_as = true;

                    if !self.peek_token_is(Kind::Ident) {
                        self.peek_msg_error(
                            "expected token to either be a quoted string or identifier after AS keyword",
                        );
                        return None;
                    }
                }

                // if next token is a comma then this is a column without an alias
                if self.peek_token_is(Kind::Ident) {
                    self.next_token();

                    // check if expression was a wildcard
                    // add to the columns
                    if matches!(expression, ast::Expression::Literal(ref token) if token.kind() == Kind::Asterisk)
                    {
                        columns.push(ast::SelectItem::WildcardWithAlias {
                            expression,
                            as_token: is_as,
                            alias: self.current_token.to_string(),
                        });
                    } else {
                        columns.push(ast::SelectItem::WithAlias {
                            expression,
                            as_token: is_as,
                            alias: self.current_token.to_string(),
                        });
                    }
                } else {
                    // add to the columns
                    // check if expression was a wildcard
                    if matches!(expression, ast::Expression::Literal(ref token) if token.kind() == Kind::Asterisk)
                    {
                        columns.push(ast::SelectItem::Wildcard);
                    } else {
                        columns.push(ast::SelectItem::Unnamed(expression));
                    }
                }
            } else {
                self.current_error(Kind::Ident);
                return None;
            }
        }

        match columns.len() {
            0 => {
                self.peek_msg_error("expected SELECT items in SELECT expression");
                None
            }
            _ => Some(columns),
        }
    }

    fn parse_grouping(&mut self) -> Option<ast::Expression> {
        if !self.expect_current(Kind::LeftParen) {
            return None;
        }

        self.next_token();

        let grouping;

        if let Some(expression) = self.parse_expression(PRECEDENCE_LOWEST) {
            grouping = Some(ast::Expression::Grouping(Box::new(expression)));
        } else {
            // TODO: error handling
            return None;
        }
        if !self.expect_peek(Kind::RightParen) {
            return None;
        } else {
            grouping
        }
    }

    fn parse_offset(&mut self) -> Option<ast::OffsetArg> {
        // skip the OFFSET keyword
        self.next_token();

        // get the offset value
        if let Some(offset) = self.parse_expression(PRECEDENCE_LOWEST) {
            if !self.expect_peek_multi(
                &[Kind::Keyword(Keyword::ROW), Kind::Keyword(Keyword::ROWS)],
                Kind::Keyword(Keyword::ROW),
            ) {
                // TODO: error handling
                return None;
            }
            let row = match self.current_token.kind() {
                Kind::Keyword(Keyword::ROW) => ast::RowOrRows::Row,
                Kind::Keyword(Keyword::ROWS) => ast::RowOrRows::Rows,
                _ => {
                    // TODO: error handling
                    self.current_error(Kind::Keyword(Keyword::ROWS));
                    return None;
                }
            };
            // consume the ROW or ROWS
            Some(ast::OffsetArg { value: offset, row })
        } else {
            self.current_msg_error("expected expression after OFFSET keyword");
            None
        }
    }

    fn parse_fetch(&mut self) -> Option<ast::FetchArg> {
        // check if the next token is FIRST or NEXT
        if !self.expect_peek_multi(
            &[Kind::Keyword(Keyword::NEXT), Kind::Keyword(Keyword::FIRST)],
            Kind::Keyword(Keyword::NEXT),
        ) {
            return None;
        }
        let first = match self.current_token.kind() {
            Kind::Keyword(Keyword::FIRST) => ast::NextOrFirst::First,
            Kind::Keyword(Keyword::NEXT) => ast::NextOrFirst::Next,
            _ => {
                self.current_error(Kind::Keyword(Keyword::NEXT));
                return None;
            }
        };

        // consume the FIRST or NEXT
        self.next_token();

        // get the fetch value
        if let Some(fetch) = self.parse_expression(PRECEDENCE_LOWEST) {
            // check if the next token is ROW or ROWS
            if !self.expect_peek_multi(
                &[Kind::Keyword(Keyword::ROW), Kind::Keyword(Keyword::ROWS)],
                Kind::Keyword(Keyword::ROW),
            ) {
                // TODO: error handling
                return None;
            }
            let row = match self.current_token.kind() {
                Kind::Keyword(Keyword::ROW) => ast::RowOrRows::Row,
                Kind::Keyword(Keyword::ROWS) => ast::RowOrRows::Rows,
                _ => {
                    self.current_error(Kind::Keyword(Keyword::ROW));
                    return None;
                }
            };

            // check if we have the keyword ONLY
            if !self.expect_peek(Kind::Keyword(Keyword::ONLY)) {
                return None;
            }
            // consume the ROW or ROWS
            self.next_token();

            Some(ast::FetchArg {
                value: fetch,
                row,
                first,
            })
        } else {
            self.peek_msg_error("expected FETCH expression after FETCH FIRST|NEXT");
            None
        }
    }

    fn parse_group_by_args(&mut self) -> Option<Vec<ast::Expression>> {
        // check if the next token is BY
        if !self.expect_peek(Kind::Keyword(Keyword::BY)) {
            // TODO: error handling
            return None;
        }

        // get the columns to order by
        let mut args = vec![];
        while !self.peek_token_is(Kind::Keyword(Keyword::HAVING))
            && !self.peek_token_is(Kind::SemiColon)
            && !self.peek_token_is(Kind::Eof)
        {
            self.next_token();

            if args.len() > 0 {
                // expect a COMMA before the next GROUP BY expression
                if !self.expect_current(Kind::Comma) {
                    self.current_msg_error("expected COMMA before next ORDER BY expression");
                    return None;
                }

                // consume the COMMA
                self.next_token();
            }

            if let Some(expression) = self.parse_expression(PRECEDENCE_LOWEST) {
                args.push(expression);
            } else {
                // TODO: error handling
                self.current_error(Kind::Ident);
                return None;
            }
        }

        match args.len() {
            0 => {
                self.peek_msg_error("expected GROUP BY expression after GROUP BY");
                None
            }
            _ => Some(args),
        }
    }

    fn parse_order_by_args(&mut self) -> Option<Vec<ast::OrderByArg>> {
        // check if the next token is BY
        if !self.expect_peek(Kind::Keyword(Keyword::BY)) {
            // TODO: error handling
            return None;
        }

        // get the columns to order by
        let mut order_by_args = vec![];
        while !self.peek_token_is(Kind::Keyword(Keyword::OFFSET))
            && !self.peek_token_is(Kind::SemiColon)
            && !self.peek_token_is(Kind::Eof)
        {
            self.next_token();

            if order_by_args.len() > 0 {
                // expect a COMMA before the next ORDER BY expression
                if !self.expect_current(Kind::Comma) {
                    self.current_msg_error("expected COMMA before next ORDER BY expression");
                    return None;
                }

                // consume the COMMA
                self.next_token();
            }

            if let Some(expression) = self.parse_expression(PRECEDENCE_LOWEST) {
                let mut is_asc = None;
                // check if we have an ASC or DESC keyword
                if self.peek_token_is(Kind::Keyword(Keyword::ASC)) {
                    is_asc = Some(true);
                    self.next_token();
                } else if self.peek_token_is(Kind::Keyword(Keyword::DESC)) {
                    is_asc = Some(false);
                    self.next_token();
                }

                // we have seen an order_by_arg
                order_by_args.push(ast::OrderByArg {
                    column: expression,
                    asc: is_asc,
                });
            } else {
                self.current_error(Kind::Ident);
                return None;
            }
        }

        match order_by_args.len() {
            0 => {
                self.peek_msg_error("expected ORDERED BY expression after ORDERED BY");
                None
            }
            _ => Some(order_by_args),
        }
    }

    fn parse_expression(&mut self, precedence: u8) -> Option<ast::Expression> {
        // check if the current token is an identifier
        // or if it is a prefix operator
        let mut left_expression = self.parse_prefix_expression();

        // parse the infix expression
        while precedence < self.peek_precedence() {
            // move to the next token
            self.next_token();

            match left_expression {
                Some(expression) => {
                    left_expression = self.parse_infix_expression(expression);
                }
                None => {
                    // TODO: error handling
                    return None;
                }
            }
        }

        left_expression
    }

    fn parse_prefix_expression(&mut self) -> Option<ast::Expression> {
        match self.current_token.kind() {
            Kind::Ident | Kind::Number | Kind::Asterisk => {
                // check if the next token is a dot
                if self.peek_token_is(Kind::Period) {
                    let mut idents = vec![self.current_token.clone()];
                    while self.peek_token_is(Kind::Period) {
                        // skip to the dot
                        self.next_token();

                        if !self.expect_peek(Kind::Ident) {
                            return None;
                        }

                        idents.push(self.current_token.clone());
                    }
                    Some(ast::Expression::CompoundLiteral(idents))
                } else {
                    Some(ast::Expression::Literal(self.current_token.clone()))
                }
            }
            Kind::Plus | Kind::Minus => {
                let operator = self.current_token.clone();
                let precedence = self.current_precedence();

                self.next_token();

                // parse the expression to the right of the operator
                if let Some(right_expression) = self.parse_expression(precedence) {
                    Some(ast::Expression::Unary {
                        operator,
                        right: Box::new(right_expression),
                    })
                } else {
                    // TODO: error handling
                    None
                }
            }
            Kind::Keyword(Keyword::BETWEEN) => {
                // skip the between
                self.next_token();

                if let Some(expression) = self.parse_expression(PRECEDENCE_LOWEST) {
                    return match expression {
                        ast::Expression::Binary {
                            left,
                            operator,
                            right,
                        } if matches!(operator.kind(), Kind::Keyword(Keyword::AND)) => {
                            Some(ast::Expression::Between {
                                not: false,
                                low: left,
                                high: right,
                            })
                        }
                        _ => {
                            self.peek_msg_error("expected binary expression after BETWEEN");
                            return None;
                        }
                    };
                }
                return None;
            }
            Kind::Keyword(Keyword::NOT) => {
                if self.peek_token_is(Kind::Keyword(Keyword::BETWEEN)) {
                    // skip the NOT
                    self.next_token();

                    if let Some(expression) = self.parse_expression(PRECEDENCE_LOWEST) {
                        return match expression {
                            ast::Expression::Between { high, low, .. } => {
                                Some(ast::Expression::Between {
                                    not: true,
                                    low,
                                    high,
                                })
                            }
                            _ => {
                                self.peek_msg_error("expected binary expression after NOT BETWEEN");
                                return None;
                            }
                        };
                    }

                    return None;
                } else {
                    let operator = self.current_token.clone();
                    let precedence = self.current_precedence();
                    self.next_token();
                    if let Some(expression) = self.parse_expression(precedence) {
                        Some(ast::Expression::Unary {
                            operator,
                            right: Box::new(expression),
                        })
                    } else {
                        None
                    }
                }
            }
            Kind::Keyword(Keyword::IS) => {
                if self.peek_token_is(Kind::Keyword(Keyword::NOT)) {
                    self.next_token();
                    // check if we are looking at is not null
                    if self.peek_token_is(Kind::Keyword(Keyword::NULL)) {
                        self.next_token();
                        if let Some(expression) = self.parse_expression(PRECEDENCE_LOWEST) {
                            Some(ast::Expression::IsNotNull(Box::new(expression)))
                        } else {
                            self.current_msg_error("expected expression after IS NOT NULL");
                            None
                        }
                    } else {
                        if let Some(expression) = self.parse_expression(PRECEDENCE_LOWEST) {
                            Some(ast::Expression::IsNotTrue(Box::new(expression)))
                        } else {
                            self.current_msg_error("expected expression after IS NOT");
                            None
                        }
                    }
                } else if self.peek_token_is(Kind::Keyword(Keyword::NULL)) {
                    if let Some(expression) = self.parse_expression(PRECEDENCE_LOWEST) {
                        Some(ast::Expression::IsNull(Box::new(expression)))
                    } else {
                        self.current_msg_error("expected expression after IS");
                        None
                    }
                } else {
                    if let Some(expression) = self.parse_expression(PRECEDENCE_LOWEST) {
                        Some(ast::Expression::IsTrue(Box::new(expression)))
                    } else {
                        self.current_msg_error("expected expression after IS");
                        None
                    }
                }
            }
            Kind::LeftParen => {
                if self.peek_token_is(Kind::Keyword(Keyword::SELECT)) {
                    // go to select keyword
                    self.next_token();

                    if let Some(statement) = self.parse_select_statement() {
                        let expression = Some(ast::Expression::Subquery(Box::new(statement)));

                        // check if we have a closing parenthesis
                        if !self.expect_peek(Kind::RightParen) {
                            return None;
                        }

                        return expression;
                    } else {
                        return None;
                    }
                } else {
                    self.parse_grouping()
                }
            }
            _ => None,
        }
    }

    fn parse_infix_expression(&mut self, left: ast::Expression) -> Option<ast::Expression> {
        match self.current_token.kind() {
            Kind::Plus
            | Kind::Minus
            | Kind::Asterisk
            | Kind::Divide
            | Kind::Equal
            | Kind::NotEqual
            | Kind::LessThan
            | Kind::LessThanEqual
            | Kind::GreaterThan
            | Kind::GreaterThanEqual
            | Kind::Keyword(Keyword::ALL)
            | Kind::Keyword(Keyword::AND)
            | Kind::Keyword(Keyword::ANY)
            | Kind::Keyword(Keyword::BETWEEN)
            | Kind::Keyword(Keyword::IN)
            | Kind::Keyword(Keyword::LIKE)
            | Kind::Keyword(Keyword::OR)
            | Kind::Keyword(Keyword::SOME) => {
                let operator = self.current_token.clone();
                let precedence = self.current_precedence();
                self.next_token();

                // parse the expression to the right of the operator
                if let Some(right_expression) = self.parse_expression(precedence) {
                    Some(ast::Expression::Binary {
                        left: Box::new(left),
                        operator,
                        right: Box::new(right_expression),
                    })
                } else {
                    // TODO: error handling
                    None
                }
            }
            _ => None,
        }
    }

    fn peek_precedence(&self) -> u8 {
        self.map_precedence(self.peek_token.kind())
    }

    fn current_precedence(&self) -> u8 {
        self.map_precedence(self.current_token.kind())
    }

    fn map_precedence(&self, token: Kind) -> u8 {
        match token {
            Kind::Tilde => PRECEDENCE_HIGHEST,
            Kind::Asterisk | Kind::Divide => PRECEDENCE_PRODUCT,
            Kind::Plus | Kind::Minus => PRECEDENCE_SUM,
            Kind::Equal
            | Kind::NotEqual
            | Kind::LessThan
            | Kind::LessThanEqual
            | Kind::GreaterThan
            | Kind::GreaterThanEqual => PRECEDENCE_COMPARISON,
            Kind::Keyword(Keyword::NOT) => PRECEDENCE_NOT,
            Kind::Keyword(Keyword::AND) => PRECEDENCE_AND,
            Kind::Keyword(Keyword::ALL)
            | Kind::Keyword(Keyword::ANY)
            | Kind::Keyword(Keyword::BETWEEN)
            | Kind::Keyword(Keyword::IN)
            | Kind::Keyword(Keyword::LIKE)
            | Kind::Keyword(Keyword::OR)
            | Kind::Keyword(Keyword::SOME) => PRECEDENCE_OTHER_LOGICALS,
            _ => PRECEDENCE_LOWEST,
        }
    }

    fn current_token_is(&self, token_kind: Kind) -> bool {
        self.current_token.kind() == token_kind
    }

    fn peek_token_is(&self, token_kind: Kind) -> bool {
        self.peek_token.kind() == token_kind
    }

    fn expect_peek(&mut self, token_kind: Kind) -> bool {
        if self.peek_token_is(token_kind) {
            self.next_token();
            true
        } else {
            self.peek_error(token_kind);
            false
        }
    }

    fn expect_peek_multi(&mut self, token_kinds: &[Kind], default_token: Kind) -> bool {
        for token_kind in token_kinds {
            if self.peek_token_is(*token_kind) {
                self.next_token();
                return true;
            }
        }

        self.peek_error(default_token);
        false
    }

    fn expect_current(&mut self, token_kind: Kind) -> bool {
        if self.current_token_is(token_kind) {
            true
        } else {
            self.current_error(token_kind);
            false
        }
    }

    #[allow(dead_code)]
    fn expect_current_multi(&mut self, token_kinds: &[Kind], default_token: Kind) -> bool {
        for token_kind in token_kinds {
            if self.current_token_is(*token_kind) {
                return true;
            }
        }
        self.current_error(default_token);
        false
    }

    fn make_string_error(&mut self, msg: &str, token: Token) -> String {
        let mut pointer_literal_len = match token.literal() {
            Literal::String(string) => string.len(),
            Literal::QuotedString { value, .. } => value.len() + 2,
            Literal::Number(num) => num.to_string().len(),
        };
        if pointer_literal_len == 0 {
            pointer_literal_len = 1;
        }
        let pointer_line = format!(
            "{}{}",
            " ".repeat(token.location().column),
            "^".repeat(pointer_literal_len)
        );

        format!(
            "Error at {}: {:?}, got {:?} instead\n{}\n{}",
            token.location(),
            msg,
            token.literal(),
            self.lexer.current_line_input(),
            pointer_line
        )
    }

    fn make_error(&mut self, token_kind: Kind, token: Token) -> String {
        let mut pointer_literal_len = match token.literal() {
            Literal::String(string) => string.len(),
            Literal::QuotedString { value, .. } => value.len() + 2,
            Literal::Number(num) => num.to_string().len(),
        };
        if pointer_literal_len == 0 {
            pointer_literal_len = 1;
        }
        let pointer_line = format!(
            "{}{}",
            " ".repeat(token.location().column),
            "^".repeat(pointer_literal_len)
        );

        format!(
            "Error at {}: expected token to be {:?}, got {:?} instead\n{}\n{}",
            token.location(),
            token_kind,
            token.literal(),
            self.lexer.current_line_input(),
            pointer_line
        )
    }

    #[allow(dead_code)]
    fn peek_msg_error(&mut self, msg: &str) {
        let msg = self.make_string_error(msg, self.peek_token.clone());

        self.errors.push(msg);
    }

    fn current_msg_error(&mut self, msg: &str) {
        let msg = self.make_string_error(msg, self.current_token.clone());
        self.errors.push(msg);
    }

    fn peek_error(&mut self, token_kind: Kind) {
        let msg = self.make_error(token_kind, self.peek_token.clone());

        self.errors.push(msg);
    }

    fn current_error(&mut self, token_kind: Kind) {
        let msg = self.make_error(token_kind, self.current_token.clone());
        self.errors.push(msg);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn select_statement_with_order_by() {
        let input = "SELECT name FROM users where lastname >= 'bob' order by dob asc, name desc offset 10 rows fetch next 5 rows only";
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let query = parser.parse();

        let expected_query = ast::Query {
            statements: vec![ast::Statement::Select(Box::new(ast::SelectStatement {
                distinct: false,
                top: None,
                columns: vec![ast::SelectItem::Unnamed(ast::Expression::Literal(
                    Token::wrap(Kind::Ident, Literal::new_string("name")),
                ))],
                into_table: None,
                table: Some(ast::TableArg {
                    table: ast::TableSource::Table {
                        name: ast::Expression::Literal(Token::wrap(
                            Kind::Ident,
                            Literal::new_string("users"),
                        )),
                        is_an: false,
                        alias: None,
                    },
                    joins: vec![],
                }),
                where_clause: Some(ast::Expression::Binary {
                    left: Box::new(ast::Expression::Literal(Token::wrap(
                        Kind::Ident,
                        Literal::new_string("lastname"),
                    ))),
                    operator: Token::wrap(Kind::GreaterThanEqual, Literal::new_string(">=")),
                    right: Box::new(ast::Expression::Literal(Token::wrap(
                        Kind::Ident,
                        Literal::new_string("'bob'"),
                    ))),
                }),
                order_by: vec![
                    ast::OrderByArg {
                        column: ast::Expression::Literal(Token::wrap(
                            Kind::Ident,
                            Literal::new_string("dob"),
                        )),
                        asc: Some(true),
                    },
                    ast::OrderByArg {
                        column: ast::Expression::Literal(Token::wrap(
                            Kind::Ident,
                            Literal::new_string("name"),
                        )),
                        asc: Some(false),
                    },
                ],
                group_by: vec![],
                having: None,
                offset: Some(ast::OffsetArg {
                    value: ast::Expression::Literal(Token::wrap(
                        Kind::Number,
                        Literal::Number(10.0),
                    )),
                    row: ast::RowOrRows::Rows,
                }),
                fetch: Some(ast::FetchArg {
                    value: ast::Expression::Literal(Token::wrap(
                        Kind::Number,
                        Literal::Number(5.0),
                    )),
                    first: ast::NextOrFirst::Next,
                    row: ast::RowOrRows::Rows,
                }),
            }))],
        };

        assert_eq!(expected_query, query);
    }

    #[test]
    fn select_statement_with_numbers() {
        let input = "SELECT distinct top 50 percent name, 1 FROM users where lastname >= 1;";
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let query = parser.parse();

        let expected_query = ast::Query {
            statements: vec![ast::Statement::Select(Box::new(ast::SelectStatement {
                distinct: true,
                top: Some(ast::TopArg {
                    with_ties: false,
                    percent: true,
                    quantity: ast::Expression::Literal(Token::wrap(
                        Kind::Number,
                        Literal::Number(50.0),
                    )),
                }),
                columns: vec![
                    ast::SelectItem::Unnamed(ast::Expression::Literal(Token::wrap(
                        Kind::Ident,
                        Literal::new_string("name"),
                    ))),
                    ast::SelectItem::Unnamed(ast::Expression::Literal(Token::wrap(
                        Kind::Number,
                        Literal::Number(1.0),
                    ))),
                ],
                into_table: None,
                table: Some(ast::TableArg {
                    table: ast::TableSource::Table {
                        name: ast::Expression::Literal(Token::wrap(
                            Kind::Ident,
                            Literal::new_string("users"),
                        )),
                        is_an: false,
                        alias: None,
                    },
                    joins: vec![],
                }),
                where_clause: Some(ast::Expression::Binary {
                    left: Box::new(ast::Expression::Literal(Token::wrap(
                        Kind::Ident,
                        Literal::new_string("lastname"),
                    ))),
                    operator: Token::wrap(Kind::GreaterThanEqual, Literal::new_string(">=")),
                    right: Box::new(ast::Expression::Literal(Token::wrap(
                        Kind::Number,
                        Literal::Number(1.0),
                    ))),
                }),
                group_by: vec![],
                having: None,
                order_by: vec![],
                offset: None,
                fetch: None,
            }))],
        };

        assert_eq!(expected_query, query);
    }

    #[test]
    fn basic_select_into_statement() {
        let input = "SELECT all *, name, firstname, lastname, [first], dob INTO NewUsers ON testFileGroup FROM users;";
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let query = parser.parse();

        let expected_query = ast::Query {
            statements: vec![ast::Statement::Select(Box::new(ast::SelectStatement {
                distinct: false,
                top: None,
                columns: vec![
                    ast::SelectItem::Wildcard,
                    ast::SelectItem::Unnamed(ast::Expression::Literal(Token::wrap(
                        Kind::Ident,
                        Literal::new_string("name"),
                    ))),
                    ast::SelectItem::Unnamed(ast::Expression::Literal(Token::wrap(
                        Kind::Ident,
                        Literal::new_string("firstname"),
                    ))),
                    ast::SelectItem::Unnamed(ast::Expression::Literal(Token::wrap(
                        Kind::Ident,
                        Literal::new_string("lastname"),
                    ))),
                    ast::SelectItem::Unnamed(ast::Expression::Literal(Token::wrap(
                        Kind::Ident,
                        Literal::new_string("[first]"),
                    ))),
                    ast::SelectItem::Unnamed(ast::Expression::Literal(Token::wrap(
                        Kind::Ident,
                        Literal::new_string("dob"),
                    ))),
                ],
                into_table: Some(ast::IntoArg {
                    table: ast::Expression::Literal(Token::wrap(
                        Kind::Ident,
                        Literal::new_string("NewUsers"),
                    )),
                    file_group: Some(ast::Expression::Literal(Token::wrap(
                        Kind::Ident,
                        Literal::new_string("testFileGroup"),
                    ))),
                }),
                table: Some(ast::TableArg {
                    table: ast::TableSource::Table {
                        name: ast::Expression::Literal(Token::wrap(
                            Kind::Ident,
                            Literal::new_string("users"),
                        )),
                        is_an: false,
                        alias: None,
                    },
                    joins: vec![],
                }),
                where_clause: None,
                group_by: vec![],
                having: None,
                order_by: vec![],
                offset: None,
                fetch: None,
            }))],
        };

        assert_eq!(expected_query, query);
    }

    #[test]
    fn basic_select_statement_with_joins() {
        let input = "SELECT *,  [firstName], lastname,  dob FROM users u inner join potatoes as pt on  u.potato = pt.name left join biodata on biodata.height = u.height;";
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let query = parser.parse();

        let expected_query = ast::Query {
            statements: vec![ast::Statement::Select(Box::new(ast::SelectStatement {
                distinct: false,
                top: None,
                columns: vec![
                    ast::SelectItem::Wildcard,
                    ast::SelectItem::Unnamed(ast::Expression::Literal(Token::wrap(
                        Kind::Ident,
                        Literal::new_quoted("firstName", '['),
                    ))),
                    ast::SelectItem::Unnamed(ast::Expression::Literal(Token::wrap(
                        Kind::Ident,
                        Literal::new_string("lastname"),
                    ))),
                    ast::SelectItem::Unnamed(ast::Expression::Literal(Token::wrap(
                        Kind::Ident,
                        Literal::new_string("dob"),
                    ))),
                ],
                into_table: None,
                table: Some(ast::TableArg {
                    table: ast::TableSource::Table {
                        name: ast::Expression::Literal(Token::wrap(
                            Kind::Ident,
                            Literal::new_string("users"),
                        )),
                        is_an: false,
                        alias: Some("u".to_string()),
                    },
                    joins: vec![
                        ast::Join {
                            join_type: ast::JoinType::Inner,
                            table: ast::TableSource::Table {
                                name: ast::Expression::Literal(Token::wrap(
                                    Kind::Ident,
                                    Literal::new_string("potatoes"),
                                )),
                                is_an: true,
                                alias: Some("pt".to_string()),
                            },
                            condition: Some(ast::Expression::Binary {
                                left: Box::new(ast::Expression::CompoundLiteral(vec![
                                    Token::wrap(Kind::Ident, Literal::new_string("u")),
                                    Token::wrap(Kind::Ident, Literal::new_string("potato")),
                                ])),
                                operator: Token::wrap(Kind::Equal, Literal::new_string("=")),
                                right: Box::new(ast::Expression::CompoundLiteral(vec![
                                    Token::wrap(Kind::Ident, Literal::new_string("pt")),
                                    Token::wrap(Kind::Ident, Literal::new_string("name")),
                                ])),
                            }),
                        },
                        ast::Join {
                            join_type: ast::JoinType::Left,
                            table: ast::TableSource::Table {
                                name: ast::Expression::Literal(Token::wrap(
                                    Kind::Ident,
                                    Literal::new_string("biodata"),
                                )),
                                is_an: false,
                                alias: None,
                            },
                            condition: Some(ast::Expression::Binary {
                                left: Box::new(ast::Expression::CompoundLiteral(vec![
                                    Token::wrap(Kind::Ident, Literal::new_string("biodata")),
                                    Token::wrap(Kind::Ident, Literal::new_string("height")),
                                ])),
                                operator: Token::wrap(Kind::Equal, Literal::new_string("=")),
                                right: Box::new(ast::Expression::CompoundLiteral(vec![
                                    Token::wrap(Kind::Ident, Literal::new_string("u")),
                                    Token::wrap(Kind::Ident, Literal::new_string("height")),
                                ])),
                            }),
                        },
                    ],
                }),
                where_clause: None,
                group_by: vec![],
                having: None,
                order_by: vec![],
                offset: None,
                fetch: None,
            }))],
        };

        assert_eq!(expected_query, query);
    }

    #[test]
    fn basic_select_statement() {
        let input = "SELECT all *, name, firstname, lastname, [first], dob FROM users;";
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let query = parser.parse();

        let expected_query = ast::Query {
            statements: vec![ast::Statement::Select(Box::new(ast::SelectStatement {
                distinct: false,
                top: None,
                columns: vec![
                    ast::SelectItem::Wildcard,
                    ast::SelectItem::Unnamed(ast::Expression::Literal(Token::wrap(
                        Kind::Ident,
                        Literal::new_string("name"),
                    ))),
                    ast::SelectItem::Unnamed(ast::Expression::Literal(Token::wrap(
                        Kind::Ident,
                        Literal::new_string("firstname"),
                    ))),
                    ast::SelectItem::Unnamed(ast::Expression::Literal(Token::wrap(
                        Kind::Ident,
                        Literal::new_string("lastname"),
                    ))),
                    ast::SelectItem::Unnamed(ast::Expression::Literal(Token::wrap(
                        Kind::Ident,
                        Literal::new_quoted("first", '['),
                    ))),
                    ast::SelectItem::Unnamed(ast::Expression::Literal(Token::wrap(
                        Kind::Ident,
                        Literal::new_string("dob"),
                    ))),
                ],
                into_table: None,
                table: Some(ast::TableArg {
                    table: ast::TableSource::Table {
                        name: ast::Expression::Literal(Token::wrap(
                            Kind::Ident,
                            Literal::new_string("users"),
                        )),
                        is_an: false,
                        alias: None,
                    },
                    joins: vec![],
                }),
                where_clause: None,
                group_by: vec![],
                having: None,
                order_by: vec![],
                offset: None,
                fetch: None,
            }))],
        };

        assert_eq!(expected_query, query);
    }

    #[test]
    fn select_statement_with_subquery() {
        let input = "SELECT name, (Select * from MarketData) FROM users where lastname = 'blah' AND firstname > 'hello';";
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let query = parser.parse();

        let expected_query = ast::Query {
            statements: vec![ast::Statement::Select(Box::new(ast::SelectStatement {
                distinct: false,
                top: None,
                columns: vec![
                    ast::SelectItem::Unnamed(ast::Expression::Literal(Token::wrap(
                        Kind::Ident,
                        Literal::new_string("name"),
                    ))),
                    ast::SelectItem::Unnamed(ast::Expression::Subquery(Box::new(
                        ast::Statement::Select(Box::new(ast::SelectStatement {
                            distinct: false,
                            top: None,
                            columns: vec![ast::SelectItem::Wildcard],
                            into_table: None,
                            table: Some(ast::TableArg {
                                table: ast::TableSource::Table {
                                    name: ast::Expression::Literal(Token::wrap(
                                        Kind::Ident,
                                        Literal::new_string("MarketData"),
                                    )),
                                    is_an: false,
                                    alias: None,
                                },
                                joins: vec![],
                            }),
                            where_clause: None,
                            group_by: vec![],
                            having: None,
                            order_by: vec![],
                            offset: None,
                            fetch: None,
                        })),
                    ))),
                ],
                into_table: None,
                table: Some(ast::TableArg {
                    table: ast::TableSource::Table {
                        name: ast::Expression::Literal(Token::wrap(
                            Kind::Ident,
                            Literal::new_string("users"),
                        )),
                        is_an: false,
                        alias: None,
                    },
                    joins: vec![],
                }),
                where_clause: Some(ast::Expression::Binary {
                    left: Box::new(ast::Expression::Binary {
                        left: Box::new(ast::Expression::Literal(Token::wrap(
                            Kind::Ident,
                            Literal::new_string("lastname"),
                        ))),
                        operator: Token::wrap(Kind::Equal, Literal::new_string("=")),
                        right: Box::new(ast::Expression::Literal(Token::wrap(
                            Kind::Ident,
                            Literal::new_quoted("blah", '\''),
                        ))),
                    }),
                    operator: Token::wrap(Kind::Keyword(Keyword::AND), Literal::new_string("AND")),
                    right: Box::new(ast::Expression::Binary {
                        left: Box::new(ast::Expression::Literal(Token::wrap(
                            Kind::Ident,
                            Literal::new_string("firstname"),
                        ))),
                        operator: Token::wrap(Kind::GreaterThan, Literal::new_string(">")),
                        right: Box::new(ast::Expression::Literal(Token::wrap(
                            Kind::Ident,
                            Literal::new_quoted("hello", '\''),
                        ))),
                    }),
                }),
                group_by: vec![],
                having: None,
                order_by: vec![],
                offset: None,
                fetch: None,
            }))],
        };

        assert_eq!(expected_query, query);
    }

    #[test]
    fn select_statement_with_where_clause() {
        let input = "SELECT name FROM users where lastname = 'blah' AND firstname > 'hello';";
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let query = parser.parse();

        let expected_query = ast::Query {
            statements: vec![ast::Statement::Select(Box::new(ast::SelectStatement {
                distinct: false,
                top: None,
                columns: vec![ast::SelectItem::Unnamed(ast::Expression::Literal(
                    Token::wrap(Kind::Ident, Literal::new_string("name")),
                ))],
                into_table: None,
                table: Some(ast::TableArg {
                    table: ast::TableSource::Table {
                        name: ast::Expression::Literal(Token::wrap(
                            Kind::Ident,
                            Literal::new_string("users"),
                        )),
                        is_an: false,
                        alias: None,
                    },
                    joins: vec![],
                }),
                where_clause: Some(ast::Expression::Binary {
                    left: Box::new(ast::Expression::Binary {
                        left: Box::new(ast::Expression::Literal(Token::wrap(
                            Kind::Ident,
                            Literal::new_string("lastname"),
                        ))),
                        operator: Token::wrap(Kind::Equal, Literal::new_string("=")),
                        right: Box::new(ast::Expression::Literal(Token::wrap(
                            Kind::Ident,
                            Literal::new_quoted("blah", '\''),
                        ))),
                    }),
                    operator: Token::wrap(Kind::Keyword(Keyword::AND), Literal::new_string("AND")),
                    right: Box::new(ast::Expression::Binary {
                        left: Box::new(ast::Expression::Literal(Token::wrap(
                            Kind::Ident,
                            Literal::new_string("firstname"),
                        ))),
                        operator: Token::wrap(Kind::GreaterThan, Literal::new_string(">")),
                        right: Box::new(ast::Expression::Literal(Token::wrap(
                            Kind::Ident,
                            Literal::new_quoted("hello", '\''),
                        ))),
                    }),
                }),
                group_by: vec![],
                having: None,
                order_by: vec![],
                offset: None,
                fetch: None,
            }))],
        };

        assert_eq!(expected_query, query);
    }
}
