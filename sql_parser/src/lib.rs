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
            match self.parse_statement() {
                Ok(statement) => query.statements.push(statement),
                Err(str) => self.errors.push(str),
            }

            self.next_token();
        }

        query
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn parse_statement(&mut self) -> Result<ast::Statement, String> {
        match self.current_token.kind() {
            Kind::Keyword(keyword) => match keyword {
                Keyword::SELECT => return self.parse_select_statement(),
                Keyword::WITH => return self.parse_cte_statement(),
                _ => return Err(format!("Unexpected keyword: {:#?}", keyword)),
            },
            _ => return Err(format!("Unexpected token: {:#?}", self.current_token)),
        }
    }

    fn parse_cte_statement(&mut self) -> Result<ast::Statement, String> {
        let mut ctes = vec![];

        while !self.peek_token_is(Kind::Keyword(Keyword::SELECT)) && !self.peek_token_is(Kind::Eof)
        {
            if ctes.len() > 0 {
                // expect a COMMA before the next CTE expression
                self.expect_kind(Kind::Comma, &self.peek_token)?;
                // consume the COMMA
                self.next_token();
            }

            // check for the expression name
            self.expect_kind(Kind::Ident, &self.peek_token)?;
            self.next_token();

            let mut select_items = None;
            let cte_name = self.current_token.clone();

            if self.peek_token_is(Kind::LeftParen) {
                // go to the left paren
                self.next_token();

                // parse the column list
                select_items = Some(self.parse_expression_list()?);

                // go to the right paren
                self.next_token();
            }

            self.expect_kind(Kind::Keyword(Keyword::AS), &self.peek_token)?;
            self.next_token();
            self.expect_kind(Kind::LeftParen, &self.peek_token)?;
            self.next_token();

            // check for the select keyword
            self.expect_kind(Kind::Keyword(Keyword::SELECT), &self.peek_token)?;
            self.next_token();
            let select_statement = self.parse_select_statement()?;

            self.expect_kind(Kind::RightParen, &self.peek_token)?;
            self.next_token();

            let cte = ast::CommonTableExpression {
                name: ast::Expression::Literal(cte_name),
                columns: if let Some(select_items) = select_items {
                    select_items
                } else {
                    vec![]
                },
                query: select_statement,
            };
            ctes.push(cte);
        }

        // check for the select keyword
        self.expect_kind(Kind::Keyword(Keyword::SELECT), &self.peek_token)?;
        self.next_token();
        let query = self.parse_select_statement()?;

        Ok(ast::Statement::CTE {
            ctes,
            statement: Box::new(query),
        })
    }

    fn parse_select_statement(&mut self) -> Result<ast::Statement, String> {
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

            let expression = self.parse_expression(PRECEDENCE_LOWEST)?;
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

                self.expect_kind(Kind::Keyword(Keyword::TIES), &self.peek_token)?;
                self.next_token();
                is_with_ties = true;
            }

            statement.top = Some(ast::TopArg {
                with_ties: is_with_ties,
                percent: is_percent,
                quantity: expression,
            });
        }

        // check for columns
        statement.columns = self.parse_select_items()?;

        // check if we have a INTO keyword
        if self.peek_token_is(Kind::Keyword(Keyword::INTO)) {
            // go to the INTO keyword
            self.next_token();

            // check if the next token is an identifier
            self.expect_kind(Kind::Ident, &self.peek_token)?;
            self.next_token();

            let into_table = ast::Expression::Literal(self.current_token.clone());
            let mut file_group: Option<ast::Expression> = None;

            // check if we ON keyword
            if self.peek_token_is(Kind::Keyword(Keyword::ON)) {
                // skip the ON keyword
                self.next_token();

                // check if the next token is an identifier
                self.expect_kind(Kind::Ident, &self.peek_token)?;
                self.next_token();

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
            self.expect_kind(Kind::Keyword(Keyword::FROM), &self.peek_token)?;
            self.next_token();

            statement.table = Some(self.parse_table_arg()?);
        } else {
            // check if we have a FROM keyword
            if self.peek_token_is(Kind::Keyword(Keyword::FROM)) {
                // go to the FROM keyword
                self.next_token();

                statement.table = Some(self.parse_table_arg()?);
            }
        }

        // check if we have any where clause
        if self.peek_token_is(Kind::Keyword(Keyword::WHERE)) {
            // skip the WHERE keyword
            self.next_token();
            self.next_token();

            let expression = self.parse_expression(PRECEDENCE_LOWEST)?;
            if !matches!(
                expression,
                ast::Expression::Binary { .. }
                    | ast::Expression::Between { .. }
                    | ast::Expression::Unary { .. }
                    | ast::Expression::Any { .. }
                    | ast::Expression::All { .. }
                    | ast::Expression::Some { .. }
                    | ast::Expression::InList { .. }
            ) {
                self.expected(
                    "expected expression after WHERE keyword",
                    &self.current_token,
                )?;
            }

            statement.where_clause = Some(expression);
        }

        // check if we have any GROUP BY clause
        if self.peek_token_is(Kind::Keyword(Keyword::GROUP)) {
            // skip the GROUP keyword
            self.next_token();

            statement.group_by = self.parse_group_by_args()?;
        }

        // check if we have any having clause
        if self.peek_token_is(Kind::Keyword(Keyword::HAVING)) {
            // skip the having keyword
            self.next_token();
            self.next_token();

            let expression = self.parse_expression(PRECEDENCE_LOWEST)?;
            if !matches!(expression, ast::Expression::Binary { .. }) {
                self.expected("expected expression after HAVING keyword", &self.peek_token)?;
            }

            statement.having = Some(expression);
        }

        // order by expression
        if self.peek_token_is(Kind::Keyword(Keyword::ORDER)) {
            // go to order keyword
            self.next_token();

            statement.order_by = self.parse_order_by_args()?;

            if self.peek_token_is(Kind::Keyword(Keyword::OFFSET)) {
                // go to offset keyword
                self.next_token();

                statement.offset = Some(self.parse_offset()?);

                // check if we have a FETCH keyword
                if self.peek_token_is(Kind::Keyword(Keyword::FETCH)) {
                    // go to fetch keyword
                    self.next_token();

                    statement.fetch = Some(self.parse_fetch()?);
                    self.next_token();
                }
            }
        }

        Ok(ast::Statement::Select(Box::new(statement)))
    }

    fn parse_table_source(&mut self) -> Result<ast::TableSource, String> {
        // check if the next token is an identifier
        self.expect_kind(Kind::Ident, &self.peek_token)?;
        self.next_token();

        let table = self.parse_expression(PRECEDENCE_LOWEST)?;
        // if !matches!(
        //     table,
        //     ast::Expression::Literal(_) | ast::Expression::CompoundLiteral(_)
        // ) {
        //     self.expected("expected table name", &self.current_token)?;
        // }

        // // if this true then we have a function call
        // let mut function_params = None;
        // if self.peek_token_is(Kind::LeftParen) {
        //     self.next_token();
        //     let params = self.parse_expression(PRECEDENCE_LOWEST)?;
        //     if !matches!(params, ast::Expression::ExpressionList(_)) {
        //         self.expected("expected function parameters", &self.current_token)?;
        //     }
        //     function_params = Some(params);
        // }

        // check if we have an alias
        let is_as;
        let mut alias = None;
        if self.peek_token_is(Kind::Keyword(Keyword::AS)) {
            // skip the AS keyword
            self.next_token();

            // check if the next token is an identifier
            self.expect_kind(Kind::Ident, &self.peek_token)?;
            is_as = true;
        } else {
            is_as = false;
        }

        if self.peek_token_is(Kind::Ident) {
            self.next_token();
            alias = Some(self.current_token.to_string());
        }

        if matches!(
            table,
            ast::Expression::Literal(_) | ast::Expression::CompoundLiteral(_)
        ) {
            return Ok(ast::TableSource::Table {
                name: table,
                is_as,
                alias,
            });
        } else if matches!(table, ast::Expression::Function { .. }) {
            return Ok(ast::TableSource::TableValuedFunction {
                function: table,
                is_as,
                alias,
            });
        } else {
            self.expected("expected table name or function", &self.current_token)?;
            unreachable!();
        }
    }

    fn parse_table_arg(&mut self) -> Result<ast::TableArg, String> {
        let table_source = self.parse_table_source()?;
        // check if we have joins
        let mut joins = vec![];
        loop {
            let join = if self.peek_token_is(Kind::Keyword(Keyword::INNER)) {
                // skip the INNER keyword
                self.next_token();

                // skip the JOIN keyword
                self.expect_kind(Kind::Keyword(Keyword::JOIN), &self.peek_token)?;
                self.next_token();

                // parse the table source
                let table_source = self.parse_table_source()?;
                // check if we have an ON keyword
                self.expect_kind(Kind::Keyword(Keyword::ON), &self.peek_token)?;
                self.next_token();

                // skip the ON keyword
                self.next_token();

                // parse the search condition
                let expression = self.parse_expression(PRECEDENCE_LOWEST)?;

                ast::Join {
                    join_type: ast::JoinType::Inner,
                    table: table_source,
                    condition: Some(expression),
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
                self.expect_kind(Kind::Keyword(Keyword::JOIN), &self.peek_token)?;
                self.next_token();

                // get the table source
                let table_source = self.parse_table_source()?;

                // check if we have an ON keyword
                self.expect_kind(Kind::Keyword(Keyword::ON), &self.peek_token)?;
                self.next_token();

                // skip the ON keyword
                self.next_token();

                // parse the search condition
                let expression = self.parse_expression(PRECEDENCE_LOWEST)?;

                ast::Join {
                    join_type,
                    table: table_source,
                    condition: Some(expression),
                }
            } else {
                break;
            };

            joins.push(join);
        }

        Ok(ast::TableArg {
            table: table_source,
            joins,
        })
    }

    fn parse_select_items(&mut self) -> Result<Vec<ast::SelectItem>, String> {
        // check if the next token is an identifier
        // return an error if the next token is not an identifier or number
        self.expect_many_kind(
            &[Kind::Ident, Kind::Number, Kind::Asterisk, Kind::LeftParen],
            &self.peek_token,
        )?;

        // get the columns to select
        let mut columns: Vec<ast::SelectItem> = vec![];
        while !self.peek_token_is(Kind::Keyword(Keyword::FROM))
            && !self.peek_token_is(Kind::Keyword(Keyword::INTO))
            && !self.peek_token_is(Kind::Eof)
        {
            self.next_token();

            if columns.len() > 0 {
                // expect a COMMA before the next GROUP BY expression
                self.expect_kind(Kind::Comma, &self.current_token)?;

                // consume the COMMA
                self.next_token();
            }

            // parse the expression
            let expression = self.parse_expression(PRECEDENCE_LOWEST)?;

            // check if it is a compounded identifier
            // check if the next token is the keyword AS
            let mut is_as = false;
            if self.peek_token_is(Kind::Keyword(Keyword::AS)) {
                // skip the AS keyword
                self.next_token();
                is_as = true;

                if !self.peek_token_is(Kind::Ident) {
                    self.expected(
                        "token to either be a quoted string or identifier after AS keyword",
                        &self.peek_token,
                    )?;
                }
            }

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
        }

        if columns.len() == 0 {
            self.expected("SELECT items in SELECT expression", &self.peek_token)?;
        }

        Ok(columns)
    }

    fn parse_grouping(&mut self) -> Result<ast::Expression, String> {
        self.expect_kind(Kind::LeftParen, &self.current_token)?;
        self.next_token();

        let expression = self.parse_expression(PRECEDENCE_LOWEST)?;

        let grouping = ast::Expression::Grouping(Box::new(expression));

        self.expect_kind(Kind::RightParen, &self.peek_token)?;
        self.next_token();

        Ok(grouping)
    }

    fn parse_offset(&mut self) -> Result<ast::OffsetArg, String> {
        // skip the OFFSET keyword
        self.next_token();

        // get the offset value
        let offset = self.parse_expression(PRECEDENCE_LOWEST)?;

        self.expect_many_kind(
            &[Kind::Keyword(Keyword::ROW), Kind::Keyword(Keyword::ROWS)],
            &self.peek_token,
        )?;
        self.next_token();

        let row = match self.current_token.kind() {
            Kind::Keyword(Keyword::ROW) => ast::RowOrRows::Row,
            Kind::Keyword(Keyword::ROWS) => ast::RowOrRows::Rows,
            _ => {
                unreachable!();
            }
        };

        // consume the ROW or ROWS
        Ok(ast::OffsetArg { value: offset, row })
    }

    fn parse_fetch(&mut self) -> Result<ast::FetchArg, String> {
        // check if the next token is FIRST or NEXT
        self.expect_many_kind(
            &[Kind::Keyword(Keyword::NEXT), Kind::Keyword(Keyword::FIRST)],
            &self.peek_token,
        )?;
        self.next_token();

        let first = match self.current_token.kind() {
            Kind::Keyword(Keyword::FIRST) => ast::NextOrFirst::First,
            Kind::Keyword(Keyword::NEXT) => ast::NextOrFirst::Next,
            _ => {
                unreachable!();
            }
        };

        // consume the FIRST or NEXT
        self.next_token();

        // get the fetch value
        let fetch = self.parse_expression(PRECEDENCE_LOWEST)?;

        // check if the next token is ROW or ROWS
        self.expect_many_kind(
            &[Kind::Keyword(Keyword::ROW), Kind::Keyword(Keyword::ROWS)],
            &self.peek_token,
        )?;
        self.next_token();

        let row = match self.current_token.kind() {
            Kind::Keyword(Keyword::ROW) => ast::RowOrRows::Row,
            Kind::Keyword(Keyword::ROWS) => ast::RowOrRows::Rows,
            _ => {
                unreachable!();
            }
        };

        // check if we have the keyword ONLY
        self.expect_kind(Kind::Keyword(Keyword::ONLY), &self.peek_token)?;
        self.next_token();

        // consume the ROW or ROWS
        self.next_token();

        Ok(ast::FetchArg {
            value: fetch,
            row,
            first,
        })
    }

    fn parse_partition_by_args(&mut self) -> Result<Vec<ast::Expression>, String> {
        // check if the next token is BY
        self.expect_kind(Kind::Keyword(Keyword::BY), &self.peek_token)?;
        self.next_token();

        // get the columns to order by
        let mut args = vec![];
        while !self.peek_token_is(Kind::Keyword(Keyword::ORDER))
            && !self.peek_token_is(Kind::Keyword(Keyword::ROWS))
            && !self.peek_token_is(Kind::Keyword(Keyword::RANGE))
            && !self.peek_token_is(Kind::RightParen)
            && !self.peek_token_is(Kind::Eof)
        {
            self.next_token();

            if args.len() > 0 {
                // expect a COMMA before the next GROUP BY expression
                self.expect_kind(Kind::Comma, &self.current_token)?;

                // consume the COMMA
                self.next_token();
            }

            let expression = self.parse_expression(PRECEDENCE_LOWEST)?;
            args.push(expression);
        }

        if args.len() == 0 {
            self.expected(
                "PARTITION BY items in PARTITION BY expression",
                &self.peek_token,
            )?;
        }

        Ok(args)
    }

    fn parse_group_by_args(&mut self) -> Result<Vec<ast::Expression>, String> {
        // check if the next token is BY
        self.expect_kind(Kind::Keyword(Keyword::BY), &self.peek_token)?;
        self.next_token();

        // get the columns to order by
        let mut args = vec![];
        while !self.peek_token_is(Kind::Keyword(Keyword::HAVING))
            && !self.peek_token_is(Kind::SemiColon)
            && !self.peek_token_is(Kind::Eof)
        {
            self.next_token();

            if args.len() > 0 {
                // expect a COMMA before the next GROUP BY expression
                self.expect_kind(Kind::Comma, &self.current_token)?;

                // consume the COMMA
                self.next_token();
            }

            let expression = self.parse_expression(PRECEDENCE_LOWEST)?;
            args.push(expression);
        }

        if args.len() == 0 {
            self.expected("GROUP BY items in GROUP BY expression", &self.peek_token)?;
        }

        Ok(args)
    }

    fn parse_order_by_args(&mut self) -> Result<Vec<ast::OrderByArg>, String> {
        // check if the next token is BY
        self.expect_kind(Kind::Keyword(Keyword::BY), &self.peek_token)?;
        self.next_token();

        // get the columns to order by
        let mut order_by_args = vec![];
        while !self.peek_token_is(Kind::Keyword(Keyword::OFFSET))
            && !self.peek_token_is(Kind::RightParen)
            && !self.peek_token_is(Kind::Keyword(Keyword::ROWS))
            && !self.peek_token_is(Kind::SemiColon)
            && !self.peek_token_is(Kind::Eof)
        {
            self.next_token();

            if order_by_args.len() > 0 {
                // expect a COMMA before the next ORDER BY expression
                self.expect_kind(Kind::Comma, &self.current_token)?;

                // consume the COMMA
                self.next_token();
            }

            let expression = self.parse_expression(PRECEDENCE_LOWEST)?;
            if !matches!(
                expression,
                ast::Expression::Literal(_) | ast::Expression::CompoundLiteral(_)
            ) {
                self.expected("column in ORDER BY expression", &self.peek_token)?;
            }

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
        }

        if order_by_args.len() == 0 {
            self.expected("ORDER BY items in ORDER BY expression", &self.peek_token)?;
        }

        Ok(order_by_args)
    }

    fn parse_expression_list(&mut self) -> Result<Vec<ast::Expression>, String> {
        // make sure the list has the same type of expressions
        // either all idents, or all numbers
        let mut expressions = vec![];
        while !self.peek_token_is(Kind::RightParen) {
            if !self.peek_token_is(Kind::Number)
                && !self.peek_token_is(Kind::Ident)
                && !self.peek_token_is(Kind::Comma)
            {
                self.expected("STRING LITERAL or NUMBER or Identifier", &self.peek_token)?;
            }
            self.next_token();

            if expressions.len() > 0 {
                // expect a COMMA before the next expression
                self.expect_kind(Kind::Comma, &self.current_token)?;

                // consume the COMMA
                if !self.peek_token_is(Kind::Number)
                    && !self.peek_token_is(Kind::Ident)
                    && !self.peek_token_is(Kind::Comma)
                {
                    self.expected("STRING LITERAL or NUMBER or Identifier", &self.peek_token)?;
                }
                self.next_token();
            }

            expressions.push(ast::Expression::Literal(self.current_token.clone()));
        }

        Ok(expressions)
    }

    fn parse_in_expression(&mut self, left: ast::Expression) -> Result<ast::Expression, String> {
        let mut is_not = false;
        if self.current_token_is(Kind::Keyword(Keyword::NOT)) {
            is_not = true;
            self.next_token();
        }
        // check if we have an IN keyword
        self.expect_kind(Kind::Keyword(Keyword::IN), &self.current_token)?;

        // parse the expression list
        self.expect_kind(Kind::LeftParen, &self.peek_token)?;
        self.next_token();

        // skip the left parenthesis
        let expression_list = self.parse_expression_list()?;
        self.next_token();

        Ok(ast::Expression::InList {
            expression: Box::new(left),
            list: expression_list,
            not: is_not,
        })
    }

    fn parse_over_clause(&mut self) -> Result<ast::OverClause, String> {
        self.expect_kind(Kind::Keyword(Keyword::OVER), &self.peek_token)?;
        self.next_token();
        self.expect_kind(Kind::LeftParen, &self.peek_token)?;
        self.next_token();

        let mut partition_by = vec![];
        if self.peek_token_is(Kind::Keyword(Keyword::PARTITION)) {
            self.next_token();
            partition_by = self.parse_partition_by_args()?;
        }

        let mut order_by = vec![];
        if self.peek_token_is(Kind::Keyword(Keyword::ORDER)) {
            self.next_token();
            order_by = self.parse_order_by_args()?;
        }

        let mut window_frame = None;
        if self.peek_token_is(Kind::Keyword(Keyword::ROWS))
            || self.peek_token_is(Kind::Keyword(Keyword::RANGE))
        {
            self.next_token();
            let mut rows_or_range = ast::RowsOrRange::Rows;
            if self.current_token_is(Kind::Keyword(Keyword::RANGE)) {
                rows_or_range = ast::RowsOrRange::Range;
            }

            // parse window frame
            let mut window_frame_start = None;
            let mut window_frame_end = None;
            if self.peek_token_is(Kind::Keyword(Keyword::BETWEEN)) {
                self.next_token();
                // check for unbounded preceding
                if self.peek_token_is(Kind::Keyword(Keyword::UNBOUNDED)) {
                    self.next_token();
                    self.expect_kind(Kind::Keyword(Keyword::PRECEDING), &self.peek_token)?;
                    self.next_token();
                    window_frame_start = Some(ast::WindowFrameBound::UnboundedPreceding);
                }
                // check for current row
                else if self.peek_token_is(Kind::Keyword(Keyword::CURRENT)) {
                    self.next_token();
                    self.expect_kind(Kind::Keyword(Keyword::ROW), &self.peek_token)?;
                    self.next_token();
                    window_frame_start = Some(ast::WindowFrameBound::CurrentRow);
                }
                // check for number
                else if self.peek_token_is(Kind::Number) {
                    self.next_token();
                    let number = self.current_token.clone();
                    window_frame_start = Some(ast::WindowFrameBound::Preceding(
                        ast::Expression::Literal(number),
                    ));
                } else {
                    self.expected(
                        "UNBOUNDED PRECEDING or CURRENT ROW or NUMBER",
                        &self.peek_token,
                    )?;
                }

                // check for AND
                self.expect_kind(Kind::Keyword(Keyword::AND), &self.peek_token)?;
                self.next_token();

                // check for unbounded following
                if self.peek_token_is(Kind::Keyword(Keyword::UNBOUNDED)) {
                    self.next_token();
                    self.expect_kind(Kind::Keyword(Keyword::FOLLOWING), &self.peek_token)?;
                    self.next_token();
                    window_frame_end = Some(ast::WindowFrameBound::UnboundedFollowing);
                }
                // check for current row
                else if self.peek_token_is(Kind::Keyword(Keyword::CURRENT)) {
                    self.next_token();
                    self.expect_kind(Kind::Keyword(Keyword::ROW), &self.peek_token)?;
                    self.next_token();
                    window_frame_end = Some(ast::WindowFrameBound::CurrentRow);
                }
                // check for number
                else if self.peek_token_is(Kind::Number) {
                    self.next_token();
                    let number = self.current_token.clone();
                    window_frame_end = Some(ast::WindowFrameBound::Preceding(
                        ast::Expression::Literal(number),
                    ));
                } else {
                    self.expected(
                        "UNBOUNDED FOLLOWING or CURRENT ROW or NUMBER",
                        &self.peek_token,
                    )?;
                }
            } else {
                // check for unbounded preceding
                if self.peek_token_is(Kind::Keyword(Keyword::UNBOUNDED)) {
                    self.next_token();
                    self.expect_kind(Kind::Keyword(Keyword::PRECEDING), &self.peek_token)?;
                    self.next_token();
                    window_frame_start = Some(ast::WindowFrameBound::UnboundedPreceding);
                }
                // check for current row
                else if self.peek_token_is(Kind::Keyword(Keyword::CURRENT)) {
                    self.next_token();
                    self.expect_kind(Kind::Keyword(Keyword::ROW), &self.peek_token)?;
                    self.next_token();
                    window_frame_start = Some(ast::WindowFrameBound::CurrentRow);
                }
                // check for number
                else if self.peek_token_is(Kind::Number) {
                    self.next_token();
                    let number = self.current_token.clone();
                    window_frame_start = Some(ast::WindowFrameBound::Preceding(
                        ast::Expression::Literal(number),
                    ));
                } else {
                    self.expected(
                        "UNBOUNDED PRECEDING or CURRENT ROW or NUMBER",
                        &self.peek_token,
                    )?;
                }
            }

            if let Some(window_frame_start_unwrapped) = window_frame_start {
                window_frame = Some(ast::WindowFrame {
                    rows_or_range,
                    start: window_frame_start_unwrapped,
                    end: window_frame_end,
                });
            } else {
                self.expected("window frame start", &self.peek_token)?;
            }
        }

        self.expect_kind(Kind::RightParen, &self.peek_token)?;
        self.next_token();

        let over = ast::OverClause {
            partition_by,
            order_by,
            window_frame,
        };

        Ok(over)
    }

    fn parse_function(
        &mut self,
        function_name: ast::Expression,
    ) -> Result<ast::Expression, String> {
        // check if the current token is a left parenthesis
        self.expect_kind(Kind::LeftParen, &self.peek_token)?;
        // skip the left parenthesis
        self.next_token();

        // parse the expression list
        let expression_list = self.parse_expression_list()?;
        self.expect_kind(Kind::RightParen, &self.peek_token)?;
        // go to right parenthesis
        self.next_token();

        if self.peek_token_is(Kind::Keyword(Keyword::OVER)) {
            // parse the over clause
            let over = self.parse_over_clause()?;
            return Ok(ast::Expression::Function {
                name: Box::new(function_name),
                args: Box::new(ast::Expression::ExpressionList(expression_list)),
                over: Some(Box::new(over)),
            });
        }

        return Ok(ast::Expression::Function {
            name: Box::new(function_name),
            args: Box::new(ast::Expression::ExpressionList(expression_list)),
            over: None,
        });
    }

    fn parse_expression(&mut self, precedence: u8) -> Result<ast::Expression, String> {
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

    fn parse_prefix_expression(&mut self) -> Result<ast::Expression, String> {
        match self.current_token.kind() {
            Kind::Ident | Kind::Number | Kind::Asterisk => {
                // check if the next token is a dot
                if self.peek_token_is(Kind::Period) {
                    let mut idents = vec![self.current_token.clone()];
                    while self.peek_token_is(Kind::Period) {
                        // skip to the dot
                        self.next_token();

                        self.expect_kind(Kind::Ident, &self.peek_token)?;
                        self.next_token();

                        idents.push(self.current_token.clone());
                    }
                    if self.peek_token_is(Kind::LeftParen) {
                        return Ok(self.parse_function(ast::Expression::CompoundLiteral(idents))?);
                    } else {
                        return Ok(ast::Expression::CompoundLiteral(idents));
                    }
                } else {
                    let ident = self.current_token.clone();
                    if self.peek_token_is(Kind::LeftParen) {
                        return Ok(self.parse_function(ast::Expression::Literal(ident))?);
                    } else {
                        return Ok(ast::Expression::Literal(ident));
                    }
                }
            }
            Kind::Plus | Kind::Minus => {
                let operator = self.current_token.clone();
                let precedence = self.current_precedence();

                self.next_token();

                // parse the expression to the right of the operator
                let right_expression = self.parse_expression(precedence)?;

                return Ok(ast::Expression::Unary {
                    operator,
                    right: Box::new(right_expression),
                });
            }
            Kind::Keyword(Keyword::BETWEEN) => {
                // skip the between
                self.next_token();

                let expression = self.parse_expression(PRECEDENCE_LOWEST)?;

                match expression {
                    ast::Expression::Binary {
                        left,
                        operator,
                        right,
                    } if matches!(operator.kind(), Kind::Keyword(Keyword::AND)) => {
                        return Ok(ast::Expression::Between {
                            not: false,
                            low: left,
                            high: right,
                        });
                    }
                    _ => {
                        return Err(
                            self.expected_err("binary expression after BETWEEN", &self.peek_token)
                        );
                    }
                }
            }
            Kind::Keyword(Keyword::NOT) => {
                if self.peek_token_is(Kind::Keyword(Keyword::BETWEEN)) {
                    // skip the NOT
                    self.next_token();

                    let expression = self.parse_expression(PRECEDENCE_LOWEST)?;
                    match expression {
                        ast::Expression::Between { high, low, .. } => {
                            return Ok(ast::Expression::Between {
                                not: true,
                                low,
                                high,
                            });
                        }
                        _ => {
                            return Err(self.expected_err(
                                "binary expression after NOT BETWEEN",
                                &self.peek_token,
                            ));
                        }
                    }
                } else {
                    let operator = self.current_token.clone();
                    let precedence = self.current_precedence();
                    self.next_token();
                    let expression = self.parse_expression(precedence)?;

                    return Ok(ast::Expression::Unary {
                        operator,
                        right: Box::new(expression),
                    });
                }
            }
            Kind::Keyword(Keyword::IS) => {
                if self.peek_token_is(Kind::Keyword(Keyword::NOT)) {
                    self.next_token();
                    // check if we are looking at is not null
                    if self.peek_token_is(Kind::Keyword(Keyword::NULL)) {
                        self.next_token();
                        let expression = self.parse_expression(PRECEDENCE_LOWEST)?;
                        return Ok(ast::Expression::IsNotNull(Box::new(expression)));
                    } else {
                        let expression = self.parse_expression(PRECEDENCE_LOWEST)?;
                        return Ok(ast::Expression::IsNotTrue(Box::new(expression)));
                    }
                } else if self.peek_token_is(Kind::Keyword(Keyword::NULL)) {
                    let expression = self.parse_expression(PRECEDENCE_LOWEST)?;
                    return Ok(ast::Expression::IsNull(Box::new(expression)));
                } else {
                    let expression = self.parse_expression(PRECEDENCE_LOWEST)?;
                    return Ok(ast::Expression::IsTrue(Box::new(expression)));
                }
            }
            Kind::Keyword(Keyword::EXISTS) => {
                self.next_token();

                let expression = self.parse_expression(PRECEDENCE_LOWEST)?;

                return Ok(ast::Expression::Exists(Box::new(expression)));
            }
            Kind::LeftParen => {
                // if the first token is select we need to parse a subquery
                if self.peek_token_is(Kind::Keyword(Keyword::SELECT)) {
                    // go to select keyword
                    self.next_token();

                    let statement = self.parse_select_statement()?;
                    let expression = ast::Expression::Subquery(Box::new(statement));

                    // check if we have a closing parenthesis
                    self.expect_kind(Kind::RightParen, &self.peek_token)?;
                    self.next_token();

                    return Ok(expression);
                }
                // if the first token is an literal/identifier, we need to parse an
                // expression list
                else if self.peek_token_is(Kind::Ident) || self.peek_token_is(Kind::Number) {
                    let expression_list = self.parse_expression_list()?;
                    // check if we have a closing parenthesis
                    self.expect_kind(Kind::RightParen, &self.peek_token)?;
                    self.next_token();

                    return Ok(ast::Expression::ExpressionList(expression_list));
                } else {
                    return self.parse_grouping();
                }
            }
            Kind::Keyword(Keyword::DENSE_RANK)
            | Kind::Keyword(Keyword::RANK)
            | Kind::Keyword(Keyword::ROW_NUMBER)
            | Kind::Keyword(Keyword::ABS)
            | Kind::Keyword(Keyword::ACOS)
            | Kind::Keyword(Keyword::ASIN)
            | Kind::Keyword(Keyword::ATAN)
            | Kind::Keyword(Keyword::CEILING)
            | Kind::Keyword(Keyword::COS)
            | Kind::Keyword(Keyword::COT)
            | Kind::Keyword(Keyword::DEGREES)
            | Kind::Keyword(Keyword::EXP)
            | Kind::Keyword(Keyword::FLOOR)
            | Kind::Keyword(Keyword::LOG)
            | Kind::Keyword(Keyword::LOG10)
            | Kind::Keyword(Keyword::PI)
            | Kind::Keyword(Keyword::POWER)
            | Kind::Keyword(Keyword::RADIANS)
            | Kind::Keyword(Keyword::RANDS)
            | Kind::Keyword(Keyword::ROUND)
            | Kind::Keyword(Keyword::SIGN)
            | Kind::Keyword(Keyword::SIN)
            | Kind::Keyword(Keyword::SQRT)
            | Kind::Keyword(Keyword::SQUARE)
            | Kind::Keyword(Keyword::TAN)
            | Kind::Keyword(Keyword::FIRST_VALUE)
            | Kind::Keyword(Keyword::LAST_VALUE)
            | Kind::Keyword(Keyword::LAG)
            | Kind::Keyword(Keyword::LEAD)
            | Kind::Keyword(Keyword::AVG)
            | Kind::Keyword(Keyword::COUNT)
            | Kind::Keyword(Keyword::MAX)
            | Kind::Keyword(Keyword::MIN)
            | Kind::Keyword(Keyword::STDEV)
            | Kind::Keyword(Keyword::STDEVP)
            | Kind::Keyword(Keyword::SUM)
            | Kind::Keyword(Keyword::VAR)
            | Kind::Keyword(Keyword::VARP) => {
                let function_name = self.current_token.clone();
                Ok(self.parse_function(ast::Expression::Literal(function_name))?)
            }
            _ => return Err(self.expected_err("expression", &self.current_token)),
        }
    }

    fn parse_infix_expression(&mut self, left: ast::Expression) -> Result<ast::Expression, String> {
        // | Kind::Keyword(Keyword::IN)
        // | Kind::Keyword(Keyword::LIKE)
        match self.current_token.kind() {
            Kind::Plus
            | Kind::Minus
            | Kind::Asterisk
            | Kind::Divide
            | Kind::Keyword(Keyword::AND)
            | Kind::Keyword(Keyword::OR) => {
                let operator = self.current_token.clone();
                let precedence = self.current_precedence();
                self.next_token();

                // parse the expression to the right of the operator
                let right_expression = self.parse_expression(precedence)?;
                return Ok(ast::Expression::Binary {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right_expression),
                });
            }
            Kind::Equal
            | Kind::NotEqual
            | Kind::LessThan
            | Kind::LessThanEqual
            | Kind::GreaterThan
            | Kind::GreaterThanEqual => {
                if self.peek_token.kind() == Kind::Keyword(Keyword::ANY) {
                    let operator = self.current_token.clone();
                    let precedence = self.current_precedence();
                    // go to the ANY keyword
                    self.next_token();
                    // go to the next token
                    self.next_token();
                    let right_expression = self.parse_expression(precedence)?;

                    return Ok(ast::Expression::Any {
                        left: Box::new(left),
                        operator,
                        right: Box::new(right_expression),
                    });
                } else if self.peek_token.kind() == Kind::Keyword(Keyword::ALL) {
                    let operator = self.current_token.clone();
                    let precedence = self.current_precedence();
                    // go to the ALL keyword
                    self.next_token();
                    // go to the next token
                    self.next_token();
                    let right_expression = self.parse_expression(precedence)?;

                    return Ok(ast::Expression::All {
                        left: Box::new(left),
                        operator,
                        right: Box::new(right_expression),
                    });
                } else if self.peek_token.kind() == Kind::Keyword(Keyword::SOME) {
                    let operator = self.current_token.clone();
                    let precedence = self.current_precedence();
                    // go to the SOME keyword
                    self.next_token();
                    // go to the next token
                    self.next_token();
                    let right_expression = self.parse_expression(precedence)?;

                    return Ok(ast::Expression::Some {
                        left: Box::new(left),
                        operator,
                        right: Box::new(right_expression),
                    });
                } else {
                    let operator = self.current_token.clone();
                    let precedence = self.current_precedence();
                    self.next_token();

                    // parse the expression to the right of the operator
                    let right_expression = self.parse_expression(precedence)?;
                    return Ok(ast::Expression::Binary {
                        left: Box::new(left),
                        operator,
                        right: Box::new(right_expression),
                    });
                }
            }
            Kind::Keyword(Keyword::IN) => self.parse_in_expression(left),
            Kind::Keyword(Keyword::NOT) if self.peek_token_is(Kind::Keyword(Keyword::IN)) => {
                self.parse_in_expression(left)
            }
            _ => Err(self.expected_err("infix expression", &self.current_token)),
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

    fn expect_kind(&self, expected: Kind, found: &Token) -> Result<(), String> {
        if expected == found.kind() {
            return Ok(());
        }

        self.expected(&expected.to_string(), found)
    }

    fn expect_many_kind(&self, expecting: &[Kind], found: &Token) -> Result<(), String> {
        for expected in expecting {
            if *expected == found.kind() {
                return Ok(());
            }
        }
        let mut expected_string = String::new();
        for (i, expected) in expecting.iter().enumerate() {
            if i == 0 {
                expected_string.push_str(&format!("{}", expected));
            } else {
                expected_string.push_str(&format!("or {}", expected));
            }
        }
        self.expected(&expected_string, found)
    }

    fn expected(&self, expected: &str, found: &Token) -> Result<(), String> {
        Err(self.expected_err(expected, found))
    }

    fn expected_err(&self, expected: &str, found: &Token) -> String {
        let mut pointer_literal_len = match found.literal() {
            Literal::String(string) => string.len(),
            Literal::QuotedString { value, .. } => value.len() + 2,
            Literal::Number(num) => num.to_string().len(),
        };
        if pointer_literal_len == 0 {
            pointer_literal_len = 1;
        }
        let pointer_line = format!(
            "{}{}",
            " ".repeat(found.location().column),
            "^".repeat(pointer_literal_len)
        );
        format!(
            "Expected '{}', found {} at {}\n{}\n{}. ",
            expected,
            found.literal(),
            found.location(),
            self.lexer.current_line_input(),
            pointer_line
        )
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
        for error in parser.errors() {
            println!("{}", error);
        }

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
                        is_as: false,
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
                        is_as: false,
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
                        is_as: false,
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
                        is_as: false,
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
                                is_as: true,
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
                                is_as: false,
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
                        is_as: false,
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
                                    is_as: false,
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
                        is_as: false,
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
                        is_as: false,
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
