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
        // check if we saw a column after comma
        let mut column_seen = false;
        while !self.peek_token_is(Kind::Keyword(Keyword::FROM))
            && !self.peek_token_is(Kind::Keyword(Keyword::INTO))
            && !self.peek_token_is(Kind::Eof)
        {
            self.next_token();
            match self.current_token.kind() {
                Kind::Comma => {
                    // check if we have an identifier after a comma
                    column_seen = false;
                    // self.next_token();
                }
                _ => {
                    if let Some(expression) = self.parse_expression(PRECEDENCE_LOWEST) {
                        // confirm we saw a column
                        column_seen = true;

                        statement.columns.push(expression);
                        // self.next_token();
                    } else {
                        self.current_error(Kind::Ident);
                        return None;
                    }
                }
            }
        }
        // check if we saw a column after comma
        if !column_seen {
            self.current_msg_error("expected column after comma");
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
                ast::Expression::Literal(token) => matches!(token.kind(), Kind::Number),
                _ => false,
            })
            .count();

        if number_of_non_literal_tokens > 0 {
            // at this point we should have a FROM keyword
            // but we should make sure
            if !self.expect_peek(Kind::Keyword(Keyword::FROM)) {
                return None;
            }

            // get the table name to select from
            // check if the next token is an identifier
            if !self.expect_peek(Kind::Ident) {
                return None;
            } else {
                statement
                    .table
                    .push(ast::Expression::Literal(self.current_token.clone()));
            }
        } else {
            // check if we have a FROM keyword
            if self.peek_token_is(Kind::Keyword(Keyword::FROM)) {
                // go to the FROM keyword
                self.next_token();

                // get the table name to select from
                // check if the next token is an identifier
                if !self.expect_peek(Kind::Ident) {
                    return None;
                } else {
                    statement
                        .table
                        .push(ast::Expression::Literal(self.current_token.clone()));
                }
            }
        }

        // check if we have any where clause
        if self.peek_token_is(Kind::Keyword(Keyword::WHERE)) {
            // skip the WHERE keyword
            self.next_token();
            self.next_token();

            let expression = self.parse_expression(PRECEDENCE_LOWEST);
            if expression
                .as_ref()
                .is_some_and(|ex| !matches!(*ex, ast::Expression::Binary { .. }))
            {
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
        // needed to check if we have an expression after comma
        let mut seen_arg = false;
        while !self.peek_token_is(Kind::Keyword(Keyword::HAVING))
            && !self.peek_token_is(Kind::SemiColon)
            && !self.peek_token_is(Kind::Eof)
        {
            self.next_token();

            match self.current_token.kind() {
                Kind::Comma => {
                    seen_arg = false;
                }
                _ => {
                    if let Some(expression) = self.parse_expression(PRECEDENCE_LOWEST) {
                        // we have seen an group_by_arg
                        seen_arg = true;
                        args.push(expression);
                    } else {
                        // TODO: error handling
                        self.current_error(Kind::Ident);
                        return None;
                    }
                }
            }
        }

        match (args.len(), seen_arg) {
            (0, _) => {
                self.peek_msg_error("expected GROUP BY expression after GROUP BY");
                None
            }
            (_, false) => {
                self.peek_msg_error("expected GROUP BY expression after COMMA");
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
        // needed to check if we have an expression after comma
        let mut seen_order_by_arg = false;
        while !self.peek_token_is(Kind::Keyword(Keyword::OFFSET))
            && !self.peek_token_is(Kind::SemiColon)
            && !self.peek_token_is(Kind::Eof)
        {
            self.next_token();

            match self.current_token.kind() {
                Kind::Comma => {
                    seen_order_by_arg = false;
                }
                _ => {
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
                        seen_order_by_arg = true;
                        order_by_args.push(ast::OrderByArg {
                            column: expression,
                            asc: is_asc,
                        });
                    } else {
                        self.current_error(Kind::Ident);
                        return None;
                    }
                }
            }
        }

        match (order_by_args.len(), seen_order_by_arg) {
            (0, _) => {
                self.peek_msg_error("expected ORDERED BY expression after ORDERED BY");
                None
            }
            (_, false) => {
                self.peek_msg_error("expected ORDERED BY expression after COMMA");
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
            dbg!(&self.current_token);

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
                Some(ast::Expression::Literal(self.current_token.clone()))
            }
            Kind::Plus | Kind::Minus | Kind::Keyword(Keyword::NOT) => {
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
                dbg!(&self.current_token);

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
                columns: vec![ast::Expression::Literal(Token::wrap(
                    Kind::Ident,
                    Literal::new_string("name"),
                ))],
                into_table: None,
                table: vec![ast::Expression::Literal(Token::wrap(
                    Kind::Ident,
                    Literal::new_string("users"),
                ))],
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
                    ast::Expression::Literal(Token::wrap(Kind::Ident, Literal::new_string("name"))),
                    ast::Expression::Literal(Token::wrap(Kind::Number, Literal::Number(1.0))),
                ],
                into_table: None,
                table: vec![ast::Expression::Literal(Token::wrap(
                    Kind::Ident,
                    Literal::new_string("users"),
                ))],
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
                    ast::Expression::Literal(Token::wrap(Kind::Asterisk, Literal::new_string("*"))),
                    ast::Expression::Literal(Token::wrap(Kind::Ident, Literal::new_string("name"))),
                    ast::Expression::Literal(Token::wrap(
                        Kind::Ident,
                        Literal::new_string("firstname"),
                    )),
                    ast::Expression::Literal(Token::wrap(
                        Kind::Ident,
                        Literal::new_string("lastname"),
                    )),
                    ast::Expression::Literal(Token::wrap(
                        Kind::Ident,
                        Literal::new_string("[first]"),
                    )),
                    ast::Expression::Literal(Token::wrap(Kind::Ident, Literal::new_string("dob"))),
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
                table: vec![ast::Expression::Literal(Token::wrap(
                    Kind::Ident,
                    Literal::new_string("users"),
                ))],
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
                    ast::Expression::Literal(Token::wrap(Kind::Asterisk, Literal::new_string("*"))),
                    ast::Expression::Literal(Token::wrap(Kind::Ident, Literal::new_string("name"))),
                    ast::Expression::Literal(Token::wrap(
                        Kind::Ident,
                        Literal::new_string("firstname"),
                    )),
                    ast::Expression::Literal(Token::wrap(
                        Kind::Ident,
                        Literal::new_string("lastname"),
                    )),
                    ast::Expression::Literal(Token::wrap(
                        Kind::Ident,
                        Literal::new_string("[first]"),
                    )),
                    ast::Expression::Literal(Token::wrap(Kind::Ident, Literal::new_string("dob"))),
                ],
                into_table: None,
                table: vec![ast::Expression::Literal(Token::wrap(
                    Kind::Ident,
                    Literal::new_string("users"),
                ))],
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
                    ast::Expression::Literal(Token::wrap(Kind::Ident, Literal::new_string("name"))),
                    ast::Expression::Subquery(Box::new(ast::Statement::Select(Box::new(
                        ast::SelectStatement {
                            distinct: false,
                            top: None,
                            columns: vec![ast::Expression::Literal(Token::wrap(
                                Kind::Asterisk,
                                Literal::new_string("*"),
                            ))],
                            into_table: None,
                            table: vec![ast::Expression::Literal(Token::wrap(
                                Kind::Ident,
                                Literal::new_string("MarketData"),
                            ))],
                            where_clause: None,
                            group_by: vec![],
                            having: None,
                            order_by: vec![],
                            offset: None,
                            fetch: None,
                        },
                    )))),
                ],
                into_table: None,
                table: vec![ast::Expression::Literal(Token::wrap(
                    Kind::Ident,
                    Literal::new_string("users"),
                ))],
                where_clause: Some(ast::Expression::Binary {
                    left: Box::new(ast::Expression::Binary {
                        left: Box::new(ast::Expression::Literal(Token::wrap(
                            Kind::Ident,
                            Literal::new_string("lastname"),
                        ))),
                        operator: Token::wrap(Kind::Equal, Literal::new_string("=")),
                        right: Box::new(ast::Expression::Literal(Token::wrap(
                            Kind::Ident,
                            Literal::new_string("'blah'"),
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
                            Literal::new_string("'hello'"),
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
                columns: vec![ast::Expression::Literal(Token::wrap(
                    Kind::Ident,
                    Literal::new_string("name"),
                ))],
                into_table: None,
                table: vec![ast::Expression::Literal(Token::wrap(
                    Kind::Ident,
                    Literal::new_string("users"),
                ))],
                where_clause: Some(ast::Expression::Binary {
                    left: Box::new(ast::Expression::Binary {
                        left: Box::new(ast::Expression::Literal(Token::wrap(
                            Kind::Ident,
                            Literal::new_string("lastname"),
                        ))),
                        operator: Token::wrap(Kind::Equal, Literal::new_string("=")),
                        right: Box::new(ast::Expression::Literal(Token::wrap(
                            Kind::Ident,
                            Literal::new_string("'blah'"),
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
                            Literal::new_string("'hello'"),
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
