pub mod ast;
pub mod error;
mod expr_start;
mod operator;
pub mod visitor;

use crate::ast::Keyword;
use crate::error::{parse_error, ParseError, ParseErrorType};
use crate::expr_start::{
    BUILTIN_FN_START, EXPRESSION_LIST_START, FUNCTION_ARGS_START, GROUP_BY_START,
    ORDER_BY_ARGS_START, PARTITION_BY_START, SELECT_ITEM_TYPE_START, TABLE_SOURCE_START,
};
use crate::operator::{get_precedence, Precedence};
use ast::{Comment, DataTypeSize, Symbol};
use error::parse_lexical_error;
use lexer::{Lexer, Span, Token, TokenKind};

#[derive(Debug, Clone)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    // tokens: Vec<Token<'a>>,
    peek_token: Option<Token<'a>>,

    comments: Vec<Comment>,
    parse_errors: Vec<ParseError<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut parser = Parser {
            lexer,
            peek_token: None,
            comments: vec![],
            parse_errors: vec![],
        };
        parser.advance();
        parser
    }

    pub fn errors(&self) -> &[ParseError<'a>] {
        &self.parse_errors
    }

    pub fn comments(&self) -> &[Comment] {
        &self.comments
    }

    fn advance(&mut self) {
        let _ = self.next_token();
    }

    fn next_token(&mut self) -> Option<Token<'a>> {
        let token = self.peek_token.take();
        let mut next_tok;

        loop {
            match self.lexer.next() {
                Some(r) => match r {
                    Ok(token) => match token.kind_as_ref() {
                        TokenKind::Comment(s) => {
                            self.comments.push(Comment {
                                content: s.to_string(),
                                span: token.location(),
                            });
                        }
                        _ => {
                            next_tok = Some(token);
                            break;
                        }
                    },
                    Err(e) => self.parse_errors.push(parse_lexical_error(e)),
                },
                None => {
                    next_tok = None;
                    break;
                }
            }
        }

        self.peek_token = next_tok.take();
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
        self.unexpected_token(vec![token_kind.to_string()])
    }

    #[allow(dead_code)]
    fn expect_token_any(&mut self, token_kinds: &[TokenKind]) -> Result<Token<'a>, ParseError<'a>> {
        if self.token_is_any(token_kinds) {
            let tok = self.peek_token.unwrap();
            self.advance();
            return Ok(tok);
        }
        self.unexpected_token(token_kinds.iter().map(|s| s.to_string()).collect())
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
            Some(t) => parse_error(
                ParseErrorType::UnexpectedToken {
                    token: *t.kind_as_ref(),
                    expected,
                },
                t.location(),
            ),
            None => parse_error(
                ParseErrorType::UnexpectedToken {
                    token: TokenKind::Eof,
                    expected,
                },
                Span::default(),
            ),
        }
    }

    fn parse_error<T>(&self, parse_error_type: ParseErrorType<'a>) -> Result<T, ParseError<'a>> {
        let span = match self.peek_token {
            Some(t) => t.location(),
            None => Span::default(),
        };
        parse_error(parse_error_type, span)
    }

    fn expect_function_args_start(&mut self) -> Result<(), ParseError<'a>> {
        if let Some(token) = self.peek_token {
            for start_token in FUNCTION_ARGS_START {
                if start_token.shallow_eq_token(token.kind_as_ref()) {
                    return Ok(());
                }
            }
        }
        self.unexpected_token(
            FUNCTION_ARGS_START
                .iter()
                .map(|s| s.string_type().to_string())
                .collect(),
        )
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
                .map(|s| s.string_type().to_string())
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
        self.unexpected_token(
            GROUP_BY_START
                .iter()
                .map(|s| s.string_type().to_string())
                .collect(),
        )
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
                .map(|s| s.string_type().to_string())
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
        self.unexpected_token(
            ORDER_BY_ARGS_START
                .iter()
                .map(|s| s.string_type().to_string())
                .collect(),
        )
    }

    fn expect_partition_by_start(&mut self) -> Result<(), ParseError<'a>> {
        if let Some(token) = self.peek_token {
            for start_token in PARTITION_BY_START {
                if start_token.shallow_eq_token(token.kind_as_ref()) {
                    return Ok(());
                }
            }
        }
        self.unexpected_token(
            PARTITION_BY_START
                .iter()
                .map(|s| s.string_type().to_string())
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
        self.unexpected_token(
            TABLE_SOURCE_START
                .iter()
                .map(|s| s.string_type().to_string())
                .collect(),
        )
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
        self.unexpected_token(vec![kind.to_string()])
    }
}

impl<'a> Parser<'a> {
    pub fn parse(&mut self) -> ast::Query {
        let mut query = ast::Query::new();

        while self.peek_token.is_some_and(|t| t.kind() != TokenKind::Eof) {
            match self.parse_statement() {
                Ok(statement) => query.statements.push(statement),
                Err(parse_error) => self.parse_errors.push(parse_error),
            }
        }

        query
    }

    fn parse_statement(&mut self) -> Result<ast::Statement, ParseError<'a>> {
        match self.peek_token {
            Some(maybe_token) => match maybe_token.kind_as_ref() {
                TokenKind::Select => {
                    return Ok(ast::Statement::Select(self.parse_select_statement()?))
                }
                TokenKind::Insert => return self.parse_insert_statement(),

                // TokenKind::Update => {
                //     return Ok(ast::Statement::Update(self.parse_update_statement()?))
                // }
                // TokenKind::Delete => {
                //     return Ok(ast::Statement::Delete(self.parse_delete_statement()?))
                // }
                TokenKind::With => return self.parse_cte_statement(),
                TokenKind::Declare => return self.parse_declare_statement(),
                TokenKind::Set => return self.parse_set_local_variable_statement(),
                TokenKind::Exec | TokenKind::Execute => return self.parse_execute_statement(),
                _ => {
                    let err = self.unexpected_token(vec![
                        TokenKind::Select.to_string(),
                        TokenKind::Insert.to_string(),
                        TokenKind::With.to_string(),
                        TokenKind::Declare.to_string(),
                        TokenKind::Set.to_string(),
                        TokenKind::Exec.to_string(),
                        TokenKind::Execute.to_string(),
                    ]);
                    self.advance();
                    return err;
                }
            },
            None => todo!(),
        }
    }

    fn parse_insert_statement(&mut self) -> Result<ast::Statement, ParseError<'a>> {
        let insert_kw = self.consume_keyword(TokenKind::Insert)?;
        let into_kw = self.maybe_keyword(TokenKind::Into);
        let object = self.parse_object_table_name()?;

        if self.token_is(&TokenKind::Select) {
            let select_kw = self.consume_keyword(TokenKind::Select)?;
            let top = if let Some(kw) = self.maybe_keyword(TokenKind::Top) {
                Some(self.parse_top_clause(kw)?)
            } else {
                None
            };
            let columns = self.parse_expression_list()?;
            let from_kw = self.consume_keyword(TokenKind::From)?;
            let table = self.parse_table_arg(from_kw)?;
            let where_clause = if let Some(kw) = self.maybe_keyword(TokenKind::Where) {
                Some(self.parse_where_clause(kw)?)
            } else {
                None
            };
            let insert_statement = ast::InsertStatement::Table {
                insert_kw,
                into_kw,
                object,
                select_kw,
                top,
                columns,
                table,
                where_clause,
            };

            Ok(ast::Statement::Insert(insert_statement))
        } else {
            let columns = if self.token_is(&TokenKind::LeftParen) {
                let left_paren: Symbol = self.expect_token(&TokenKind::LeftParen)?.into();
                let columns = self.parse_expression_list()?;
                let right_paren: Symbol = self.expect_token(&TokenKind::RightParen)?.into();
                Some(ast::ExpressionList {
                    left_paren,
                    items: columns,
                    right_paren,
                })
            } else {
                None
            };
            let values_kw = self.consume_keyword(TokenKind::Values)?;
            let left_paren: Symbol = self.expect_token(&TokenKind::LeftParen)?.into();
            let values = self.parse_expression_list()?;
            let right_paren: Symbol = self.expect_token(&TokenKind::RightParen)?.into();

            let insert_statement = ast::InsertStatement::Values {
                insert_kw,
                into_kw,
                object,
                columns,
                values_kw,
                values: ast::ExpressionList {
                    left_paren,
                    items: values,
                    right_paren,
                },
            };

            Ok(ast::Statement::Insert(insert_statement))
        }
    }

    fn parse_object_table_name(&mut self) -> Result<ast::Expression, ParseError<'a>> {
        if self.token_is_any(&[TokenKind::QuotedIdentifier(""), TokenKind::Identifier("")]) {
            let object = ast::Expression::try_from(self.peek_token)?;
            self.advance();

            if self.token_is(&TokenKind::Period) {
                Ok(self.parse_compound_identifier(object)?)
            } else {
                Ok(object)
            }
        } else {
            self.parse_error(ParseErrorType::ExpectedObjectToInsertTo)
        }
    }

    fn parse_set_local_variable_statement(&mut self) -> Result<ast::Statement, ParseError<'a>> {
        let set_kw = self.consume_keyword(TokenKind::Set)?;
        let local_variable: ast::Expression =
            self.expect_token(&TokenKind::LocalVariable(""))?.into();
        let equal_sign: Symbol = self.expect_token(&TokenKind::Equal)?.into();
        let value = self.parse_expression(Precedence::Lowest)?;
        let semicolon: Symbol = self.expect_token(&TokenKind::SemiColon)?.into();

        Ok(ast::Statement::SetLocalVariable {
            set_kw,
            name: local_variable,
            equal_sign,
            value,
            semicolon,
        })
    }

    fn parse_declare_statement(&mut self) -> Result<ast::Statement, ParseError<'a>> {
        let declare_kw = self.consume_keyword(TokenKind::Declare)?;

        let mut variables = vec![];
        loop {
            let local_variable = self.expect_token(&TokenKind::LocalVariable(""))?;
            let data_type = self.parse_data_type()?;
            let value = if self.token_is(&TokenKind::Equal) {
                let equal_sign: Symbol = self.expect_token(&TokenKind::Equal)?.into();
                Some((equal_sign, self.parse_expression(Precedence::Lowest)?))
            } else {
                None
            };

            variables.push(ast::LocalVariable {
                name: local_variable.into(),
                data_type,
                value,
            });
            if !self.token_is(&TokenKind::Comma) {
                break;
            }
            self.advance();
        }
        let semicolon: Symbol = self.expect_token(&TokenKind::SemiColon)?.into();

        Ok(ast::Statement::Declare {
            declare_kw,
            variables,
            semicolon,
        })
    }

    fn parse_cte_statement(&mut self) -> Result<ast::Statement, ParseError<'a>> {
        let with_kw = self.consume_keyword(TokenKind::With)?;
        let mut ctes = vec![];
        loop {
            let cte_name = ast::Expression::try_from(self.peek_token)?;
            self.advance();
            let column_list = if self.token_is(&TokenKind::LeftParen) {
                let left_paren: Symbol = self.expect_token(&TokenKind::LeftParen)?.into();
                let expr_list = self.parse_expression_list()?;
                let right_paren: Symbol = self.expect_token(&TokenKind::RightParen)?.into();
                Some(ast::ExpressionList {
                    left_paren,
                    items: expr_list,
                    right_paren,
                })
            } else {
                None
            };
            let as_kw = self.consume_keyword(TokenKind::As)?;
            let left_paren: Symbol = self.expect_token(&TokenKind::LeftParen)?.into();
            let query = self.parse_select_statement()?;
            let right_paren: Symbol = self.expect_token(&TokenKind::RightParen)?.into();

            ctes.push(ast::CommonTableExpression {
                name: cte_name,
                columns: column_list,
                as_kw,
                left_paren,
                query,
                right_paren,
            });

            if !self.token_is(&TokenKind::Comma) {
                break;
            }
            self.advance();
        }
        let final_query = self.parse_select_statement()?;
        Ok(ast::Statement::CTE {
            with_kw,
            ctes,
            statement: ast::CommonTableExpressionStatement::Select(final_query),
        })
    }

    fn parse_execute_statement(&mut self) -> Result<ast::Statement, ParseError<'a>> {
        let exec_kw = if let Some(kw) = self.maybe_keyword(TokenKind::Exec) {
            kw
        } else {
            self.consume_keyword(TokenKind::Execute)?
        };

        // get the procedure name
        let procedure_name = ast::Expression::try_from(self.peek_token)?;
        self.advance();
        let parameters = self.parse_procedure_parameters()?;

        Ok(ast::Statement::Execute {
            exec_kw,
            procedure_name,
            parameters,
        })
    }

    fn parse_procedure_parameters(
        &mut self,
    ) -> Result<Vec<ast::ProcedureParameter>, ParseError<'a>> {
        let mut params = vec![];

        loop {
            let name = if self.token_is(&TokenKind::LocalVariable("")) {
                let tok = self.peek_token;
                self.advance();
                if self.token_is(&TokenKind::Equal) {
                    let equal_sign: Symbol = self.expect_token(&TokenKind::Equal)?.into();
                    let name = ast::ProcedureParameterName::try_from(tok)?;

                    Some((name, equal_sign))
                } else {
                    let expr: ast::Expression = tok.try_into()?;
                    params.push(ast::ProcedureParameter {
                        name: None,
                        value: expr,
                    });

                    if !self.token_is(&TokenKind::Comma) {
                        break;
                    }
                    self.advance();
                    continue;
                }
            } else {
                None
            };
            let expr = ast::Expression::try_from(self.peek_token)?;
            params.push(ast::ProcedureParameter { name, value: expr });

            self.advance();

            if !self.token_is(&TokenKind::Comma) {
                break;
            }
            self.advance()
        }

        Ok(params)
    }

    fn parse_select_statement(&mut self) -> Result<ast::SelectStatement, ParseError<'a>> {
        let mut select_statement = ast::SelectStatement::default();

        select_statement.select = self.consume_keyword(TokenKind::Select)?;
        select_statement.distinct = self.maybe_keyword(TokenKind::Distinct);
        select_statement.all = self.maybe_keyword(TokenKind::All);

        if let Some(kw) = self.maybe_keyword(TokenKind::Top) {
            select_statement.top = Some(self.parse_top_clause(kw)?);
        }

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
            let expression = self.parse_expression(Precedence::Lowest)?;

            // column_alias = expression
            if matches!(
                expression,
                ast::Expression::Identifier(..)
                    | ast::Expression::QuotedIdentifier(..)
                    | ast::Expression::StringLiteral(..)
                    | ast::Expression::LocalVariable(..)
            ) && self.token_is(&TokenKind::Equal)
            {
                let _ = self.expect_token(&TokenKind::Equal)?;
                let expr = self.parse_expression(Precedence::Lowest)?;
                columns.push(ast::SelectItem::ReverseAliasAssign {
                    alias: expression,
                    expression: expr,
                });
            } else {
                // normal checking for alias
                let as_kw = self.maybe_keyword(TokenKind::As);

                // check for alias
                if self.token_is_any(&[
                    TokenKind::Identifier(""),
                    TokenKind::QuotedIdentifier(""),
                    TokenKind::StringLiteral(""),
                ]) {
                    let alias = ast::Expression::try_from(self.peek_token)?;
                    self.advance();

                    if matches!(expression, ast::Expression::Asterisk(_)) {
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
                    if let ast::Expression::Asterisk(s) = expression {
                        columns.push(ast::SelectItem::Wildcard(s));
                    } else {
                        columns.push(ast::SelectItem::Unnamed(expression));
                    }
                } else {
                    return self.parse_error(ParseErrorType::MissingAliasAfterAsKeyword);
                }
            }

            if !self.token_is(&TokenKind::Comma) {
                break;
            }
            self.advance();
        }

        if columns.is_empty() {
            return self.parse_error(ParseErrorType::EmptySelectColumns);
        }

        Ok(columns)
    }

    fn parse_top_clause(&mut self, top_kw: Keyword) -> Result<ast::Top, ParseError<'a>> {
        let top_expr = ast::Expression::try_from(self.peek_token)?;
        match top_expr {
            ast::Expression::NumberLiteral(_) => {}
            _ => return self.unexpected_token(vec!["numeric literal".to_string()]),
        }

        self.advance();
        let percent_kw = self.maybe_keyword(TokenKind::Percent);

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
                let new_expr: ast::Expression = token.into();
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

            if self.token_is(&TokenKind::On) {
                let on_kw = self.consume_keyword(TokenKind::On)?;
                let search_condition = self.parse_expression(Precedence::Lowest)?;
                let join = ast::Join {
                    join: join_keyword,
                    join_type,
                    table: table_source,
                    condition: Some(ast::JoinCondition {
                        on_kw,
                        condition: search_condition,
                    }),
                };
                joins.push(join);
            } else {
                let join = ast::Join {
                    join: join_keyword,
                    join_type,
                    table: table_source,
                    condition: None,
                };
                joins.push(join);
            }
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

        let left_paren: Symbol = self.expect_token(&TokenKind::LeftParen)?.into();
        let mut args = None;
        if !self.token_is(&TokenKind::RightParen) {
            args = Some(self.parse_function_args()?);
        }
        let right_paren: Symbol = self.expect_token(&TokenKind::RightParen)?.into();

        if let Some(kw) = self.maybe_keyword(TokenKind::Over) {
            let over_clause = self.parse_function_over_clause(kw)?;
            return Ok(ast::Expression::Function {
                name: Box::new(function_name),
                left_paren,
                args,
                right_paren,
                over: Some(Box::new(over_clause)),
            });
        }

        Ok(ast::Expression::Function {
            name: Box::new(function_name),
            left_paren,
            args,
            right_paren,
            over: None,
        })
    }

    fn parse_function_args(&mut self) -> Result<Vec<ast::Expression>, ParseError<'a>> {
        let mut args = vec![];

        loop {
            self.expect_function_args_start()?;
            let expr = self.parse_expression(Precedence::Lowest)?;
            args.push(expr);

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
        let left_paren: Symbol = self.expect_token(&TokenKind::LeftParen)?.into();

        let partition_by_kws;
        let partition_by_clause;
        if let Some(partition_kw) = self.maybe_keyword(TokenKind::Partition) {
            let by_kw = self.consume_keyword(TokenKind::By)?;
            partition_by_kws = Some(vec![partition_kw, by_kw]);
            partition_by_clause = self.parse_function_partition_clause()?;
        } else {
            partition_by_kws = None;
            partition_by_clause = vec![];
        }

        let order_by_kws;
        let order_by_args;
        if let Some(order_kw) = self.maybe_keyword(TokenKind::Order) {
            let by_kw = self.consume_keyword(TokenKind::By)?;
            order_by_kws = Some(vec![order_kw, by_kw]);
            order_by_args = self.parse_order_by_args()?;
        } else {
            order_by_kws = None;
            order_by_args = vec![];
        }

        let mut window_frame_clause = None;
        if self.token_is_any(&[TokenKind::Rows, TokenKind::Range]) {
            window_frame_clause = Some(self.parse_function_window_frame_clause()?);
        }

        let right_paren: Symbol = self.expect_token(&TokenKind::RightParen)?.into();

        Ok(ast::OverClause {
            over_kw,
            left_paren,
            partition_by_kws,
            partition_by: partition_by_clause,
            order_by_kws,
            order_by: order_by_args,
            window_frame: window_frame_clause,
            right_paren,
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
            return self.parse_error(ParseErrorType::EmptyPartitionByClause);
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
            return self.parse_error(ParseErrorType::EmptyOrderByArgs);
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
            return self.parse_error(ParseErrorType::MissingRowsOrRangeInWindowFrameClause);
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
            return self.parse_error(
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
            return self.parse_error(
                ParseErrorType::ExpectedUnboundedFollowingCurrentRowOrNumberFollowing,
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
            let expression = self.parse_expression(Precedence::Lowest)?;

            expressions.push(expression);
            if !self.token_is(&TokenKind::Comma) {
                break;
            }
            self.advance();
        }

        if expressions.is_empty() {
            return self.parse_error(ParseErrorType::EmptyGroupByClause);
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
        let left_paren: Symbol = self.expect_token(&TokenKind::LeftParen)?.into();

        let expression = self.parse_expression(Precedence::Lowest)?;

        let as_kw = self.consume_keyword(TokenKind::As)?;
        let data_type = self.parse_data_type()?;
        let right_paren: Symbol = self.expect_token(&TokenKind::RightParen)?.into();

        Ok(ast::Expression::Cast {
            cast_kw,
            left_paren,
            expression: Box::new(expression),
            as_kw,
            data_type,
            right_paren,
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
            let data_type_size = self.parse_data_type_size()?;
            ast::DataType::Float(keyword, data_type_size)
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
            let data_type_size = self.parse_data_type_size()?;
            ast::DataType::Varchar(keyword, data_type_size)
        } else {
            return self.parse_error(ParseErrorType::ExpectedDataType);
        };

        Ok(data_type)
    }

    fn parse_data_type_size(&mut self) -> Result<Option<DataTypeSize>, ParseError<'a>> {
        if self.token_is(&TokenKind::LeftParen) {
            let left_paren: Symbol = self.expect_token(&TokenKind::LeftParen)?.into();
            let numeric_literal = self.expect_token(&TokenKind::NumberLiteral(""))?.into();
            let size: u32 = match numeric_literal {
                ast::Expression::NumberLiteral(n) => match n.content.parse() {
                    Ok(n) => n,
                    Err(_) => return self.parse_error(ParseErrorType::ExpectedDataTypeSize),
                },
                _ => return self.parse_error(ParseErrorType::ExpectedDataTypeSize),
            };
            let right_paren: Symbol = self.expect_token(&TokenKind::RightParen)?.into();
            Ok(Some(DataTypeSize {
                left_paren,
                size,
                right_paren,
            }))
        } else {
            Ok(None)
        }
    }

    fn parse_float_precision(&mut self) -> Result<u32, ParseError<'a>> {
        let numeric_literal: ast::Expression =
            self.expect_token(&TokenKind::NumberLiteral(""))?.into();
        let size: u32 = match numeric_literal {
            ast::Expression::NumberLiteral(n) => match n.content.parse() {
                Ok(n) => n,
                Err(_) => return self.parse_error(ParseErrorType::ExpectedDataTypeSize),
            },
            _ => return self.parse_error(ParseErrorType::ExpectedDataTypeSize),
        };
        Ok(size)
    }

    fn parse_numeric_size(&mut self) -> Result<Option<ast::NumericSize>, ParseError<'a>> {
        let left_paren: Symbol = self.expect_token(&TokenKind::LeftParen)?.into();
        if self.token_is(&TokenKind::RightParen) {
            return Ok(None);
        }

        let float_precision = self.parse_float_precision()?;

        let scale = if self.token_is(&TokenKind::Comma) {
            let _ = self.expect_token(&TokenKind::Comma)?;
            Some(self.parse_numeric_scale()?)
        } else {
            None
        };

        let right_paren: Symbol = self.expect_token(&TokenKind::RightParen)?.into();

        Ok(Some(ast::NumericSize {
            left_paren,
            precision: float_precision,
            scale,
            right_paren,
        }))
    }

    fn parse_numeric_scale(&mut self) -> Result<u32, ParseError<'a>> {
        let numeric_literal: ast::Expression =
            self.expect_token(&TokenKind::NumberLiteral(""))?.into();
        let size: u32 = match numeric_literal {
            ast::Expression::NumberLiteral(n) => match n.content.parse() {
                Ok(n) => n,
                Err(_) => return self.parse_error(ParseErrorType::ExpectedDataTypeSize),
            },
            _ => return self.parse_error(ParseErrorType::ExpectedDataTypeSize),
        };

        Ok(size)
    }

    fn parse_expression_list(&mut self) -> Result<Vec<ast::Expression>, ParseError<'a>> {
        let mut expressions = vec![];

        loop {
            self.expect_expression_list_start()?;
            let expression = self.parse_expression(Precedence::Lowest)?;

            expressions.push(expression);
            if !self.token_is(&TokenKind::Comma) {
                break;
            }
            self.advance();
        }

        Ok(expressions)
    }

    fn parse_subquery(&mut self) -> Result<ast::Expression, ParseError<'a>> {
        let left_paren: Symbol = self.expect_token(&TokenKind::LeftParen)?.into();
        let select_statement = self.parse_select_statement()?;
        let right_paren: Symbol = self.expect_token(&TokenKind::RightParen)?.into();

        Ok(ast::Expression::Subquery {
            left_paren,
            select_statement: Box::new(select_statement),
            right_paren,
        })
    }

    fn parse_in_expression(
        &mut self,
        test_expression: ast::Expression,
        in_kw: Keyword,
        not_kw: Option<Keyword>,
    ) -> Result<ast::Expression, ParseError<'a>> {
        let left_paren: Symbol = self.expect_token(&TokenKind::LeftParen)?.into();
        let expr = if self.token_is(&TokenKind::Select) {
            let select_statement = self.parse_select_statement()?;
            let right_paren: Symbol = self.expect_token(&TokenKind::RightParen)?.into();
            let subquery = ast::Expression::Subquery {
                left_paren,
                select_statement: Box::new(select_statement),
                right_paren,
            };
            ast::Expression::InSubquery {
                test_expression: Box::new(test_expression),
                in_kw,
                not_kw,
                subquery: Box::new(subquery),
            }
        } else if self.token_is_any(&EXPRESSION_LIST_START) {
            let list = self.parse_expression_list()?;
            let right_paren: Symbol = self.expect_token(&TokenKind::RightParen)?.into();
            ast::Expression::InExpressionList {
                test_expression: Box::new(test_expression),
                in_kw,
                not_kw,
                left_paren,
                list,
                right_paren,
            }
        } else {
            return self.parse_error(ParseErrorType::ExpectedSubqueryOrExpressionList);
        };

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

    fn parse_like_expression(
        &mut self,
        match_expression: ast::Expression,
        not_kw: Option<Keyword>,
        like_kw: Keyword,
    ) -> Result<ast::Expression, ParseError<'a>> {
        let expr = self.parse_expression(Precedence::Lowest)?;

        Ok(ast::Expression::Like {
            match_expression: Box::new(match_expression),
            not_kw,
            like_kw,
            pattern: Box::new(expr),
        })
    }

    fn parse_case_expression(&mut self) -> Result<ast::Expression, ParseError<'a>> {
        let case_kw = self.consume_keyword(TokenKind::Case)?;
        if self.token_is(&TokenKind::When) {
            let conditions = self.parse_case_expressions()?;
            let end_kw = self.consume_keyword(TokenKind::End)?;
            Ok(ast::Expression::SearchedCase {
                case_kw,
                conditions,
                end_kw,
            })
        } else {
            let input_expr = self.parse_expression(Precedence::Lowest)?;
            let conditions = self.parse_case_expressions()?;
            let end_kw = self.consume_keyword(TokenKind::End)?;
            Ok(ast::Expression::SimpleCase {
                case_kw,
                input_expression: Box::new(input_expr),
                conditions,
                end_kw,
            })
        }
    }

    fn parse_case_expressions(&mut self) -> Result<Vec<ast::CaseCondition>, ParseError<'a>> {
        let mut conditions = vec![];

        loop {
            let when_kw = self.consume_keyword(TokenKind::When)?;
            let when_expr = self.parse_expression(Precedence::Lowest)?;
            let then_kw = self.consume_keyword(TokenKind::Then)?;
            let result_expr = self.parse_expression(Precedence::Lowest)?;

            conditions.push(ast::CaseCondition::WhenCondition {
                when_kw,
                when_expression: when_expr,
                then_kw,
                result_expression: result_expr,
            });

            if let Some(else_kw) = self.maybe_keyword(TokenKind::Else) {
                let result_expr = self.parse_expression(Precedence::Lowest)?;
                conditions.push(ast::CaseCondition::ElseCondition {
                    else_kw,
                    result_expression: result_expr,
                });
                break;
            } else if self.token_is(&TokenKind::End) {
                break;
            }
        }

        Ok(conditions)
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

            self.unexpected_token(vec![TokenKind::LeftParen.string_type().to_string()])?
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
        } else if self.token_is(&TokenKind::Exists) {
            let exists_kw = self.consume_keyword(TokenKind::Exists)?;
            let subquery = self.parse_subquery()?;
            return Ok(ast::Expression::Exists {
                exists_kw,
                subquery: Box::new(subquery),
            });
        } else if self.token_is(&TokenKind::Case) {
            let case_expr = self.parse_case_expression()?;
            return Ok(case_expr);
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
            if let Some(kw) = self.maybe_keyword(TokenKind::All) {
                let subquery = self.parse_subquery()?;
                return Ok(ast::Expression::All {
                    all_kw: kw,
                    scalar_expression: Box::new(left),
                    comparison_op: op,
                    subquery: Box::new(subquery),
                });
            } else if let Some(kw) = self.maybe_keyword(TokenKind::Some) {
                let subquery = self.parse_subquery()?;
                return Ok(ast::Expression::Some {
                    some_kw: kw,
                    scalar_expression: Box::new(left),
                    comparison_op: op,
                    subquery: Box::new(subquery),
                });
            } else if let Some(kw) = self.maybe_keyword(TokenKind::Any) {
                let subquery = self.parse_subquery()?;
                return Ok(ast::Expression::Any {
                    any_kw: kw,
                    scalar_expression: Box::new(left),
                    comparison_op: op,
                    subquery: Box::new(subquery),
                });
            } else {
                let right = self.parse_expression(precedence)?;

                return Ok(ast::Expression::Comparison {
                    operator: op,
                    left: Box::new(left),
                    right: Box::new(right),
                });
            }
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
            let right = self.parse_expression(precedence)?;

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
        } else if self.token_is(&TokenKind::Like) {
            let like_kw = self.consume_keyword(TokenKind::Like)?;
            return Ok(self.parse_like_expression(left, None, like_kw)?);
        } else if self.token_is(&TokenKind::Not) {
            let not_kw = self.consume_keyword(TokenKind::Not)?;
            if let Some(in_kw) = self.maybe_keyword(TokenKind::In) {
                return Ok(self.parse_in_expression(left, in_kw, Some(not_kw))?);
            } else if let Some(between_kw) = self.maybe_keyword(TokenKind::Between) {
                return Ok(self.parse_between_expression(left, Some(not_kw), between_kw)?);
            } else if let Some(like_kw) = self.maybe_keyword(TokenKind::Like) {
                return Ok(self.parse_like_expression(left, Some(not_kw), like_kw)?);
            } else {
                return self.parse_error(ParseErrorType::ExpectedSubqueryOrExpressionList);
            }
        }

        self.unexpected_token(vec!["expression".to_string()])
    }
}
