use crate::comments::CommentMapper;
use sql_lexer::Span;
use sql_parser::{
    ast::{Comment, DataType, Expression, SelectItem, TableSource},
    visitor::Visitor,
};

use crate::settings::{FormatterSettings, IndentCommaLists, KeywordCase};

pub struct Formatter {
    settings: FormatterSettings,
    indent_level: u32,
    formatted_query: String,
    comment_map_before_line: Vec<(Span, Comment)>,
    comment_map_same_line: Vec<(Span, Comment)>,
}

impl Formatter {
    pub fn new(settings: FormatterSettings) -> Self {
        let formatted_query = "".to_string();
        Self {
            settings,
            indent_level: 0,
            formatted_query,
            comment_map_before_line: vec![],
            comment_map_same_line: vec![],
        }
    }

    pub fn format(&mut self, input: &str) -> Result<(), String> {
        let lexer = sql_lexer::Lexer::new(input);
        let mut parser = sql_parser::Parser::new(lexer);
        let query = parser.parse();
        let mut error_string = String::new();
        for (i, err) in parser.errors().iter().enumerate() {
            if i > 0 {
                error_string.push('\n');
            }
            error_string.push_str(format!("[{}]: {}", err.location(input), err.details()).as_str());
        }
        if !error_string.is_empty() {
            return Err(error_string);
        }

        let mut comment_mapper = CommentMapper::new(input, parser.comments());
        comment_mapper.map(&query);
        self.comment_map_before_line = comment_mapper.comment_map_before_line;
        self.comment_map_same_line = comment_mapper.comment_map_same_line;
        // walk the ast
        self.visit_query(&query);
        let last_char = self.formatted_query.pop();
        if last_char.is_some_and(|ch| ch != '\n') {
            self.formatted_query.push(last_char.unwrap());
        }

        Ok(())
    }

    pub fn formatted_query(&self) -> &str {
        &self.formatted_query
    }

    fn increase_indent(&mut self) {
        self.indent_level += 1;
    }

    fn decrease_indent(&mut self) {
        self.indent_level -= 1;
    }

    fn print_keyword(&mut self, keyword: &str) {
        match self.settings.keyword_case {
            KeywordCase::Upper => self.formatted_query.push_str(&keyword.to_uppercase()),
            KeywordCase::Lower => self.formatted_query.push_str(&keyword.to_lowercase()),
        }
    }

    fn print_indent(&mut self) {
        let indent_string = if self.settings.use_tab { "\t" } else { " " }
            .repeat(self.settings.indent_width as usize)
            .repeat(self.indent_level as usize);
        self.formatted_query.push_str(&indent_string);
    }

    fn print_space(&mut self) {
        self.formatted_query.push_str(" ");
    }

    fn print_new_line(&mut self) {
        if self
            .formatted_query
            .lines()
            .last()
            .is_some_and(|l| !l.trim().is_empty())
        {
            self.formatted_query.push_str("\n");
            self.print_indent();
        }
    }

    fn get_new_line_str(&self) -> String {
        let mut str = String::from("\n");
        let indent_string = if self.settings.use_tab { "\t" } else { " " }
            .repeat(self.settings.indent_width as usize)
            .repeat(self.indent_level as usize);
        str.push_str(&indent_string);
        str
    }

    fn print_select_column_comma(&mut self) {
        if let Some(indent_comma_lists) = self.settings.indent_comma_lists {
            match indent_comma_lists {
                IndentCommaLists::TrailingComma => {
                    self.formatted_query.push_str(",");
                    self.print_new_line();
                }
                IndentCommaLists::SpaceAfterComma => {
                    self.print_new_line();
                    self.formatted_query.push_str(", ");
                }
            }
        } else {
            self.print_new_line();
            self.formatted_query.push_str(",");
        }
    }

    fn print_in_list_comma(&mut self) {
        if self.settings.indent_in_lists {
            self.increase_indent();
            self.print_select_column_comma();
            self.decrease_indent();
        } else {
            self.formatted_query.push_str(", ");
        }
    }

    fn print_column_list_open_paren_symbol(&mut self, symbol: &sql_parser::ast::Symbol) {
        self.increase_indent();
        self.visit_symbol(symbol);
        if self.settings.indent_comma_lists.is_none() {
            self.print_new_line();
        }
        self.decrease_indent();
    }

    fn print_column_list_close_paren_symbol(&mut self, symbol: &sql_parser::ast::Symbol) {
        if self.settings.indent_comma_lists.is_none() {
            self.print_new_line();
        }
        self.visit_symbol(symbol);
    }

    fn print_comments_before(&mut self, location: Span) {
        let mut comment_present = false;
        let comma_char = if self
            .formatted_query
            .chars()
            .last()
            .is_some_and(|ch| ch == ',')
        {
            self.formatted_query.pop()
        } else {
            None
        };
        for (_, comment) in self
            .comment_map_before_line
            .iter()
            .filter(|(s, _)| *s == location)
        {
            if self
                .formatted_query
                .lines()
                .last()
                .is_some_and(|l| !l.trim().is_empty())
            {
                self.formatted_query += self.get_new_line_str().as_str();
            }
            self.formatted_query += "-- ";
            self.formatted_query += &comment.content;
            comment_present = true;
        }
        if comment_present {
            self.formatted_query += self.get_new_line_str().as_str();
        }
        if let Some(comma_char) = comma_char {
            self.formatted_query.push(comma_char);
        }
    }

    fn print_comments_same_line(&mut self, location: Span) {
        for (span, comment) in self.comment_map_same_line.iter() {
            if *span == location {
                self.formatted_query += " -- ";
                self.formatted_query += &comment.content;
                self.formatted_query += self.get_new_line_str().as_str();
            }
        }
    }
}

macro_rules! walk_list_two {
    ($visitor: expr, $method: ident, $list: expr, $before_visit: stmt) => {
        for element in $list.iter() {
            $before_visit
            $visitor.$method(element);
        }
    };
}

macro_rules! walk_opt_two {
    ($visitor: expr, $method: ident, $opt: expr, $before_visit: stmt) => {
        if let Some(o) = $opt {
            $before_visit
            $visitor.$method(o);
        }
    };
}

impl Visitor for Formatter {
    type Result = ();

    fn visit_symbol_kind(&mut self, kind: sql_parser::ast::SymbolKind) -> Self::Result {
        self.formatted_query += kind.to_string().as_str();
    }

    fn visit_symbol(&mut self, symbol: &sql_parser::ast::Symbol) -> Self::Result {
        self.print_comments_before(symbol.location);
        self.visit_symbol_kind(symbol.kind);
        self.print_comments_same_line(symbol.location);
    }

    fn visit_keyword_kind(&mut self, kind: sql_parser::ast::KeywordKind) -> Self::Result {
        self.print_keyword(kind.to_string().as_str())
    }

    fn visit_keyword(&mut self, keyword: &sql_parser::ast::Keyword) -> Self::Result {
        self.print_comments_before(keyword.location);
        self.visit_keyword_kind(keyword.kind);
        self.print_comments_same_line(keyword.location);
    }

    fn visit_literal(&mut self, literal: &sql_parser::ast::Literal) -> Self::Result {
        self.formatted_query += &literal.content;
        self.visit_span(&literal.location);
    }

    fn visit_comparison_operator_kind(
        &mut self,
        kind: sql_parser::ast::ComparisonOperatorKind,
    ) -> Self::Result {
        self.formatted_query += kind.to_string().as_str();
    }

    fn visit_arithmetic_operator_kind(
        &mut self,
        kind: sql_parser::ast::ArithmeticOperatorKind,
    ) -> Self::Result {
        self.formatted_query += kind.to_string().as_str();
    }

    fn visit_unary_operator_kind(
        &mut self,
        kind: sql_parser::ast::UnaryOperatorKind,
    ) -> Self::Result {
        self.formatted_query += kind.to_string().as_str();
    }

    fn visit_query(&mut self, query: &sql_parser::ast::Query) -> Self::Result {
        for (i, s) in query.statements.iter().enumerate() {
            if i > 0 {
                self.print_new_line();
                self.print_new_line();
            }
            self.visit_statement(s);
        }
    }

    fn visit_data_type_numeric_size(&mut self, ns: &sql_parser::ast::NumericSize) -> Self::Result {
        self.visit_symbol(&ns.left_paren);
        self.formatted_query += ns.precision.to_string().as_str();
        if let Some(n) = ns.scale {
            self.formatted_query += ", ";
            self.formatted_query += n.to_string().as_str();
        }
        self.visit_symbol(&ns.right_paren);
    }

    fn visit_data_type_size(
        &mut self,
        data_type_size: &sql_parser::ast::DataTypeSize,
    ) -> Self::Result {
        self.visit_symbol(&data_type_size.left_paren);
        self.formatted_query += data_type_size.size.to_string().as_str();
        self.visit_symbol(&data_type_size.right_paren);
    }

    fn visit_data_type(&mut self, data_type: &sql_parser::ast::DataType) -> Self::Result {
        match data_type {
            DataType::Int(k)
            | DataType::BigInt(k)
            | DataType::TinyInt(k)
            | DataType::SmallInt(k)
            | DataType::Datetime(k)
            | DataType::Time(k)
            | DataType::Real(k)
            | DataType::Date(k)
            | DataType::Bit(k) => self.visit_keyword(&k),
            DataType::Decimal(k, ns) | DataType::Numeric(k, ns) => {
                self.visit_keyword(&k);
                if let Some(ns) = ns {
                    self.visit_data_type_numeric_size(ns);
                }
            }
            DataType::Float(k, n) | DataType::Varchar(k, n) => {
                self.visit_keyword(&k);
                if let Some(n) = n {
                    self.visit_data_type_size(n);
                }
            }
        }
    }

    fn visit_statement(&mut self, stmt: &sql_parser::ast::Statement) -> Self::Result {
        match stmt {
            sql_parser::ast::Statement::Select(s) => self.visit_select_statement(s),
            sql_parser::ast::Statement::Insert(i) => self.visit_insert_statement(i),
            sql_parser::ast::Statement::Update(_) => unimplemented!(),
            sql_parser::ast::Statement::Delete(_) => unimplemented!(),
            sql_parser::ast::Statement::CTE {
                with_kw,
                ctes,
                statement,
            } => {
                self.visit_keyword(with_kw);
                self.print_space();
                self.increase_indent();
                for (i, cte) in ctes.iter().enumerate() {
                    if i > 0 {
                        self.print_select_column_comma();
                    }
                    self.visit_common_table_expression(cte);
                }
                self.decrease_indent();
                self.print_new_line();
                self.print_new_line();
                self.visit_common_table_expression_statement(statement);
            }
            sql_parser::ast::Statement::Declare {
                declare_kw,
                variables,
                semicolon,
            } => {
                self.visit_keyword(declare_kw);
                self.print_space();
                self.increase_indent();
                for (i, var) in variables.iter().enumerate() {
                    if i > 0 {
                        self.print_select_column_comma();
                    }
                    self.visit_local_variable(var);
                }
                self.decrease_indent();
                self.visit_symbol(semicolon);
            }
            sql_parser::ast::Statement::SetLocalVariable {
                set_kw,
                name,
                equal_sign,
                value,
                semicolon,
            } => {
                self.visit_keyword(set_kw);
                self.print_space();
                self.visit_expression(name);
                self.print_space();
                self.visit_symbol(equal_sign);
                self.print_space();
                self.visit_expression(value);
                self.visit_symbol(semicolon);
            }
            sql_parser::ast::Statement::Execute {
                exec_kw,
                procedure_name,
                parameters,
            } => {
                self.visit_keyword(exec_kw);
                self.print_space();
                self.visit_expression(procedure_name);
                self.print_space();
                for (i, p) in parameters.iter().enumerate() {
                    if i > 0 {
                        self.formatted_query += ", ";
                    }
                    self.visit_execute_statement_procedure_parameter(p);
                }
            }
        }
    }

    fn visit_common_table_expression(
        &mut self,
        cte: &sql_parser::ast::CommonTableExpression,
    ) -> Self::Result {
        self.visit_expression(&cte.name);
        self.print_space();
        if let Some(columns) = &cte.columns {
            self.print_column_list_open_paren_symbol(&columns.left_paren);
            for (i, column) in columns.items.iter().enumerate() {
                if i > 0 {
                    self.print_in_list_comma();
                }
                self.visit_expression(column);
            }
            self.print_column_list_close_paren_symbol(&columns.right_paren);
        }
        self.visit_keyword(&cte.as_kw);
        self.visit_symbol(&cte.left_paren);
        self.increase_indent();
        self.print_new_line();
        self.visit_select_statement(&cte.query);
        self.decrease_indent();
        self.print_new_line();
        self.visit_symbol(&cte.right_paren);
    }

    fn visit_local_variable(
        &mut self,
        local_variable: &sql_parser::ast::LocalVariable,
    ) -> Self::Result {
        self.visit_expression(&local_variable.name);
        self.print_space();
        self.visit_data_type(&local_variable.data_type);
        if let Some(value) = &local_variable.value {
            self.print_space();
            self.visit_symbol(&value.0);
            self.print_space();
            self.visit_expression(&value.1);
        }
    }

    fn visit_execute_statement_procedure_parameter_name(
        &mut self,
        name: &sql_parser::ast::ProcedureParameterName,
    ) -> Self::Result {
        self.formatted_query += "@";
        self.formatted_query += name.content.as_str();
    }

    fn visit_execute_statement_procedure_parameter(
        &mut self,
        param: &sql_parser::ast::ProcedureParameter,
    ) -> Self::Result {
        if let Some(name) = &param.name {
            self.visit_execute_statement_procedure_parameter_name(&name.0);
            self.print_space();
            self.visit_symbol(&name.1);
            self.print_space();
        }
        self.visit_expression(&param.value);
    }

    fn visit_common_table_expression_statement(
        &mut self,
        stmt: &sql_parser::ast::CommonTableExpressionStatement,
    ) -> Self::Result {
        match stmt {
            sql_parser::ast::CommonTableExpressionStatement::Select(s) => {
                self.visit_select_statement(s)
            }
            sql_parser::ast::CommonTableExpressionStatement::Insert(i) => {
                self.visit_insert_statement(i)
            }
        }
    }

    fn visit_select_statement(&mut self, stmt: &sql_parser::ast::SelectStatement) -> Self::Result {
        self.visit_keyword(&stmt.select);
        walk_opt_two!(self, visit_keyword, &stmt.distinct, self.print_space());
        walk_opt_two!(self, visit_keyword, &stmt.all, self.print_space());
        walk_opt_two!(self, visit_top_clause, &stmt.top, self.print_space());
        if stmt.columns.len() == 1 {
            self.print_space();
        } else {
            self.increase_indent();
            self.print_new_line();
        }
        for (i, select_item) in stmt.columns.iter().enumerate() {
            if i > 0 {
                self.print_select_column_comma();
            }
            self.visit_select_item(select_item);
        }
        if stmt.columns.len() > 1 {
            self.decrease_indent();
        }
        walk_opt_two!(self, visit_table_clause, &stmt.table, self.print_new_line());
        walk_opt_two!(
            self,
            visit_where_clause,
            &stmt.where_clause,
            self.print_new_line()
        );
        walk_opt_two!(
            self,
            visit_group_by_clause,
            &stmt.group_by,
            self.print_new_line()
        );
        walk_opt_two!(
            self,
            visit_having_clause,
            &stmt.having,
            self.print_new_line()
        );
        walk_opt_two!(
            self,
            visit_order_by_clause,
            &stmt.order_by,
            self.print_new_line()
        );
    }

    fn visit_select_item(&mut self, select_item: &sql_parser::ast::SelectItem) -> Self::Result {
        match select_item {
            SelectItem::Wildcard(s) => self.visit_symbol(s),
            SelectItem::Unnamed(e) => self.visit_expression(e),
            SelectItem::WithAlias {
                expression,
                as_kw,
                alias,
            } => {
                self.visit_expression(expression);
                walk_opt_two!(self, visit_keyword, as_kw, self.print_space());
                self.print_space();
                self.visit_expression(alias);
            }
            SelectItem::WildcardWithAlias {
                expression,
                as_kw,
                alias,
            } => {
                self.visit_expression(expression);
                walk_opt_two!(self, visit_keyword, as_kw, self.print_space());
                self.print_space();
                self.visit_expression(alias);
            }
            SelectItem::ReverseAliasAssign { alias, expression } => {
                self.visit_expression(alias);
                self.print_space();
                self.formatted_query += "=";
                self.print_space();
                self.visit_expression(expression)
            }
        }
    }

    fn visit_expression(&mut self, expr: &sql_parser::ast::Expression) -> Self::Result {
        match expr {
            Expression::Asterisk(s) => self.visit_symbol(s),
            Expression::Identifier(l) => {
                self.print_comments_before(l.location);
                self.visit_literal(l);
                self.print_comments_same_line(l.location);
            }
            Expression::QuotedIdentifier(l) => {
                self.print_comments_before(l.location);
                self.formatted_query += "[";
                self.visit_literal(l);
                self.formatted_query += "]";
                self.print_comments_same_line(l.location);
            }
            Expression::StringLiteral(l) => {
                self.print_comments_before(l.location);
                self.formatted_query += "'";
                self.visit_literal(l);
                self.formatted_query += "'";
                self.print_comments_same_line(l.location);
            }
            Expression::NumberLiteral(l) => {
                self.print_comments_before(l.location);
                self.visit_literal(l);
                self.print_comments_same_line(l.location);
            }
            Expression::LocalVariable(l) => {
                self.print_comments_before(l.location);
                self.formatted_query += "@";
                self.visit_literal(l);
                self.print_comments_same_line(l.location);
            }
            Expression::Keyword(k) => self.visit_keyword(&k),
            Expression::Compound(e) => {
                for (i, expr) in e.iter().enumerate() {
                    if i > 0 {
                        self.formatted_query += "."
                    }
                    self.visit_expression(expr);
                }
            }
            Expression::Arithmetic {
                operator,
                left,
                right,
            } => {
                self.visit_expression(left);
                self.print_space();
                self.visit_arithmetic_operator(operator);
                self.print_space();
                self.visit_expression(right)
            }
            Expression::And {
                and_kw,
                left,
                right,
            } => {
                self.visit_expression(left);
                self.print_new_line();
                self.visit_keyword(and_kw);
                self.print_space();
                self.visit_expression(right);
            }
            Expression::Or { or_kw, left, right } => {
                self.visit_expression(left);
                self.print_space();
                self.visit_keyword(or_kw);
                self.print_space();
                self.visit_expression(right)
            }
            Expression::Comparison {
                operator,
                left,
                right,
            } => {
                self.visit_expression(left);
                self.print_space();
                self.visit_comparison_operator(operator);
                self.print_space();
                self.visit_expression(right)
            }
            Expression::Unary { operator, right } => {
                self.visit_unary_operator(operator);
                self.visit_expression(right)
            }
            Expression::Function {
                name,
                left_paren,
                args,
                right_paren,
                over,
            } => {
                self.visit_function_name(name);
                self.visit_symbol(left_paren);
                if let Some(args) = args {
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            self.formatted_query += ", ";
                        }
                        self.visit_expression(arg);
                    }
                }
                self.visit_symbol(right_paren);
                self.increase_indent();
                walk_opt_two!(
                    self,
                    visit_function_over_clause,
                    over,
                    self.print_new_line()
                );
                self.decrease_indent();
            }
            Expression::Cast {
                cast_kw,
                left_paren,
                expression,
                as_kw,
                data_type,
                right_paren,
            } => {
                self.visit_keyword(cast_kw);
                self.visit_symbol(left_paren);
                self.visit_expression(expression);
                self.print_space();
                self.visit_keyword(as_kw);
                self.print_space();
                self.visit_data_type(data_type);
                self.visit_symbol(right_paren);
            }
            Expression::InExpressionList {
                test_expression,
                in_kw,
                not_kw,
                left_paren,
                list,
                right_paren,
            } => {
                self.visit_expression(test_expression);
                self.print_space();
                self.visit_keyword(in_kw);
                walk_opt_two!(self, visit_keyword, not_kw, self.print_space());
                self.print_space();
                self.print_column_list_open_paren_symbol(left_paren);
                for (i, item) in list.iter().enumerate() {
                    if i > 0 {
                        self.print_in_list_comma();
                    }
                    self.visit_expression(item);
                }
                self.print_column_list_close_paren_symbol(right_paren);
            }
            Expression::InSubquery {
                test_expression,
                in_kw,
                not_kw,
                subquery,
            } => {
                self.visit_expression(test_expression);
                self.print_space();
                self.visit_keyword(in_kw);
                walk_opt_two!(self, visit_keyword, not_kw, self.print_space());
                self.print_space();
                self.visit_expression(subquery);
            }
            Expression::Subquery {
                left_paren,
                select_statement,
                right_paren,
            } => {
                self.visit_symbol(left_paren);
                self.increase_indent();
                self.print_new_line();
                self.visit_select_statement(select_statement);
                self.decrease_indent();
                self.print_new_line();
                self.visit_symbol(right_paren);
            }
            Expression::Between {
                test_expression,
                not_kw,
                between_kw,
                begin,
                and_kw,
                end,
            } => {
                self.visit_expression(test_expression);
                walk_opt_two!(self, visit_keyword, not_kw, self.print_space());
                self.print_space();
                self.visit_keyword(between_kw);
                self.print_space();
                self.visit_expression(begin);
                self.increase_indent();
                self.print_new_line();
                self.visit_keyword(and_kw);
                self.print_space();
                self.visit_expression(end);
                self.decrease_indent();
            }
            Expression::Not { not_kw, expression } => {
                self.visit_keyword(not_kw);
                self.print_space();
                self.visit_expression(expression)
            }
            Expression::Exists {
                exists_kw,
                subquery,
            } => {
                self.visit_keyword(exists_kw);
                self.print_space();
                self.visit_expression(subquery);
            }
            Expression::All {
                all_kw,
                scalar_expression,
                comparison_op,
                subquery,
            } => {
                self.visit_keyword(all_kw);
                self.print_space();
                self.visit_expression(scalar_expression);
                self.print_space();
                self.visit_comparison_operator(comparison_op);
                self.print_space();
                self.visit_expression(subquery)
            }
            Expression::Some {
                some_kw,
                scalar_expression,
                comparison_op,
                subquery,
            } => {
                self.visit_keyword(some_kw);
                self.print_space();
                self.visit_expression(scalar_expression);
                self.print_space();
                self.visit_comparison_operator(comparison_op);
                self.print_space();
                self.visit_expression(subquery)
            }
            Expression::Any {
                any_kw,
                scalar_expression,
                comparison_op,
                subquery,
            } => {
                self.visit_keyword(any_kw);
                self.print_space();
                self.visit_expression(scalar_expression);
                self.print_space();
                self.visit_comparison_operator(comparison_op);
                self.print_space();
                self.visit_expression(subquery)
            }
            Expression::Like {
                match_expression,
                not_kw,
                like_kw,
                pattern,
            } => {
                self.visit_expression(match_expression);
                walk_opt_two!(self, visit_keyword, not_kw, self.print_space());
                self.print_space();
                self.visit_keyword(like_kw);
                self.print_space();
                self.visit_expression(pattern)
            }
            Expression::SimpleCase {
                case_kw,
                input_expression,
                conditions,
                end_kw,
            } => {
                self.visit_keyword(case_kw);
                self.print_space();
                self.visit_expression(input_expression);
                // for (i, c) in conditions.iter().enumerate() {
                //    self.visit_case_condition(c);
                // }
                self.increase_indent();
                walk_list_two!(
                    self,
                    visit_case_condition,
                    conditions,
                    self.print_new_line()
                );
                self.print_new_line();
                self.visit_keyword(end_kw);
                self.decrease_indent();
            }
            Expression::SearchedCase {
                case_kw,
                conditions,
                end_kw,
            } => {
                self.visit_keyword(case_kw);
                self.print_space();
                // for (i, c) in conditions.iter().enumerate() {
                //    self.visit_case_condition(c);
                // }
                self.increase_indent();
                walk_list_two!(
                    self,
                    visit_case_condition,
                    conditions,
                    self.print_new_line()
                );
                self.print_new_line();
                self.visit_keyword(end_kw);
                self.decrease_indent();
            }
        }
    }

    fn visit_case_condition(
        &mut self,
        case_condition: &sql_parser::ast::CaseCondition,
    ) -> Self::Result {
        match case_condition {
            sql_parser::ast::CaseCondition::WhenCondition {
                when_kw,
                when_expression,
                then_kw,
                result_expression,
            } => {
                self.visit_keyword(when_kw);
                self.print_space();
                self.visit_expression(when_expression);
                self.increase_indent();
                self.print_new_line();
                self.visit_keyword(then_kw);
                self.print_space();
                self.visit_expression(result_expression);
                self.decrease_indent();
            }
            sql_parser::ast::CaseCondition::ElseCondition {
                else_kw,
                result_expression,
            } => {
                self.visit_keyword(else_kw);
                self.print_space();
                self.visit_expression(result_expression);
            }
        }
    }

    fn visit_where_clause(&mut self, where_clause: &sql_parser::ast::WhereClause) -> Self::Result {
        self.increase_indent();
        self.visit_keyword(&where_clause.where_kw);
        self.print_space();
        self.visit_expression(&where_clause.expression);
        self.decrease_indent();
    }

    fn visit_having_clause(
        &mut self,
        having_clause: &sql_parser::ast::HavingClause,
    ) -> Self::Result {
        self.increase_indent();
        self.visit_keyword(&having_clause.having_kw);
        self.print_space();
        self.visit_expression(&having_clause.expression);
        self.decrease_indent();
    }

    fn visit_group_by_clause(
        &mut self,
        group_by_clause: &sql_parser::ast::GroupByClause,
    ) -> Self::Result {
        self.increase_indent();
        for (i, kw) in group_by_clause.group_by_kws.iter().enumerate() {
            if i > 0 {
                self.print_space();
            }
            self.visit_keyword(kw);
        }
        self.print_space();
        for (i, e) in group_by_clause.expressions.iter().enumerate() {
            if i > 0 {
                self.formatted_query += ", "
            }
            self.visit_expression(e);
        }
        self.decrease_indent();
    }

    fn visit_order_by_clause(
        &mut self,
        order_by_clause: &sql_parser::ast::OrderByClause,
    ) -> Self::Result {
        self.increase_indent();
        for (i, kw) in order_by_clause.order_by_kws.iter().enumerate() {
            if i > 0 {
                self.print_space();
            }
            self.visit_keyword(kw);
        }
        self.print_space();
        for (i, arg) in order_by_clause.expressions.iter().enumerate() {
            if i > 0 {
                self.formatted_query += ", "
            }
            self.visit_order_by_arg(arg);
        }
        self.decrease_indent();
    }

    fn visit_order_by_arg(&mut self, order_by_arg: &sql_parser::ast::OrderByArg) -> Self::Result {
        self.visit_expression(&order_by_arg.column);
        walk_opt_two!(
            self,
            visit_keyword,
            &order_by_arg.order_kw,
            self.print_space()
        );
    }

    fn visit_table_clause(&mut self, table_clause: &sql_parser::ast::TableArg) -> Self::Result {
        self.visit_keyword(&table_clause.from);
        self.print_space();
        self.visit_table_source(&table_clause.table);
        for join in table_clause.joins.iter() {
            self.print_new_line();
            self.visit_table_join(join);
        }
    }

    fn visit_table_source(&mut self, table_source: &TableSource) -> Self::Result {
        match table_source {
            TableSource::Table { name, alias } => {
                self.visit_expression(name);
                walk_opt_two!(self, visit_expression, alias, self.print_space());
            }
            TableSource::Derived { query, alias } => {
                self.visit_expression(query);
                self.print_space();
                self.visit_expression(alias);
            }
            TableSource::TableValuedFunction { function, alias } => {
                self.visit_expression(function);
                walk_opt_two!(self, visit_expression, alias, self.print_space());
            }
        }
    }

    fn visit_table_join(&mut self, table_join: &sql_parser::ast::Join) -> Self::Result {
        for (i, kw) in table_join.join.iter().enumerate() {
            if i > 0 {
                self.print_space();
            }
            self.visit_keyword(kw);
        }
        self.print_space();
        self.visit_table_source(&table_join.table);
        walk_opt_two!(
            self,
            visit_table_join_condition,
            &table_join.condition,
            self.print_space()
        );
    }

    fn visit_table_join_condition(
        &mut self,
        table_join_condition: &sql_parser::ast::JoinCondition,
    ) -> Self::Result {
        self.visit_keyword(&table_join_condition.on_kw);
        self.print_space();
        self.visit_expression(&table_join_condition.condition);
    }

    fn visit_top_clause(&mut self, top_clause: &sql_parser::ast::Top) -> Self::Result {
        self.visit_keyword(&top_clause.top);
        self.print_space();
        self.visit_expression(&top_clause.quantity);
        if let Some(kw) = &top_clause.percent {
            self.print_space();
            self.visit_keyword(kw);
        }
        if let Some(kws) = &top_clause.with_ties {
            for kw in kws.iter() {
                self.print_space();
                self.visit_keyword(kw);
            }
        }
    }

    fn visit_function_over_clause(
        &mut self,
        over_clause: &sql_parser::ast::OverClause,
    ) -> Self::Result {
        self.visit_keyword(&over_clause.over_kw);
        self.print_space();
        self.visit_symbol(&over_clause.left_paren);
        if let Some(kws) = &over_clause.partition_by_kws {
            self.increase_indent();
            self.print_new_line();
            for (i, kw) in kws.iter().enumerate() {
                if i > 0 {
                    self.print_space();
                }
                self.visit_keyword(kw);
            }
            self.print_space();
            for (i, e) in over_clause.partition_by.iter().enumerate() {
                if i > 0 {
                    self.formatted_query += ", "
                }
                self.visit_expression(e);
            }
            self.decrease_indent();
        }
        if let Some(kws) = &over_clause.order_by_kws {
            self.increase_indent();
            self.print_new_line();
            for (i, kw) in kws.iter().enumerate() {
                if i > 0 {
                    self.print_space();
                }
                self.visit_keyword(kw);
            }
            self.print_space();
            for (i, o) in over_clause.order_by.iter().enumerate() {
                if i > 0 {
                    self.formatted_query += ", "
                }
                self.visit_order_by_arg(o);
            }
            self.decrease_indent();
        }
        if let Some(window_frame) = &over_clause.window_frame {
            self.increase_indent();
            self.print_new_line();
            self.visit_function_over_clause_window_frame(window_frame);
            self.decrease_indent();
        }
        if over_clause.partition_by_kws.is_some()
            || over_clause.order_by_kws.is_some()
            || over_clause.window_frame.is_some()
        {
            self.print_new_line();
        }
        self.visit_symbol(&over_clause.right_paren);
    }

    fn visit_function_over_clause_window_frame(
        &mut self,
        window_frame: &sql_parser::ast::WindowFrame,
    ) -> Self::Result {
        self.visit_keyword(&window_frame.rows_or_range_kw);
        self.print_space();
        if let Some(kw) = &window_frame.between_kw {
            self.visit_keyword(kw);
            self.print_space();
        }
        match &window_frame.start {
            sql_parser::ast::WindowFrameBound::Preceding(e)
            | sql_parser::ast::WindowFrameBound::Following(e) => {
                self.visit_expression(e);
                self.print_space();
            }
            _ => {}
        }
        for (i, kw) in window_frame.start_bound_keywords.iter().enumerate() {
            if i > 0 {
                self.print_space();
            }
            self.visit_keyword(kw);
        }
        if let Some(kw) = &window_frame.and_kw {
            self.print_space();
            self.visit_keyword(kw);
            self.print_space();
        }
        if let Some(end) = &window_frame.end {
            match end {
                sql_parser::ast::WindowFrameBound::Preceding(e)
                | sql_parser::ast::WindowFrameBound::Following(e) => {
                    self.visit_expression(e);
                    self.print_space();
                }
                _ => {}
            }
        }
        if let Some(kws) = &window_frame.end_bound_keywords {
            for (i, kw) in kws.iter().enumerate() {
                if i > 0 {
                    self.print_space();
                }
                self.visit_keyword(kw);
            }
        }
    }
}
