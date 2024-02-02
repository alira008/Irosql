use sql_parser::visitor::{walk_query, Visitor};

use crate::settings::{FormatterSettings, KeywordCase};

pub struct Formatter {
    settings: FormatterSettings,
    formatted_query: String,
}

impl Formatter {
    pub fn new(settings: FormatterSettings) -> Self {
        let formatted_query = "".to_string();
        Self {
            settings,
            formatted_query,
        }
    }

    pub fn format(&mut self, input: &str) {
        let lexer = sql_parser::lexer::Lexer::new(input);
        let mut parser = sql_parser::Parser::new(lexer);
        let query = parser.parse();

        // walk the ast
        walk_query(self, &query);
    }

    pub fn formatted_query(&self) -> &str {
        &self.formatted_query
    }

    fn print_keyword(&mut self, keyword: &str) {
        match self.settings.keyword_case {
            KeywordCase::Upper => self.formatted_query.push_str(&keyword.to_uppercase()),
            KeywordCase::Lower => self.formatted_query.push_str(&keyword.to_lowercase()),
            _ => self.formatted_query.push_str(&keyword),
        }
    }

    fn print_indent(&mut self) {
        let indent = self.settings.indent_width;
        if self.settings.use_tab {
            for _ in 0..indent {
                self.formatted_query.push_str("\t");
            }
        } else {
            for _ in 0..indent {
                self.formatted_query.push_str(" ");
            }
        }
    }
}

impl Visitor for Formatter {
    fn visit_token(&mut self, token: &sql_parser::token::Token) {
        match token.kind() {
            sql_parser::token::Kind::Keyword(_) => match self.settings.keyword_case {
                KeywordCase::Upper => {
                    self.formatted_query
                        .push_str(&token.literal().to_string().to_uppercase());
                }
                KeywordCase::Lower => {
                    self.formatted_query
                        .push_str(&token.literal().to_string().to_lowercase());
                }
                _ => self.formatted_query.push_str(&token.literal().to_string()),
            },
            _ => self.formatted_query.push_str(&token.literal().to_string()),
        }
    }

    fn visit_select_query(&mut self, query: &sql_parser::ast::SelectStatement) {
        self.print_keyword("SELECT ");
        self.visit_select_top_argument(&query.top);
        self.visit_select_columns(&query.columns);
        self.visit_select_into_table(&query.into_table);
        self.visit_select_table(&query.table);
        self.visit_select_where_clause(&query.where_clause);
        self.visit_select_group_by(&query.group_by);
        self.visit_select_having(&query.having);
        for order_by in &query.order_by {
            self.visit_select_order_by(order_by);
        }
        self.visit_select_offset(&query.offset);
        self.visit_select_fetch(&query.fetch);
    }

    fn visit_select_top_argument(&mut self, top: &Option<sql_parser::ast::TopArg>) {
        if let Some(top) = top {
            self.print_keyword("TOP ");
            self.visit_expression(&top.quantity);
            self.formatted_query.push_str(" ");
            if top.percent {
                self.print_keyword("PERCENT ");
            }
            if top.with_ties {
                self.print_keyword("WITH TIES ");
            }
        }
    }

    fn visit_select_columns(&mut self, columns: &[sql_parser::ast::SelectItem]) {
        for (i, column) in columns.iter().enumerate() {
            if i == 0 && columns.len() > 1 {
                self.formatted_query.push_str("\n");
                self.print_indent();
            }
            if i > 0 {
                if let Some(indent_comma_lists) = self.settings.indent_comma_lists {
                    match indent_comma_lists {
                        crate::IndentCommaLists::TrailingComma => {
                            self.formatted_query.push_str("\n");
                            self.print_indent();
                            self.formatted_query.push_str(",");
                        }
                        crate::IndentCommaLists::TrailingCommaWithSpaceAfter => {
                            self.formatted_query.push_str("\n");
                            self.print_indent();
                            self.formatted_query.push_str(", ");
                        }
                        crate::settings::IndentCommaLists::SpaceAfterComma => {
                            self.formatted_query.push_str(",\n");
                            self.print_indent();
                        }
                    }
                }
            }
            self.visit_select_item(column);
        }
        self.formatted_query.push_str(" ");
    }

    fn visit_select_item(&mut self, item: &sql_parser::ast::SelectItem) {
        match item {
            sql_parser::ast::SelectItem::Unnamed(expr) => {
                self.visit_expression(expr);
            }
            sql_parser::ast::SelectItem::WithAlias {
                expression,
                as_token,
                alias,
            } => {
                self.visit_expression(expression);
                if *as_token {
                    self.print_keyword(" AS ");
                }
                self.formatted_query.push_str(alias);
            }
            sql_parser::ast::SelectItem::WildcardWithAlias {
                expression,
                as_token,
                alias,
            } => {
                self.visit_expression(expression);
                if *as_token {
                    self.print_keyword(" AS ");
                }
                self.formatted_query.push_str(alias);
            }
            sql_parser::ast::SelectItem::Wildcard => {
                self.formatted_query.push_str("*");
            }
        }
    }
}
