use sql_parser::visitor::{walk_query, Visitor, VisitorResult};

use crate::settings::{FormatterSettings, KeywordCase};

pub struct Formatter {
    settings: FormatterSettings,
    indent_level: u32,
    formatted_query: String,
}

impl Formatter {
    pub fn new(settings: FormatterSettings) -> Self {
        let formatted_query = "".to_string();
        Self {
            settings,
            indent_level: 0,
            formatted_query,
        }
    }

    pub fn format(&mut self, input: &str) -> Result<(), String> {
        let lexer = sql_lexer::Lexer::new(input);
        let mut parser = sql_parser::Parser::new(lexer);
        let query = parser.parse();

        // walk the ast
        walk_query(self, &query);

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

    fn print_new_line(&mut self) {
        self.formatted_query.push_str("\n");
        self.print_indent();
    }

    fn print_select_column_comma(&mut self) {
        self.increase_indent();
        if let Some(indent_comma_lists) = self.settings.indent_comma_lists {
            match indent_comma_lists {
                crate::IndentCommaLists::TrailingComma => {
                    self.formatted_query.push_str(",");
                    self.print_new_line();
                }
                crate::IndentCommaLists::SpaceAfterComma => {
                    self.print_new_line();
                    self.formatted_query.push_str(", ");
                }
            }
        } else {
            self.print_new_line();
            self.formatted_query.push_str(",");
        }
        self.decrease_indent();
    }

    fn print_expression_list_comma(&mut self) {
        self.formatted_query.push_str(", ");
    }

    fn print_in_list_comma(&mut self) {
        if self.settings.indent_in_lists {
            self.print_select_column_comma();
        } else {
            self.formatted_query.push_str(", ");
        }
    }

    fn print_column_list_open_paren(&mut self) {
        self.increase_indent();
        self.formatted_query.push_str(&'('.to_string());
        if self.settings.indent_comma_lists.is_none() {
            self.print_new_line();
        }
        self.decrease_indent();
    }

    fn print_column_list_close_paren(&mut self) {
        self.increase_indent();
        if self.settings.indent_comma_lists.is_none() {
            self.print_new_line();
        }
        self.formatted_query.push_str(&')'.to_string());
        self.decrease_indent();
    }
}

impl Visitor for Formatter {
    type Result = ();

    fn visit_keyword(&mut self, keyword: &sql_parser::ast::Keyword) -> Self::Result {
        self.print_keyword(keyword.kind.to_string().as_str())
    }

    fn visit_literal(&mut self, literal: &sql_parser::ast::Literal) -> Self::Result {
        self.formatted_query += &literal.content;
    }
}
