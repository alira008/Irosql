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
}
