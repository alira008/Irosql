use std::fmt;

use clap::{Args, ValueEnum};

#[derive(ValueEnum, Debug, Clone, Copy)]
pub enum IndentCommaLists {
    TrailingComma,
    SpaceAfterComma,
}

#[derive(Clone, Copy, Debug, ValueEnum)]
pub enum KeywordCase {
    Upper,
    Lower,
}

impl fmt::Display for KeywordCase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            KeywordCase::Upper => f.write_str("upper"),
            KeywordCase::Lower => f.write_str("lower"),
        }
    }
}

#[derive(Args, Clone, Debug, Copy)]
pub struct FormatterSettings {
    pub indent_comma_lists: Option<IndentCommaLists>,
    pub indent_in_lists: bool,
    pub indent_between_conditions: bool,
    pub keyword_case: KeywordCase,
    pub max_width: u32,
    pub indent_width: u32,
    pub use_tab: bool,
}
