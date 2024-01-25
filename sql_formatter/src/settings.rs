use clap::{Args, ValueEnum};

#[derive(ValueEnum, Debug, Clone, Copy)]
pub enum IndentCommaLists {
    TrailingCommaWithSpaceAfter,
    TrailingComma,
    SpaceAfterComma,
}

#[derive(Clone, Copy, Debug, ValueEnum)]
pub enum KeywordCase {
    Upper,
    Lower,
    NoChange,
}

#[derive(Args, Clone, Debug, Copy)]
pub struct FormatterSettings {
    indent_comma_lists: Option<IndentCommaLists>,
    indent_in_lists: bool,
    ident_between_conditions: bool,
    keyword_case: KeywordCase,
    max_width: u32,
    indent_width: u32,
    use_tab: bool,
    tab_width: u32,
}

pub struct FormatterSettingsBuilder {
    indent_comma_lists: Option<IndentCommaLists>,
    indent_in_lists: Option<bool>,
    ident_between_conditions: Option<bool>,
    keyword_case: Option<KeywordCase>,
    max_width: Option<u32>,
    indent_width: Option<u32>,
    use_tab: Option<bool>,
    tab_width: Option<u32>,
}

impl FormatterSettingsBuilder {
    pub fn new() -> Self {
        FormatterSettingsBuilder {
            indent_comma_lists: None,
            indent_in_lists: None,
            ident_between_conditions: None,
            keyword_case: None,
            max_width: None,
            indent_width: None,
            use_tab: None,
            tab_width: None,
        }
    }

    pub fn indent_comma_lists(mut self, indent_comma_lists: IndentCommaLists) -> Self {
        self.indent_comma_lists = Some(indent_comma_lists);
        self
    }

    pub fn indent_in_lists(mut self, indent_in_lists: bool) -> Self {
        self.indent_in_lists = Some(indent_in_lists);
        self
    }

    pub fn ident_between_conditions(mut self, ident_between_conditions: bool) -> Self {
        self.ident_between_conditions = Some(ident_between_conditions);
        self
    }

    pub fn keyword_case(mut self, keyword_case: KeywordCase) -> Self {
        self.keyword_case = Some(keyword_case);
        self
    }

    pub fn max_width(mut self, max_width: u32) -> Self {
        self.max_width = Some(max_width);
        self
    }

    pub fn indent_width(mut self, indent_width: u32) -> Self {
        self.indent_width = Some(indent_width);
        self
    }

    pub fn use_tab(mut self, use_tab: bool) -> Self {
        self.use_tab = Some(use_tab);
        self
    }

    pub fn tab_width(mut self, tab_width: u32) -> Self {
        self.tab_width = Some(tab_width);
        self
    }

    pub fn build(self) -> FormatterSettings {
        FormatterSettings {
            indent_comma_lists: self.indent_comma_lists,
            indent_in_lists: self.indent_in_lists.unwrap_or(false),
            ident_between_conditions: self.ident_between_conditions.unwrap_or(false),
            keyword_case: self.keyword_case.unwrap_or(KeywordCase::Upper),
            max_width: self.max_width.unwrap_or(80),
            indent_width: self.indent_width.unwrap_or(4),
            use_tab: self.use_tab.unwrap_or(true),
            tab_width: self.tab_width.unwrap_or(4),
        }
    }
}

