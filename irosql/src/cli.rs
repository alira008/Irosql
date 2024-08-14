use clap::{Parser, Subcommand};
use formatter::settings::{FormatterSettings, IndentCommaLists, KeywordCase};

#[derive(Parser, Debug, Clone)]
pub struct Format {
    pub input: String,
    #[arg(short = 'c', long)]
    pub indent_comma_lists: Option<IndentCommaLists>,
    #[arg(short = 'i', long, default_value_t = false)]
    pub indent_in_lists: bool,
    #[arg(short = 'b', long, default_value_t = false)]
    pub indent_between_conditions: bool,
    #[arg(short, long, default_value_t = KeywordCase::Upper)]
    pub keyword_case: KeywordCase,
    #[arg(short, long, default_value_t = 80)]
    pub max_width: u32,
    #[arg(short = 'w', long, default_value_t = 4)]
    pub indent_width: u32,
    #[arg(short, long, default_value_t = false)]
    pub use_tab: bool,
}

#[derive(Subcommand, Debug, Clone)]
pub enum Command {
    #[command(name = "format", about = "Formats T-SQL code in opionated style")]
    Format(Format),
}

#[derive(Parser, Debug, Clone)]
#[command(name = "irosql", version = "0.1.0", author = "Ariel")]
pub struct Cli {
    #[command(subcommand)]
    pub command: Command,
}

impl From<Format> for FormatterSettings {
    fn from(value: Format) -> Self {
        Self {
            indent_comma_lists: value.indent_comma_lists,
            indent_in_lists: value.indent_in_lists,
            indent_between_conditions: value.indent_between_conditions,
            keyword_case: value.keyword_case,
            max_width: value.max_width,
            indent_width: value.indent_width,
            use_tab: value.use_tab,
        }
    }
}
