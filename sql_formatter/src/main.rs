use clap::Parser;
use settings::{IndentCommaLists, KeywordCase};

mod settings;
mod formatter;

#[derive(Parser)]
#[command(name = "sql_formatter", version = "0.1.0", author = "Ariel")]
struct Cli {
    input: String,
    #[arg(short = 'c', long)]
    indent_comma_lists: Option<IndentCommaLists>,
    #[arg(short = 'i', long)]
    indent_in_lists: Option<bool>,
    #[arg(short = 'b', long)]
    ident_between_conditions: Option<bool>,
    #[arg(short, long)]
    keyword_case: Option<KeywordCase>,
    #[arg(short, long)]
    max_width: Option<u32>,
    #[arg(short = 'w', long)]
    indent_width: Option<u32>,
    #[arg(short, long)]
    use_tab: Option<bool>,
    #[arg(short, long)]
    tab_width: Option<u32>,
}

fn main() {
    let cli = Cli::parse();
    let mut formatter_settings_builder = settings::FormatterSettingsBuilder::new();
    if let Some(indent_comma_lists) = cli.indent_comma_lists {
        formatter_settings_builder =
            formatter_settings_builder.indent_comma_lists(indent_comma_lists);
    }
    if let Some(indent_in_lists) = cli.indent_in_lists {
        formatter_settings_builder = formatter_settings_builder.indent_in_lists(indent_in_lists);
    }
    if let Some(ident_between_conditions) = cli.ident_between_conditions {
        formatter_settings_builder =
            formatter_settings_builder.ident_between_conditions(ident_between_conditions);
    }
    if let Some(keyword_case) = cli.keyword_case {
        formatter_settings_builder = formatter_settings_builder.keyword_case(keyword_case);
    }
    if let Some(max_width) = cli.max_width {
        formatter_settings_builder = formatter_settings_builder.max_width(max_width);
    }
    if let Some(indent_width) = cli.indent_width {
        formatter_settings_builder = formatter_settings_builder.indent_width(indent_width);
    }
    if let Some(use_tab) = cli.use_tab {
        formatter_settings_builder = formatter_settings_builder.use_tab(use_tab);
    }
    if let Some(tab_width) = cli.tab_width {
        formatter_settings_builder = formatter_settings_builder.tab_width(tab_width);
    }
    let formatter_settings = formatter_settings_builder.build();

    let lexer = sql_parser::lexer::Lexer::new(&cli.input);
    let mut parser = sql_parser::Parser::new(lexer);
    let query = parser.parse();
    let output = String::new();
    for statement in query.statements {
        println!("{:#?}", statement);
    }
}
