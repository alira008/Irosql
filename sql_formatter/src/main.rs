use clap::Parser;
use settings::{IndentCommaLists, KeywordCase};

mod formatter;
mod settings;

#[derive(Parser)]
#[command(name = "sql_formatter", version = "0.1.0", author = "Ariel")]
struct Cli {
    input: String,
    #[arg(short = 'c', long)]
    indent_comma_lists: Option<IndentCommaLists>,
    #[arg(short = 'i', long, default_value_t = false)]
    indent_in_lists: bool,
    #[arg(short = 'b', long, default_value_t = false)]
    indent_between_conditions: bool,
    #[arg(short, long, default_value_t = KeywordCase::Upper)]
    keyword_case: KeywordCase,
    #[arg(short, long, default_value_t = 80)]
    max_width: u32,
    #[arg(short = 'w', long, default_value_t = 4)]
    indent_width: u32,
    #[arg(short, long, default_value_t = false)]
    use_tab: bool,
}

fn main() {
    let cli = Cli::parse();
    let formatter_settings = settings::FormatterSettings {
        indent_comma_lists: cli.indent_comma_lists,
        indent_in_lists: cli.indent_in_lists,
        indent_between_conditions: cli.indent_between_conditions,
        keyword_case: cli.keyword_case,
        max_width: cli.max_width,
        indent_width: cli.indent_width,
        use_tab: cli.use_tab,
    };
    let mut formatter = formatter::Formatter::new(formatter_settings);
    if let Err(e) = formatter.format(&cli.input) {
        eprintln!("Error: {}", e);
        return;
    }

    println!("{}", formatter.formatted_query());
}
