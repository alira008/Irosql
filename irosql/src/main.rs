use clap::Parser as _;
use cli::Cli;
use formatter::{formatter::Formatter, settings::FormatterSettings};

mod cli;

fn main() {
    let cli = Cli::parse();
    match cli.command {
        cli::Command::Format(format_args) => {
            let input = format_args.input.clone();
            let formatter_settings: FormatterSettings = format_args.into();
            let mut formatter = Formatter::new(formatter_settings);
            if let Err(e) = formatter.format(input.as_str()) {
                eprintln!("Error: {}", e);
                return;
            }

            println!("{}", formatter.formatted_query());
        }
    }
}
