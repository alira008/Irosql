use std::io::Write;

const PROMPT: &str = ">> ";

pub fn start(stdin: &std::io::Stdin) {
    loop {
        print!("{}", PROMPT);
        std::io::stdout().flush().unwrap();
        let mut input = String::new();
        stdin.read_line(&mut input).unwrap();
        if input.trim() == "exit" {
            break;
        }
        let mut lexer = sql_parser::lexer::Lexer::new(&input);
        loop {
            let token = lexer.next_token();
            if token.kind == sql_parser::token::Kind::Eof {
                break;
            }
            println!("{:?}", token);
        }
    }
}
