const PROMPT: &str = ">> ";

pub fn start(stdin: &std::io::Stdin) {
    loop {
        print!("{}", PROMPT);
        let mut input = String::new();
        stdin.read_line(&mut input).unwrap();
        if input.trim() == "exit" {
            break;
        }
        let mut lexer = sql_parser::lexer::Lexer::new(&input);
        loop {
            let token = lexer.next_token();
            if token == sql_parser::token::TokenType::Eof {
                break;
            }
            println!("{:?}", token);
        }
    }
}
