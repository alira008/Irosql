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
        let lexer = sql_parser::lexer_new::Lexer::new(&input);
        let mut parser = sql_parser::parser_new::Parser::new(lexer);
        let query = parser.parse();
        // if parser.errors().len() > 0 {
        //     println!("Error parsing input: ");
        //     for error in parser.errors() {
        //         println!("{}", error);
        //     }
        //             
        // } else {
        //     println!("{}", query);
        // }
        // loop {
        // let token = lexer.next_token();
        // if token.kind == sql_parser::token::Kind::Eof {
        //     break;
        // }
        //     println!("{:?}", token);
        // }
    }
}
