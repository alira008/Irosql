use sql_lexer::{Lexer, Span};
use sql_parser::ast::{self, Keyword, KeywordKind};
use sql_parser::parser_new::Parser;

#[test]
fn exec_statement() {
    let input = r"exec usp_test_func hello, 'yes', @testvar";
    let expected_query = String::from("exec usp_test_func hello, 'yes', @testvar");
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}
