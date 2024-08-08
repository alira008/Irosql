use sql_lexer::Lexer;
use sql_parser::Parser;

#[test]
fn exec_statement() {
    let input = r"exec usp_test_func hello, 'yes', @testvar   , @CoolParam = 'yes'";
    let mut expected_query = String::from("exec usp_test_func hello, 'yes', @testvar");
    expected_query += ", @CoolParam = 'yes'";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}
