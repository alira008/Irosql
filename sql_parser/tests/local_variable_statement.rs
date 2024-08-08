use sql_lexer::Lexer;
use sql_parser::parser_new::Parser;

#[test]
fn declare_local_variable_statement() {
    let input = r"declare @Date Date = '1-2-24', @TestVar float = 3.14159;";
    let mut expected_query = String::from("declare @Date date = '1-2-24', @TestVar float");
    expected_query += " = 3.14159;";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}

#[test]
fn set_local_variable_statement() {
    let input = r"SET @MyCounter = 0;";
    let expected_query = String::from("set @MyCounter = 0;");
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}
