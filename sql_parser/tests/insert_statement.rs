use sql_lexer::Lexer;
use sql_parser::Parser;

#[test]
fn insert_values_statement() {
    let input = r"INSERT INTO Cities (Location) VALUES ( cast(Point as float) )";
    let mut expected_query = String::from("insert into Cities (Location) values");
    expected_query += " (cast(Point as float))";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}

#[test]
fn insert_from_table_statement() {
    let input = r"insert into dbo.TestTable select c.* from Customer c where c.yearsmeasured 
    = 2009 and c.speed > 32";
    let mut expected_query = String::from("insert into dbo.TestTable select c.* from");
    expected_query += " Customer c where c.yearsmeasured = 2009 and c.speed > 32";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}
