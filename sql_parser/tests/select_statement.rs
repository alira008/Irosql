use sql_lexer::{Lexer, Span};
use sql_parser::ast::{self, Keyword, KeywordKind, Query};
use sql_parser::parser_new::Parser;

#[test]
fn basic_select_statement_new() {
    let input = "SELECT distInct all name, firstname from";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    let mut select_statement = ast::SelectStatement::default();
    select_statement.select = Keyword::new(Span::new(0, 5), KeywordKind::Select);
    select_statement.distinct = Some(Keyword::new(Span::new(7, 14), KeywordKind::Distinct));
    select_statement.columns = vec![
        ast::SelectItem::Unnamed(ast::Expression::Identifier("name".to_string())),
        ast::SelectItem::Unnamed(ast::Expression::Identifier("firstname".to_string())),
    ];
    select_statement.all = Some(Keyword::new(Span::new(16, 18), KeywordKind::All));
    let expected_query = ast::Query {
        statements: vec![ast::Statement::Select(select_statement)],
    };

    assert_eq!(expected_query, query);
}
