use sql_lexer::{Lexer, Span};
use sql_parser::ast::{self, Keyword, KeywordKind};
use sql_parser::parser_new::Parser;

#[test]
fn basic_select_statement_new() {
    let input = "SELECT distInct all name, firstname, [dbo].lmao.bruhCalculate(bruh) from testtable";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    let mut select_statement = ast::SelectStatement::default();
    select_statement.select = Keyword::new(Span::new(0, 5), KeywordKind::Select);
    select_statement.distinct = Some(Keyword::new(Span::new(7, 14), KeywordKind::Distinct));
    select_statement.all = Some(Keyword::new(Span::new(16, 18), KeywordKind::All));
    select_statement.columns = vec![
        ast::SelectItem::Unnamed(ast::Expression::Identifier(ast::Literal {
            content: "name".to_string(),
            location: Span::new(20, 23),
        })),
        ast::SelectItem::Unnamed(ast::Expression::Identifier(ast::Literal {
            content: "firstname".to_string(),
            location: Span::new(26, 34),
        })),
        ast::SelectItem::Unnamed(ast::Expression::Function {
            name: Box::new(ast::FunctionName::User(ast::Expression::Compound(vec![
                ast::Expression::QuotedIdentifier(ast::Literal {
                    location: Span::new(37, 41),
                    content: "dbo".to_string(),
                }),
                ast::Expression::Identifier(ast::Literal {
                    location: Span::new(43, 46),
                    content: "lmao".to_string(),
                }),
                ast::Expression::Identifier(ast::Literal {
                    location: Span::new(48, 60),
                    content: "bruhCalculate".to_string(),
                }),
            ]))),
            args: Some(vec![ast::Expression::Identifier(ast::Literal {
                location: Span::new(62, 65),
                content: "bruh".to_string(),
            })]),
            over: None,
        }),
    ];
    select_statement.table = Some(ast::TableArg {
        from: Keyword::new(Span::new(68, 71), KeywordKind::From),
        table: ast::TableSource::Table {
            name: ast::Expression::Identifier(ast::Literal {
                content: "testtable".to_string(),
                location: Span::new(73, 81),
            }),
            is_as: false,
            alias: None,
        },
        joins: vec![],
    });
    let expected_query = ast::Query {
        statements: vec![ast::Statement::Select(select_statement)],
    };

    dbg!(query.to_string());

    assert_eq!(expected_query, query);
}
