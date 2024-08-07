use sql_lexer::{Lexer, Span};
use sql_parser::ast::{self, Keyword, KeywordKind};
use sql_parser::parser_new::Parser;

#[test]
fn basic_select_statement_new() {
    let input =
        "SELECT distInct all name, firstname, [dbo].lmao.bruhCalculate(bruh) from testtable";
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

    assert_eq!(expected_query, query);
}

#[test]
fn basic_select_statement_new_no_spans() {
    let input =
        "SELECT distInct all name, firstname, [dbo].lmao.bruhCalculate(bruh) from testtable";
    let expected_query =
        "select distinct all name, firstname, [dbo].lmao.bruhCalculate(bruh) from testtable";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}

#[test]
fn select_statement_with_builtin_fn() {
    let input = r"SELECT   name, sum(lastprice) over(partition by symbol, insertdate
    order by inserttime dESc rows betWEEN UNBOunded prECEDing and cURRENT ROW), [dbo].
    lmao.bruhCalculate(bruh) as hello , yes from testtable";
    let mut expected_query = String::from("select name, sum(lastprice) over(partition by symbol,");
    expected_query += " insertdate order by inserttime desc rows between unbounded preceding ";
    expected_query +=
        "and current row), [dbo].lmao.bruhCalculate(bruh) as hello, yes from testtable";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}

#[test]
fn select_statement_with_builtin_fn_two() {
    let input = r"SELECT   name, sum(lastprice) over(partition by symbol, insertdate
    order by inserttime dESc range 242024 prECEDing), [dbo].
    lmao.bruhCalculate(bruh) as hello , yes from testtable";
    let mut expected_query = String::from("select name, sum(lastprice) over(partition by symbol,");
    expected_query += " insertdate order by inserttime desc range 242024 preceding";
    expected_query += "), [dbo].lmao.bruhCalculate(bruh) as hello, yes from testtable";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}

#[test]
fn select_statement_with_builtin_fn_three() {
    let input = r"SELECT   name, sum(lastprice) over(partition by symbol, insertdate
    order by inserttime dESc rows betWEEN 2 prECEDing and 242024 following), [dbo].
    lmao.bruhCalculate(bruh) as hello , yes from testtable";
    let mut expected_query = String::from("select name, sum(lastprice) over(partition by symbol,");
    expected_query += " insertdate order by inserttime desc rows between 2 preceding ";
    expected_query += "and 242024 following), [dbo].lmao.bruhCalculate(bruh) as hello, yes";
    expected_query += " from testtable";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}

#[test]
fn select_statement_with_top() {
    let input = r"SELECT distinct top 50 percent with ties  name, yes from testtable";
    let mut expected_query = String::from("select distinct top 50 percent with ties name");
    expected_query += ", yes from testtable";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}

#[test]
fn select_statement_with_top_two() {
    let input = r"SELECT all top 12 percent   name, yes from testtable";
    let mut expected_query = String::from("select all top 12 percent name");
    expected_query += ", yes from testtable";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}

#[test]
fn select_statement_with_top_three() {
    let input = r"SELECT top 213 with ties  name 'no', lastprice from testtable";
    let mut expected_query = String::from("select top 213 with ties name 'no'");
    expected_query += ", lastprice from testtable";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}

#[test]
fn select_statement_with_where_clause() {
    let input = r"SELECT Symbol, LastPrice, PC 'PercentChange' from MarketData where LastPrice
    > 20.0 or [PercentChange] > 5";
    let mut expected_query = String::from("select Symbol, LastPrice, PC 'PercentChange'");
    expected_query += " from MarketData where LastPrice > 20.0 or [PercentChange] > 5";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}

#[test]
fn select_statement_with_where_clause_two() {
    let input = r"SELECT Symbol, LastPrice, PC 'PercentChange' from MarketData where LastPrice
    > 20.0 and [PercentChange] > 5";
    let mut expected_query = String::from("select Symbol, LastPrice, PC 'PercentChange'");
    expected_query += " from MarketData where LastPrice > 20.0 and [PercentChange] > 5";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}

#[test]
fn select_statement_with_where_clause_three() {
    let input = r"SELECT Symbol, LastPrice, PC 'PercentChange' from MarketData where 
    symbol = 'amzn' and LastPrice > 20.0 or [PercentChange] > 5";
    let mut expected_query = String::from("select Symbol, LastPrice, PC 'PercentChange'");
    expected_query += " from MarketData where symbol = 'amzn' and LastPrice > 20.0 or";
    expected_query += " [PercentChange] > 5";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}

#[test]
fn select_statement_with_where_clause_three_with_cast() {
    let input = r"SELECT Symbol, LastPrice, PC 'PercentChange' from MarketData where 
    symbol = 'amzn' and LastPrice > 20.0 and cast(getdate() as date)";
    let mut expected_query = String::from("select Symbol, LastPrice, PC 'PercentChange'");
    expected_query += " from MarketData where symbol = 'amzn' and LastPrice > 20.0 and";
    expected_query += " cast(getdate() as date)";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}

#[test]
fn select_statement_with_where_clause_three_with_cast_two() {
    let input = r"SELECT Symbol, LastPrice, PC 'PercentChange' from MarketData where 
    symbol = 'amzn' and LastPrice > 20.0 and cast('444.3515' as float)";
    let mut expected_query = String::from("select Symbol, LastPrice, PC 'PercentChange'");
    expected_query += " from MarketData where symbol = 'amzn' and LastPrice > 20.0 and";
    expected_query += " cast('444.3515' as float)";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}
