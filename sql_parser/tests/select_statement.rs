use sql_lexer::{Lexer, Span};
use sql_parser::ast::{self, Keyword, KeywordKind};
use sql_parser::Parser;

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
fn basic_select_statement_reverse_assign_alias() {
    let input =
        "SELECT distInct all name, firstname = (select top 1 FirstName from Names), [dbo].lmao.bruhCalculate(bruh) from testtable";
    let expected_query =
        "select distinct all name, firstname = (select top 1 FirstName from Names), [dbo].lmao.bruhCalculate(bruh) from testtable";
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
fn select_statement_with_where_clause_four() {
    let input = r"SELECT Symbol, LastPrice, PC 'PercentChange' from MarketData where 
    symbol = 'amzn' and LastPrice > 20.0 or [PercentChange] + 10 > 5";
    let mut expected_query = String::from("select Symbol, LastPrice, PC 'PercentChange'");
    expected_query += " from MarketData where symbol = 'amzn' and LastPrice > 20.0 or";
    expected_query += " [PercentChange] + 10 > 5";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}

#[test]
fn select_statement_with_where_clause_five() {
    let input = r"SELECT Symbol, LastPrice, PC 'PercentChange' from MarketData where 
    symbol nOT In ('amzn', 'googl', 'zm')  and LastPrice > 20.0 or [PercentChange] + 10 > 5";
    let mut expected_query = String::from("select Symbol, LastPrice, PC 'PercentChange'");
    expected_query +=
        " from MarketData where symbol not in ('amzn', 'googl', 'zm') and LastPrice > 20.0 or";
    expected_query += " [PercentChange] + 10 > 5";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}

#[test]
fn select_statement_with_where_clause_six() {
    let input = r"SELECT Symbol, LastPrice, PC 'PercentChange' from MarketData where 
    symbol  In ('amzn', 'googl', 'zm')  and LastPrice > 20.0 or [PercentChange] + 10 > 5";
    let mut expected_query = String::from("select Symbol, LastPrice, PC 'PercentChange'");
    expected_query +=
        " from MarketData where symbol in ('amzn', 'googl', 'zm') and LastPrice > 20.0 or";
    expected_query += " [PercentChange] + 10 > 5";
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

#[test]
fn select_statement_with_group_by() {
    let input = r"SELECT Symbol, LastPrice, PC 'PercentChange' from MarketData group 
    by Symbol, Exchange";
    let mut expected_query = String::from("select Symbol, LastPrice, PC 'PercentChange'");
    expected_query += " from MarketData group by Symbol, Exchange";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}

#[test]
fn select_statement_with_having() {
    let input = r"SELECT Symbol, LastPrice, PC 'PercentChange' from MarketData having
    LastPrice > 32";
    let mut expected_query = String::from("select Symbol, LastPrice, PC 'PercentChange'");
    expected_query += " from MarketData having LastPrice > 32";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}

#[test]
fn select_statement_with_where_and_order_by() {
    let input = r"SELECT Symbol, LastPrice, PC 'PercentChange' from MarketData where Symbol =
    'amzn' and PercentChange > 2 order by QuoteTime, Symbol desc offset 4 rows fetch first
    50 row only";
    let mut expected_query = String::from("select Symbol, LastPrice, PC 'PercentChange'");
    expected_query += " from MarketData where Symbol = 'amzn' and PercentChange > 2";
    expected_query += " order by QuoteTime, Symbol desc offset 4 rows fetch first 50 row only";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}

#[test]
fn select_statement_with_where_and_order_by_two() {
    let input = r"SELECT Symbol, LastPrice, PC 'PercentChange' from MarketData where Symbol =
    'amzn' and PercentChange > 2 order by QuoteTime, Symbol desc offset 4 row ";
    let mut expected_query = String::from("select Symbol, LastPrice, PC 'PercentChange'");
    expected_query += " from MarketData where Symbol = 'amzn' and PercentChange > 2";
    expected_query += " order by QuoteTime, Symbol desc offset 4 row";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}

#[test]
fn select_statement_with_where_and_order_by_three() {
    let input = r"SELECT Symbol, LastPrice, PC 'PercentChange' from MarketData where Symbol =
    'amzn' and PercentChange > 2 order by QuoteTime, Symbol desc offset 4 row fetch next
    54 rows only";
    let mut expected_query = String::from("select Symbol, LastPrice, PC 'PercentChange'");
    expected_query += " from MarketData where Symbol = 'amzn' and PercentChange > 2";
    expected_query += " order by QuoteTime, Symbol desc offset 4 row fetch next 54 rows only";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}

#[test]
fn select_statement_with_subquery() {
    let input = r"SELECT Symbol, LastPrice, PercentChange, (select Top 1 Exchange from
    MarketIndices mi where mi.Symbol = m.Symbol) 'TopExchange', OpenPrice from Market m";
    let mut expected_query = String::from("select Symbol, LastPrice, PercentChange, (select ");
    expected_query += "top 1 Exchange from MarketIndices mi where mi.Symbol = m.Symbol) ";
    expected_query += "'TopExchange', OpenPrice from Market m";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}

#[test]
fn select_statement_with_where_and_in_subquery() {
    let input = r"SELECT Symbol, LastPrice, PercentChange, (select Top 1 Exchange from
    MarketIndices mi where mi.Symbol = m.Symbol) 'TopExchange', OpenPrice from Market m
    where Symbol in (select Symbol from MarketData where QuoteDate = cast('1-3-24' as date))";
    let mut expected_query = String::from("select Symbol, LastPrice, PercentChange, (select ");
    expected_query += "top 1 Exchange from MarketIndices mi where mi.Symbol = m.Symbol) ";
    expected_query += "'TopExchange', OpenPrice from Market m where Symbol in (select Symbol ";
    expected_query += "from MarketData where QuoteDate = cast('1-3-24' as date))";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}

#[test]
fn select_statement_with_where_and_between() {
    let input = r"SELECT Symbol, LastPrice, PercentChange, (select Top 1 Exchange from
    MarketIndices mi where mi.Symbol = m.Symbol) 'TopExchange', OpenPrice from Market m
    where Symbol in (select Symbol from MarketData where QuoteDate = cast('1-3-24' as date))
    and [PercentChange] between 0.56 and 2.4";
    let mut expected_query = String::from("select Symbol, LastPrice, PercentChange, (select ");
    expected_query += "top 1 Exchange from MarketIndices mi where mi.Symbol = m.Symbol) ";
    expected_query += "'TopExchange', OpenPrice from Market m where Symbol in (select Symbol ";
    expected_query += "from MarketData where QuoteDate = cast('1-3-24' as date)) and ";
    expected_query += "[PercentChange] between 0.56 and 2.4";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}

#[test]
fn select_statement_with_where_and_not_expression() {
    let input = r"SELECT Symbol, LastPrice, PercentChange from Market m
    where nOT [PercentChange] > 0.56";
    let mut expected_query = String::from("select Symbol, LastPrice, PercentChange ");
    expected_query += "from Market m where not [PercentChange] > 0.56";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}

#[test]
fn select_statement_with_where_and_exists_expression() {
    let input = r"SELECT Symbol, LastPrice, PercentChange from Market m
    where nOT exists (select Symbol from MarketData)";
    let mut expected_query = String::from("select Symbol, LastPrice, PercentChange ");
    expected_query += "from Market m where not exists (select Symbol from MarketData)";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}

#[test]
fn select_statement_with_where_and_all_expression() {
    let input = r"SELECT Symbol, LastPrice, PercentChange from Market m
    where ManufactureDays >= all (select ManufactureDays from MarketData)";
    let mut expected_query = String::from("select Symbol, LastPrice, PercentChange ");
    expected_query +=
        "from Market m where ManufactureDays >= all (select ManufactureDays from MarketData)";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}

#[test]
fn select_statement_with_where_and_some_expression() {
    let input = r"SELECT Symbol, LastPrice, PercentChange from Market m
    where ManufactureDays >= some (select ManufactureDays from MarketData)";
    let mut expected_query = String::from("select Symbol, LastPrice, PercentChange ");
    expected_query +=
        "from Market m where ManufactureDays >= some (select ManufactureDays from MarketData)";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}

#[test]
fn select_statement_with_where_and_any_expression() {
    let input = r"SELECT Symbol, LastPrice, PercentChange from Market m
    where ManufactureDays >= any (select ManufactureDays from MarketData)";
    let mut expected_query = String::from("select Symbol, LastPrice, PercentChange ");
    expected_query +=
        "from Market m where ManufactureDays >= any (select ManufactureDays from MarketData)";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}

#[test]
fn select_statement_with_where_and_like_expression() {
    let input = r"SELECT Symbol, LastPrice, PercentChange from Market m
    where Symbol like 'AM%' and LastPrice > 32";
    let mut expected_query = String::from("select Symbol, LastPrice, PercentChange ");
    expected_query += "from Market m where Symbol like 'AM%' and LastPrice > 32";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}

#[test]
fn select_statement_with_where_and_like_expression_two() {
    let input = r"SELECT Symbol, LastPrice, PercentChange from Market m
    where Symbol not like @TestPattern and LastPrice > 32";
    let mut expected_query = String::from("select Symbol, LastPrice, PercentChange ");
    expected_query += "from Market m where Symbol not like @TestPattern and LastPrice > 32";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}

#[test]
fn select_statement_with_case_statement() {
    let input = r"SELECT Symbol, LastPrice, PercentChange, case Exchange when 'nsdq' then 'Nasdaq'
    when 'dji' then 'Dow Jones' else 'SP500' end from Market m where Symbol not like
    @TestPattern and LastPrice > 32";
    let mut expected_query = String::from("select Symbol, LastPrice, PercentChange, case Exchange");
    expected_query += " when 'nsdq' then 'Nasdaq' when 'dji' then 'Dow Jones' else 'SP500' end";
    expected_query += " from Market m where Symbol not like @TestPattern and LastPrice > 32";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}

#[test]
fn select_statement_with_cte() {
    let input = r"with testcte as (select * from MarketLake) SELECT Symbol, LastPrice, 
    PercentChange from Market m inner join testcte tc on tc.Symbol = m.Symbol where Symbol not like
    @TestPattern and LastPrice > 32";
    let mut expected_query = String::from("with testcte as (select * from MarketLake)");
    expected_query += " select Symbol, LastPrice, PercentChange from Market m";
    expected_query += " inner join testcte tc on tc.Symbol = m.Symbol";
    expected_query += " where Symbol not like @TestPattern and LastPrice > 32";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}

#[test]
fn select_statement_with_cte_two() {
    let input = r"with testcte (testcol, poolc)as (select * from MarketLake), potato as (select
    Symbol,Title, Author from News) SELECT Symbol, LastPrice, PercentChange from Market m inner
    join testcte tc on tc.Symbol = m.Symbol inner join potato p on m.Symbol = p.Symbol where 
    Symbol not like @TestPattern and LastPrice > 32";
    let mut expected_query = String::from("with testcte (testcol, poolc) as (select * from");
    expected_query += " MarketLake), potato as (select Symbol, Title, Author from News)";
    expected_query += " select Symbol, LastPrice, PercentChange from Market m";
    expected_query += " inner join testcte tc on tc.Symbol = m.Symbol";
    expected_query += " inner join potato p on m.Symbol = p.Symbol";
    expected_query += " where Symbol not like @TestPattern and LastPrice > 32";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let query = parser.parse();

    assert_eq!(expected_query, query.to_string());
}
