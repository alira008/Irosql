use formatter::formatter::Formatter;
use formatter::settings::{FormatterSettings, IndentCommaLists, KeywordCase};

#[test]
fn basic_select_statement() -> Result<(), String> {
    let formatter_settings = FormatterSettings {
        indent_comma_lists: None,
        indent_in_lists: true,
        indent_between_conditions: true,
        keyword_case: KeywordCase::Upper,
        max_width: 80,
        indent_width: 4,
        use_tab: false,
    };
    let mut formatter = Formatter::new(formatter_settings);

    let input = r"Select top 30 percent LastPrice, [Time] , @Hello, PC as 'PercentChange',
	143245 from MarketTable mkt inner join IndexTable it on mkt.[Time] = it.QuoteTime where 
	QuoteTime between '6:30' and '13:00' and Symbol in (select distinct Symbol from 
	MarketSymbols) and InsertTime = cast(getdate() as Time) oRDer By Symbol deSC";
    let expected = r"SELECT TOP 30 PERCENT
    LastPrice
    ,[Time]
    ,@Hello
    ,PC AS 'PercentChange'
    ,143245
FROM MarketTable mkt
INNER JOIN IndexTable it ON mkt.[Time] = it.QuoteTime
WHERE QuoteTime BETWEEN '6:30'
        AND '13:00'
    AND Symbol IN (
        SELECT DISTINCT Symbol
        FROM MarketSymbols
    )
    AND InsertTime = CAST(GETDATE() AS TIME)
ORDER BY Symbol DESC";
    formatter.format(input)?;

    let formatted_query = formatter.formatted_query();
    assert_eq!(expected, formatted_query);

    Ok(())
}

#[test]
fn basic_select_statement_two() -> Result<(), String> {
    let formatter_settings = FormatterSettings {
        indent_comma_lists: None,
        indent_in_lists: true,
        indent_between_conditions: true,
        keyword_case: KeywordCase::Lower,
        max_width: 80,
        indent_width: 4,
        use_tab: false,
    };
    let mut formatter = Formatter::new(formatter_settings);

    let input = r"SELECT top 30 percent LastPrice, [Time] , @Hello, PC as 'PercentChange',
	143245 from MarketTable mkt inner join IndexTable it on mkt.[Time] = it.QuoteTime where 
	QuoteTime between '6:30' and '13:00' and Symbol in (select distinct Symbol from 
	MarketSymbols) and InsertTime = cast(getdate() as Time) oRDer By Symbol deSC";
    let expected = r"select top 30 percent
    LastPrice
    ,[Time]
    ,@Hello
    ,PC as 'PercentChange'
    ,143245
from MarketTable mkt
inner join IndexTable it on mkt.[Time] = it.QuoteTime
where QuoteTime between '6:30'
        and '13:00'
    and Symbol in (
        select distinct Symbol
        from MarketSymbols
    )
    and InsertTime = cast(getdate() as time)
order by Symbol desc";
    formatter.format(input)?;

    let formatted_query = formatter.formatted_query();
    assert_eq!(expected, formatted_query);

    Ok(())
}

#[test]
fn basic_select_statement_three() -> Result<(), String> {
    let formatter_settings = FormatterSettings {
        indent_comma_lists: None,
        indent_in_lists: true,
        indent_between_conditions: true,
        keyword_case: KeywordCase::Lower,
        max_width: 80,
        indent_width: 4,
        use_tab: false,
    };
    let mut formatter = Formatter::new(formatter_settings);

    let input = r"SELECT top 30 percent with ties LastPrice, PC as 'PercentChange'
	from MarketTable mkt where QuoteTime between '6:30' and '13:00' and Symbol in
    ('aal', 'googl', 'zm') and InsertTime = cast(getdate() as Time) oRDer By Symbol deSC
    , InsertTime";
    let expected = r"select top 30 percent with ties
    LastPrice
    ,PC as 'PercentChange'
from MarketTable mkt
where QuoteTime between '6:30'
        and '13:00'
    and Symbol in (
        'aal'
        ,'googl'
        ,'zm'
    )
    and InsertTime = cast(getdate() as time)
order by Symbol desc, InsertTime";
    formatter.format(input)?;

    let formatted_query = formatter.formatted_query();
    assert_eq!(expected, formatted_query);

    Ok(())
}

#[test]
fn basic_select_statement_four() -> Result<(), String> {
    let formatter_settings = FormatterSettings {
        indent_comma_lists: None,
        indent_in_lists: true,
        indent_between_conditions: true,
        keyword_case: KeywordCase::Lower,
        max_width: 80,
        indent_width: 4,
        use_tab: false,
    };
    let mut formatter = Formatter::new(formatter_settings);

    let input = r"SELECT top 30 percent with ties LastPrice, PC as 'PercentChange',
    case LastPrice when 02 then 'blah' else 'no' end, case when LastPrice > 7 then 'blah'
    when LastPrice > 55 then 'yo' else 'no' end
	from MarketTable mkt where QuoteTime between '6:30' and '13:00' and Symbol in
    ('aal', 'googl', 'zm') and InsertTime = cast(getdate() as Time) oRDer By Symbol deSC
    , InsertTime";
    let expected = r"select top 30 percent with ties
    LastPrice
    ,PC as 'PercentChange'
    ,case LastPrice
        when 02
            then 'blah'
        else 'no'
        end
    ,case 
        when LastPrice > 7
            then 'blah'
        when LastPrice > 55
            then 'yo'
        else 'no'
        end
from MarketTable mkt
where QuoteTime between '6:30'
        and '13:00'
    and Symbol in (
        'aal'
        ,'googl'
        ,'zm'
    )
    and InsertTime = cast(getdate() as time)
order by Symbol desc, InsertTime";
    formatter.format(input)?;

    let formatted_query = formatter.formatted_query();
    assert_eq!(expected, formatted_query);

    Ok(())
}

#[test]
fn basic_select_statement_five() -> Result<(), String> {
    let formatter_settings = FormatterSettings {
        indent_comma_lists: None,
        indent_in_lists: true,
        indent_between_conditions: true,
        keyword_case: KeywordCase::Lower,
        max_width: 80,
        indent_width: 4,
        use_tab: false,
    };
    let mut formatter = Formatter::new(formatter_settings);

    let input = r"SELECT LastPrice, PC as 'PercentChange'
	from MarketTable mkt select * from PotatoTable";
    let expected = r"select
    LastPrice
    ,PC as 'PercentChange'
from MarketTable mkt

select *
from PotatoTable";
    formatter.format(input)?;

    let formatted_query = formatter.formatted_query();
    assert_eq!(expected, formatted_query);

    Ok(())
}
