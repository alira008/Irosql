# T-SQL Interpreter

## SQL_LSP

### Todo

My Language Server for T-SQL. Thinking about making this work for possibly SSMS
but also trying to make it work for Neovim

## SQL_PARSER

Parses T-SQL queries and places them into abstract syntax to make it easier for other crates
make sense of the queries that are given to it

## SQL_Formatter

Formats T-SQL queries into a standardized readable format. Settings are inspired by 
[Poor Man's TSQL Formatter](https://github.com/TaoK/PoorMansTSqlFormatter). Right now
this is a CLI where you pass options and pass the input and it outputs result query into stdout.
Errors are returned in stderr.

### Example run 

`sql_formatter -k upper -b -i 'select top 50 percent LastPrice, HighPrice, LowPrice, QuoteTime from Market mt where QuoteTime > '\''6:30'\'' and PercentChange > 0 oRDer By Symbol'`

### Output

```sql
SELECT TOP 50 PERCENT LastPrice
    ,HighPrice
    ,LowPrice
    ,QuoteTime
FROM Market mt
WHERE QuoteTime > '6:30'
    AND PercentChange > 0
ORDER BY Symbol
```

### Help

```
Usage: sql_formatter [OPTIONS] <INPUT>

Arguments:
  <INPUT>

Options:
  -c, --indent-comma-lists <INDENT_COMMA_LISTS>
          [possible values: trailing-comma, space-after-comma]
  -i, --indent-in-lists

  -b, --indent-between-conditions

  -k, --keyword-case <KEYWORD_CASE>
          [default: upper] [possible values: upper, lower]
  -m, --max-width <MAX_WIDTH>
          [default: 80]
  -w, --indent-width <INDENT_WIDTH>
          [default: 4]
  -u, --use-tab

  -h, --help
          Print help
  -V, --version
          Print version
```

### Features In Progress

- [x] Select Queries
    - [x] \[ ALL | DISTINCT ]
    - [x] TOP (expression) \[ PERCENT ] | \[ WITH TIES ]  
    - [x] select items
        - [x] with subqueries
        - [x] with numbers
        - [x] with identifiers
        - [x] with aliases
        - [x] with aggregate functions
    - [ ] from clause
        - [x] basic table
        - [x] table with alias
        - [x] table valued function
        - [ ] pivot table
        - [ ] unpivot table
        - [x] joins
    - [x] where clause 
        - [x] with subqueries
        - [x] with numbers
        - [x] with identifiers
        - [x] with aggregate functions
    - [x] group by clause
        - [x] with numbers
        - [x] with identifiers
        - [x] with aggregate functions
    - [x] having clause
        - [x] with subqueries
        - [x] with numbers
        - [x] with identifiers
        - [x] with aggregate functions
    - [x] order by clause
        - [x] with numbers
        - [x] with identifiers

- [x] CTEs
- [x] Insert Queries
- [ ] Bulk Insert Queries
- [ ] Delete Queries
- [ ] Update Queries
- [ ] Unions
