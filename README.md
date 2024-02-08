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

### Example 

`sql_formatter -k upper -b -i 'select top 50 percent LastPrice, HighPrice, LowPrice, QuoteTime from Market mt where QuoteTime > '\''6:30'\'' and PercentChange > 0 oRDer By Symbol'`

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
- [ ] Insert Queries
- [ ] Bulk Insert Queries
- [ ] Delete Queries
- [ ] Update Queries
