# T-SQL LSP

## SQL_LSP

Language Server Microsoft SQL Server

## SQL_PARSER

Parses T-SQL queries and places them into abstract syntax to make it easier for other crates
make sense of the queries that are given to it


### Features In Progress

- [ ] Select Queries
    - [x] \[ ALL | DISTINCT ]
    - [x] TOP (expression) \[ PERCENT ] | \[ WITH TIES ]  
    - [ ] select items
        - [x] with subqueries
        - [x] with numbers
        - [x] with identifiers
        - [x] with aliases
        - [ ] with aggregate functions
    - [ ] from clause
        - [x] basic table
        - [x] table with alias
        - [ ] table valued function
        - [ ] pivot table
        - [ ] unpivot table
        - [ ] function
        - [x] joins
    - [ ] where clause 
        - [x] with subqueries
        - [x] with numbers
        - [x] with identifiers
        - [ ] with aggregate functions
    - [ ] group by clause
        - [x] with numbers
        - [x] with identifiers
        - [ ] with aggregate functions
    - [ ] having clause
        - [x] with subqueries
        - [x] with numbers
        - [x] with identifiers
        - [ ] with aggregate functions
    - [ ] order by clause
        - [x] with numbers
        - [x] with identifiers

- [ ] CTEs
- [ ] Insert Queries
- [ ] Bulk Insert Queries
- [ ] Delete Queries
- [ ] Update Queries
