pub mod ast;
mod keywords;
pub mod lexer;
pub mod token;
use keywords::Keyword;
use token::{Kind, Literal, Token};

#[derive(Debug, PartialEq, Clone)]
pub struct Parser<'a> {
    lexer: lexer::Lexer<'a>,
    current_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

// create a precedence table
// this will be used to determine the precedence of operators
const PRECEDENCE_HIGHEST: u8 = 8;
const PRECEDENCE_PRODUCT: u8 = 7;
const PRECEDENCE_SUM: u8 = 6;
const PRECEDENCE_COMPARISON: u8 = 5;
const PRECEDENCE_NOT: u8 = 4;
const PRECEDENCE_AND: u8 = 3;
const PRECEDENCE_OTHER_LOGICALS: u8 = 2;
const PRECEDENCE_LOWEST: u8 = 1;

impl<'a> Parser<'a> {
    pub fn new(lexer: lexer::Lexer<'a>) -> Self {
        let mut parser = Parser {
            lexer,
            current_token: Token {
                kind: Kind::Eof,
                literal: Literal::new_string(""),
            },
            peek_token: Token {
                kind: Kind::Eof,
                literal: Literal::new_string(""),
            },
            errors: vec![],
        };
        parser.next_token();
        parser.next_token();
        parser
    }

    pub fn errors(&self) -> Vec<String> {
        self.errors.clone()
    }

    pub fn parse(&mut self) -> ast::Query {
        let mut query = ast::Query::new();

        while self.current_token.kind != Kind::Eof {
            if let Some(statement) = self.parse_statement() {
                query.statements.push(statement);
            }

            self.next_token();
        }

        query
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn parse_statement(&mut self) -> Option<ast::Statement> {
        match self.current_token.kind {
            Kind::Keyword(keyword) => match keyword {
                Keyword::SELECT => {
                    let select_statement = self.parse_select_statement();
                    select_statement
                }
                _ => None,
            },
            _ => None,
        }
    }

    fn parse_select_statement(&mut self) -> Option<ast::Statement> {
        let mut statement = ast::SelectStatement::new();

        // check if the next token is an identifier
        if !self.expect_peek(Kind::Ident) {
            return None;
        }

        // get the columns to select
        while !self.current_token_is(Kind::Keyword(Keyword::FROM)) {
            dbg!(&self.current_token);
            match self.current_token.kind {
                Kind::Ident => {
                    statement
                        .columns
                        .push(ast::Expression::Literal(self.current_token.clone()));

                    self.next_token();
                }
                Kind::Comma => {
                    // check if we have an identifier after a comma
                    if !self.expect_peek(Kind::Ident) {
                        return None;
                    }
                }
                _ => return None,
            }
        }

        // at this point we should have a FROM keyword
        // but we should make sure
        if !self.current_token_is(Kind::Keyword(Keyword::FROM)) {
            return None;
        }

        // get the table to select from
        // check if the next token is an identifier
        if !self.expect_peek(Kind::Ident) {
            return None;
        }

        match self.current_token.kind {
            Kind::Ident => statement
                .table
                .push(ast::Expression::Literal(self.current_token.clone())),
            _ => return None,
        }

        // check if we have any where clause
        if self.peek_token_is(Kind::Keyword(Keyword::WHERE)) {
            // skip the WHERE keyword
            self.next_token();
            self.next_token();

            let where_clause = self.parse_expression(PRECEDENCE_LOWEST);
            statement.where_clause = where_clause;
        }

        Some(ast::Statement::Select(statement))
    }

    fn parse_expression(&mut self, precedence: u8) -> Option<ast::Expression> {
        // check if the current token is an identifier
        // or if it is a prefix operator
        let mut left_expression = match self.current_token_is(Kind::Ident) {
            true => Some(ast::Expression::Literal(self.current_token.clone())),
            false => self.parse_prefix_expression(),
        };

        // 1 + 2 * 3
        // parse the infix expression
        while !self.peek_token_is(Kind::Ident) && precedence < self.peek_precedence() {
            // move to the next token
            self.next_token();

            match left_expression {
                Some(expression) => {
                    left_expression = self.parse_infix_expression(expression);
                }
                None => return None,
            }
        }

        left_expression
    }

    fn parse_prefix_expression(&mut self) -> Option<ast::Expression> {
        match self.current_token.kind {
            Kind::Plus | Kind::Minus | Kind::Keyword(Keyword::NOT) => {
                let operator = self.current_token.clone();
                let precedence = self.current_precedence();

                self.next_token();

                // parse the expression to the right of the operator
                if let Some(right_expression) = self.parse_expression(precedence) {
                    Some(ast::Expression::Unary {
                        operator,
                        right: Box::new(right_expression),
                    })
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn parse_infix_expression(&mut self, left: ast::Expression) -> Option<ast::Expression> {
        match self.current_token.kind {
            Kind::Plus
            | Kind::Minus
            | Kind::Multiply
            | Kind::Divide
            | Kind::Equal
            | Kind::NotEqual
            | Kind::LessThan
            | Kind::LessThanEqual
            | Kind::GreaterThan
            | Kind::GreaterThanEqual
            | Kind::Keyword(Keyword::ALL)
            | Kind::Keyword(Keyword::AND)
            | Kind::Keyword(Keyword::ANY)
            | Kind::Keyword(Keyword::BETWEEN)
            | Kind::Keyword(Keyword::IN)
            | Kind::Keyword(Keyword::LIKE)
            | Kind::Keyword(Keyword::OR)
            | Kind::Keyword(Keyword::SOME) => {
                let operator = self.current_token.clone();
                let precedence = self.current_precedence();
                self.next_token();

                // parse the expression to the right of the operator
                if let Some(right_expression) = self.parse_expression(precedence) {
                    Some(ast::Expression::Binary {
                        left: Box::new(left),
                        operator,
                        right: Box::new(right_expression),
                    })
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn peek_precedence(&self) -> u8 {
        match self.peek_token.kind {
            Kind::Tilde => PRECEDENCE_HIGHEST,
            Kind::Multiply | Kind::Divide => PRECEDENCE_PRODUCT,
            Kind::Plus | Kind::Minus => PRECEDENCE_SUM,
            Kind::Equal
            | Kind::NotEqual
            | Kind::LessThan
            | Kind::LessThanEqual
            | Kind::GreaterThan
            | Kind::GreaterThanEqual => PRECEDENCE_COMPARISON,
            Kind::Keyword(Keyword::NOT) => PRECEDENCE_NOT,
            Kind::Keyword(Keyword::AND) => PRECEDENCE_AND,
            Kind::Keyword(Keyword::ALL)
            | Kind::Keyword(Keyword::ANY)
            | Kind::Keyword(Keyword::BETWEEN)
            | Kind::Keyword(Keyword::IN)
            | Kind::Keyword(Keyword::LIKE)
            | Kind::Keyword(Keyword::OR)
            | Kind::Keyword(Keyword::SOME) => PRECEDENCE_OTHER_LOGICALS,
            _ => PRECEDENCE_LOWEST,
        }
    }

    fn current_precedence(&self) -> u8 {
        match self.current_token.kind {
            Kind::Tilde => PRECEDENCE_HIGHEST,
            Kind::Multiply | Kind::Divide => PRECEDENCE_PRODUCT,
            Kind::Plus | Kind::Minus => PRECEDENCE_SUM,
            Kind::Equal
            | Kind::NotEqual
            | Kind::LessThan
            | Kind::LessThanEqual
            | Kind::GreaterThan
            | Kind::GreaterThanEqual => PRECEDENCE_COMPARISON,
            Kind::Keyword(Keyword::NOT) => PRECEDENCE_NOT,
            Kind::Keyword(Keyword::AND) => PRECEDENCE_AND,
            Kind::Keyword(Keyword::ALL)
            | Kind::Keyword(Keyword::ANY)
            | Kind::Keyword(Keyword::BETWEEN)
            | Kind::Keyword(Keyword::IN)
            | Kind::Keyword(Keyword::LIKE)
            | Kind::Keyword(Keyword::OR)
            | Kind::Keyword(Keyword::SOME) => PRECEDENCE_OTHER_LOGICALS,
            _ => PRECEDENCE_LOWEST,
        }
    }

    fn current_token_is(&self, token_kind: Kind) -> bool {
        self.current_token.kind == token_kind
    }

    fn peek_token_is(&self, token_kind: Kind) -> bool {
        self.peek_token.kind == token_kind
    }

    fn expect_peek(&mut self, token_kind: Kind) -> bool {
        if self.peek_token_is(token_kind) {
            self.next_token();
            true
        } else {
            self.peek_error(token_kind);
            false
        }
    }

    fn peek_error(&mut self, token_kind: Kind) {
        let msg = format!(
            "expected next token to be {:?}, got {:?} instead",
            token_kind, self.peek_token.kind
        );
        self.errors.push(msg);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_select_statement() {
        let input = "SELECT *, name, firstname, lastname, [first], dob FROM users;";
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let query = parser.parse();

        let expected_query = ast::Query {
            statements: vec![ast::Statement::Select(ast::SelectStatement {
                columns: vec![
                    ast::Expression::Literal(Token {
                        kind: Kind::Ident,
                        literal: Literal::new_string("*"),
                    }),
                    ast::Expression::Literal(Token {
                        kind: Kind::Ident,
                        literal: Literal::new_string("name"),
                    }),
                    ast::Expression::Literal(Token {
                        kind: Kind::Ident,
                        literal: Literal::new_string("firstname"),
                    }),
                    ast::Expression::Literal(Token {
                        kind: Kind::Ident,
                        literal: Literal::new_string("lastname"),
                    }),
                    ast::Expression::Literal(Token {
                        kind: Kind::Ident,
                        literal: Literal::new_string("[first]"),
                    }),
                    ast::Expression::Literal(Token {
                        kind: Kind::Ident,
                        literal: Literal::new_string("dob"),
                    }),
                ],
                table: vec![ast::Expression::Literal(Token {
                    kind: Kind::Ident,
                    literal: Literal::new_string("users"),
                })],
                where_clause: None,
            })],
        };

        assert_eq!(expected_query, query);
    }

    #[test]
    fn select_statement_with_where_clause() {
        let input = "SELECT name FROM users where lastname = 'blah' AND firstname > 'hello';";
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let query = parser.parse();

        let expected_query = ast::Query {
            statements: vec![ast::Statement::Select(ast::SelectStatement {
                columns: vec![ast::Expression::Literal(Token {
                    kind: Kind::Ident,
                    literal: Literal::new_string("name"),
                })],
                table: vec![ast::Expression::Literal(Token {
                    kind: Kind::Ident,
                    literal: Literal::new_string("users"),
                })],
                where_clause: Some(ast::Expression::Binary {
                    left: Box::new(ast::Expression::Binary {
                        left: Box::new(ast::Expression::Literal(Token {
                            kind: Kind::Ident,
                            literal: Literal::new_string("lastname"),
                        })),
                        operator: Token {
                            kind: Kind::Equal,
                            literal: Literal::new_string("="),
                        },
                        right: Box::new(ast::Expression::Literal(Token {
                            kind: Kind::Ident,
                            literal: Literal::new_string("'blah'"),
                        })),
                    }),
                    operator: Token {
                        kind: Kind::Keyword(Keyword::AND),
                        literal: Literal::new_string("AND"),
                    },
                    right: Box::new(ast::Expression::Binary {
                        left: Box::new(ast::Expression::Literal(Token {
                            kind: Kind::Ident,
                            literal: Literal::new_string("firstname"),
                        })),
                        operator: Token {
                            kind: Kind::GreaterThan,
                            literal: Literal::new_string(">"),
                        },
                        right: Box::new(ast::Expression::Literal(Token {
                            kind: Kind::Ident,
                            literal: Literal::new_string("'hello'"),
                        })),
                    }),
                }),
            })],
        };

        assert_eq!(expected_query, query);
    }
}
