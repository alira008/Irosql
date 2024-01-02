pub mod ast;
mod keywords;
pub mod lexer;
pub mod token;
use token::{Kind, Token};
use keywords::Keyword;

struct Parser<'a> {
    lexer: lexer::Lexer<'a>,
    current_token: Token,
    peek_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: lexer::Lexer<'a>) -> Self {
        let mut parser = Parser {
            lexer,
            current_token: Token {
                kind: Kind::Eof,
                literal: "".to_string(),
            },
            peek_token: Token {
                kind: Kind::Eof,
                literal: "".to_string(),
            },
        };
        parser.next_token();
        parser.next_token();
        parser
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        // self.peek_token = Some(self.lexer.next_token());
    }

    pub fn parse(&mut self) -> Option<ast::Query> {
        let mut query = ast::Query::new();

        self.next_token();

        while self.current_token.kind != Kind::Eof {
            if let Some(statement) = self.parse_statement() {
                query.statements.push(statement);
            }

            self.next_token();
        }

        None
    }

    fn parse_statement(&mut self) -> Option<ast::Statement> {
        match self.current_token.kind {
            Kind::Keyword(keyword) => {
                match keyword {
                    Keyword::SELECT => {
                        let select_statement = self.parse_select_statement();
                        select_statement
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn parse_select_statement(&mut self) -> Option<ast::Statement> {
        let statement = ast::SelectStatement::new(self.current_token.clone());
        while !self.current_token_is(Kind::Keyword(Keyword::FROM)) {
            self.next_token();
        }

        // check for columns
        // match self.peek_token {
        //     Some(identifier) => {
        //         match identifier {
        //             token::TokenType::Ident(ident) => {
        //                 statement.columns.push(ast::Expression::Identifier(ident));
        //                 self.next_token();
        //             }
        //             _ => return None,
        //         }
        //         self.next_token();
        //     }
        //     _ => return None,
        // }
        // if !self.expect_peek(token::TokenType::Ident) {
        //     return None;
        // }
        None
    }

    fn current_token_is(&self, token_kind: Kind) -> bool {
        self.current_token.kind == token_kind
    }

    fn peek_token_is(&self, token_kind: Kind) -> bool {
        self.peek_token.kind == token_kind
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        // let result = add(2, 2);
        // assert_eq!(result, 4);
    }
}
