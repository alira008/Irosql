pub mod ast;
mod keywords;
pub mod lexer;
pub mod token;

struct Parser<'a> {
    lexer: lexer::Lexer<'a>,
    current_token: Option<token::TokenType>,
    peek_token: Option<token::TokenType>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: lexer::Lexer<'a>) -> Self {
        let mut parser = Parser {
            lexer,
            current_token: None,
            peek_token: None,
        };
        parser.next_token();
        parser.next_token();
        parser
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = Some(self.lexer.next_token());
    }

    pub fn parse(&mut self) -> Option<ast::Query> {
        let mut query = ast::Query::new();

        self.next_token();

        while self.current_token != Some(token::TokenType::Eof) {
            if let Some(statement) = self.parse_statement() {
                query.statements.push(statement);
            }

            self.next_token();
        }

        None
    }

    fn parse_statement(&mut self) -> Option<ast::Statement> {
        if let Some(token) = &self.current_token {
            match token {
                token::TokenType::Keyword(keywords::Keyword::SELECT) => {
                    let select_statement = self.parse_select_statement(token::TokenType::Keyword(
                        keywords::Keyword::SELECT,
                    ));
                    select_statement
                }
                _ => None,
            }
        } else {
            None
        }
    }

    fn parse_select_statement(&self, token: token::TokenType) -> Option<ast::Statement> {
        let statement = ast::SelectStatement::new(token);

        // check for columns
        match self.peek_token {
            Some(identifier) => {
                match identifier {
                    token::TokenType::Ident(ident) => {
                        statement.columns.push(ast::Expression::Identifier(ident));
                        self.next_token();
                    }
                    _ => return None,
                }
                self.next_token();
            }
            _ => return None,
        }
        if !self.expect_peek(token::TokenType::Ident) {
            return None;
        }
        None
    }

    fn current_token_is(&self, token: &token::TokenType) -> bool {
        if let Some(current_token) = &self.current_token {
            current_token == token
        } else {
            false
        }
    }

    fn peek_token_is(&self, token: &token::TokenType) -> bool {
        if let Some(peek_token) = &self.peek_token {
            peek_token == token
        } else {
            false
        }
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
