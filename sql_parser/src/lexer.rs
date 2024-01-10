use crate::keywords;
use crate::token::{Kind, Literal, Token};

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    input: &'a str,
    chars: std::iter::Peekable<std::str::Chars<'a>>,
    current_position: usize, // current position in input (points to current char)
    read_position: usize,    // current reading position in input (after current char)
    ch: Option<char>,        // current char under examination
    line: usize,            // the current line number
    col: usize,          // the current column number
}

impl<'a> Lexer<'a> {
    pub fn new(input: &str) -> Lexer {
        let mut lexer = Lexer {
            input,
            chars: input.chars().peekable(),
            current_position: 0,
            read_position: 0,
            ch: None,
            line: 0,
            col: 0
        };
        lexer.read_char();
        lexer
    }

    fn has_more_tokens(&self) -> bool {
        self.read_position < self.input.len()
    }

    fn read_char(&mut self) {
        if self.has_more_tokens() {
            self.ch = self.chars.next();
            if self.ch == Some('\n') {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
                    
        } else {
            self.ch = None;
        }
        self.current_position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let token = match self.ch {
            Some(ch) => match ch {
                ',' => Token::new(
                    Kind::Comma,
                    Literal::new_string(","),
                ),
                '(' => Token::new(
                    Kind::LeftParen,
                    Literal::new_string("("),
                ),
                ')' => Token::new(
                    Kind::RightParen,
                    Literal::new_string(")"),
                ),
                '=' => {
                    if self.chars.peek() == Some(&'=') {
                        self.read_char();
                        Token::new(
                            Kind::DoubleEqual,
                            Literal::new_string("=="),
                        )
                    } else {
                        Token::new(
                            Kind::Equal,
                            Literal::new_string("="),
                        )
                    }
                }
                '!' => {
                    if self.chars.peek() == Some(&'=') {
                        self.read_char();
                        Token::new(
                            Kind::NotEqual,
                            Literal::new_string("!="),
                        )
                    } else {
                        Token::new(
                            Kind::ExclamationMark,
                            Literal::new_string("!"),
                        )
                    }
                }
                '<' => {
                    if self.chars.peek() == Some(&'=') {
                        self.read_char();
                        Token::new(
                            Kind::LessThanEqual,
                            Literal::new_string("<="),
                        )
                    } else {
                        Token::new(
                            Kind::LessThan,
                            Literal::new_string("<"),
                        )
                    }
                }
                '>' => {
                    if self.chars.peek() == Some(&'=') {
                        self.read_char();
                        Token::new(
                            Kind::GreaterThanEqual,
                            Literal::new_string(">="),
                        )
                    } else {
                        Token::new(
                            Kind::GreaterThan,
                            Literal::new_string(">"),
                        )
                    }
                }
                '+' => Token::new(
                    Kind::Plus,
                    Literal::new_string("+"),
                ),
                '-' => Token::new(
                    Kind::Minus,
                    Literal::new_string("-"),
                ),
                '/' => Token::new(
                    Kind::Divide,
                    Literal::new_string("/"),
                ),
                '*' => Token::new(
                    Kind::Asterisk,
                    Literal::new_string("*"),
                ),
                '%' => Token::new(
                    Kind::Mod,
                    Literal::new_string("%"),
                ),
                '.' => Token::new(
                    Kind::Period,
                    Literal::new_string("."),
                ),
                ';' => Token::new(
                    Kind::SemiColon,
                    Literal::new_string(";"),
                ),
                '[' => {
                    let peaked_char = self.chars.peek();
                    if peaked_char.is_some_and(|c| c.is_alphabetic()) {
                        // Read the identifier until the next non-alphabetic character
                        // We should be reading until the next ']'
                        if let Some(ident) = self.read_quoted_ident('[') {
                            return Token::new(
                                Kind::Ident,
                                Literal::String(ident),
                            );
                        } else {
                           return Token::new(
                                Kind::Illegal,
                                Literal::new_string(""),
                            )
                        }
                    } else {
                        Token::new(
                            Kind::LeftBracket,
                            Literal::new_string("["),
                        )
                    }
                }
                ']' => Token::new(
                    Kind::RightBracket,
                    Literal::new_string("]"),
                ),
                '\'' => {
                    // Read the identifier until the next non-alphabetic character
                    // We should be reading until the next '\''
                    if let Some(ident) = self.read_quoted_ident('\'') {
                        return Token::new(
                            Kind::Ident,
                            Literal::String(ident),
                        );
                    } else {
                        return Token::new(
                            Kind::Illegal,
                            Literal::new_string(""),
                        )
                    }
                }
                '{' => Token::new(
                    Kind::LeftBrace,
                    Literal::new_string("{"),
                ),
                '}' => Token::new(
                    Kind::RightBrace,
                    Literal::new_string("}"),
                ),
                '~' => Token::new(
                    Kind::Tilde,
                    Literal::new_string("~"),
                ),
                _ => {
                    if ch.is_alphabetic() {
                        // Read the identifier until the next non-alphabetic character
                        let ident = self.read_ident();
                        // Check if the identifier is a keyword
                        // if it is the return the keyword token type
                        return match keywords::lookup_keyword(&ident) {
                            Some(keyword) => Token::new(
                                Kind::Keyword(keyword),
                                Literal::String(ident.to_uppercase()),
                            ),
                            None => Token::new(
                                Kind::Ident,
                                Literal::String(ident),
                            ),
                        };
                    } else if ch.is_numeric() {
                        if let Some(number) = self.read_number() {
                            return Token::new(
                                Kind::Number,
                                Literal::Number(number),
                            )
                        } else {
                           return Token::new(
                                Kind::Illegal,
                                Literal::new_string(""),
                            )
                        }
                    } else {
                        Token::new(
                            Kind::Illegal,
                            Literal::new_string(""),
                       ) 
                    }
                }
            },
            None => crate::token::Token::new(
                crate::token::Kind::Eof,
                Literal::new_string(""),
                                ),
        };

        self.read_char();
        token
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_some_and(|ch| ch.is_whitespace()) {
            self.read_char();
        }
    }

    fn read_ident(&mut self) -> String {
        let start = self.current_position;
        while self.ch.is_some_and(|ch| ch.is_alphabetic()) {
            self.read_char();
        }
        self.input[start..self.current_position].to_string()
    }

    fn read_quoted_ident(&mut self, quote_char: char) -> Option<String> {
        // Read the string until the next single quote
        // current position is at the quote character
        let start = self.current_position;
        match quote_char {
            '[' => {
                while self.ch.is_some_and(|ch| ch != ']') {
                    self.read_char();
                }
                // read the closing bracket
                self.read_char();
                Some(self.input[start..self.current_position].to_string())
            }
            '\'' => {
                self.read_char();
                while self.ch.is_some_and(|ch| ch != '\'') {
                    self.read_char();
                }
                // read the closing quote
                self.read_char();
                Some(self.input[start..self.current_position].to_string())
            }
            _ => None,
        }
    }

    fn read_number(&mut self) -> Option<f64> {
        let start = self.current_position;
        while self.ch.is_some_and(|ch| ch.is_numeric()) {
            self.read_char();
        }

        // check if the number has a period
        // aka number is a float
        if self.ch == Some('.') {
            self.read_char();
            while self.ch.is_some_and(|ch| ch.is_numeric()) {
                self.read_char();
            }
        }

        let str_val = self.input[start..self.current_position].to_string();
        match str_val.parse::<f64>() {
            Ok(num) => Some(num),
            Err(_) => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::keywords::Keyword;
    use super::*;

    #[test]
    fn test_identifiers_unquoted() {
        let input = "select name, id from users";
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        while lexer.has_more_tokens() {
            let token = lexer.next_token();
            tokens.push(token);
        }

        let expected_tokens = vec![
            Token::new(
                Kind::Keyword(Keyword::SELECT),
                Literal::new_string("SELECT"),
            ),
            Token::new(
                Kind::Ident,
                Literal::new_string("name"),
            ),
            Token::new(
                Kind::Comma,
                Literal::new_string(","),
            ),
            Token::new(
                Kind::Ident,
                Literal::new_string("id"),
            ),
            Token::new(
                Kind::Keyword(Keyword::FROM),
                Literal::new_string("FROM"),
            ),
            Token::new(
                Kind::Ident,
                Literal::new_string("users"),
            ),
        ];

        assert_eq!(expected_tokens, tokens);
    }

    #[test]
    fn test_identifiers_quoted() {
        let input = "select [name], id from users";
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        while lexer.has_more_tokens() {
            let token = lexer.next_token();
            tokens.push(token);
        }

        let expected_tokens = vec![
            Token::new(
                Kind::Keyword(Keyword::SELECT),
                Literal::new_string("SELECT"),
            ),
            Token::new(
                Kind::Ident,
                Literal::new_string("[name]"),
            ),
            Token::new(
                Kind::Comma,
                Literal::new_string(","),
            ),
            Token::new(
                Kind::Ident,
                Literal::new_string("id"),
            ),
            Token::new(
                Kind::Keyword(Keyword::FROM),
                Literal::new_string("FROM"),
            ),
            Token::new(
                Kind::Ident,
                Literal::new_string("users"),
            ),
        ];

        assert_eq!(expected_tokens, tokens);
    }

    #[test]
    fn test_string() {
        let input = "select name as 'SuperName', id from users";
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        while lexer.has_more_tokens() {
            let token = lexer.next_token();
            tokens.push(token);
        }

        let expected_tokens = vec![
            Token::new(
                Kind::Keyword(Keyword::SELECT),
                Literal::new_string("SELECT"),
            ),
            Token::new(
                Kind::Ident,
                Literal::new_string("name"),
            ),
            Token::new(
                Kind::Keyword(Keyword::AS),
                Literal::new_string("AS"),
            ),
            Token::new(
                Kind::Ident,
                Literal::new_string("'SuperName'"),
            ),
            Token::new(
                Kind::Comma,
                Literal::new_string(","),
            ),
            Token::new(
                Kind::Ident,
                Literal::new_string("id"),
            ),
            Token::new(
                Kind::Keyword(Keyword::FROM),
                Literal::new_string("FROM"),
            ),
            Token::new(
                Kind::Ident,
                Literal::new_string("users"),
            ),
        ];

        assert_eq!(expected_tokens, tokens);
    }
}
