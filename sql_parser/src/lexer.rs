use crate::keywords;
use crate::token::{Identifier, TokenType};

pub struct Lexer<'a> {
    input: &'a str,
    current_position: usize, // current position in input (points to current char)
    read_position: usize,    // current reading position in input (after current char)
    ch: Option<char>,        // current char under examination
}

impl<'a> Lexer<'a> {
    pub fn new(input: &str) -> Lexer {
        let mut lexer = Lexer {
            input,
            current_position: 0,
            read_position: 0,
            ch: None,
        };
        lexer.read_char();
        lexer
    }

    fn has_more_tokens(&self) -> bool {
        self.read_position < self.input.len()
    }

    fn read_char(&mut self) {
        if self.has_more_tokens() {
            self.ch = self.input.chars().nth(self.read_position);
        } else {
            self.ch = None;
        }
        self.current_position = self.read_position;
        self.read_position += 1;
    }

    fn peak_char(&self) -> Option<char> {
        if self.has_more_tokens() {
            self.input.chars().nth(self.read_position)
        } else {
            None
        }
    }

    pub fn next_token(&mut self) -> TokenType {
        self.skip_whitespace();

        let token = match self.ch {
            Some(ch) => match ch {
                ',' => TokenType::Comma,
                '(' => TokenType::LeftParen,
                ')' => TokenType::RightParen,
                '*' => TokenType::Asterisk,
                '=' => {
                    if self.peak_char() == Some('=') {
                        self.read_char();
                        TokenType::DoubleEqual
                    } else {
                        TokenType::Equal
                    }
                }
                '!' => {
                    if self.peak_char() == Some('=') {
                        self.read_char();
                        TokenType::NotEqual
                    } else {
                        TokenType::ExclamationMark
                    }
                }
                '<' => {
                    if self.peak_char() == Some('=') {
                        self.read_char();
                        TokenType::LessThanEqual
                    } else {
                        TokenType::LessThan
                    }
                }
                '>' => {
                    if self.peak_char() == Some('=') {
                        self.read_char();
                        TokenType::GreaterThanEqual
                    } else {
                        TokenType::GreaterThan
                    }
                }
                '+' => TokenType::Plus,
                '-' => TokenType::Minus,
                '/' => TokenType::Divide,
                '%' => TokenType::Mod,
                '.' => TokenType::Period,
                ';' => TokenType::SemiColon,
                '[' => {
                    let peaked_char = self.peak_char();
                    if peaked_char.is_some_and(|c| c.is_alphabetic()) {
                        // Read the identifier until the next non-alphabetic character
                        // We should be reading until the next ']'
                        self.read_char();
                        let ident = self.read_ident();
                        if self.ch == Some(']') {
                            self.read_char();
                            return TokenType::Ident(Identifier {
                                value: ident,
                                quote_style: Some('['),
                            });
                        } else {
                            return TokenType::Illegal;
                        }
                    } else {
                        TokenType::LeftBracket
                    }
                }
                ']' => TokenType::RightBracket,
                '\'' => {
                    self.read_char();
                    // Read the string until the next single quote
                    let start = self.current_position;
                    while self.ch.is_some() && self.ch.unwrap() != '\'' {
                        self.read_char();
                    }
                    let value = self.input[start..self.current_position].to_string();
                    self.read_char();
                    return TokenType::QuotedString(value);
                }
                '{' => TokenType::LeftBrace,
                '}' => TokenType::RightBrace,
                '~' => TokenType::Tilde,
                _ => {
                    if ch.is_alphabetic() {
                        // Read the identifier until the next non-alphabetic character
                        let ident = self.read_ident();
                        // Check if the identifier is a keyword
                        // if it is the return the keyword token type
                        return match keywords::lookup_keyword(&ident) {
                            Some(keyword) => TokenType::Keyword(keyword),
                            None => TokenType::Ident(Identifier {
                                value: ident,
                                quote_style: None,
                            }),
                        };
                    } else if ch.is_numeric() {
                        return TokenType::Number(self.read_number());
                    } else {
                        TokenType::Illegal
                    }
                }
            },
            None => TokenType::Eof,
        };

        self.read_char();
        token
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_some() && self.ch.unwrap().is_whitespace() {
            self.read_char();
        }
    }

    fn read_ident(&mut self) -> String {
        let start = self.current_position;
        while self.ch.is_some() && self.ch.unwrap().is_alphabetic() {
            self.read_char();
        }
        self.input[start..self.current_position].to_string()
    }

    fn read_number(&mut self) -> f64 {
        let start = self.current_position;
        while self.ch.is_some() && self.ch.unwrap().is_numeric() {
            self.read_char();
        }
        self.input[start..self.current_position]
            .parse::<f64>()
            .unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::TokenType::*;
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
            TokenType::Keyword(keywords::Keyword::SELECT),
            TokenType::Ident(Identifier {
                value: "name".to_string(),
                quote_style: None,
            }),
            TokenType::Comma,
            TokenType::Ident(Identifier {
                value: "id".to_string(),
                quote_style: None,
            }),
            TokenType::Keyword(keywords::Keyword::FROM),
            TokenType::Ident(Identifier {
                value: "users".to_string(),
                quote_style: None,
            }),
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
            TokenType::Keyword(keywords::Keyword::SELECT),
            TokenType::Ident(Identifier {
                value: "name".to_string(),
                quote_style: Some('['),
            }),
            TokenType::Comma,
            TokenType::Ident(Identifier {
                value: "id".to_string(),
                quote_style: None,
            }),
            TokenType::Keyword(keywords::Keyword::FROM),
            TokenType::Ident(Identifier {
                value: "users".to_string(),
                quote_style: None,
            }),
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
            TokenType::Keyword(keywords::Keyword::SELECT),
            TokenType::Ident(Identifier {
                value: "name".to_string(),
                quote_style: None,
            }),
            TokenType::Keyword(keywords::Keyword::AS),
            TokenType::QuotedString("SuperName".to_string()),
            TokenType::Comma,
            TokenType::Ident(Identifier {
                value: "id".to_string(),
                quote_style: None,
            }),
            TokenType::Keyword(keywords::Keyword::FROM),
            TokenType::Ident(Identifier {
                value: "users".to_string(),
                quote_style: None,
            }),
        ];

        assert_eq!(expected_tokens, tokens);
    }
}
