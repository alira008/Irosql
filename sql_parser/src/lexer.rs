use crate::keywords;
use crate::token::{Kind, Literal, Location, Token};

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    input: &'a str,
    chars: std::iter::Peekable<std::str::Chars<'a>>,
    current_position: usize, // current position in input (points to current char)
    read_position: usize,    // current reading position in input (after current char)
    ch: Option<char>,        // current char under examination
    reading_location: Location, // the location being read
    current_location: Location, // the location of the current token
}

impl<'a> Lexer<'a> {
    pub fn new(input: &str) -> Lexer {
        let mut lexer = Lexer {
            input,
            chars: input.chars().peekable(),
            current_position: 0,
            read_position: 0,
            ch: None,
            reading_location: Location::zero(),
            current_location: Location::zero(),
        };
        lexer.read_char();
        lexer
    }

    pub fn current_line_input(&self) -> &str {
        match self.input.lines().nth(self.current_location.line) {
            Some(line) => line,
            None => "",
        }
    }

    fn has_more_tokens(&self) -> bool {
        self.read_position < self.input.len()
    }

    fn read_char(&mut self) {
        if self.has_more_tokens() {
            self.ch = self.chars.next();
            if self.ch == Some('\n') {
                self.reading_location.line += 1;
                self.reading_location.column = 1;
            } else {
                self.reading_location.column += 1;
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
                ',' => Token::new(Kind::Comma, Literal::new_string(","), self.current_location),
                '(' => Token::new(
                    Kind::LeftParen,
                    Literal::new_string("("),
                    self.current_location,
                ),
                ')' => Token::new(
                    Kind::RightParen,
                    Literal::new_string(")"),
                    self.current_location,
                ),
                '=' => {
                    if self.chars.peek() == Some(&'=') {
                        self.read_char();
                        Token::new(
                            Kind::DoubleEqual,
                            Literal::new_string("=="),
                            self.current_location,
                        )
                    } else {
                        Token::new(Kind::Equal, Literal::new_string("="), self.current_location)
                    }
                }
                '!' => {
                    if self.chars.peek() == Some(&'=') {
                        self.read_char();
                        Token::new(
                            Kind::NotEqual,
                            Literal::new_string("!="),
                            self.current_location,
                        )
                    } else {
                        Token::new(
                            Kind::ExclamationMark,
                            Literal::new_string("!"),
                            self.current_location,
                        )
                    }
                }
                '<' => {
                    if self.chars.peek() == Some(&'=') {
                        self.read_char();
                        Token::new(
                            Kind::LessThanEqual,
                            Literal::new_string("<="),
                            self.current_location,
                        )
                    } else {
                        Token::new(
                            Kind::LessThan,
                            Literal::new_string("<"),
                            self.current_location,
                        )
                    }
                }
                '>' => {
                    if self.chars.peek() == Some(&'=') {
                        self.read_char();
                        Token::new(
                            Kind::GreaterThanEqual,
                            Literal::new_string(">="),
                            self.current_location,
                        )
                    } else {
                        Token::new(
                            Kind::GreaterThan,
                            Literal::new_string(">"),
                            self.current_location,
                        )
                    }
                }
                '+' => Token::new(Kind::Plus, Literal::new_string("+"), self.current_location),
                '-' => Token::new(Kind::Minus, Literal::new_string("-"), self.current_location),
                '/' => Token::new(
                    Kind::Divide,
                    Literal::new_string("/"),
                    self.current_location,
                ),
                '*' => Token::new(
                    Kind::Asterisk,
                    Literal::new_string("*"),
                    self.current_location,
                ),
                '%' => Token::new(Kind::Mod, Literal::new_string("%"), self.current_location),
                '.' => Token::new(
                    Kind::Period,
                    Literal::new_string("."),
                    self.current_location,
                ),
                ';' => Token::new(
                    Kind::SemiColon,
                    Literal::new_string(";"),
                    self.current_location,
                ),
                '[' => {
                    let peaked_char = self.chars.peek();
                    if peaked_char.is_some_and(|c| c.is_alphabetic()) {
                        // Read the identifier until the next non-alphabetic character
                        // We should be reading until the next ']'
                        if let Some(ident) = self.read_quoted_ident('[') {
                            Token::new(
                                Kind::Ident,
                                Literal::QuotedString {
                                    value: ident,
                                    quote_style: '[',
                                },
                                self.current_location,
                            )
                        } else {
                            Token::new(
                                Kind::Illegal,
                                Literal::new_string(""),
                                self.current_location,
                            )
                        }
                    } else {
                        Token::new(
                            Kind::LeftBracket,
                            Literal::new_string("["),
                            self.current_location,
                        )
                    }
                }
                ']' => Token::new(
                    Kind::RightBracket,
                    Literal::new_string("]"),
                    self.current_location,
                ),
                '\'' => {
                    // Read the identifier until the next '\''
                    if let Some(ident) = self.read_quoted_ident('\'') {
                        Token::new(
                            Kind::Ident,
                            Literal::QuotedString {
                                value: ident,
                                quote_style: '\'',
                            },
                            self.current_location,
                        )
                    } else {
                        Token::new(
                            Kind::Illegal,
                            Literal::new_string(""),
                            self.current_location,
                        )
                    }
                }
                '{' => Token::new(
                    Kind::LeftBrace,
                    Literal::new_string("{"),
                    self.current_location,
                ),
                '}' => Token::new(
                    Kind::RightBrace,
                    Literal::new_string("}"),
                    self.current_location,
                ),
                '~' => Token::new(Kind::Tilde, Literal::new_string("~"), self.current_location),
                '@' => {
                    // read the identifier until the next non-identifier
                    let local_variable = self.read_local_variable();
                    Token::new(
                        Kind::LocalVariable,
                        Literal::new_string(&local_variable),
                        self.current_location,
                    )
                }
                _ => {
                    if ch.is_alphabetic() || ch == '_' {
                        // Read the identifier until the next non-alphabetic character
                        let ident = self.read_ident();
                        // Check if the identifier is a keyword
                        // if it is the return the keyword token type
                        match keywords::lookup_keyword(&ident) {
                            Some(keyword) => Token::new(
                                Kind::Keyword(keyword),
                                Literal::String(ident.to_uppercase()),
                                self.current_location,
                            ),
                            None => Token::new(
                                Kind::Ident,
                                Literal::String(ident),
                                self.current_location,
                            ),
                        }
                    } else if ch.is_numeric() {
                        if let Some(number) = self.read_number() {
                            Token::new(Kind::Number, Literal::Number(number), self.current_location)
                        } else {
                            Token::new(
                                Kind::Illegal,
                                Literal::new_string(""),
                                self.current_location,
                            )
                        }
                    } else {
                        Token::new(
                            Kind::Illegal,
                            Literal::new_string(""),
                            self.current_location,
                        )
                    }
                }
            },
            None => {
                return crate::token::Token::new(
                    crate::token::Kind::Eof,
                    Literal::new_string(""),
                    self.current_location,
                );
            }
        };

        self.read_char();
        self.current_location = self.reading_location;
        token
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_some_and(|ch| ch.is_whitespace()) {
            self.read_char();
        }
    }

    fn read_local_variable(&mut self) -> String {
        let start = self.current_position;
        while self
            .chars
            .peek()
            .is_some_and(|ch| ch.is_alphanumeric() || ch == &'_' || ch == &'@')
        {
            self.read_char();
        }
        self.input[start..self.current_position + 1].to_string()
    }

    fn read_ident(&mut self) -> String {
        let start = self.current_position;
        while self
            .chars
            .peek()
            .is_some_and(|ch| ch.is_alphanumeric() || ch == &'_')
        {
            self.read_char();
        }
        self.input[start..self.current_position + 1].to_string()
    }

    fn read_quoted_ident(&mut self, quote_char: char) -> Option<String> {
        // skip the quote character
        self.read_char();
        // Read the string until the next single quote
        // current position is at the quote character
        let start = self.current_position;
        match quote_char {
            '[' => {
                while self.chars.peek().is_some_and(|ch| ch != &']') {
                    self.read_char();
                }
                // go to the closing bracket
                self.read_char();
                Some(self.input[start..self.current_position].to_string())
            }
            '\'' => {
                self.read_char();
                while self.chars.peek().is_some_and(|ch| ch != &'\'') {
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
        // read all the digits
        while self.chars.peek().is_some_and(|ch| ch.is_numeric()) {
            self.read_char();
        }

        // check if the number has a period
        // aka number is a float
        if self.chars.peek() == Some(&'.') {
            // go to the period
            self.read_char();

            while self.chars.peek().is_some_and(|ch| ch.is_numeric()) {
                self.read_char();
            }
        }

        let str_val = self.input[start..self.current_position + 1].to_string();
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
    fn test_random_tokens() {
        let input = "DESC current , preceding following";
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        while lexer.has_more_tokens() {
            let token = lexer.next_token();
            tokens.push(token);
        }
        let expected_tokens = vec![
            Token::wrap(Kind::Keyword(Keyword::DESC), Literal::new_string("DESC")),
            Token::wrap(
                Kind::Keyword(Keyword::CURRENT),
                Literal::new_string("CURRENT"),
            ),
            Token::wrap(Kind::Comma, Literal::new_string(",")),
            Token::wrap(
                Kind::Keyword(Keyword::PRECEDING),
                Literal::new_string("PRECEDING"),
            ),
            Token::wrap(
                Kind::Keyword(Keyword::FOLLOWING),
                Literal::new_string("FOLLOWING"),
            ),
        ];

        assert_eq!(expected_tokens, tokens);
    }

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
            Token::wrap(
                Kind::Keyword(Keyword::SELECT),
                Literal::new_string("SELECT"),
            ),
            Token::wrap(Kind::Ident, Literal::new_string("name")),
            Token::wrap(Kind::Comma, Literal::new_string(",")),
            Token::wrap(Kind::Ident, Literal::new_string("id")),
            Token::wrap(Kind::Keyword(Keyword::FROM), Literal::new_string("FROM")),
            Token::wrap(Kind::Ident, Literal::new_string("users")),
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
            Token::wrap(
                Kind::Keyword(Keyword::SELECT),
                Literal::new_string("SELECT"),
            ),
            Token::wrap(Kind::Ident, Literal::new_string("[name]")),
            Token::wrap(Kind::Comma, Literal::new_string(",")),
            Token::wrap(Kind::Ident, Literal::new_string("id")),
            Token::wrap(Kind::Keyword(Keyword::FROM), Literal::new_string("FROM")),
            Token::wrap(Kind::Ident, Literal::new_string("users")),
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
            Token::wrap(
                Kind::Keyword(Keyword::SELECT),
                Literal::new_string("SELECT"),
            ),
            Token::wrap(Kind::Ident, Literal::new_string("name")),
            Token::wrap(Kind::Keyword(Keyword::AS), Literal::new_string("AS")),
            Token::wrap(Kind::Ident, Literal::new_string("'SuperName'")),
            Token::wrap(Kind::Comma, Literal::new_string(",")),
            Token::wrap(Kind::Ident, Literal::new_string("id")),
            Token::wrap(Kind::Keyword(Keyword::FROM), Literal::new_string("FROM")),
            Token::wrap(Kind::Ident, Literal::new_string("users")),
        ];

        assert_eq!(expected_tokens, tokens);
    }
}
