use crate::keywords;
use core::fmt;

#[derive(Debug, Clone)]
pub struct Token {
    kind: Kind,
    literal: Literal,
    location: Location,
}

impl PartialEq<Token> for Token {
    fn eq(&self, other: &Token) -> bool {
        self.kind == other.kind
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "line {}, column {}", self.line + 1, self.column + 1)
    }
}

impl Location {
    pub fn new(line: usize, column: usize) -> Self {
        Location { line, column }
    }

    pub fn zero() -> Self {
        Location { line: 0, column: 0 }
    }
}

impl Token {
    pub fn new(kind: Kind, literal: Literal, location: Location) -> Self {
        Token {
            kind,
            literal,
            location,
        }
    }

    pub fn wrap(kind: Kind, literal: Literal) -> Self {
        Token {
            kind,
            literal,
            location: Location::zero(),
        }
    }

    pub fn wrap_kind(kind: Kind) -> Self {
        Token {
            kind,
            literal: Literal::new_string(""),
            location: Location::zero(),
        }
    }

    pub fn kind(&self) -> Kind {
        self.kind
    }

    pub fn literal(&self) -> &Literal {
        &self.literal
    }

    pub fn location(&self) -> &Location {
        &self.location
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.literal())?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    String(String),
    Number(f64),
    QuotedString { value: String, quote_style: char },
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Literal::String(string) => write!(f, "{}", string),
            Literal::QuotedString { value, quote_style } => match quote_style {
                '\'' => write!(f, "'{}'", value),
                '[' => write!(f, "[{}]", value),
                _ => unreachable!(),
            },
            Literal::Number(number) => write!(f, "{}", number),
        }
    }
}

impl Literal {
    pub fn new_quoted(string: &str, ch: char) -> Self {
        Literal::QuotedString {
            value: string.to_string(),
            quote_style: ch,
        }
    }
    pub fn new_string(string: &str) -> Self {
        Literal::String(string.to_string())
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Kind {
    Ident,
    Keyword(keywords::Keyword),
    Number,
    Comma,
    LeftParen,
    RightParen,
    DoubleEqual,
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Plus,
    Minus,
    Divide,
    Asterisk,
    Mod,
    Period,
    SemiColon,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    Tilde,
    ExclamationMark,
    Illegal,
    Eof,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum WhiteSpace {
    Space,
    Tab,
    NewLine,
    CarriageReturn,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    value: String,
    quote_style: Option<char>,
}
