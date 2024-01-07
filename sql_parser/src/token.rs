use crate::keywords;
use core::fmt;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: Kind,
    pub literal: Literal,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.literal {
            Literal::String(string) => write!(f, "{string}"),
            Literal::Number(number) => write!(f, "{number}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    String(String),
    Number(f64),
}

impl Literal {
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
    Multiply,
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
    pub value: String,
    pub quote_style: Option<char>,
}
