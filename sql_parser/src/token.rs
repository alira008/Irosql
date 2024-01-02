use crate::keywords;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: Kind,
    pub literal: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Identifer(String),
    Number(f64),
    Keyword(keywords::Keyword),
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

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Ident(Identifier),
    Keyword(keywords::Keyword),
    Number(f64),
    QuotedString(String),

    WhiteSpace(WhiteSpace),
    Asterisk,
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
