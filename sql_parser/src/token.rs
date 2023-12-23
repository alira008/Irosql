use crate::keywords;

#[derive(Debug, PartialEq)]
pub enum TokenType {
    Ident(Identifier),
    Keyword(keywords::Keyword),
    Number(f64),
    QuotedString(String),

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

#[derive(Debug, PartialEq)]
pub struct Identifier {
    pub value: String,
    pub quote_style: Option<char>
}
