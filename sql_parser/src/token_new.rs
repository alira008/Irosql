use crate::{ast::Span, keywords::Keyword};
use core::fmt;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Token<'a> {
    kind: TokenKind<'a>,
    location: Span,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind<'a>, location: Span) -> Token<'a> {
        Self {kind, location}
    }

    pub fn kind(&self) -> &TokenKind<'a> {
        &self.kind
    }

    pub fn shallow_eq_token(&self, other: &Token) -> bool {
        let our = self.kind;
        let other_token = other.kind;
        match (our, other_token) {
            (TokenKind::Identifier(_), TokenKind::Identifier(_)) => true,
            (TokenKind::QuotedIdentifier(_), TokenKind::QuotedIdentifier(_)) => true,
            (TokenKind::StringLiteral(_), TokenKind::StringLiteral(_)) => true,
            (TokenKind::NumberLiteral(_), TokenKind::NumberLiteral(_)) => true,
            (TokenKind::LocalVariable(_), TokenKind::LocalVariable(_)) => true,
            (TokenKind::Comment(_), TokenKind::Comment(_)) => true,
            (TokenKind::Keyword(_), TokenKind::Keyword(_)) => true,
            (TokenKind::Comma, TokenKind::Comma) => true,
            (TokenKind::LeftParen, TokenKind::LeftParen) => true,
            (TokenKind::RightParen, TokenKind::RightParen) => true,
            (TokenKind::Equal, TokenKind::Equal) => true,
            (TokenKind::BangEqual, TokenKind::BangEqual) => true,
            (TokenKind::LessThanGreaterThan, TokenKind::LessThanGreaterThan) => true,
            (TokenKind::LessThan, TokenKind::LessThan) => true,
            (TokenKind::LessThanEqual, TokenKind::LessThanEqual) => true,
            (TokenKind::GreaterThan, TokenKind::GreaterThan) => true,
            (TokenKind::GreaterThanEqual, TokenKind::GreaterThanEqual) => true,
            (TokenKind::Plus, TokenKind::Plus) => true,
            (TokenKind::Minus, TokenKind::Minus) => true,
            (TokenKind::ForwardSlash, TokenKind::ForwardSlash) => true,
            (TokenKind::Asterisk, TokenKind::Asterisk) => true,
            (TokenKind::Percent, TokenKind::Percent) => true,
            (TokenKind::Period, TokenKind::Period) => true,
            (TokenKind::SemiColon, TokenKind::SemiColon) => true,
            (TokenKind::Eof, TokenKind::Eof) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenKind<'a> {
    Identifier(&'a str),
    QuotedIdentifier(&'a str),
    StringLiteral(&'a str),
    NumberLiteral(&'a str),
    LocalVariable(&'a str),
    Comment(&'a str),
    Keyword(Keyword),
    Comma,
    LeftParen,
    RightParen,
    Equal,
    BangEqual,
    LessThanGreaterThan,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Plus,
    Minus,
    ForwardSlash,
    Asterisk,
    Percent,
    Period,
    SemiColon,
    Eof,
    // PlusEqual,
    // MinusEqual,
    // DivideEqual,
    // MultiplyEqual,
    // PercentEqual,
    // AndEqual,
    // OrEqual,
    // CaretEqual,
}

impl<'a> TokenKind<'a> {
    pub fn default_identifier() -> Self {
        Self::Identifier("")
    }
    pub fn default_quoted_identifier() -> Self {
        Self::QuotedIdentifier("")
    }
    pub fn default_string_literal() -> Self {
        Self::StringLiteral("")
    }
    pub fn default_number_literal() -> Self {
        Self::NumberLiteral("")
    }
    pub fn default_local_variable() -> Self {
        Self::LocalVariable("")
    }
    pub fn default_comment() -> Self {
        Self::Comment("")
    }
    pub fn shallow_eq_token(&self, other: &TokenKind) -> bool {
        match (self, other) {
            (&TokenKind::Identifier(_), &TokenKind::Identifier(_)) => true,
            (&TokenKind::QuotedIdentifier(_), &TokenKind::QuotedIdentifier(_)) => true,
            (&TokenKind::StringLiteral(_), &TokenKind::StringLiteral(_)) => true,
            (&TokenKind::NumberLiteral(_), &TokenKind::NumberLiteral(_)) => true,
            (&TokenKind::LocalVariable(_), &TokenKind::LocalVariable(_)) => true,
            (&TokenKind::Comment(_), &TokenKind::Comment(_)) => true,
            (&TokenKind::Keyword(_), &TokenKind::Keyword(_)) => true,
            (&TokenKind::Comma, &TokenKind::Comma) => true,
            (&TokenKind::LeftParen, &TokenKind::LeftParen) => true,
            (&TokenKind::RightParen, &TokenKind::RightParen) => true,
            (&TokenKind::Equal, &TokenKind::Equal) => true,
            (&TokenKind::BangEqual, &TokenKind::BangEqual) => true,
            (&TokenKind::LessThanGreaterThan, &TokenKind::LessThanGreaterThan) => true,
            (&TokenKind::LessThan, &TokenKind::LessThan) => true,
            (&TokenKind::LessThanEqual, &TokenKind::LessThanEqual) => true,
            (&TokenKind::GreaterThan, &TokenKind::GreaterThan) => true,
            (&TokenKind::GreaterThanEqual, &TokenKind::GreaterThanEqual) => true,
            (&TokenKind::Plus, &TokenKind::Plus) => true,
            (&TokenKind::Minus, &TokenKind::Minus) => true,
            (&TokenKind::ForwardSlash, &TokenKind::ForwardSlash) => true,
            (&TokenKind::Asterisk, &TokenKind::Asterisk) => true,
            (&TokenKind::Percent, &TokenKind::Percent) => true,
            (&TokenKind::Period, &TokenKind::Period) => true,
            (&TokenKind::SemiColon, &TokenKind::SemiColon) => true,
            (&TokenKind::Eof, &TokenKind::Eof) => true,
            _ => false,
        }
    }
}

impl<'a> fmt::Display for TokenKind<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenKind::Identifier(i) => write!(f, "{}", i),
            TokenKind::QuotedIdentifier(i) => write!(f, "{}", i),
            TokenKind::StringLiteral(s) => write!(f, "{}", s),
            TokenKind::NumberLiteral(n) => write!(f, "{}", n),
            TokenKind::LocalVariable(v) => write!(f, "{}", v),
            TokenKind::Comment(c) => write!(f, "-- {}", c),
            TokenKind::Keyword(k) => write!(f, "{}", k),
            TokenKind::Comma => f.write_str(","),
            TokenKind::LeftParen => f.write_str("("),
            TokenKind::RightParen => f.write_str(")"),
            TokenKind::Equal => f.write_str("="),
            TokenKind::BangEqual => f.write_str("!="),
            TokenKind::LessThanGreaterThan => f.write_str("<>"),
            TokenKind::LessThan => f.write_str("<"),
            TokenKind::LessThanEqual => f.write_str("<="),
            TokenKind::GreaterThan => f.write_str(">"),
            TokenKind::GreaterThanEqual => f.write_str(">="),
            TokenKind::Plus => f.write_str("+"),
            TokenKind::Minus => f.write_str("-"),
            TokenKind::ForwardSlash => f.write_str("/"),
            TokenKind::Asterisk => f.write_str("*"),
            TokenKind::Percent => f.write_str("%"),
            TokenKind::Period => f.write_str("."),
            TokenKind::SemiColon => f.write_str(";"),
            // Token::LeftBracket => f.write_str("["),
            // Token::RightBracket => f.write_str("]"),
            // Token::LeftBrace => f.write_str("{"),
            // Token::RightBrace => f.write_str("}"),
            TokenKind::Eof => f.write_str("EOF"),
            // Token::PlusEqual => f.write_str("+="),
            // Token::MinusEqual => f.write_str("-="),
            // Token::DivideEqual => f.write_str("/="),
            // Token::MultiplyEqual => f.write_str("*="),
            // Token::PercentEqual => f.write_str("%="),
            // Token::AndEqual => f.write_str("&="),
            // Token::OrEqual => f.write_str("|="),
            // Token::CaretEqual => f.write_str("^="),
        }
    }
}
