use sql_lexer::Keyword;
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
    LocalVariable,
    Keyword(Keyword),
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
    PlusEqual,
    MinusEqual,
    DivideEqual,
    MultiplyEqual,
    PercentEqual,
    AndEqual,
    OrEqual,
    CaretEqual,
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

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        
        match self {
            Kind::Ident => f.write_str("IDENTIFIER"),
            Kind::LocalVariable => f.write_str("LOCAL_VARIABLE"),
            Kind::Keyword(k) => write!(f, "KEYWORD({:?})", k),
            Kind::Number => f.write_str("NUMBER"),
            Kind::Comma => f.write_str(","),
            Kind::LeftParen => f.write_str("("),
            Kind::RightParen => f.write_str(")"),
            Kind::DoubleEqual => f.write_str("=="),
            Kind::Equal => f.write_str("="),
            Kind::NotEqual => f.write_str("!="),
            Kind::LessThan => f.write_str("<"),
            Kind::LessThanEqual => f.write_str("<="),
            Kind::GreaterThan => f.write_str(">"),
            Kind::GreaterThanEqual => f.write_str(">="),
            Kind::Plus => f.write_str("+"),
            Kind::Minus => f.write_str("-"),
            Kind::Divide => f.write_str("/"),
            Kind::Asterisk => f.write_str("*"),
            Kind::Mod => f.write_str("%"),
            Kind::Period => f.write_str("."),
            Kind::SemiColon => f.write_str(";"),
            Kind::LeftBracket => f.write_str("["),
            Kind::RightBracket => f.write_str("]"),
            Kind::LeftBrace => f.write_str("{"),
            Kind::RightBrace => f.write_str("}"),
            Kind::Tilde => f.write_str("~"),
            Kind::ExclamationMark => f.write_str("!"),
            Kind::Illegal => f.write_str("ILLEGAL"),
            Kind::Eof => f.write_str("EOF"),
            Kind::PlusEqual => f.write_str("+="),
            Kind::MinusEqual => f.write_str("-="),
            Kind::DivideEqual => f.write_str("/="),
            Kind::MultiplyEqual => f.write_str("*="),
            Kind::PercentEqual => f.write_str("%="),
            Kind::AndEqual => f.write_str("&="),
            Kind::OrEqual => f.write_str("|="),
            Kind::CaretEqual => f.write_str("^="),
        }
    }
}
