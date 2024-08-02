use crate::{token_new::TokenKind, keywords::Keyword};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Lowest,
    Assignment,
    OtherLogicals,
    And,
    Not,
    Comparison,
    Sum,
    Product,
    Highest,
}

pub fn get_precedence(token: &TokenKind) -> Precedence {
    match token {
        TokenKind::Asterisk | TokenKind::ForwardSlash => Precedence::Product,
        TokenKind::Plus | TokenKind::Minus => Precedence::Sum,
        TokenKind::Equal
        | TokenKind::BangEqual
        | TokenKind::LessThanGreaterThan
        | TokenKind::LessThan
        | TokenKind::LessThanEqual
        | TokenKind::GreaterThan
        | TokenKind::GreaterThanEqual => Precedence::Comparison,
        TokenKind::Keyword(Keyword::NOT) => Precedence::Not,
        TokenKind::Keyword(Keyword::AND) => Precedence::And,
        TokenKind::Keyword(Keyword::ALL)
        | TokenKind::Keyword(Keyword::ANY)
        | TokenKind::Keyword(Keyword::BETWEEN)
        | TokenKind::Keyword(Keyword::IN)
        | TokenKind::Keyword(Keyword::LIKE)
        | TokenKind::Keyword(Keyword::OR)
        | TokenKind::Keyword(Keyword::SOME) => Precedence::OtherLogicals,
        _ => Precedence::Lowest,
    }
}
