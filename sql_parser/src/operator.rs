use sql_lexer::TokenKind;

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
        TokenKind::Not => Precedence::Not,
        TokenKind::And => Precedence::And,
        TokenKind::All
        | TokenKind::Any
        | TokenKind::Between
        | TokenKind::In
        | TokenKind::Like
        | TokenKind::Or
        | TokenKind::Some => Precedence::OtherLogicals,
        _ => Precedence::Lowest,
    }
}
