use crate::{token_new::Token, keywords::Keyword};

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

pub fn get_precedence(token: Token) -> Precedence {
    match token {
        Token::Asterisk | Token::ForwardSlash => Precedence::Product,
        Token::Plus | Token::Minus => Precedence::Sum,
        Token::Equal
        | Token::BangEqual
        | Token::LessThanGreaterThan
        | Token::LessThan
        | Token::LessThanEqual
        | Token::GreaterThan
        | Token::GreaterThanEqual => Precedence::Comparison,
        Token::Keyword(Keyword::NOT) => Precedence::Not,
        Token::Keyword(Keyword::AND) => Precedence::And,
        Token::Keyword(Keyword::ALL)
        | Token::Keyword(Keyword::ANY)
        | Token::Keyword(Keyword::BETWEEN)
        | Token::Keyword(Keyword::IN)
        | Token::Keyword(Keyword::LIKE)
        | Token::Keyword(Keyword::OR)
        | Token::Keyword(Keyword::SOME) => Precedence::OtherLogicals,
        _ => Precedence::Lowest,
    }
}
