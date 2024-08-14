use lexer::TokenKind;

pub const SELECT_ITEM_TYPE_START: &'static [TokenKind<'static>] = &[
    TokenKind::Identifier(""),
    TokenKind::QuotedIdentifier(""),
    TokenKind::NumberLiteral(""),
    TokenKind::StringLiteral(""),
    TokenKind::LocalVariable(""),
    TokenKind::LeftParen,
    TokenKind::Case,
    TokenKind::Asterisk,
    TokenKind::Minus,
    TokenKind::Plus,
];

pub const GROUP_BY_START: &'static [TokenKind<'static>] =
    &[TokenKind::Identifier(""), TokenKind::QuotedIdentifier("")];

pub const EXPRESSION_LIST_START: &'static [TokenKind<'static>] = &[
    TokenKind::Identifier(""),
    TokenKind::QuotedIdentifier(""),
    TokenKind::NumberLiteral(""),
    TokenKind::StringLiteral(""),
    TokenKind::LocalVariable(""),
];

pub const BUILTIN_FN_START: &'static [TokenKind<'static>] = &[
    TokenKind::Abs,
    TokenKind::Acos,
    TokenKind::Asin,
    TokenKind::Atan,
    TokenKind::Avg,
    // TokenKind::Cast,
    TokenKind::Ceil,
    TokenKind::Ceiling,
    TokenKind::Cos,
    TokenKind::Cot,
    TokenKind::Count,
    TokenKind::Degrees,
    TokenKind::DenseRank,
    TokenKind::Exp,
    TokenKind::Floor,
    TokenKind::Getdate,
    TokenKind::Log,
    TokenKind::Log10,
    TokenKind::Max,
    TokenKind::Min,
    TokenKind::Nullif,
    TokenKind::Pi,
    TokenKind::Power,
    TokenKind::Radians,
    TokenKind::Rank,
    TokenKind::Round,
    TokenKind::RowNumber,
    TokenKind::Sqrt,
    TokenKind::Square,
    TokenKind::Stage,
    TokenKind::Stdev,
    TokenKind::Stdevp,
    TokenKind::Sum,
    TokenKind::Tan,
    TokenKind::Var,
    TokenKind::Varp,
];

pub const ORDER_BY_ARGS_START: &'static [TokenKind<'static>] = &[
    TokenKind::Identifier(""),
    TokenKind::QuotedIdentifier(""),
    TokenKind::NumberLiteral(""),
    TokenKind::LocalVariable(""),
];

pub const PARTITION_BY_START: &'static [TokenKind<'static>] =
    &[TokenKind::Identifier(""), TokenKind::QuotedIdentifier("")];

pub const FUNCTION_ARGS_START: &'static [TokenKind<'static>] = &[
    TokenKind::Identifier(""),
    TokenKind::QuotedIdentifier(""),
    TokenKind::NumberLiteral(""),
    TokenKind::StringLiteral(""),
    TokenKind::LocalVariable(""),
];

pub const TABLE_SOURCE_START: &'static [TokenKind<'static>] = &[
    TokenKind::Identifier(""),
    TokenKind::QuotedIdentifier(""),
    TokenKind::LocalVariable(""),
    TokenKind::LeftParen,
];

