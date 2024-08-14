use sql_lexer::{Lexer, LexicalError, LexicalErrorType, Span, TokenKind};

#[test]
fn test_random_tokens() {
    let input = "DesC current , preceding ;    .";
    let lexer = Lexer::new(input);
    let mut tokens = Vec::new();
    for result in lexer {
        let token = result.unwrap();
        tokens.push(token.kind());
        if result.is_ok_and(|t| t.shallow_eq_token_kind(&TokenKind::Eof)) {
            break;
        }
    }
    let expected_tokens = vec![
        TokenKind::Desc,
        TokenKind::Current,
        TokenKind::Comma,
        TokenKind::Preceding,
        TokenKind::SemiColon,
        TokenKind::Period,
        TokenKind::Eof,
    ];

    assert_eq!(expected_tokens, tokens);
}

#[test]
fn test_identifiers_unquoted() {
    let input = "select name, id from users";
    let lexer = Lexer::new(input);
    let mut tokens = Vec::new();
    for result in lexer {
        let token = result.unwrap();
        tokens.push(token.kind());
        if result.is_ok_and(|t| t.shallow_eq_token_kind(&TokenKind::Eof)) {
            break;
        }
    }

    let expected_tokens = vec![
        TokenKind::Select,
        TokenKind::Identifier("name"),
        TokenKind::Comma,
        TokenKind::Identifier("id"),
        TokenKind::From,
        TokenKind::Identifier("users"),
        TokenKind::Eof,
    ];

    assert_eq!(expected_tokens, tokens);
}

#[test]
fn test_identifiers_quoted() {
    let input = "select [name], @hello id from users";
    let lexer = Lexer::new(input);
    let mut tokens = Vec::new();
    for result in lexer {
        let token = result.unwrap();
        tokens.push(token.kind());
        if result.is_ok_and(|t| t.shallow_eq_token_kind(&TokenKind::Eof)) {
            break;
        }
    }

    let expected_tokens = vec![
        TokenKind::Select,
        TokenKind::QuotedIdentifier("name"),
        TokenKind::Comma,
        TokenKind::LocalVariable("hello"),
        TokenKind::Identifier("id"),
        TokenKind::From,
        TokenKind::Identifier("users"),
        TokenKind::Eof,
    ];

    assert_eq!(expected_tokens, tokens);
}

#[test]
fn test_string() {
    let input = "select name as 'SuperName', id from users";
    let lexer = Lexer::new(input);
    let mut tokens = Vec::new();
    for result in lexer {
        let token = result.unwrap();
        tokens.push(token.kind());
        if result.is_ok_and(|t| t.shallow_eq_token_kind(&TokenKind::Eof)) {
            break;
        }
    }

    let expected_tokens = vec![
        TokenKind::Select,
        TokenKind::Identifier("name"),
        TokenKind::As,
        TokenKind::StringLiteral("SuperName"),
        TokenKind::Comma,
        TokenKind::Identifier("id"),
        TokenKind::From,
        TokenKind::Identifier("users"),
        TokenKind::Eof,
    ];

    assert_eq!(expected_tokens, tokens);
}

#[test]
fn test_comment() {
    let input = "select name as 'SuperName',--yes id \nfrom users";
    let lexer = Lexer::new(input);
    let mut tokens = Vec::new();
    for result in lexer {
        let token = result.unwrap();
        tokens.push(token.kind());
        if result.is_ok_and(|t| t.shallow_eq_token_kind(&TokenKind::Eof)) {
            break;
        }
    }

    let expected_tokens = vec![
        TokenKind::Select,
        TokenKind::Identifier("name"),
        TokenKind::As,
        TokenKind::StringLiteral("SuperName"),
        TokenKind::Comma,
        TokenKind::Comment("yes id"),
        TokenKind::From,
        TokenKind::Identifier("users"),
        TokenKind::Eof,
    ];

    assert_eq!(expected_tokens, tokens);
}

#[test]
fn test_illegal_string_literal() {
    let input = "select name as 'SuperName, yess id from users";
    let lexer = Lexer::new(input);
    let mut tokens = Vec::new();
    for result in lexer {
        tokens.push(result.map(|t| t.kind()));
        if result.is_ok_and(|t| t.shallow_eq_token_kind(&TokenKind::Eof)) {
            break;
        }
    }

    let expected_tokens = vec![
        Ok(TokenKind::Select),
        Ok(TokenKind::Identifier("name")),
        Ok(TokenKind::As),
        Err(LexicalError {
            error: LexicalErrorType::UnexpectedStringEnd,
            span: Span { start: 16, end: 44 },
        }),
        Ok(TokenKind::Eof),
    ];

    assert_eq!(expected_tokens, tokens);
}

#[test]
fn test_illegal_quoted_identifier() {
    let input = "select name as [SuperName, yess id from users";
    let lexer = Lexer::new(input);
    let mut tokens = Vec::new();
    for result in lexer {
        tokens.push(result.map(|t| t.kind()));
        if result.is_ok_and(|t| t.shallow_eq_token_kind(&TokenKind::Eof)) {
            break;
        }
    }

    let expected_tokens = vec![
        Ok(TokenKind::Select),
        Ok(TokenKind::Identifier("name")),
        Ok(TokenKind::As),
        Err(LexicalError {
            error: LexicalErrorType::UnexpectedQuotedIdentifierEnd,
            span: Span { start: 16, end: 44 },
        }),
        Ok(TokenKind::Eof),
    ];

    assert_eq!(expected_tokens, tokens);
}
