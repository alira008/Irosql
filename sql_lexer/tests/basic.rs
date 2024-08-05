#[cfg(test)]
mod tests {
    use sql_lexer::{Lexer, TokenKind, Keyword, LexicalErrorType, LexicalError};

    #[test]
    fn test_random_tokens() {
        let input = "DesC current , preceding ;    .";
        let lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        for result in lexer {
            let token = result.unwrap();
            tokens.push(token.kind());
        }
        let expected_tokens = vec![
            TokenKind::Keyword(Keyword::DESC),
            TokenKind::Keyword(Keyword::CURRENT),
            TokenKind::Comma,
            TokenKind::Keyword(Keyword::PRECEDING),
            TokenKind::SemiColon,
            TokenKind::Period,
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
        }

        let expected_tokens = vec![
            TokenKind::Keyword(Keyword::SELECT),
            TokenKind::Identifier("name"),
            TokenKind::Comma,
            TokenKind::Identifier("id"),
            TokenKind::Keyword(Keyword::FROM),
            TokenKind::Identifier("users"),
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
        }

        let expected_tokens = vec![
            TokenKind::Keyword(Keyword::SELECT),
            TokenKind::QuotedIdentifier("name"),
            TokenKind::Comma,
            TokenKind::LocalVariable("hello"),
            TokenKind::Identifier("id"),
            TokenKind::Keyword(Keyword::FROM),
            TokenKind::Identifier("users"),
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
        }

        let expected_tokens = vec![
            TokenKind::Keyword(Keyword::SELECT),
            TokenKind::Identifier("name"),
            TokenKind::Keyword(Keyword::AS),
            TokenKind::StringLiteral("SuperName"),
            TokenKind::Comma,
            TokenKind::Identifier("id"),
            TokenKind::Keyword(Keyword::FROM),
            TokenKind::Identifier("users"),
        ];

        assert_eq!(expected_tokens, tokens);
    }

    #[test]
    fn test_comment() {
        let input = "select name as 'SuperName',-- yes id \nfrom users";
        let lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        for result in lexer {
            let token = result.unwrap();
            tokens.push(token.kind());
        }

        let expected_tokens = vec![
            TokenKind::Keyword(Keyword::SELECT),
            TokenKind::Identifier("name"),
            TokenKind::Keyword(Keyword::AS),
            TokenKind::StringLiteral("SuperName"),
            TokenKind::Comma,
            TokenKind::Comment("yes id"),
            TokenKind::Keyword(Keyword::FROM),
            TokenKind::Identifier("users"),
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
        }

        let expected_tokens = vec![
            Ok(TokenKind::Keyword(Keyword::SELECT)),
            Ok(TokenKind::Identifier("name")),
            Ok(TokenKind::Keyword(Keyword::AS)),
            Err(LexicalError {
                error: LexicalErrorType::UnexpectedStringEnd,
            }),
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
        }

        let expected_tokens = vec![
            Ok(TokenKind::Keyword(Keyword::SELECT)),
            Ok(TokenKind::Identifier("name")),
            Ok(TokenKind::Keyword(Keyword::AS)),
            Err(LexicalError {
                error: LexicalErrorType::UnexpectedQuotedIdentifierEnd,
            }),
        ];

        assert_eq!(expected_tokens, tokens);
    }
}
