use std::fmt;

/// A parse error with location information for pretty-printing.
#[derive(Debug, Clone)]
pub enum ParseError {
    /// An invalid token was encountered at the given byte offset.
    InvalidToken { location: usize },

    /// The parser reached the end of input unexpectedly.
    UnrecognizedEof {
        location: usize,
        expected: Vec<String>,
    },

    /// An unexpected token was encountered.
    UnrecognizedToken {
        /// Byte range `(start, end)` of the offending token.
        span: (usize, usize),
        /// The text of the token that was found.
        token: String,
        /// Raw LALRPOP token names that would have been valid here.
        expected: Vec<String>,
    },

    /// A valid parse was followed by an extra token.
    ExtraToken { span: (usize, usize), token: String },

    /// A semantic validation error (e.g. missing annotation).
    Validation { message: String },
}

impl ParseError {
    pub(crate) fn from_lalrpop<E: fmt::Display>(
        err: lalrpop_util::ParseError<usize, lalrpop_util::lexer::Token<'_>, E>,
    ) -> Self {
        match err {
            lalrpop_util::ParseError::InvalidToken { location } => {
                ParseError::InvalidToken { location }
            }
            lalrpop_util::ParseError::UnrecognizedEof { location, expected } => {
                ParseError::UnrecognizedEof { location, expected }
            }
            lalrpop_util::ParseError::UnrecognizedToken {
                token: (start, tok, end),
                expected,
            } => ParseError::UnrecognizedToken {
                span: (start, end),
                token: tok.1.to_string(),
                expected,
            },
            lalrpop_util::ParseError::ExtraToken {
                token: (start, tok, end),
            } => ParseError::ExtraToken {
                span: (start, end),
                token: tok.1.to_string(),
            },
            lalrpop_util::ParseError::User { error } => ParseError::Validation {
                message: error.to_string(),
            },
        }
    }

    /// Byte span of the error, if available.
    pub fn span(&self) -> Option<(usize, usize)> {
        match self {
            ParseError::InvalidToken { location } => Some((*location, *location + 1)),
            ParseError::UnrecognizedEof { location, .. } => Some((*location, *location)),
            ParseError::UnrecognizedToken { span, .. } | ParseError::ExtraToken { span, .. } => {
                Some(*span)
            }
            ParseError::Validation { .. } => None,
        }
    }

    /// Human-readable description of the expected tokens, if applicable.
    pub fn format_expected(&self) -> Option<String> {
        let expected = match self {
            ParseError::UnrecognizedToken { expected, .. }
            | ParseError::UnrecognizedEof { expected, .. } => expected,
            _ => return None,
        };
        if expected.is_empty() {
            return None;
        }

        let mut names: Vec<String> = expected.iter().map(|t| clean_token_name(t)).collect();
        names.sort();
        names.dedup();

        if names.len() == 1 {
            Some(format!("expected {}", names[0]))
        } else {
            let (last, rest) = names.split_last().unwrap();
            Some(format!("expected {}, or {last}", rest.join(", ")))
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::InvalidToken { location } => {
                write!(f, "invalid token at position {location}")
            }
            ParseError::UnrecognizedEof { .. } => {
                write!(f, "unexpected end of input")?;
                if let Some(expected) = self.format_expected() {
                    write!(f, ", {expected}")?;
                }
                Ok(())
            }
            ParseError::UnrecognizedToken { token, .. } => {
                write!(f, "unexpected token `{token}`")?;
                if let Some(expected) = self.format_expected() {
                    write!(f, ", {expected}")?;
                }
                Ok(())
            }
            ParseError::ExtraToken { token, .. } => {
                write!(f, "unexpected extra token `{token}`")
            }
            ParseError::Validation { message } => write!(f, "{message}"),
        }
    }
}

impl std::error::Error for ParseError {}

/// Translate a raw LALRPOP token name into something humans can read.
fn clean_token_name(raw: &str) -> String {
    if let Some(inner) = raw.strip_prefix('"').and_then(|s| s.strip_suffix('"')) {
        return format!("`{inner}`");
    }
    if let Some(inner) = raw.strip_prefix("r#\"").and_then(|s| s.strip_suffix("\"#")) {
        return describe_regex_pattern(inner);
    }
    raw.to_string()
}

fn describe_regex_pattern(pattern: &str) -> String {
    if pattern.contains('@') {
        return "annotation (e.g. `@topic`)".to_string();
    }
    if pattern.starts_with("//") || pattern.starts_with("/\\*") {
        return "comment".to_string();
    }
    if pattern.starts_with("[a-zA-Z_]") {
        return "identifier".to_string();
    }
    if pattern.starts_with("-?0[xX]") {
        return "hex literal".to_string();
    }
    if pattern.starts_with("-?0[bB]") {
        return "binary literal".to_string();
    }
    if pattern.starts_with("-?0[0-7]") {
        return "octal literal".to_string();
    }
    if pattern.starts_with("-?[0-9]+\\.[0-9]") || pattern.starts_with("-?\\.[0-9]") {
        return "number literal".to_string();
    }
    if pattern.starts_with("-?[0-9]+[eE]") {
        return "number literal".to_string();
    }
    if pattern.starts_with("-?0") || pattern.starts_with("-?[1-9]") {
        return "integer literal".to_string();
    }
    if pattern.starts_with('"') {
        return "string literal".to_string();
    }
    if pattern.starts_with('\'') {
        return "char literal".to_string();
    }
    format!("`{pattern}`")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cleans_keyword_tokens() {
        assert_eq!(clean_token_name("\"const\""), "`const`");
        assert_eq!(clean_token_name("\"module\""), "`module`");
        assert_eq!(clean_token_name("\";\""), "`;`");
    }

    #[test]
    fn cleans_regex_tokens() {
        let annotation = "r#\"@(::)?[a-zA-Z_][a-zA-Z0-9_]*(::[a-zA-Z_][a-zA-Z0-9_]*)*\"#";
        assert_eq!(clean_token_name(annotation), "annotation (e.g. `@topic`)");
    }

    #[test]
    fn format_expected_deduplicates() {
        let err = ParseError::UnrecognizedToken {
            span: (0, 1),
            token: "x".into(),
            expected: vec!["\"const\"".into(), "\"const\"".into(), "\"enum\"".into()],
        };
        let formatted = err.format_expected().unwrap();
        assert_eq!(formatted, "expected `const`, or `enum`");
    }
}
