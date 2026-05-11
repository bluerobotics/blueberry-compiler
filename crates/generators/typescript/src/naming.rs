//! Identifier conversion + escaping helpers for the TypeScript generator.
#![allow(dead_code)]

/// Convert `snake_case` or `SCREAMING_SNAKE_CASE` to `camelCase`.
pub fn camel_case(input: &str) -> String {
    let mut out = String::new();
    let mut capitalize = false;
    for (i, ch) in input.chars().enumerate() {
        if ch == '_' || ch == '-' {
            capitalize = true;
            continue;
        }
        if i == 0 {
            out.push(ch.to_ascii_lowercase());
        } else if capitalize {
            out.push(ch.to_ascii_uppercase());
            capitalize = false;
        } else {
            out.push(ch);
        }
    }
    if out.is_empty() {
        out.push('_');
    }
    escape_reserved(&out)
}

/// Convert to `PascalCase` (capitalize each underscore-delimited segment).
pub fn pascal_case(input: &str) -> String {
    let mut out = String::new();
    let mut capitalize = true;
    for ch in input.chars() {
        if ch == '_' || ch == '-' {
            capitalize = true;
            continue;
        }
        if capitalize {
            out.push(ch.to_ascii_uppercase());
            capitalize = false;
        } else {
            out.push(ch);
        }
    }
    if out.is_empty() {
        out.push('_');
    }
    out
}

/// Convert to `snake_case` (used for file names).
pub fn snake_case(input: &str) -> String {
    let mut result = String::new();
    let mut prev_lower = false;
    for ch in input.chars() {
        if ch.is_ascii_uppercase() {
            if prev_lower && !result.ends_with('_') {
                result.push('_');
            }
            result.push(ch.to_ascii_lowercase());
            prev_lower = false;
        } else if ch == ' ' || ch == '-' {
            if !result.ends_with('_') {
                result.push('_');
            }
            prev_lower = false;
        } else {
            result.push(ch.to_ascii_lowercase());
            prev_lower = ch.is_ascii_alphanumeric() && ch.is_ascii_lowercase();
        }
    }
    result
}

/// JSON-escape a string suitable for emitting as a TS double-quoted literal.
pub fn quoted_string(value: &str) -> String {
    let mut out = String::with_capacity(value.len() + 2);
    out.push('\'');
    for ch in value.chars() {
        match ch {
            '\'' => out.push_str("\\'"),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            _ => out.push(ch),
        }
    }
    out.push('\'');
    out
}

/// Wrap an identifier in `_` if it collides with a TypeScript reserved word.
fn escape_reserved(name: &str) -> String {
    const RESERVED: &[&str] = &[
        "break",
        "case",
        "catch",
        "class",
        "const",
        "continue",
        "debugger",
        "default",
        "delete",
        "do",
        "else",
        "enum",
        "export",
        "extends",
        "false",
        "finally",
        "for",
        "function",
        "if",
        "import",
        "in",
        "instanceof",
        "new",
        "null",
        "return",
        "super",
        "switch",
        "this",
        "throw",
        "true",
        "try",
        "typeof",
        "var",
        "void",
        "while",
        "with",
        "yield",
        "let",
        "static",
        "implements",
        "interface",
        "package",
        "private",
        "protected",
        "public",
        "as",
        "any",
        "boolean",
        "constructor",
        "declare",
        "module",
        "namespace",
        "number",
        "string",
        "symbol",
        "type",
        "from",
        "of",
    ];
    if RESERVED.iter().any(|r| *r == name) {
        format!("{name}_")
    } else {
        name.to_string()
    }
}

/// Combine an inner scope and a name into a single PascalCase identifier
/// (used to disambiguate nested-module declarations once they're flattened
/// into a single file).
pub fn flattened_pascal(inner_scope: &[String], name: &str) -> String {
    if inner_scope.is_empty() {
        pascal_case(name)
    } else {
        let mut joined: Vec<String> = inner_scope.iter().map(|s| pascal_case(s)).collect();
        joined.push(pascal_case(name));
        joined.join("")
    }
}

/// Combine an inner scope and a constant name into a SCREAMING_SNAKE constant.
pub fn flattened_screaming_snake(inner_scope: &[String], name: &str) -> String {
    if inner_scope.is_empty() {
        snake_case(name).to_ascii_uppercase()
    } else {
        let mut joined: Vec<String> = inner_scope.iter().map(|s| snake_case(s)).collect();
        joined.push(snake_case(name));
        joined.join("_").to_ascii_uppercase()
    }
}
