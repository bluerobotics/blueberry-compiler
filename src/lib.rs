use lalrpop_util::lalrpop_mod;

pub mod ast;
lalrpop_mod!(grammar);
#[cfg(target_arch = "wasm32")]
pub mod wasm;

pub use ast::*;
pub use grammar::*;

/// Parse an IDL file string into a vector of definitions
pub fn parse_idl(input: &str) -> Result<Vec<Definition>, String> {
    grammar::IdlFileParser::new()
        .parse(input)
        .map_err(|e| format!("Parse error: {:?}", e))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn parses_example_fixture() {
        let input = fs::read_to_string("tests/fixtures/example.idl")
            .expect("example fixture should be available during tests");
        let defs = parse_idl(&input).expect("example fixture should parse successfully");
        assert!(
            !defs.is_empty(),
            "example fixture should yield at least one definition"
        );
    }
}
