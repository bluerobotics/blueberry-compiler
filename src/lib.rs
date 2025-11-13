use lalrpop_util::lalrpop_mod;

pub mod ast;
lalrpop_mod!(grammar);

pub use grammar::*;
pub use ast::*;

/// Parse an IDL file string into a vector of definitions
pub fn parse_idl(input: &str) -> Result<Vec<Definition>, String> {
    grammar::IdlFileParser::new()
        .parse(input)
        .map_err(|e| format!("Parse error: {:?}", e))
}
