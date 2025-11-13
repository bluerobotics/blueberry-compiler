use lalrpop_util::lalrpop_mod;

pub mod ast;
pub mod generator;
lalrpop_mod!(grammar);
#[cfg(target_arch = "wasm32")]
pub mod wasm;

pub use ast::*;
pub use grammar::*;

/// Parse an IDL file string into a vector of definitions
pub fn parse_idl(input: &str) -> Result<Vec<Definition>, String> {
    let mut defs = grammar::IdlFileParser::new()
        .parse(input)
        .map_err(|e| format!("Parse error: {:?}", e))?;

    // Pos processing AST (Abstract Syntax Tree)
    propagate_enum_values(&mut defs);

    Ok(defs)
}

fn propagate_enum_values(defs: &mut [Definition]) {
    for def in defs {
        match def {
            Definition::EnumDef(enum_def) => assign_enum_members(&mut enum_def.node.enumerators),
            Definition::ModuleDef(module_def) => {
                propagate_enum_values(&mut module_def.node.definitions)
            }
            _ => {}
        }
    }
}

fn assign_enum_members(members: &mut [EnumMember]) {
    let mut next_value = Some(IntegerLiteral::new(0, IntegerBase::Decimal));
    for member in members.iter_mut() {
        match &member.value {
            Some(ConstValue::Integer(integer_literal)) => {
                let mut new_value = integer_literal.clone();
                new_value.value = new_value.value.checked_add(1).unwrap();
                next_value = Some(new_value);
            }
            Some(_) => {
                next_value = None;
            }
            None => {
                if let Some(value) = next_value {
                    member.value = Some(ConstValue::Integer(value.clone()));
                    let mut new_value = value.clone();
                    new_value.value = new_value.value.checked_add(1).unwrap();
                    next_value = Some(new_value);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn parses_example_fixture() {
        let defs = load_fixture("example.idl");
        assert!(
            !defs.is_empty(),
            "example fixture should yield at least one definition"
        );
    }

    fn load_fixture(name: &str) -> Vec<Definition> {
        let path = format!("tests/fixtures/{name}");
        let input = fs::read_to_string(&path)
            .unwrap_or_else(|err| panic!("failed to read fixture {}: {}", path, err));
        parse_idl(&input).expect("fixture should parse successfully")
    }
}
