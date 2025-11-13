use rust_lalrpop_experiment::parse_idl;
use std::fs;

fn main() {
    // Load IDL code from file
    let idl_code = match fs::read_to_string("example.idl") {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading example.idl: {}", e);
            std::process::exit(1);
        }
    };

    match parse_idl(&idl_code) {
        Ok(definitions) => {
            println!("Successfully parsed {} definition(s):\n", definitions.len());
            for (i, def) in definitions.iter().enumerate() {
                println!("Definition {}: {:#?}\n", i + 1, def);
            }
        }
        Err(e) => {
            eprintln!("Error parsing IDL: {}", e);
            std::process::exit(1);
        }
    }
}
