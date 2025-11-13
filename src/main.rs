use rust_lalrpop_experiment::parse_idl;

fn main() {
    // Example IDL code
    let idl_code = r#"
        /**
         * Some test comment
         */
        // Other comment
        module Example {
            // Basic types example
            typedef long MyLong;
            typedef string MyString;

            // Enum example
            enum Status {
                ACTIVE,
                INACTIVE,
                PENDING
            };

            // Struct example
            struct Person {
                string name;
                long age;
                boolean isActive;
            };

            // Sequence example
            typedef sequence<long> LongList;
            typedef sequence<string, 100> BoundedStringList;
            typedef sequence<Person> PersonList;

            // Nested sequences
            typedef sequence<sequence<long>> Matrix;
        };
    "#;

    match parse_idl(idl_code) {
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
