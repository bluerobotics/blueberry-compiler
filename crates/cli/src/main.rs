use blueberry_generator_rust::generate_rust;
use blueberry_idl_generator::generate_idl;
use blueberry_parser::parse_idl;
use clap::Parser;
use std::{
    error::Error,
    fmt, fs,
    path::{Path, PathBuf},
    process,
};

fn main() {
    let options = CliOptions::parse();

    if let Err(err) = run(&options) {
        eprintln!("error: {err}");
        process::exit(1);
    }
}

fn run(options: &CliOptions) -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string(&options.input)?;
    let definitions = match parse_idl(&contents) {
        Ok(defs) => defs,
        Err(message) => {
            return Err(Box::new(ParseFailure::new(&options.input, message)));
        }
    };

    println!(
        "Parsed {} top-level definitions from {}",
        definitions.len(),
        options.input.display()
    );

    if options.emit_idl {
        let generated = generate_idl(&definitions);
        println!("\n// normalized IDL output\n{generated}");
    }

    if options.emit_rust {
        let generated = generate_rust(&definitions);
        println!("\n// generated Rust module\n{generated}");
    }

    Ok(())
}

#[derive(Parser)]
#[command(
    name = "blueberry-cli",
    version,
    about = "Parse Blueberry IDL files and optionally emit normalized IDL or Rust source."
)]
struct CliOptions {
    /// Path to the IDL source file to parse.
    #[arg(value_name = "IDL_PATH")]
    input: PathBuf,

    /// Emit normalized IDL to stdout.
    #[arg(long)]
    emit_idl: bool,

    /// Emit generated Rust bindings to stdout.
    #[arg(long)]
    emit_rust: bool,
}

#[derive(Debug)]
struct ParseFailure {
    path: PathBuf,
    message: String,
}

impl ParseFailure {
    fn new(path: &Path, message: String) -> Self {
        Self {
            path: path.to_path_buf(),
            message,
        }
    }
}

impl fmt::Display for ParseFailure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "failed to parse {}: {}",
            self.path.display(),
            self.message
        )
    }
}

impl Error for ParseFailure {}
