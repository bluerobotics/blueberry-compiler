use blueberry_codegen_core::{CodegenError, GeneratedFile};
use blueberry_generator_c as generator_c;
use blueberry_generator_cpp as generator_cpp;
use blueberry_generator_idl::generate_idl;
use blueberry_generator_python as generator_python;
use blueberry_generator_rust::generate_rust;
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
        let files = generate_rust(&definitions).map_err(|err| fmt_codegen_error("Rust", err))?;
        print_generated_files("Rust", &files);
    }

    if options.emit_c {
        let files =
            generator_c::generate(&definitions).map_err(|err| fmt_codegen_error("C", err))?;
        print_generated_files("C", &files);
    }

    if options.emit_cpp {
        let files =
            generator_cpp::generate(&definitions).map_err(|err| fmt_codegen_error("C++", err))?;
        print_generated_files("C++", &files);
    }

    if options.emit_python {
        let files = generator_python::generate(&definitions)
            .map_err(|err| fmt_codegen_error("Python", err))?;
        print_generated_files("Python", &files);
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

    /// Emit generated C bindings to stdout.
    #[arg(long)]
    emit_c: bool,

    /// Emit generated C++ bindings to stdout.
    #[arg(long)]
    emit_cpp: bool,

    /// Emit generated Python bindings to stdout.
    #[arg(long)]
    emit_python: bool,
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

fn fmt_codegen_error(language: &str, err: CodegenError) -> Box<dyn Error> {
    let message = match err {
        CodegenError::MissingTopic { message } => {
            format!("{language} generation failed: missing @topic for message {message}")
        }
        CodegenError::UnsupportedMessageBase { message } => format!(
            "{language} generation failed: message {message} uses inheritance which is unsupported"
        ),
        CodegenError::UnsupportedMemberType {
            message,
            member,
            type_name,
        } => format!(
            "{language} generation failed: member {member} in {message} uses unsupported type {type_name}"
        ),
    };
    message.into()
}

fn print_generated_files(language: &str, files: &[GeneratedFile]) {
    if files.is_empty() {
        println!("\n// generated {language} files: none");
        return;
    }
    println!("\n// generated {language} files");
    for file in files {
        println!("\n// File: {}", file.path);
        println!("{}", file.contents);
    }
}
