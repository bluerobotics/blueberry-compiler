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

    let output_root = options
        .output_dir
        .clone()
        .unwrap_or_else(|| PathBuf::from("."));

    if options.emit_rust {
        let files = generate_rust(&definitions).map_err(|err| fmt_codegen_error("Rust", err))?;
        write_generated_files("Rust", &output_root, &files)?;
    }

    if options.emit_c {
        let files =
            generator_c::generate(&definitions).map_err(|err| fmt_codegen_error("C", err))?;
        write_generated_files("C", &output_root, &files)?;
    }

    if options.emit_cpp {
        let files =
            generator_cpp::generate(&definitions).map_err(|err| fmt_codegen_error("C++", err))?;
        write_generated_files("C++", &output_root, &files)?;
    }

    if options.emit_python {
        let files = generator_python::generate(&definitions)
            .map_err(|err| fmt_codegen_error("Python", err))?;
        write_generated_files("Python", &output_root, &files)?;
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

    /// Emit generated Rust bindings.
    #[arg(long)]
    emit_rust: bool,

    /// Emit generated C bindings.
    #[arg(long)]
    emit_c: bool,

    /// Emit generated C++ bindings.
    #[arg(long)]
    emit_cpp: bool,

    /// Emit generated Python bindings.
    #[arg(long)]
    emit_python: bool,

    /// Directory where generated files will be written. Defaults to the current directory.
    #[arg(long, value_name = "OUTPUT_DIR")]
    output_dir: Option<PathBuf>,
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

fn write_generated_files(
    language: &str,
    output_root: &Path,
    files: &[GeneratedFile],
) -> Result<(), Box<dyn Error>> {
    if files.is_empty() {
        println!("\n// generated {language} files: none");
        return Ok(());
    }
    println!(
        "\n// writing generated {language} files under {}",
        output_root.display()
    );
    for file in files {
        let path = output_root.join(&file.path);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }
        fs::write(&path, &file.contents)?;
        println!("Wrote {}", path.display());
    }

    Ok(())
}
