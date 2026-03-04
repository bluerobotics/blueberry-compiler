use clap::{
    Parser,
    builder::styling::{AnsiColor, Styles},
};
use std::path::PathBuf;
use std::sync::OnceLock;

static INSTANCE: OnceLock<CliOptions> = OnceLock::new();

/// Parse command-line arguments and store them in the global singleton.
/// Must be called exactly once, typically at the start of `main()`.
pub fn init() {
    INSTANCE
        .set(CliOptions::parse())
        .expect("cli::init() called more than once");
}

/// Return a static reference to the parsed CLI options.
pub fn get() -> &'static CliOptions {
    INSTANCE
        .get()
        .expect("CLI not initialised — call cli::init() first")
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
pub enum OutputFormat {
    Pretty,
    Github,
}

const STYLES: Styles = Styles::styled()
    .header(AnsiColor::Blue.on_default().bold())
    .usage(AnsiColor::Green.on_default().bold())
    .literal(AnsiColor::Cyan.on_default().bold())
    .placeholder(AnsiColor::Cyan.on_default())
    .error(AnsiColor::Red.on_default().bold())
    .valid(AnsiColor::Cyan.on_default().bold())
    .invalid(AnsiColor::Yellow.on_default().bold());

#[derive(Debug, Parser)]
#[command(
    name = "blueberry-cli",
    version,
    about = "Parse Blueberry IDL files and optionally emit normalized IDL or Rust source.",
    styles = STYLES,
)]
pub struct CliOptions {
    /// Path to an IDL source file or a directory of IDL files.
    /// When a directory is given, the folder structure and file names are used
    /// as module scoping in the combined output (only --emit-idl supported).
    #[arg(value_name = "IDL_PATH")]
    pub input: PathBuf,

    /// Emit normalized IDL to stdout.
    #[arg(long)]
    pub emit_idl: bool,

    /// Emit generated Rust bindings.
    #[arg(long)]
    pub emit_rust: bool,

    /// Emit generated C bindings.
    #[arg(long)]
    pub emit_c: bool,

    /// Emit generated C++ bindings.
    #[arg(long)]
    pub emit_cpp: bool,

    /// Emit generated Python bindings.
    #[arg(long)]
    pub emit_python: bool,

    /// Directory where generated files will be written. Defaults to the current directory.
    #[arg(long, value_name = "OUTPUT_DIR")]
    pub output_dir: Option<PathBuf>,

    /// Format for diagnostic messages (errors and warnings).
    #[arg(long, value_enum, default_value_t = OutputFormat::Pretty)]
    pub output_format: OutputFormat,
}
