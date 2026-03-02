use ariadne::{Color, Label, Report, ReportKind, Source};
use blueberry_codegen_core::{CodegenError, GeneratedFile};
use blueberry_generator_c as generator_c;
use blueberry_generator_cpp as generator_cpp;
use blueberry_generator_idl::generate_idl;
use blueberry_generator_python as generator_python;
use blueberry_generator_rust::generate_rust;
use blueberry_parser::{Annotation, Commented, Definition, ModuleDef, ParseError, parse_idl};
use clap::{
    Parser,
    builder::styling::{AnsiColor, Styles},
};
use std::collections::HashMap;

const STYLES: Styles = Styles::styled()
    .header(AnsiColor::Blue.on_default().bold())
    .usage(AnsiColor::Green.on_default().bold())
    .literal(AnsiColor::Cyan.on_default().bold())
    .placeholder(AnsiColor::Cyan.on_default())
    .error(AnsiColor::Red.on_default().bold())
    .valid(AnsiColor::Cyan.on_default().bold())
    .invalid(AnsiColor::Yellow.on_default().bold());
use std::{
    error::Error,
    fmt, fs,
    path::{Path, PathBuf},
    process,
};

fn main() {
    let options = CliOptions::parse();

    if let Err(err) = run(&options) {
        if err.downcast_ref::<DiagnosticAlreadyPrinted>().is_none() {
            eprintln!("error: {err}");
        }
        process::exit(1);
    }
}

fn run(options: &CliOptions) -> Result<(), Box<dyn Error>> {
    let is_dir = options.input.is_dir();

    if is_dir && (options.emit_rust || options.emit_c || options.emit_cpp || options.emit_python) {
        return Err("folder input is currently only supported with --emit-idl".into());
    }

    let definitions = if is_dir {
        load_definitions_from_dir(&options.input)?
    } else {
        parse_file(&options.input)?
    };

    eprintln!(
        "Parsed {} top-level definitions from {}",
        definitions.len(),
        options.input.display()
    );

    if options.emit_idl {
        let generated = generate_idl(&definitions);
        print!("{generated}");
    }

    let output_root = options
        .output_dir
        .clone()
        .unwrap_or_else(|| PathBuf::from("."));

    if options.emit_rust {
        let files = generate_rust(&definitions).map_err(|err| fmt_codegen_error("Rust", err))?;
        write_generated_files("Rust", &output_root, &files)?;
        run_rustfmt(&output_root, &files);
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
    about = "Parse Blueberry IDL files and optionally emit normalized IDL or Rust source.",
    styles = STYLES,
)]
struct CliOptions {
    /// Path to an IDL source file or a directory of IDL files.
    /// When a directory is given, the folder structure and file names are used
    /// as module scoping in the combined output (only --emit-idl supported).
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

fn parse_file(path: &Path) -> Result<Vec<Definition>, Box<dyn Error>> {
    let contents = fs::read_to_string(path)?;
    parse_idl(&contents).map_err(|err| {
        report_parse_error(path, &contents, &err);
        Box::new(DiagnosticAlreadyPrinted) as Box<dyn Error>
    })
}

fn report_parse_error(path: &Path, source: &str, error: &ParseError) {
    let filename = path.display().to_string();
    let f = filename.as_str();

    match error {
        ParseError::UnrecognizedToken {
            span,
            token,
            expected: _,
        } => {
            let label_msg = error
                .format_expected()
                .unwrap_or_else(|| "unexpected token".into());

            Report::build(ReportKind::Error, f, span.0)
                .with_message(format!("unexpected token `{token}`"))
                .with_label(
                    Label::new((f, span.0..span.1))
                        .with_message(label_msg)
                        .with_color(Color::Red),
                )
                .finish()
                .eprint((f, Source::from(source)))
                .unwrap();
        }
        ParseError::UnrecognizedEof {
            location,
            expected: _,
        } => {
            let loc = (*location).min(source.len());
            let label_msg = error
                .format_expected()
                .unwrap_or_else(|| "unexpected end of input".into());

            Report::build(ReportKind::Error, f, loc)
                .with_message("unexpected end of input")
                .with_label(
                    Label::new((f, loc..loc))
                        .with_message(label_msg)
                        .with_color(Color::Red),
                )
                .finish()
                .eprint((f, Source::from(source)))
                .unwrap();
        }
        ParseError::InvalidToken { location } => {
            let end = (*location + 1).min(source.len());

            Report::build(ReportKind::Error, f, *location)
                .with_message("invalid token")
                .with_label(
                    Label::new((f, *location..end))
                        .with_message("unexpected character")
                        .with_color(Color::Red),
                )
                .finish()
                .eprint((f, Source::from(source)))
                .unwrap();
        }
        ParseError::ExtraToken { span, token } => {
            Report::build(ReportKind::Error, f, span.0)
                .with_message(format!("unexpected extra token `{token}`"))
                .with_label(
                    Label::new((f, span.0..span.1))
                        .with_message("not expected here")
                        .with_color(Color::Red),
                )
                .finish()
                .eprint((f, Source::from(source)))
                .unwrap();
        }
        ParseError::Validation { message } => {
            Report::<(&str, std::ops::Range<usize>)>::build(ReportKind::Error, f, 0)
                .with_message(message)
                .finish()
                .eprint((f, Source::from(source)))
                .unwrap();
        }
    }
}

/// Sentinel error: the diagnostic was already printed via ariadne, so
/// `main()` should exit without printing anything else.
#[derive(Debug)]
struct DiagnosticAlreadyPrinted;

impl fmt::Display for DiagnosticAlreadyPrinted {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

impl Error for DiagnosticAlreadyPrinted {}

fn load_definitions_from_dir(root: &Path) -> Result<Vec<Definition>, Box<dyn Error>> {
    let idl_files = collect_idl_files(root)?;
    if idl_files.is_empty() {
        return Err(format!("no .idl files found in {}", root.display()).into());
    }

    let mut all_defs: Vec<Definition> = Vec::new();
    for path in &idl_files {
        let defs = parse_file(path)?;

        let rel = path
            .strip_prefix(root)
            .expect("collected path should be under root");
        let segments: Vec<String> = rel
            .parent()
            .into_iter()
            .flat_map(|p| p.components())
            .map(|c| c.as_os_str().to_string_lossy().into_owned())
            .chain(std::iter::once(
                rel.file_stem()
                    .expect("idl file should have a stem")
                    .to_string_lossy()
                    .into_owned(),
            ))
            .collect();

        let wrapped = wrap_in_modules(&segments, defs);
        merge_definitions(&mut all_defs, wrapped);
    }

    propagate_module_key_annotations(&mut all_defs);

    Ok(all_defs)
}

fn collect_idl_files(dir: &Path) -> Result<Vec<PathBuf>, Box<dyn Error>> {
    let mut files = Vec::new();
    collect_idl_files_recursive(dir, &mut files)?;
    files.sort();
    Ok(files)
}

fn collect_idl_files_recursive(dir: &Path, out: &mut Vec<PathBuf>) -> Result<(), Box<dyn Error>> {
    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            if path.file_name().is_some_and(|n| n == "test") {
                continue;
            }
            collect_idl_files_recursive(&path, out)?;
        } else if path.extension().is_some_and(|ext| ext == "idl") {
            out.push(path);
        }
    }
    Ok(())
}

fn is_module_key_annotation(ann: &Annotation) -> bool {
    ann.name
        .last()
        .map(|s| s.eq_ignore_ascii_case("module_key"))
        .unwrap_or(false)
}

fn propagate_module_key_annotations(definitions: &mut [Definition]) {
    let mut module_keys: HashMap<String, Annotation> = HashMap::new();
    collect_module_key_annotations(definitions, &mut module_keys);
    if !module_keys.is_empty() {
        apply_module_key_annotations(definitions, &module_keys);
    }
}

fn collect_module_key_annotations(
    definitions: &[Definition],
    keys: &mut HashMap<String, Annotation>,
) {
    for def in definitions {
        if let Definition::ModuleDef(module) = def {
            if let Some(ann) = module
                .annotations
                .iter()
                .find(|a| is_module_key_annotation(a))
            {
                keys.entry(module.node.name.clone())
                    .or_insert_with(|| ann.clone());
            }
            collect_module_key_annotations(&module.node.definitions, keys);
        }
    }
}

fn apply_module_key_annotations(
    definitions: &mut [Definition],
    keys: &HashMap<String, Annotation>,
) {
    for def in definitions.iter_mut() {
        if let Definition::ModuleDef(module) = def {
            let has_mk = module.annotations.iter().any(is_module_key_annotation);
            if !has_mk && let Some(ann) = keys.get(&module.node.name) {
                module.annotations.push(ann.clone());
            }
            apply_module_key_annotations(&mut module.node.definitions, keys);
        }
    }
}

/// Wraps definitions in nested modules matching the given path segments.
/// For segments `["a", "b"]` and defs `[D1, D2]`, produces
/// `[ModuleDef("a", [ModuleDef("b", [D1, D2])])]`.
fn wrap_in_modules(segments: &[String], defs: Vec<Definition>) -> Vec<Definition> {
    if segments.is_empty() {
        return defs;
    }

    let innermost = Definition::ModuleDef(Commented::new(
        ModuleDef {
            name: segments.last().unwrap().clone(),
            definitions: defs,
        },
        Vec::new(),
    ));

    let mut current = vec![innermost];
    for name in segments[..segments.len() - 1].iter().rev() {
        current = vec![Definition::ModuleDef(Commented::new(
            ModuleDef {
                name: name.clone(),
                definitions: current,
            },
            Vec::new(),
        ))];
    }

    current
}

/// Merges `incoming` definitions into `target`, combining `ModuleDef` nodes
/// that share the same name at the same level instead of duplicating them.
fn merge_definitions(target: &mut Vec<Definition>, incoming: Vec<Definition>) {
    for def in incoming {
        if let Definition::ModuleDef(ref incoming_mod) = def {
            let existing = target.iter_mut().find(
                |d| matches!(d, Definition::ModuleDef(m) if m.node.name == incoming_mod.node.name),
            );
            if let Some(Definition::ModuleDef(existing_mod)) = existing {
                let inner = incoming_mod.node.definitions.clone();
                merge_definitions(&mut existing_mod.node.definitions, inner);
                continue;
            }
        }
        target.push(def);
    }
}

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

fn run_rustfmt(output_root: &Path, files: &[GeneratedFile]) {
    use std::io::Write;

    let rs_paths: Vec<_> = files
        .iter()
        .filter(|f| f.path.ends_with(".rs"))
        .map(|f| output_root.join(&f.path))
        .collect();

    if rs_paths.is_empty() {
        return;
    }

    let mut formatted = 0usize;
    for path in &rs_paths {
        let Ok(source) = fs::read_to_string(path) else {
            continue;
        };
        let result = std::process::Command::new("rustfmt")
            .arg("--edition")
            .arg("2021")
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::null())
            .spawn()
            .and_then(|mut child| {
                child.stdin.take().unwrap().write_all(source.as_bytes())?;
                child.wait_with_output()
            });
        match result {
            Ok(out) if out.status.success() => {
                let _ = fs::write(path, &out.stdout);
                formatted += 1;
            }
            _ => {}
        }
    }
    println!(
        "Formatted {formatted}/{} Rust files with rustfmt",
        rs_paths.len()
    );
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
