use blueberry_generator_rust::generate_rust;
use blueberry_parser::parse_idl;
use std::{fs, path::PathBuf, process::Command};

fn compile_fixture(relative_path: &str) {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let workspace_root = manifest_dir
        .parent()
        .and_then(|path| path.parent())
        .and_then(|path| path.parent())
        .expect("workspace root directory");
    let fixture_path = workspace_root.join(relative_path);

    let contents = fs::read_to_string(&fixture_path).expect("failed to read example IDL fixture");
    let definitions = parse_idl(&contents).expect("failed to parse example IDL");
    let generated_rust = generate_rust(&definitions);

    let temp_dir = tempfile::tempdir().expect("failed to create temp dir");
    let source_path = temp_dir.path().join("generated.rs");
    fs::write(&source_path, &generated_rust).expect("failed to write generated rust file");
    if let Ok(dir) = std::env::var("BLUEBERRY_DUMP_GENERATED") {
        let mut file_name = relative_path.replace('/', "_");
        if !file_name.ends_with(".rs") {
            file_name.push_str(".rs");
        }
        let dump_path = PathBuf::from(dir).join(file_name);
        if let Some(parent) = dump_path.parent() {
            fs::create_dir_all(parent).expect("failed to create dump directory");
        }
        fs::write(&dump_path, &generated_rust).expect("failed to dump generated rust");
        eprintln!("Wrote generated output to {}", dump_path.display());
    }
    let output_path = temp_dir.path().join("generated.rlib");

    let output = Command::new("rustc")
        .arg("--edition=2024")
        .arg("--crate-type=lib")
        .arg(&source_path)
        .arg("-o")
        .arg(&output_path)
        .output()
        .expect("failed to invoke rustc");

    assert!(
        output.status.success(),
        "rustc failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn generated_rust_from_blueberry_full_compiles() {
    compile_fixture("crates/parser/tests/fixtures/blueberry_full.idl");
}
