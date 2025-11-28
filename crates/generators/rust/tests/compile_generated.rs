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
    let files = generate_rust(&definitions).expect("generation should succeed");

    let temp_dir = tempfile::tempdir().expect("failed to create temp dir");
    for file in &files {
        let path = temp_dir.path().join(&file.path);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).expect("failed to create directories for generated output");
        }
        fs::write(&path, &file.contents).expect("failed to write generated rust file");
    }
    let source_path = temp_dir.path().join("rust").join("blueberry_generated.rs");
    assert!(
        source_path.exists(),
        "root generated file should exist at {}",
        source_path.display()
    );
    if let Ok(dir) = std::env::var("BLUEBERRY_DUMP_GENERATED") {
        for file in &files {
            let dump_path = PathBuf::from(&dir).join(file.path.clone());
            if let Some(parent) = dump_path.parent() {
                fs::create_dir_all(parent).expect("failed to create dump directory");
            }
            fs::write(&dump_path, &file.contents).expect("failed to dump generated rust");
            eprintln!("Wrote generated output to {}", dump_path.display());
        }
    }
    let manifest_path = temp_dir.path().join("Cargo.toml");
    fs::write(
        &manifest_path,
        r#"[package]
name = "blueberry-generated"
version = "0.0.0"
edition = "2024"

[dependencies]
serde = { version = "1", features = ["derive"] }
cdr = "0.2"
zenoh = "1.6.2"
"#,
    )
    .expect("failed to write Cargo.toml");

    // Place generated files where Cargo expects them.
    let src_dir = temp_dir.path().join("src");
    fs::create_dir_all(&src_dir).expect("failed to create src directory");
    fs::write(
        src_dir.join("lib.rs"),
        fs::read_to_string(&source_path).unwrap(),
    )
    .expect("failed to copy root file");
    let runtime_src = temp_dir.path().join("rust/blueberry_generated/runtime.rs");
    let runtime_dest = src_dir.join("blueberry_generated");
    fs::create_dir_all(&runtime_dest).expect("failed to create runtime directory");
    fs::write(
        runtime_dest.join("runtime.rs"),
        fs::read_to_string(runtime_src).unwrap(),
    )
    .expect("failed to copy runtime file");

    let output = Command::new("cargo")
        .arg("check")
        .arg("--manifest-path")
        .arg(&manifest_path)
        .output()
        .expect("failed to invoke cargo");

    assert!(
        output.status.success(),
        "cargo check failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn generated_rust_from_blueberry_full_compiles() {
    compile_fixture("crates/parser/tests/fixtures/blueberry_full.idl");
}
