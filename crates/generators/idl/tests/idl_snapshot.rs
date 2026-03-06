//! Snapshot tests for IDL round-trip: parse -> generate -> compare.
//!
//! These tests ensure that parsing an IDL file and generating it back
//! produces a consistent, normalized output. All `.idl` files in the
//! fixtures folder are automatically discovered and tested.

use blueberry_generator_idl::generate_idl;
use blueberry_parser::parse_idl;
use std::{fs, path::PathBuf};

fn fixture_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../parser/tests/fixtures")
}

/// Snapshot test for all IDL fixtures.
///
/// Parses each `.idl` file in the fixtures folder, generates it back to IDL,
/// and compares against stored snapshots.
#[test]
fn snapshot_all_fixtures() {
    let fixture_dir = fixture_dir();
    let mut entries: Vec<_> = fs::read_dir(&fixture_dir)
        .unwrap_or_else(|err| panic!("failed to read fixture dir: {}", err))
        .filter_map(|entry| entry.ok())
        .filter(|entry| {
            entry
                .path()
                .extension()
                .map(|ext| ext == "idl")
                .unwrap_or(false)
        })
        .collect();

    // Sort for deterministic ordering
    entries.sort_by_key(|e| e.path());

    for entry in entries {
        let path = entry.path();
        let name = path.file_stem().unwrap().to_string_lossy();

        let input = fs::read_to_string(&path)
            .unwrap_or_else(|err| panic!("failed to read {}: {}", path.display(), err));

        let defs =
            parse_idl(&input).unwrap_or_else(|err| panic!("failed to parse {}: {}", name, err));

        let generated = generate_idl(&defs);

        insta::assert_snapshot!(name.to_string(), generated);
    }
}

/// Test that all fixtures can be round-tripped: parse -> generate -> reparse.
///
/// Note: This test verifies that the generated IDL is valid and can be reparsed,
/// but does not require byte-for-byte AST equality. Comment formatting may differ
/// (e.g., multiline block comments become multiple single-line comments).
#[test]
fn round_trip_all_fixtures() {
    let fixture_dir = fixture_dir();
    let entries = fs::read_dir(&fixture_dir)
        .unwrap_or_else(|err| panic!("failed to read fixture dir: {}", err));

    for entry in entries {
        let entry = entry.expect("failed to read entry");
        let path = entry.path();
        if path.extension().map(|ext| ext == "idl").unwrap_or(false) {
            let name = path.file_name().unwrap().to_string_lossy();
            let input = fs::read_to_string(&path)
                .unwrap_or_else(|err| panic!("failed to read {}: {}", name, err));

            let original_defs =
                parse_idl(&input).unwrap_or_else(|err| panic!("failed to parse {}: {}", name, err));

            let generated = generate_idl(&original_defs);

            // Verify the generated IDL can be parsed
            let reparsed_defs = parse_idl(&generated).unwrap_or_else(|err| {
                panic!(
                    "failed to reparse generated {}: {}\n\nGenerated:\n{}",
                    name, err, generated
                )
            });

            // Verify the reparsed IDL generates identical output (stable round-trip)
            let regenerated = generate_idl(&reparsed_defs);
            assert_eq!(
                generated, regenerated,
                "round-trip not stable for {}: second generation differs from first",
                name
            );
        }
    }
}
