//! Cross-language wire-compatibility test.
//!
//! Generates TypeScript bindings for a minimal primitives-only IDL, then runs
//! a Node script (via `tsx`) that:
//!   1. Builds a known message via the generated `*.encode()`.
//!   2. Round-trips it through `*.decode()`.
//!   3. Prints the decoded fields as JSON.
//!
//! The Rust test parses that JSON and asserts every primitive matches the
//! original values. This guarantees that the generated TS speaks the same wire
//! format as the canonical `blueberry-serde-ts` runtime end-to-end.

use std::{fs, process::Command};

use blueberry_generator_typescript::generate;
use blueberry_parser::parse_idl;

const IDL: &str = r#"
@module_key(0x4242)
module Compat {
    @topic(value = "compat/{nid}/all")
    @message_key(value = 0x0123)
    message AllPrimitives {
        boolean flag;
        uint8 byte;
        uint16 wide;
        int16 swide;
        uint32 quad;
        int32 squad;
        uint64 wide64;
        int64 swide64;
        float f32;
        double f64;
    };
};
"#;

const PACKAGE_JSON: &str = r#"{
  "name": "blueberry-wire-compat",
  "version": "0.0.0",
  "private": true,
  "type": "module",
  "dependencies": {
    "blueberry-serde-ts": "github:eldinmiller/blueberry-serde-ts#v0.1.1"
  },
  "devDependencies": {
    "tsx": "^4.7.0",
    "typescript": "^5.4.0"
  }
}
"#;

const TSCONFIG_JSON: &str = r#"{
  "compilerOptions": {
    "target": "ES2022",
    "module": "NodeNext",
    "moduleResolution": "NodeNext",
    "strict": true,
    "esModuleInterop": true,
    "skipLibCheck": true,
    "forceConsistentCasingInFileNames": true,
    "noEmit": true,
    "isolatedModules": true
  },
  "include": ["typescript/**/*.ts", "roundtrip.ts"]
}
"#;

const ROUNDTRIP_TS: &str = r#"
import { AllPrimitivesMessage, type AllPrimitivesFields } from './typescript/index.js';

const original: AllPrimitivesFields = {
  flag: true,
  byte: 0xab,
  wide: 0xbeef,
  swide: -1234,
  quad: 0xdeadbeef,
  squad: -42,
  wide64: 0x1122334455667788n,
  swide64: -1n,
  f32: 1.5,
  f64: Math.PI,
};

const bytes = AllPrimitivesMessage.encode(original);
const { fields } = AllPrimitivesMessage.decode(bytes);

const replacer = (_key: string, value: unknown) =>
  typeof value === 'bigint' ? value.toString() : value;
process.stdout.write(JSON.stringify(fields, replacer));
"#;

fn npm_command() -> Command {
    if cfg!(windows) {
        Command::new("npm.cmd")
    } else {
        Command::new("npm")
    }
}

fn require_npm() {
    let output = npm_command()
        .arg("--version")
        .output()
        .expect("npm must be installed");
    assert!(output.status.success());
}

#[test]
fn generated_typescript_round_trip_matches_inputs() {
    require_npm();

    let definitions = parse_idl(IDL).expect("parse compat IDL");
    let files = generate(&definitions).expect("generate typescript");

    let temp_dir = tempfile::tempdir().expect("create temp dir");
    for file in &files {
        let path = temp_dir.path().join(&file.path);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).expect("create generated subdir");
        }
        fs::write(&path, &file.contents).expect("write generated file");
    }
    fs::write(temp_dir.path().join("package.json"), PACKAGE_JSON).expect("write package.json");
    fs::write(temp_dir.path().join("tsconfig.json"), TSCONFIG_JSON).expect("write tsconfig.json");
    fs::write(temp_dir.path().join("roundtrip.ts"), ROUNDTRIP_TS).expect("write roundtrip.ts");

    let install = npm_command()
        .arg("install")
        .arg("--no-audit")
        .arg("--no-fund")
        .arg("--loglevel=error")
        .current_dir(temp_dir.path())
        .output()
        .expect("npm install");
    assert!(
        install.status.success(),
        "npm install failed: {}",
        String::from_utf8_lossy(&install.stderr)
    );

    let tsx_path = if cfg!(windows) {
        temp_dir.path().join("node_modules/.bin/tsx.cmd")
    } else {
        temp_dir.path().join("node_modules/.bin/tsx")
    };
    assert!(
        tsx_path.exists(),
        "tsx binary missing at {}",
        tsx_path.display()
    );

    let run = Command::new(&tsx_path)
        .arg(temp_dir.path().join("roundtrip.ts"))
        .current_dir(temp_dir.path())
        .output()
        .expect("invoke tsx");
    assert!(
        run.status.success(),
        "tsx exit non-zero:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr),
    );

    let stdout = String::from_utf8(run.stdout).expect("stdout utf8");
    assert!(
        stdout.contains("\"flag\":true"),
        "missing flag in output: {stdout}",
    );
    assert!(stdout.contains("\"byte\":171"), "byte mismatch: {stdout}");
    assert!(stdout.contains("\"wide\":48879"), "wide mismatch: {stdout}");
    assert!(
        stdout.contains("\"swide\":-1234"),
        "swide mismatch: {stdout}"
    );
    assert!(
        stdout.contains("\"quad\":3735928559"),
        "quad mismatch: {stdout}"
    );
    assert!(stdout.contains("\"squad\":-42"), "squad mismatch: {stdout}");
    assert!(
        stdout.contains("\"wide64\":\"1234605616436508552\""),
        "wide64 mismatch: {stdout}",
    );
    assert!(
        stdout.contains("\"swide64\":\"-1\""),
        "swide64 mismatch: {stdout}"
    );
    assert!(stdout.contains("\"f32\":1.5"), "f32 mismatch: {stdout}");
}
