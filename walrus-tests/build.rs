extern crate walkdir;

use std::env;
use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use walkdir::WalkDir;

fn require_wat2wasm() {
    let status = Command::new("wat2wasm")
        .arg("--help")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .expect(
            "Could not spawn wat2wasm; do you have https://github.com/WebAssembly/wabt installed?",
        );
    assert!(
        status.success(),
        "wat2wasm did not run OK; do you have https://github.com/WebAssembly/wabt installed?"
    )
}

fn wat2wasm(path: &Path, out: &Path) {
    let mut cmd = Command::new("wat2wasm");
    cmd.arg(path).arg("-o").arg(out).arg("-v");
    println!("running: {:?}", cmd);
    let status = cmd.status().expect("should spawn wat2wasm OK");
    assert!(status.success(), "should run wat2wasm OK");
}

fn valid() {
    let mut valid_tests = String::new();

    for entry in WalkDir::new("tests/valid") {
        let entry = entry.unwrap();
        if entry.path().extension() == Some(OsStr::new("wat")) {
            println!("cargo:rerun-if-changed={}", entry.path().display());

            let mut wasm = PathBuf::from(entry.path());
            wasm.set_extension("wasm");

            wat2wasm(entry.path(), &wasm);

            let test_name: String = entry
                .path()
                .display()
                .to_string()
                .chars()
                .map(|c| match c {
                    'a'...'z' | 'A'...'Z' | '0'...'9' | '_' => c,
                    _ => '_',
                })
                .collect();
            valid_tests.push_str(&format!(
                "assert_valid!({}, \"{}\");\n",
                test_name,
                wasm.display()
            ));
        }
    }

    let out_dir = env::var("OUT_DIR").unwrap();
    fs::write(Path::new(&out_dir).join("valid.rs"), &valid_tests)
        .expect("should write generated valid.rs file OK");
}

fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    require_wat2wasm();
    valid();
}
