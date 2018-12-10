use std::env;
use std::ffi::OsStr;
use std::fs;
use std::path::Path;
use walkdir::WalkDir;
use walrus_tests_utils::wat2wasm;

fn for_each_wat_file<P, F>(dir: P, mut f: F)
where
    P: AsRef<Path>,
    F: FnMut(&Path),
{
    println!("cargo:rerun-if-changed={}", dir.as_ref().display());
    for entry in WalkDir::new(dir) {
        let entry = entry.unwrap();
        if entry.path().extension() == Some(OsStr::new("wat")) {
            println!("cargo:rerun-if-changed={}", entry.path().display());
            f(entry.path());
        }
    }
}

fn path_to_ident(p: &Path) -> String {
    p.display()
        .to_string()
        .chars()
        .map(|c| match c {
            'a'...'z' | 'A'...'Z' | '0'...'9' | '_' => c,
            _ => '_',
        })
        .collect()
}

fn valid() {
    let mut valid_tests = String::new();

    for_each_wat_file("tests/valid", |path| {
        let wasm = wat2wasm(path);
        let test_name = path_to_ident(path);
        valid_tests.push_str(&format!(
            "assert_valid!({}, \"{}\");\n",
            test_name,
            wasm.display()
        ));
    });

    let out_dir = env::var("OUT_DIR").unwrap();
    fs::write(Path::new(&out_dir).join("valid.rs"), &valid_tests)
        .expect("should write generated valid.rs file OK");
}

fn ir() {
    let mut ir_tests = String::new();

    for_each_wat_file("tests/ir", |path| {
        let wasm = wat2wasm(path);
        let test_name = path_to_ident(path);
        ir_tests.push_str(&format!(
            "assert_ir!({}, \"{}\", \"{}\");\n",
            test_name,
            wasm.display(),
            path.display()
        ));
    });

    let out_dir = env::var("OUT_DIR").unwrap();
    fs::write(Path::new(&out_dir).join("ir.rs"), &ir_tests)
        .expect("should write generated ir.rs file OK");
}

fn round_trip() {
    let mut round_trip_tests = String::new();

    for_each_wat_file("tests/round_trip", |path| {
        let wasm = wat2wasm(path);
        let test_name = path_to_ident(path);
        round_trip_tests.push_str(&format!(
            "assert_round_trip!({}, \"{}\", \"{}\");\n",
            test_name,
            wasm.display(),
            path.display()
        ));
    });

    let out_dir = env::var("OUT_DIR").unwrap();
    fs::write(Path::new(&out_dir).join("round_trip.rs"), &round_trip_tests)
        .expect("should write generated round_trip.rs file OK");
}

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-env-changed=WALRUS_TESTS_DOT");

    valid();
    ir();
    round_trip();
}
