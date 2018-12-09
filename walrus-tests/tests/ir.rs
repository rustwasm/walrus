extern crate failure;
extern crate parity_wasm;
extern crate walrus;
extern crate walrus_tests;

use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use walrus::dot::Dot;

fn do_assert_ir(wasm_path: &Path, wat_path: &Path) {
    let module = match walrus::module::Module::from_file(wasm_path) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("got an error:");
            for c in e.iter_chain() {
                eprintln!("  {}", c);
            }
            eprintln!("{}", e.backtrace());
            panic!("constructing a new `walrus::Function` failed");
        }
    };
    assert_eq!(module.functions().len(), 1);

    let func = &module.functions()[0];
    let checker = walrus_tests::FileCheck::from_file(Path::new(wat_path));
    let mut output = String::new();

    if env::var("WALRUS_TESTS_DOT").is_ok() {
        let mut dot_path = PathBuf::from(wasm_path);
        dot_path.set_extension("dot");
        let mut dot_file = fs::File::create(dot_path).expect("should create dot file OK");
        func.dot(&mut dot_file)
            .expect("should generate dot file OK");
    }

    output.push_str(&func.to_string());

    let mut out_file = PathBuf::from(wasm_path);
    out_file.set_extension("out");
    fs::write(out_file, &output).expect("should write out file OK");

    checker.check(&output);
}

macro_rules! assert_ir {
    ($name:ident, $wasm_path:expr, $wat_path:expr) => {
        #[test]
        fn $name() {
            do_assert_ir(Path::new($wasm_path), Path::new($wat_path));
        }
    };
}

include!(concat!(env!("OUT_DIR"), "/ir.rs"));
