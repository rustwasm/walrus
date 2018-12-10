use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use walrus::dot::Dot;
use walrus_tests_utils::wasm2wat;

fn do_assert_round_trip(wasm_path: &Path, wat_path: &Path) {
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

    if env::var("WALRUS_TESTS_DOT").is_ok() {
        for (i, func) in module.functions().enumerate() {
            let mut dot_path = PathBuf::from(wasm_path);
            dot_path.set_extension(format!("{}.dot", i));
            let mut dot_file = fs::File::create(dot_path).expect("should create dot file OK");
            func.dot(&mut dot_file)
                .expect("should generate dot file OK");
        }
    }

    let mut out_wasm_file = PathBuf::from(wasm_path);
    out_wasm_file.set_extension("out.wasm");
    if let Err(e) = module.emit_wasm_file(&out_wasm_file) {
        eprintln!("got an error:");
        for c in e.iter_chain() {
            eprintln!("  {}", c);
        }
        eprintln!("{}", e.backtrace());
        panic!("writing a `walrus::Module` as wasm failed");
    }

    let out_wat_file = wasm2wat(&out_wasm_file);
    let checker = walrus_tests::FileCheck::from_file(Path::new(wat_path));
    let output = fs::read_to_string(out_wat_file).expect("should read wat file OK");
    checker.check(&output);
}

macro_rules! assert_round_trip {
    ($name:ident, $wasm_path:expr, $wat_path:expr) => {
        #[test]
        fn $name() {
            do_assert_round_trip(Path::new($wasm_path), Path::new($wat_path));
        }
    };
}

include!(concat!(env!("OUT_DIR"), "/round_trip.rs"));
