extern crate failure;
extern crate parity_wasm;
extern crate walrus;

use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use walrus::dot::Dot;

fn do_assert_valid(path: &Path) {
    let module = match walrus::module::Module::from_file(path) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("got an error:");
            for c in e.iter_chain() {
                eprintln!("  {}", c);
            }
            eprintln!("{}", e.backtrace());
            panic!("constructing a new `walrus::Module` failed");
        }
    };
    assert_eq!(module.functions().len(), 1);

    let f = &module.functions()[0];
    if env::var("WALRUS_TESTS_DOT").is_err() {
        let mut dot_path = PathBuf::from(path);
        dot_path.set_extension("dot");
        let mut dot_file = fs::File::create(dot_path).expect("should create dot file OK");
        f.dot(&mut dot_file).expect("should generate dot file OK");
    }
}

macro_rules! assert_valid {
    ($name:ident, $path:expr) => {
        #[test]
        fn $name() {
            do_assert_valid(Path::new($path));
        }
    };
}

include!(concat!(env!("OUT_DIR"), "/valid.rs"));
