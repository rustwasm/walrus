extern crate failure;
extern crate parity_wasm;
extern crate walrus;

use parity_wasm::elements;
use std::env;
use std::fs;
use std::path::PathBuf;
use walrus::dot::Dot;

macro_rules! assert_valid {
    ($name:ident, $path:expr) => {
        #[test]
        fn $name() {
            let module = elements::deserialize_file($path).unwrap();
            let type_section = module.type_section().unwrap();
            let func_section = module.function_section().unwrap();
            let code_section = module.code_section().unwrap();

            let validation = walrus::validation_context::ValidationContext::for_module(&module)
                .expect("could not create validation context");

            for (func, body) in func_section
                .entries()
                .iter()
                .zip(code_section.bodies().iter())
            {
                let result =
                    walrus::function::Function::new(&validation, &type_section, func, body);
                match result {
                    Err(e) => {
                        eprintln!("got an error:");
                        for c in e.iter_chain() {
                            eprintln!("  {}", c);
                        }
                        eprintln!("{}", e.backtrace());
                        panic!("constructing a new `walrus::Function` failed");
                    }
                    Ok(f) => {
                        if env::var("WALRUS_TESTS_DOT").is_err() {
                            return;
                        }
                        let mut dot_path = PathBuf::from($path);
                        dot_path.set_extension("dot");
                        let mut dot_file =
                            fs::File::create(dot_path).expect("should create dot file OK");
                        f.dot(&mut dot_file).expect("should generate dot file OK");
                    }
                }
            }
        }
    };
}

include!(concat!(env!("OUT_DIR"), "/valid.rs"));
