extern crate failure;
extern crate parity_wasm;
extern crate walrus;
extern crate walrus_tests;

use parity_wasm::elements;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use walrus::dot::Dot;

macro_rules! assert_ir {
    ($name:ident, $wasm_path:expr, $wat_path:expr) => {
        #[test]
        fn $name() {
            let module = elements::deserialize_file($wasm_path).unwrap();
            let type_section = module.type_section().unwrap();
            let func_section = module.function_section().unwrap();
            let code_section = module.code_section().unwrap();

            let validation = walrus::validation_context::ValidationContext::for_module(&module)
                .expect("could not create validation context");

            let checker = walrus_tests::FileCheck::from_file(Path::new($wat_path));
            let mut output = String::new();

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
                    Ok(func) => {
                        if env::var("WALRUS_TESTS_DOT").is_ok() {
                            let mut dot_path = PathBuf::from($wasm_path);
                            dot_path.set_extension("dot");
                            let mut dot_file =
                                fs::File::create(dot_path).expect("should create dot file OK");
                            func.dot(&mut dot_file).expect("should generate dot file OK");
                        }

                        output.push_str(&func.to_string());
                    }
                }
            }

            let mut out_file = PathBuf::from($wasm_path);
            out_file.set_extension("out");
            fs::write(out_file, &output).expect("should write out file OK");

            checker.check(&output);
        }
    };
}

include!(concat!(env!("OUT_DIR"), "/ir.rs"));
