use std::env;
use std::fs;
use std::path::Path;
use walrus::dot::Dot;
use walrus_tests_utils::{wasm2wat, wat2wasm};

fn run(wat_path: &Path) -> Result<(), failure::Error> {
    let wasm = wat2wasm(wat_path);
    let module = walrus::module::Module::from_buffer(&wasm)?;

    if env::var("WALRUS_TESTS_DOT").is_ok() {
        for (i, func) in module.functions().enumerate() {
            let mut file = String::new();
            func.dot(&mut file);
            fs::write(wat_path.with_extension(&format!("{}.dot", i)), file)?;
        }
    }

    let out_wasm_file = wat_path.with_extension("out.wasm");
    module.emit_wasm_file(&out_wasm_file)?;

    let out_wat = wasm2wat(&out_wasm_file);
    let checker = walrus_tests::FileCheck::from_file(wat_path);
    checker.check(&out_wat);
    Ok(())
}

include!(concat!(env!("OUT_DIR"), "/round_trip.rs"));
