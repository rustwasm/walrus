use std::path::Path;
use walrus_tests_utils::wat2wasm;

fn run(wat_path: &Path) -> Result<(), failure::Error> {
    let wasm = wat2wasm(wat_path);
    let module = walrus::Module::from_buffer(&wasm)?;

    assert!(module.imports.find("doggo", "husky").is_some());
    assert!(module.imports.find("doggo", "shepherd").is_some());
    assert!(module.imports.find("doggo", "siamese").is_none());
    assert!(module.imports.find("snek", "cobra").is_none());
    assert!(module.imports.find("cat", "siamese").is_some());

    Ok(())
}

include!(concat!(env!("OUT_DIR"), "/function_imports.rs"));
