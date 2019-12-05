use std::path::Path;

fn run(wat_path: &Path) -> Result<(), anyhow::Error> {
    static INIT_LOGS: std::sync::Once = std::sync::Once::new();
    INIT_LOGS.call_once(|| {
        env_logger::init();
    });

    let wasm = wat::parse_file(wat_path)?;
    let module = walrus::Module::from_buffer(&wasm)?;

    assert!(module.imports.find("doggo", "husky").is_some());
    assert!(module.imports.find("doggo", "shepherd").is_some());
    assert!(module.imports.find("doggo", "siamese").is_none());
    assert!(module.imports.find("snek", "cobra").is_none());
    assert!(module.imports.find("cat", "siamese").is_some());

    Ok(())
}

include!(concat!(env!("OUT_DIR"), "/function_imports.rs"));
