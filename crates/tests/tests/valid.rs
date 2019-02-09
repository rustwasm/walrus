use std::env;
use std::fs;
use std::path::Path;
use walrus::dot::Dot;

fn run(wat: &Path) -> Result<(), failure::Error> {
    let wasm = walrus_tests_utils::wat2wasm(wat);
    let module = walrus::Module::from_buffer(&wasm)?;

    let local_funcs: Vec<_> = module
        .functions()
        .filter(|f| match f.kind {
            walrus::FunctionKind::Local(_) => true,
            _ => false,
        })
        .collect();
    assert_eq!(local_funcs.len(), 1);

    let f = &local_funcs.first().unwrap();
    if env::var("WALRUS_TESTS_DOT").is_ok() {
        let mut file = String::new();
        f.dot(&mut file);
        fs::write(wat.with_extension("dot"), file)?;
    }

    Ok(())
}

include!(concat!(env!("OUT_DIR"), "/valid.rs"));
