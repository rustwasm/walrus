use std::path::Path;

fn run(wat: &Path) -> Result<(), failure::Error> {
    static INIT_LOGS: std::sync::Once = std::sync::Once::new();
    INIT_LOGS.call_once(|| {
        env_logger::init();
    });

    let wasm = walrus_tests_utils::wat2wasm(wat)?;
    let module = walrus::Module::from_buffer(&wasm)?;

    let local_funcs: Vec<_> = module
        .functions()
        .filter(|f| match f.kind {
            walrus::FunctionKind::Local(_) => true,
            _ => false,
        })
        .collect();
    assert_eq!(local_funcs.len(), 1);

    Ok(())
}

include!(concat!(env!("OUT_DIR"), "/valid.rs"));
