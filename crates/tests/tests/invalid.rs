use std::path::Path;
use std::sync::Once;

fn run(wat: &Path) -> Result<(), anyhow::Error> {
    static INIT_LOGS: Once = Once::new();
    INIT_LOGS.call_once(|| {
        env_logger::init();
    });

    let wasm = walrus_tests_utils::wat2wasm(wat, &["--no-check"])?;

    // NB: reading the module will do the validation.
    match walrus::Module::from_buffer(&wasm) {
        Err(e) => {
            eprintln!("Got error, as expected: {:?}", e);
        }
        Ok(_) => anyhow::bail!("expected {} to be invalid, but it was valid", wat.display()),
    }

    Ok(())
}

include!(concat!(env!("OUT_DIR"), "/invalid.rs"));
