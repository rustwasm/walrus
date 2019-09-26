use std::path::Path;
use std::sync::Once;

fn run(wat: &Path) -> Result<(), failure::Error> {
    static INIT_LOGS: Once = Once::new();
    INIT_LOGS.call_once(|| {
        env_logger::init();
    });

    let wasm = walrus_tests_utils::wat2wasm(wat, &["--no-check"])?;

    // NB: reading the module will do the validation.
    match walrus::Module::from_buffer(&wasm) {
        Err(e) => {
            eprintln!("Got error, as expected:");
            for c in e.iter_chain() {
                eprintln!("  - {}", c);
            }
        }
        Ok(_) => failure::bail!("expected {} to be invalid, but it was valid", wat.display()),
    }

    Ok(())
}

include!(concat!(env!("OUT_DIR"), "/invalid.rs"));
