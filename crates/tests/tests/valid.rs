use std::env;
use std::path::Path;
use std::sync::Once;

fn run(wat: &Path) -> Result<(), anyhow::Error> {
    static INIT_LOGS: Once = Once::new();
    INIT_LOGS.call_once(|| {
        env_logger::init();
    });

    let wasm = wat::parse_file(wat)?;

    // NB: reading the module will do the validation.
    let module = walrus::Module::from_buffer(&wasm)?;

    if env::var("WALRUS_TESTS_DOT").is_ok() {
        module.write_graphviz_dot(wat.with_extension("dot"))?;
    }

    Ok(())
}

include!(concat!(env!("OUT_DIR"), "/valid.rs"));
