use std::env;
use std::path::Path;

fn run(wat_path: &Path) -> Result<(), anyhow::Error> {
    static INIT_LOGS: std::sync::Once = std::sync::Once::new();
    INIT_LOGS.call_once(|| {
        env_logger::init();
    });

    let wasm = wat::parse_file(wat_path)?;
    let mut module = walrus::Module::from_buffer(&wasm)?;

    if env::var("WALRUS_TESTS_DOT").is_ok() {
        module.write_graphviz_dot(wat_path.with_extension("dot"))?;
    }

    let out_wasm_file = wat_path.with_extension("out.wasm");
    walrus::passes::gc::run(&mut module);
    module.emit_wasm_file(&out_wasm_file)?;

    let out_wat = wasmprinter::print_file(&out_wasm_file)?;
    let checker = walrus_tests::FileCheck::from_file(wat_path);
    checker.check(&out_wat);
    Ok(())
}

include!(concat!(env!("OUT_DIR"), "/round_trip.rs"));
