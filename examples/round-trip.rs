//! A small example which is primarily used to help benchmark walrus right now.

use std::process;

fn main() {
    if let Err(e) = try_main() {
        eprintln!("Error!");
        for c in e.iter_chain() {
            eprintln!("{}", c);
        }
        process::exit(1);
    }
}

fn try_main() -> Result<(), failure::Error> {
    env_logger::init();
    let a = std::env::args().nth(1).ok_or_else(|| {
        failure::format_err!("must provide the input wasm file as the first argument")
    })?;
    let m = walrus::Module::from_file(&a)?;
    let wasm = m.emit_wasm();
    if let Some(destination) = std::env::args().nth(2) {
        std::fs::write(destination, wasm)?;
    }
    Ok(())
}
