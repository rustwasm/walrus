#![no_main]

#[macro_use]
extern crate libfuzzer_sys;

fuzz_target!(|data: &[u8]| {
    let mut module = match walrus::Module::from_buffer(data) {
        Ok(m) => m,
        Err(_) => return,
    };
    let serialized = module.emit_wasm();
    let mut module =
        walrus::Module::from_buffer(&serialized).expect("we should only emit valid Wasm data");
    let reserialized = module.emit_wasm();
    assert_eq!(
        serialized, reserialized,
        "emitting wasm should be deterministic"
    );
});
