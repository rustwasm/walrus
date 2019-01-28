// A small example which is primarily used to help benchmark walrus right now.

fn main() {
    env_logger::init();
    let a = std::env::args().nth(1).unwrap();
    let m = walrus::module::Module::from_file(&a).unwrap();
    m.emit_wasm().unwrap();
}
