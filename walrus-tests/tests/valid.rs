extern crate failure;
extern crate parity_wasm;
extern crate walrus;

use parity_wasm::elements;

macro_rules! assert_valid {
    ($name:ident, $path:expr) => {
        #[test]
        fn $name() {
            let module = elements::deserialize_file($path).unwrap();
            let type_section = module.type_section().unwrap();
            let func_section = module.function_section().unwrap();
            let code_section = module.code_section().unwrap();

            for (func, body) in func_section.entries().iter().zip(code_section.bodies().iter()) {
                let ty = func.type_ref();
                let ty = &type_section.types()[ty as usize];
                let ty = match ty {
                    elements::Type::Function(f) => f,
                };

                if let Err(e) = walrus::Function::new(ty, body) {
                    eprintln!("got an error:");
                    for c in e.iter_chain() {
                        eprintln!("  {}", c);
                    }
                    eprintln!("{}", e.backtrace());
                    panic!("constructing a new `walrus::Function` failed");
                }
            }
        }
    }
}

include!(concat!(env!("OUT_DIR"), "/valid.rs"));
