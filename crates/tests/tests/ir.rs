use std::env;
use std::fs;
use std::path::Path;
use walrus::dot::Dot;

fn run(wat_path: &Path) -> Result<(), failure::Error> {
    let wasm = walrus_tests_utils::wat2wasm(wat_path);
    let module = walrus::module::Module::from_buffer(&wasm)?;

    let local_funcs: Vec<_> = module
        .functions()
        .filter(|f| match f.kind {
            walrus::module::functions::FunctionKind::Local(_) => true,
            _ => false,
        })
        .collect();
    assert_eq!(local_funcs.len(), 1);

    let func = &local_funcs.first().unwrap();
    let checker = walrus_tests::FileCheck::from_file(Path::new(wat_path));
    let mut output = String::new();

    if env::var("WALRUS_TESTS_DOT").is_ok() {
        let mut file = String::new();
        func.dot(&mut file);
        fs::write(wat_path.with_extension("dot"), file)?;
    }

    output.push_str(&func.to_string());

    let out_file = wat_path.with_extension("out");
    fs::write(out_file, &output)?;

    checker.check(&output);

    Ok(())
}

include!(concat!(env!("OUT_DIR"), "/ir.rs"));
