// This example constructs a WASM module from scratch.
// It imports a "negate" function that you provide and shows how to call it with a constant value.

// You can load this module using Node.js like this:
// const fs = require("fs");
// async function main() {
//  const bytes = fs.readFileSync("target/out.wasm");
//  const env = { negate: val => -val };
//  const module = await WebAssembly.instantiate(bytes, { env }).then(res => res.instance.exports);
//  const result = module.main();
//  console.log("main:", result);
// }
// main();

use walrus::{FunctionBuilder, Module, ModuleConfig, ValType};

fn main() {
    // Construct a Walrus module.
    let config = ModuleConfig::new();
    let mut module = Module::with_config(config);

    // Import the "negate" function.
    let negate_func_type = module.types.add(&[ValType::F32], &[ValType::F32]);
    let (negate_func, _) = module.add_import_func("env", "negate", negate_func_type);

    // Create the main function type.
    let main_func_type = module.types.add(&[], &[ValType::F32]);

    // Build the function.
    let mut builder = FunctionBuilder::new();
    let const_expr = builder.f32_const(42.0);
    let expr = builder.call(negate_func, Box::new([const_expr]));
    let main_func = builder.finish(main_func_type, vec![], vec![expr], &mut module);

    // Add the function to the exports.
    module.exports.add("main", main_func);

    // Emit the WASM file.
    module.emit_wasm_file("target/out.wasm").unwrap();
}
