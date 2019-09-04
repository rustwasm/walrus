use criterion::{black_box, criterion_group, criterion_main, Benchmark, Criterion};
use walrus::Module;

fn criterion_benchmark(c: &mut Criterion) {
    c.bench(
        "round-trip-with-gc",
        Benchmark::new("dodrio-todomvc.wasm", |b| {
            let input_wasm = include_bytes!("./fixtures/dodrio-todomvc.wasm");
            b.iter(|| {
                let input_wasm = black_box(input_wasm);
                let mut module = Module::from_buffer(input_wasm).unwrap();
                walrus::passes::gc::run(&mut module);
                let output_wasm = module.emit_wasm();
                black_box(output_wasm);
            });
        }),
    );
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
