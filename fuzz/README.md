# Fuzzing Walrus with `cargo fuzz`!

## Prerequisites

```
cargo install cargo-fuzz
```

## Fuzzing

```
cargo fuzz run watgen
cargo fuzz run wasm-opt-ttf
cargo fuzz run raw
```

## Learn More

[Learn more about `cargo fuzz` from the `rust-fuzz`
book.](https://rust-fuzz.github.io/book/cargo-fuzz.html)
