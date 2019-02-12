# `walrus`

**Walrus is a WebAssembly transformation library.**

[![](https://docs.rs/walrus/badge.svg)](https://docs.rs/walrus/)
[![](https://img.shields.io/crates/v/walrus.svg)](https://crates.io/crates/walrus)
[![](https://img.shields.io/crates/d/walrus.svg)](https://crates.io/crates/walrus)
[![Travis CI Build Status](https://travis-ci.org/rustwasm/walrus.svg?branch=master)](https://travis-ci.org/rustwasm/walrus)

The `walrus` crate is a Rust library for performing WebAssembly transformations
in a robust an ergonomic fashion. The crate is still in its early days but is
currently used to power the [`wasm-bindgen`] CLI tool and its own internal
transformations.

[`wasm-bindgen`]: https://github.com/rustwasm/wasm-bindgen

Using `walrus` will, in the long term, also allow transforming WebAssembly while
preserving DWARF debug information to ensure that debugging the final module is
just as nice as debugging the intermediate module.

Stay tuned for more information in the future!

## License

This project is licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or
   http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or
   http://opensource.org/licenses/MIT)

at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this project by you, as defined in the Apache-2.0 license,
shall be dual licensed as above, without any additional terms or conditions.
