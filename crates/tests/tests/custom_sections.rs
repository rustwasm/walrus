//! Tests for working with custom sections that `walrus` doesn't know about.

use std::borrow::Cow;
use walrus::{CustomSection, Module};

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct HelloCustomSection(String);

impl CustomSection for HelloCustomSection {
    fn name(&self) -> &str {
        "hello"
    }

    fn data(&self) -> Cow<[u8]> {
        let data = format!("Hello, {}!", self.0);
        data.into_bytes().into()
    }
}

#[test]
fn round_trip_unkown_custom_sections() {
    let mut module = Module::default();

    let world = HelloCustomSection("World".into());
    let world_id = module.customs.add(world.clone());
    assert_eq!(module.customs.get(world_id), &world);

    let wasm = module.emit_wasm().unwrap();
    let mut module = Module::from_buffer(&wasm).unwrap();

    let world_round_tripped = module.customs.take_raw("hello").unwrap();
    assert_eq!(world_round_tripped.data(), world.data());
}
