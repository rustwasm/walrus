//! Tests for working with custom sections that `walrus` doesn't know about.

use std::borrow::Cow;
use walrus::{CustomSection, IdsToIndices, Module, ModuleConfig};

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct HelloCustomSection(String);

impl HelloCustomSection {
    fn parse(data: &[u8]) -> Option<Self> {
        let data = std::str::from_utf8(data).ok()?;
        if !data.starts_with("Hello, ") || !data.ends_with("!") {
            return None;
        }
        let who = data["Hello, ".len()..data.len() - 1].to_string();
        Some(HelloCustomSection(who))
    }
}

impl CustomSection for HelloCustomSection {
    fn name(&self) -> &str {
        "hello"
    }

    fn data(&self, _: &IdsToIndices) -> Cow<[u8]> {
        let data = format!("Hello, {}!", self.0);
        data.into_bytes().into()
    }
}

#[test]
fn round_trip_unkown_custom_sections() {
    let mut config = ModuleConfig::new();
    config.generate_producers_section(false);

    let indices = IdsToIndices::default();

    let mut module = Module::with_config(config.clone());

    let world = HelloCustomSection("World".into());
    let world_id = module.customs.add(world.clone());
    assert_eq!(module.customs.get(world_id).unwrap(), &world);

    assert_eq!(
        module
            .customs
            .iter()
            .map(|(id, s)| (id, s.data(&indices)))
            .collect::<Vec<_>>(),
        [(world_id.into(), world.data(&indices))]
    );

    let wasm = module.emit_wasm().unwrap();
    let mut module = config.parse(&wasm).unwrap();

    let world_round_tripped = module.customs.remove_raw("hello").unwrap();
    assert_eq!(world_round_tripped.data(&indices), world.data(&indices));

    let new_world = HelloCustomSection::parse(&world.data(&indices)).unwrap();
    assert_eq!(new_world.data(&indices), world.data(&indices));
    module.customs.add(new_world);

    let new_wasm = module.emit_wasm().unwrap();
    assert_eq!(wasm, new_wasm);
}
