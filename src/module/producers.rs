//! Handling of the wasm `producers` section
//!
//! Specified upstream at
//! https://github.com/WebAssembly/tool-conventions/blob/master/ProducersSection.md

use crate::error::Result;
use failure::bail;
use parity_wasm::elements::*;
use std::collections::HashMap;

/// Representation of the wasm custom section `producers`
#[derive(Debug, Default)]
pub struct ModuleProducers {
    fields: HashMap<String, HashMap<String, String>>,
}

impl ModuleProducers {
    /// Parse a producers section from the custom section payload specified.
    pub fn parse(mut data: &[u8]) -> Result<ModuleProducers> {
        let mut ret = ModuleProducers::default();
        let amt: u32 = VarUint32::deserialize(&mut data)?.into();

        for _ in 0..amt {
            let name = String::deserialize(&mut data)?;
            let cnt: u32 = VarUint32::deserialize(&mut data)?.into();

            let map = ret.fields.entry(name).or_insert(HashMap::new());
            for _ in 0..cnt {
                let name = String::deserialize(&mut data)?;
                let version = String::deserialize(&mut data)?;
                map.insert(name, version);
            }
        }
        if data.len() != 0 {
            bail!("failed to decode all data in producers section");
        }

        Ok(ret)
    }

    /// Serialize this producers section into its binary format
    pub fn emit(&self) -> Vec<u8> {
        // re-serialize these fields back into the custom section
        let mut dst = Vec::new();
        VarUint32::from(self.fields.len() as u32)
            .serialize(&mut dst)
            .unwrap();

        // emit in a deterministic order, so be sure to sort
        let mut fields = self.fields.iter().collect::<Vec<_>>();
        fields.sort_by_key(|f| f.0);

        for (field, values) in fields {
            field.clone().serialize(&mut dst).unwrap();
            VarUint32::from(values.len() as u32)
                .serialize(&mut dst)
                .unwrap();

            // also be sure to emit the values in a deterministic order
            let mut values = values.iter().collect::<Vec<_>>();
            values.sort();

            for (name, version) in values {
                name.clone().serialize(&mut dst).unwrap();
                version.clone().serialize(&mut dst).unwrap();
            }
        }

        dst
    }

    /// Adds a new `language` (versioned) to the producers section
    pub fn add_language(&mut self, language: &str, version: &str) {
        self.field("language", language, version);
    }

    /// Adds a new `processed-by` (versioned) to the producers section
    pub fn add_processed_by(&mut self, tool: &str, version: &str) {
        self.field("processed-by", tool, version);
    }

    /// Adds a new `sdk` (versioned) to the producers section
    pub fn add_sdk(&mut self, sdk: &str, version: &str) {
        self.field("sdk", sdk, version);
    }

    fn field(&mut self, field: &str, name: &str, version: &str) {
        self.fields
            .entry(field.to_string())
            .or_insert(HashMap::new())
            .insert(name.to_string(), version.to_string());
    }
}
