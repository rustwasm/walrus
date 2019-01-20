//! Handling of the wasm `producers` section
//!
//! Specified upstream at
//! https://github.com/WebAssembly/tool-conventions/blob/master/ProducersSection.md

use crate::error::Result;
use failure::bail;
use parity_wasm::elements::*;

/// Representation of the wasm custom section `producers`
#[derive(Debug, Default)]
pub struct ModuleProducers {
    fields: Vec<Field>,
}

#[derive(Debug)]
struct Field {
    name: String,
    values: Vec<Value>,
}

#[derive(Debug)]
struct Value {
    name: String,
    version: String,
}

impl ModuleProducers {
    /// Parse a producers section from the custom section payload specified.
    pub fn parse(mut data: &[u8]) -> Result<ModuleProducers> {
        let mut ret = ModuleProducers::default();
        let amt: u32 = VarUint32::deserialize(&mut data)?.into();

        for _ in 0..amt {
            let name = String::deserialize(&mut data)?;
            let cnt: u32 = VarUint32::deserialize(&mut data)?.into();

            let mut values = Vec::with_capacity(cnt as usize);
            for _ in 0..cnt {
                let name = String::deserialize(&mut data)?;
                let version = String::deserialize(&mut data)?;
                values.push(Value { name, version });
            }
            ret.fields.push(Field { name, values });
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

        for field in self.fields.iter() {
            field.name.clone().serialize(&mut dst).unwrap();
            VarUint32::from(field.values.len() as u32)
                .serialize(&mut dst)
                .unwrap();

            for value in field.values.iter() {
                value.name.clone().serialize(&mut dst).unwrap();
                value.version.clone().serialize(&mut dst).unwrap();
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

    fn field(&mut self, field_name: &str, name: &str, version: &str) {
        let new_value = Value {
            name: name.to_string(),
            version: version.to_string(),
        };
        for field in self.fields.iter_mut() {
            if field.name != field_name {
                continue;
            }

            for value in field.values.iter_mut() {
                if value.name == name {
                    *value = new_value;
                    return;
                }
            }
            field.values.push(new_value);
            return;
        }
        self.fields.push(Field {
            name: field_name.to_string(),
            values: vec![new_value],
        })
    }
}
