//! Handling of the wasm `producers` section
//!
//! Specified upstream at
//! https://github.com/WebAssembly/tool-conventions/blob/master/ProducersSection.md

use crate::emit::{Emit, EmitContext};
use crate::error::Result;
use crate::module::Module;
use failure::bail;

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

impl Module {
    /// Parse a producers section from the custom section payload specified.
    pub(crate) fn parse_producers_section(
        &mut self,
        mut data: wasmparser::BinaryReader,
    ) -> Result<()> {
        log::debug!("parse producers section");
        for _ in 0..data.read_var_u32()? {
            let name = data.read_string()?.to_string();
            let cnt = data.read_var_u32()?;

            let mut values = Vec::with_capacity(cnt as usize);
            for _ in 0..cnt {
                let name = data.read_string()?.to_string();
                let version = data.read_string()?.to_string();
                values.push(Value { name, version });
            }
            self.producers.fields.push(Field { name, values });
        }
        if !data.eof() {
            bail!("failed to decode all data in producers section");
        }

        Ok(())
    }
}

impl Emit for ModuleProducers {
    fn emit(&self, cx: &mut EmitContext) {
        log::debug!("emit producers section");
        cx.custom_section("producers").list(&self.fields);
    }
}

impl Emit for Field {
    fn emit(&self, cx: &mut EmitContext) {
        cx.encoder.str(&self.name);
        cx.list(&self.values);
    }
}

impl Emit for Value {
    fn emit(&self, cx: &mut EmitContext) {
        cx.encoder.str(&self.name);
        cx.encoder.str(&self.version);
    }
}
