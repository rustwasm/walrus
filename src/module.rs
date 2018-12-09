//! TODO

use super::error::{ErrorKind, Result};
use super::function::Function;
use super::validation_context::ValidationContext;
use failure::Fail;
use parity_wasm::elements;
use std::path::Path;

/// TODO
#[derive(Debug)]
pub struct Module {
    types: elements::TypeSection,
    funcs: Vec<Function>,
    data: elements::DataSection,
}

impl Module {
    /// Construct a new module.
    pub fn from_file<P>(path: P) -> Result<Module>
    where
        P: AsRef<Path>,
    {
        let path = path.as_ref();
        let module = elements::deserialize_file(path)?;
        let types = module.type_section().cloned().unwrap_or_default();
        let func_section = module.function_section().cloned().unwrap_or_default();
        let code_section = module.code_section().cloned().unwrap_or_default();
        let data = module.data_section().cloned().unwrap_or_default();

        if func_section.entries().len() != code_section.bodies().len() {
            return Err(ErrorKind::InvalidWasm
                .context("different number of function section entries and code section entries")
                .into());
        }

        let mut funcs = Vec::with_capacity(func_section.entries().len());

        let validation = ValidationContext::for_module(&module)?;
        for (func, body) in func_section
            .entries()
            .iter()
            .zip(code_section.bodies().iter())
        {
            funcs.push(Function::new(&validation, &types, func, body)?);
        }

        Ok(Module { types, funcs, data })
    }

    /// Get a shared reference to this module's functions.
    pub fn functions(&self) -> &[Function] {
        &self.funcs
    }
}
