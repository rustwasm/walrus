//! Validation of a wasm module
//!
//! Currently only does some basic sanity checks, but it's intended that
//! eventually this is a full typechecking pass!

use crate::const_value::Const;
use crate::ir::Value;
use crate::ty::ValType;
use crate::error::Result;
use crate::module::globals::{Global, GlobalKind};
use crate::module::memories::Memory;
use crate::module::tables::{Table, TableKind};
use crate::module::Module;
use failure::{ResultExt, bail};

/// Validate a wasm module, returning an error if it fails to validate.
pub fn run(module: &Module) -> Result<()> {
    for memory in module.memories.iter() {
        validate_memory(memory)?;
    }
    for table in module.tables.iter() {
        validate_table(table)?;
    }
    for global in module.globals.iter() {
        validate_global(module, global)?;
    }
    Ok(())
}

fn validate_memory(m: &Memory) -> Result<()> {
    validate_limits(m.initial, m.maximum, u16::max_value().into())
        .context("when validating a memory")?;
    Ok(())
}

fn validate_table(t: &Table) -> Result<()> {
    validate_limits(t.initial, t.maximum, u32::max_value()).context("when validating a table")?;

    // Ensure that the table element type is `anyfunc`. This does
    // nothing, but if new wasm versions and future parity-wasm releases
    // get support for new table types, this may need to actually do
    // something.
    match t.kind {
        TableKind::Function(_) => {}
    }
    Ok(())
}

fn validate_limits(initial: u32, maximum: Option<u32>, k: u32) -> Result<()> {
    match (initial, maximum) {
        (min, Some(max)) if max < min || max > k => {
            bail!("invalid limits: min = {}, max = {}; k = {}", min, max, k)
        }
        (min, _) => {
            if min <= k {
                Ok(())
            } else {
                bail!("invalid limits: min = {}, k = {}", min, k)
            }
        }
    }
}

fn validate_global(module: &Module, global: &Global) -> Result<()> {
    match global.kind {
        GlobalKind::Import(_) => return Ok(()),
        GlobalKind::Local(Const::Value(value)) => {
            validate_value(value, global.ty).context("invalid type on global")?;
        }
        GlobalKind::Local(Const::Global(other)) => {
            let other = module.globals.get(other);
            match other.kind {
                GlobalKind::Import(_) => {}
                GlobalKind::Local(_) => {
                    bail!("initializer for local global must be imported global");
                }
            }
            if other.ty != global.ty {
                bail!("locally defined global does not match type of import");
            }
        }
    }
    Ok(())
}

fn validate_value(value: Value, ty: ValType) -> Result<()> {
    match (value, ty) {
        (Value::I32(_), ValType::I32) => {}
        (Value::I64(_), ValType::I64) => {}
        (Value::F32(_), ValType::F32) => {}
        (Value::F64(_), ValType::F64) => {}
        (Value::V128(_), ValType::V128) => {}
        _ => bail!("mismatched types in value")
    }
    Ok(())
}
