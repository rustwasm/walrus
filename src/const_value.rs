//! Handling wasm constant values

use crate::emit::IdsToIndices;
use crate::error::Result;
use crate::ir::Value;
use crate::module::globals::GlobalId;
use crate::parse::IndicesToIds;
use failure::bail;
use parity_wasm::elements::{self, Instruction};

/// A constant which is produced in WebAssembly, typically used in global
/// initializers or element/data offsets.
#[derive(Debug)]
pub enum Const {
    /// An immediate constant value
    Value(Value),
    /// A constant value referenced by the global specified
    Global(GlobalId),
}

impl Const {
    pub(crate) fn eval(init: &wasmparser::InitExpr, ids: &IndicesToIds) -> Result<Const> {
        use wasmparser::Operator::*;
        let mut reader = init.get_operators_reader();
        let val = match reader.read()? {
            I32Const { value } => Const::Value(Value::I32(value)),
            I64Const { value } => Const::Value(Value::I64(value)),
            F32Const { value } => Const::Value(Value::F32(f32::from_bits(value.bits()))),
            F64Const { value } => Const::Value(Value::F64(f64::from_bits(value.bits()))),
            GetGlobal { global_index } => Const::Global(ids.get_global(global_index)?),
            _ => bail!("invalid constant expression"),
        };
        match reader.read()? {
            End => {}
            _ => bail!("invalid constant expression"),
        }
        reader.ensure_end()?;
        Ok(val)
    }

    pub(crate) fn emit_instructions(&self, indices: &IdsToIndices) -> elements::InitExpr {
        let mut instrs = Vec::with_capacity(2);
        instrs.push(match *self {
            Const::Value(Value::I32(n)) => Instruction::I32Const(n),
            Const::Value(Value::I64(n)) => Instruction::I64Const(n),
            Const::Value(Value::F32(n)) => Instruction::F32Const(n.to_bits()),
            Const::Value(Value::F64(n)) => Instruction::F64Const(n.to_bits()),
            Const::Value(Value::V128(_n)) => unimplemented!(),
            Const::Global(id) => Instruction::GetGlobal(indices.get_global_index(id)),
        });
        instrs.push(Instruction::End);
        elements::InitExpr::new(instrs)
    }
}
