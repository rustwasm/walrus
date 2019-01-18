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
    pub(crate) fn eval(init: &elements::InitExpr, ids: &IndicesToIds) -> Result<Const> {
        let instrs = init.code();
        if instrs.len() != 2 {
            bail!("invalid constant expression");
        }
        match instrs[1] {
            Instruction::End => {}
            _ => bail!("invalid constant expression"),
        }
        match instrs[0] {
            Instruction::I32Const(n) => Ok(Const::Value(Value::I32(n))),
            Instruction::I64Const(n) => Ok(Const::Value(Value::I64(n))),
            Instruction::F32Const(n) => Ok(Const::Value(Value::F32(f32::from_bits(n)))),
            Instruction::F64Const(n) => Ok(Const::Value(Value::F64(f64::from_bits(n)))),
            Instruction::GetGlobal(n) => Ok(Const::Global(ids.get_global(n)?)),
            _ => bail!("invalid constant expression"),
        }
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
