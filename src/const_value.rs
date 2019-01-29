//! Handling wasm constant values

use crate::emit::{Emit, EmitContext};
use crate::error::Result;
use crate::ir::Value;
use crate::module::globals::GlobalId;
use crate::parse::IndicesToIds;
use failure::bail;

/// A constant which is produced in WebAssembly, typically used in global
/// initializers or element/data offsets.
#[derive(Debug, Copy, Clone)]
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
}

impl Emit for Const {
    fn emit(&self, cx: &mut EmitContext) {
        match *self {
            Const::Value(val) => val.emit(&mut cx.encoder),
            Const::Global(id) => {
                let idx = cx.indices.get_global_index(id);
                cx.encoder.byte(0x23); // global.get
                cx.encoder.u32(idx);
            }
        }
        cx.encoder.byte(0x0b); // end
    }
}
