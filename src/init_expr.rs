//! Handling wasm constant values

use crate::emit::{Emit, EmitContext};
use crate::ir::Value;
use crate::parse::IndicesToIds;
use crate::{GlobalId, Result};
use anyhow::bail;

/// A constant which is produced in WebAssembly, typically used in global
/// initializers or element/data offsets.
#[derive(Debug, Copy, Clone)]
pub enum InitExpr {
    /// An immediate constant value
    Value(Value),
    /// A constant value referenced by the global specified
    Global(GlobalId),
}

impl InitExpr {
    pub(crate) fn eval(init: &wasmparser::InitExpr, ids: &IndicesToIds) -> Result<InitExpr> {
        use wasmparser::Operator::*;
        let mut reader = init.get_operators_reader();
        let val = match reader.read()? {
            I32Const { value } => InitExpr::Value(Value::I32(value)),
            I64Const { value } => InitExpr::Value(Value::I64(value)),
            F32Const { value } => InitExpr::Value(Value::F32(f32::from_bits(value.bits()))),
            F64Const { value } => InitExpr::Value(Value::F64(f64::from_bits(value.bits()))),
            GetGlobal { global_index } => InitExpr::Global(ids.get_global(global_index)?),
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

impl Emit for InitExpr {
    fn emit(&self, cx: &mut EmitContext) {
        match *self {
            InitExpr::Value(val) => val.emit(&mut cx.encoder),
            InitExpr::Global(id) => {
                let idx = cx.indices.get_global_index(id);
                cx.encoder.byte(0x23); // global.get
                cx.encoder.u32(idx);
            }
        }
        cx.encoder.byte(0x0b); // end
    }
}
