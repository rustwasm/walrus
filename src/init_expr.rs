//! Handling wasm constant values

use crate::emit::{Emit, EmitContext};
use crate::ir::Value;
use crate::parse::IndicesToIds;
use crate::ValType;
use crate::{FunctionId, GlobalId, Result};
use anyhow::bail;

/// A constant which is produced in WebAssembly, typically used in global
/// initializers or element/data offsets.
#[derive(Debug, Copy, Clone)]
pub enum InitExpr {
    /// An immediate constant value
    Value(Value),
    /// A constant value referenced by the global specified
    Global(GlobalId),
    /// A null reference
    RefNull(ValType),
    /// A function initializer
    RefFunc(FunctionId),
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
            V128Const { value } => InitExpr::Value(Value::V128(v128_to_u128(&value))),
            GlobalGet { global_index } => InitExpr::Global(ids.get_global(global_index)?),
            RefNull { ty } => InitExpr::RefNull(ValType::parse(&ty)?),
            RefFunc { function_index } => InitExpr::RefFunc(ids.get_func(function_index)?),
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
            InitExpr::RefNull(ty) => {
                cx.encoder.byte(0xd0); // ref.null
                ty.emit(&mut cx.encoder);
            }
            InitExpr::RefFunc(id) => {
                cx.encoder.byte(0xd2); // ref.func
                cx.encoder.u32(cx.indices.get_func_index(id));
            }
        }
        cx.encoder.byte(0x0b); // end
    }
}

pub(crate) fn v128_to_u128(value: &wasmparser::V128) -> u128 {
    let n = value.bytes();
    ((n[0] as u128) << 0)
        | ((n[1] as u128) << 8)
        | ((n[2] as u128) << 16)
        | ((n[3] as u128) << 24)
        | ((n[4] as u128) << 32)
        | ((n[5] as u128) << 40)
        | ((n[6] as u128) << 48)
        | ((n[7] as u128) << 56)
        | ((n[8] as u128) << 64)
        | ((n[9] as u128) << 72)
        | ((n[10] as u128) << 80)
        | ((n[11] as u128) << 88)
        | ((n[12] as u128) << 96)
        | ((n[13] as u128) << 104)
        | ((n[14] as u128) << 112)
        | ((n[15] as u128) << 120)
}
