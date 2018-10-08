//! TODO

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]

extern crate failure;
extern crate parity_wasm;

pub mod arena;
pub mod ast;
pub mod chunk_list;
pub mod dot;
pub mod error;
pub mod function;
pub mod validation_context;

use failure::Fail;
use parity_wasm::elements;
use std::fmt;

/// TODO
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ValType {
    /// TODO
    I32,
    /// TODO
    I64,
    /// TODO
    F32,
    /// TODO
    F64,
    /// TODO
    V128,
}

impl<'a> From<&'a elements::ValueType> for ValType {
    fn from(x: &'a elements::ValueType) -> ValType {
        match x {
            elements::ValueType::I32 => ValType::I32,
            elements::ValueType::I64 => ValType::I64,
            elements::ValueType::F32 => ValType::F32,
            elements::ValueType::F64 => ValType::F64,
            elements::ValueType::V128 => ValType::V128,
        }
    }
}

impl ValType {
    fn from_block_ty(block_ty: &elements::BlockType) -> Vec<ValType> {
        match block_ty {
            elements::BlockType::Value(ty) => vec![ty.into()],
            elements::BlockType::NoResult => vec![],
        }
    }
}

impl fmt::Display for ValType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ValType::I32 => "i32",
                ValType::I64 => "i64",
                ValType::F32 => "f32",
                ValType::F64 => "f64",
                ValType::V128 => "v128",
            }
        )
    }
}
