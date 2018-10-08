//! TODO

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]

extern crate failure;
extern crate parity_wasm;

pub mod arena;
pub mod chunk_list;
pub mod error;
pub mod function;
pub mod validation_context;

use self::arena::Id;
use failure::Fail;
use parity_wasm::elements;
use std::fmt;

/// TODO
pub type ExprId = Id<Expr>;

/// TODO
pub type BlockId = Id<Block>;

/// TODO
#[derive(Debug)]
pub struct Block {
    exprs: Vec<ExprId>,
}

/// TODO
#[derive(Debug)]
pub enum Expr {
    /// `get_local n`
    GetLocal {
        /// The type of this local.
        ty: ValType,
        /// The n^th local.
        local: u32,
    },

    /// TODO
    I32Const(i32),

    /// TODO
    I32Add(ExprId, ExprId),

    /// TODO
    Select(ExprId, ExprId),

    /// TODO
    Unreachable,

    /// TODO
    Phi,

    /// TODO
    Br {
        /// The target block to branch to.
        block: BlockId,
        /// The arguments to the block.
        args: Box<[ExprId]>,
    },

    /// TODO
    BrIf {
        /// The condition for when to branch.
        condition: ExprId,
        /// The target block to branch to when the condition is met.
        block: BlockId,
        /// The arguments to the block.
        args: Box<[ExprId]>,
    },

    /// TODO
    BrTable {
        /// The table index of which block to branch to.
        which: ExprId,
        /// The table of target blocks.
        blocks: Box<[BlockId]>,
        /// The block that is branched to by default when `which` is out of the
        /// table's bounds.
        default: BlockId,
        /// The arguments to the block.
        args: Box<[ExprId]>,
    },
}

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
