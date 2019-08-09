//! The `walrus` WebAssembly transformations library.

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]

#[cfg(feature = "parallel")]
macro_rules! maybe_parallel {
    ($e:ident.($serial:ident | $parallel:ident)) => {
        $e.$parallel()
    };
}

#[cfg(not(feature = "parallel"))]
macro_rules! maybe_parallel {
    ($e:ident.($serial:ident | $parallel:ident)) => {
        $e.$serial()
    };
}

mod arena_set;
pub mod dot;
mod emit;
mod encode;
mod error;
mod function_builder;
mod init_expr;
pub mod ir;
mod map;
mod module;
mod parse;
pub mod passes;
mod tombstone_arena;
mod ty;

pub use crate::emit::IdsToIndices;
pub use crate::error::{ErrorKind, Result};
pub use crate::function_builder::{FunctionBuilder, InstrSeqBuilder};
pub use crate::init_expr::InitExpr;
pub use crate::ir::{Local, LocalId};
pub use crate::module::*;
pub use crate::parse::IndicesToIds;
pub use crate::ty::{Type, TypeId, ValType};
