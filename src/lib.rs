//! The `walrus` WebAssembly transformations library.

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]

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

pub use crate::error::{ErrorKind, Result};
pub use crate::function_builder::{BlockBuilder, FunctionBuilder};
pub use crate::init_expr::InitExpr;
pub use crate::ir::{Local, LocalId};
pub use crate::module::*;
pub use crate::ty::{Type, TypeId, ValType};
