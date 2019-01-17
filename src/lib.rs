//! The `walrus` WebAssembly transformations library.

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]

mod arena_set;
mod chunk_list;
pub mod const_value;
pub mod dot;
pub mod error;
pub mod ir;
pub mod module;
pub mod passes;
pub mod ty;
pub mod validation_context;
