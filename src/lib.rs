//! The `walrus` WebAssembly transformations library.

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]

mod arena_set;
pub mod const_value;
pub mod dot;
mod emit;
pub mod error;
pub mod ir;
pub mod module;
mod parse;
pub mod passes;
pub mod ty;
