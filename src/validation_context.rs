//! Wasm validation context.

use super::chunk_list::ChunkList;
use super::error::{ErrorKind, Result};
use crate::ty::{Type, ValType};
use failure::Fail;
use std::u32;

/// Wasm validation context.
///
/// https://webassembly.github.io/spec/core/valid/conventions.html#contexts
#[derive(Debug)]
pub struct ValidationContext<'a> {
    pub(crate) labels: ChunkList<'a, Box<[ValType]>>,
    pub(crate) return_: ChunkList<'a, Box<[ValType]>>,
}

impl<'a> ValidationContext<'a> {
    /// Construct a `ValidationContext` from the given module.
    pub fn new() -> ValidationContext<'a> {
        ValidationContext {
            labels: ChunkList::new(),
            return_: ChunkList::new(),
        }
    }

    /// Get a nested validation context.
    pub fn nested<'b>(&'b self) -> ValidationContext<'b> {
        ValidationContext {
            labels: ChunkList::with_tail(&self.labels),
            return_: ChunkList::with_tail(&self.return_),
        }
    }

    /// Get a nested validation context for the given function.
    pub fn for_function<'b>(&'b self, ty: &Type) -> ValidationContext<'b> {
        let labels = vec![ty
            .results()
            .iter()
            .cloned()
            .collect::<Vec<_>>()
            .into_boxed_slice()];
        let return_ = labels.clone();

        ValidationContext {
            labels: ChunkList::with_head(labels),
            return_: ChunkList::with_head(return_),
        }
    }

    /// Get a nested validation context for a block with the given label.
    pub fn for_block(&self, label: Box<[ValType]>) -> ValidationContext {
        ValidationContext {
            labels: ChunkList::with_head_and_tail(vec![label], &self.labels),
            ..self.nested()
        }
    }

    /// Get a nested validation context for a loop.
    pub fn for_loop(&self) -> ValidationContext {
        ValidationContext {
            labels: ChunkList::with_head_and_tail(vec![vec![].into_boxed_slice()], &self.labels),
            ..self.nested()
        }
    }

    /// Get a nested validation context for an if/else.
    pub fn for_if_else(&self, label: Box<[ValType]>) -> ValidationContext {
        ValidationContext {
            labels: ChunkList::with_head_and_tail(vec![label], &self.labels),
            ..self.nested()
        }
    }

    /// Get the type of the n^th local.
    pub fn label(&self, n: u32) -> Result<&[ValType]> {
        self.labels.get(n as usize).map(|t| &t[..]).ok_or_else(|| {
            ErrorKind::InvalidWasm
                .context(format!(
                    "local {} is out of bounds ({} labels)",
                    n,
                    self.labels.len()
                ))
                .into()
        })
    }
}
