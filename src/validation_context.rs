//! Wasm validation context.

use super::chunk_list::ChunkList;
use super::error::{ErrorKind, Result};
use crate::ty::{Type, ValType};
use failure::{Fail, ResultExt};
use parity_wasm::elements;
use std::iter;
use std::u16;
use std::u32;

/// Wasm validation context.
///
/// https://webassembly.github.io/spec/core/valid/conventions.html#contexts
#[derive(Debug)]
pub struct ValidationContext<'a> {
    types: ChunkList<'a, elements::FunctionType>,
    pub(crate) funcs: ChunkList<'a, elements::FunctionType>,
    tables: ChunkList<'a, elements::TableType>,
    mems: ChunkList<'a, elements::MemoryType>,
    globals: ChunkList<'a, elements::GlobalType>,
    pub(crate) locals: ChunkList<'a, ValType>,
    pub(crate) labels: ChunkList<'a, Box<[ValType]>>,
    pub(crate) return_: ChunkList<'a, Box<[ValType]>>,
}

impl<'a> ValidationContext<'a> {
    /// Construct a `ValidationContext` from the given module.
    pub fn for_module<'b>(module: &'b elements::Module) -> Result<ValidationContext<'a>> {
        let types: Vec<_> = match module.type_section() {
            None => vec![],
            Some(ts) => ts
                .types()
                .iter()
                .map(|t| match t {
                    elements::Type::Function(f) => f,
                })
                .cloned()
                .collect(),
        };

        let mut funcs = vec![];
        if let Some(fs) = module.function_section() {
            funcs.reserve(fs.entries().len());
            for f in fs.entries() {
                let ty = types.get(f.type_ref() as usize).cloned().ok_or_else(|| {
                    ErrorKind::InvalidWasm.context("function referring to an out-of-bounds type")
                })?;
                funcs.push(ty);
            }
        }

        let mut tables = vec![];
        if let Some(ts) = module.table_section() {
            for t in ts.entries() {
                validate_table(t)?;
                tables.push(t.clone());
            }
        }

        let mut mems = vec![];
        if let Some(ms) = module.memory_section() {
            for m in ms.entries() {
                validate_memory(m)?;
                mems.push(m.clone());
            }
        }

        let globals = module.global_section().map_or_else(
            || vec![],
            |gs| {
                gs.entries()
                    .iter()
                    .map(|g| g.global_type().clone())
                    .collect()
            },
        );

        Ok(ValidationContext {
            types: ChunkList::with_head(types),
            funcs: ChunkList::with_head(funcs),
            tables: ChunkList::with_head(tables),
            mems: ChunkList::with_head(mems),
            globals: ChunkList::with_head(globals),
            locals: ChunkList::new(),
            labels: ChunkList::new(),
            return_: ChunkList::new(),
        })
    }

    /// Get a nested validation context.
    pub fn nested<'b>(&'b self) -> ValidationContext<'b> {
        ValidationContext {
            types: ChunkList::with_tail(&self.types),
            funcs: ChunkList::with_tail(&self.funcs),
            tables: ChunkList::with_tail(&self.tables),
            mems: ChunkList::with_tail(&self.mems),
            globals: ChunkList::with_tail(&self.globals),
            locals: ChunkList::with_tail(&self.locals),
            labels: ChunkList::with_tail(&self.labels),
            return_: ChunkList::with_tail(&self.return_),
        }
    }

    /// Get a nested validation context for the given function.
    pub fn for_function<'b>(
        &'b self,
        ty: &Type,
        body: &elements::FuncBody,
    ) -> Result<ValidationContext<'b>> {
        let locals = ty
            .params()
            .iter()
            .cloned()
            .chain(body.locals().iter().flat_map(|l| {
                iter::repeat(ValType::from(&l.value_type())).take(l.count() as usize)
            }))
            .collect();

        let labels = vec![ty
            .results()
            .iter()
            .cloned()
            .collect::<Vec<_>>()
            .into_boxed_slice()];
        let return_ = labels.clone();

        Ok(ValidationContext {
            locals: ChunkList::with_head(locals),
            labels: ChunkList::with_head(labels),
            return_: ChunkList::with_head(return_),
            ..self.nested()
        })
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
    pub fn local(&self, n: u32) -> Result<ValType> {
        self.locals.get(n as usize).cloned().ok_or_else(|| {
            ErrorKind::InvalidWasm
                .context(format!(
                    "local {} is out of bounds ({} locals)",
                    n,
                    self.locals.len()
                ))
                .into()
        })
    }

    /// Get the type of the n^th local.
    pub fn label(&self, n: u32) -> Result<&[ValType]> {
        self.labels.get(n as usize).map(|t| &t[..]).ok_or_else(|| {
            ErrorKind::InvalidWasm
                .context(format!(
                    "local {} is out of bounds ({} locals)",
                    n,
                    self.locals.len()
                ))
                .into()
        })
    }
}

fn validate_table(t: &elements::TableType) -> Result<()> {
    validate_limits(t.limits(), u32::MAX).context("when validating a table")?;

    // Ensure that the table element type is `anyfunc`. This does
    // nothing, but if new wasm versions and future parity-wasm releases
    // get support for new table types, this may need to actually do
    // something.
    match t.elem_type() {
        elements::TableElementType::AnyFunc => Ok(()),
    }
}

fn validate_limits(l: &elements::ResizableLimits, k: u32) -> Result<()> {
    match (l.initial(), l.maximum()) {
        (min, Some(max)) if max < min || max > k => Err(ErrorKind::InvalidWasm
            .context(format!(
                "invalid limits: min = {}, max = {}; k = {}",
                min, max, k
            ))
            .into()),
        (min, _) => {
            if min <= k {
                Ok(())
            } else {
                Err(ErrorKind::InvalidWasm
                    .context(format!("invalid limits: min = {}, k = {}", min, k))
                    .into())
            }
        }
    }
}

fn validate_memory(m: &elements::MemoryType) -> Result<()> {
    validate_limits(m.limits(), u16::MAX as u32).context("when validating a memory")?;
    Ok(())
}
