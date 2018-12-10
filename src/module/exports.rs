//! TODO

use super::functions::ModuleFunctions;
use super::globals::{GlobalId, ModuleGlobals};
use super::memories::{MemoryId, ModuleMemories};
use super::tables::{ModuleTables, TableId};
use crate::arena_set::ArenaSet;
use crate::error::{ErrorKind, Result};
use crate::module::emit::{Emit, IdsToIndices};
use crate::module::functions::FunctionId;
use crate::passes::Used;
use failure::Fail;
use id_arena::Id;
use parity_wasm::elements;
use std::ops;

/// The id of an export.
pub type ExportId = Id<Export>;

/// A named item exported from the wasm.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Export {
    /// The name of this export.
    pub name: String,
    /// The item being exported.
    pub item: ExportItem,
}

impl Export {
    fn entry(&self, indices: &IdsToIndices) -> elements::ExportEntry {
        let internal = self.item.internal(indices);
        elements::ExportEntry::new(self.name.clone(), internal)
    }
}

/// An exported item.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum ExportItem {
    /// An exported function.
    Function(FunctionId),
    /// An exported table.
    Table(TableId),
    /// An exported memory.
    Memory(MemoryId),
    /// An exported global.
    Global(GlobalId),
}

impl ExportItem {
    fn internal(&self, indices: &IdsToIndices) -> elements::Internal {
        match *self {
            ExportItem::Function(f) => {
                let idx = indices.get_func_index(f);
                elements::Internal::Function(idx)
            }
            ExportItem::Table(t) => {
                let idx = indices.get_table_index(t);
                elements::Internal::Table(idx)
            }
            ExportItem::Memory(m) => {
                let idx = indices.get_memory_index(m);
                elements::Internal::Memory(idx)
            }
            ExportItem::Global(g) => {
                let idx = indices.get_global_index(g);
                elements::Internal::Global(idx)
            }
        }
    }
}

/// The set of exports in a module.
#[derive(Debug)]
pub struct ModuleExports {
    /// The arena containing this module's exports.
    pub arena: ArenaSet<Export>,
}

impl ops::Deref for ModuleExports {
    type Target = ArenaSet<Export>;

    #[inline]
    fn deref(&self) -> &ArenaSet<Export> {
        &self.arena
    }
}

impl ops::DerefMut for ModuleExports {
    #[inline]
    fn deref_mut(&mut self) -> &mut ArenaSet<Export> {
        &mut self.arena
    }
}

impl ModuleExports {
    /// Construct the export set for a wasm module.
    pub fn new(
        funcs: &ModuleFunctions,
        tables: &ModuleTables,
        memories: &ModuleMemories,
        globals: &ModuleGlobals,
        export_section: &elements::ExportSection,
    ) -> Result<ModuleExports> {
        let mut arena = ArenaSet::with_capacity(export_section.entries().len());

        for exp in export_section.entries() {
            let item = match *exp.internal() {
                elements::Internal::Function(f) => funcs
                    .function_for_index(f)
                    .map(ExportItem::Function)
                    .ok_or_else(|| {
                        ErrorKind::InvalidWasm.context("exported function does not exist")
                    })?,
                elements::Internal::Table(t) => tables
                    .table_for_index(t)
                    .map(ExportItem::Table)
                    .ok_or_else(|| {
                        ErrorKind::InvalidWasm.context("exported table does not exist")
                    })?,
                elements::Internal::Memory(m) => memories
                    .memory_for_index(m)
                    .map(ExportItem::Memory)
                    .ok_or_else(|| {
                        ErrorKind::InvalidWasm.context("exported memory does not exist")
                    })?,
                elements::Internal::Global(g) => globals
                    .global_for_index(g)
                    .map(ExportItem::Global)
                    .ok_or_else(|| {
                        ErrorKind::InvalidWasm.context("exported global does not exist")
                    })?,
            };
            arena.insert(Export {
                name: exp.field().to_string(),
                item,
            });
        }

        Ok(ModuleExports { arena })
    }
}

impl Emit for ModuleExports {
    fn emit(&self, _used: &Used, module: &mut elements::Module, indices: &mut IdsToIndices) {
        // NB: exports are always considered used. They are the roots that the
        // used analysis searches out from.

        let mut exports = vec![];

        for (_id, exp) in self.arena.iter() {
            let export = exp.entry(indices);
            exports.push(export);
        }

        if exports.is_empty() {
            return;
        }

        let exports = elements::ExportSection::with_entries(exports);
        let exports = elements::Section::Export(exports);
        module.sections_mut().push(exports);
    }
}
