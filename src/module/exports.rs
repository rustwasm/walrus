//! Exported items in a wasm module.

use super::globals::GlobalId;
use super::memories::MemoryId;
use super::tables::TableId;
use crate::arena_set::ArenaSet;
use crate::emit::{Emit, EmitContext, IdsToIndices};
use crate::error::Result;
use crate::module::functions::FunctionId;
use crate::module::parse::IndicesToIds;
use crate::module::Module;
use id_arena::Id;
use parity_wasm::elements;

/// The id of an export.
pub type ExportId = Id<Export>;

/// A named item exported from the wasm.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Export {
    id: ExportId,
    /// The name of this export.
    pub name: String,
    /// The item being exported.
    pub item: ExportItem,
}

impl Export {
    /// Returns the id of this export
    pub fn id(&self) -> ExportId {
        self.id
    }

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
#[derive(Debug, Default)]
pub struct ModuleExports {
    /// The arena containing this module's exports.
    arena: ArenaSet<Export>,
}

impl ModuleExports {
    /// Gets a reference to an export given its id
    pub fn get(&self, id: ExportId) -> &Export {
        &self.arena[id]
    }

    /// Gets a reference to an export given its id
    pub fn get_mut(&mut self, id: ExportId) -> &mut Export {
        &mut self.arena[id]
    }

    /// Get a shared reference to this module's exports.
    pub fn iter(&self) -> impl Iterator<Item = &Export> {
        self.arena.iter().map(|(_, f)| f)
    }
}

impl Module {
    /// Construct the export set for a wasm module.
    pub(crate) fn parse_exports(
        &mut self,
        section: &elements::ExportSection,
        ids: &IndicesToIds,
    ) -> Result<()> {
        for exp in section.entries() {
            let item = match *exp.internal() {
                elements::Internal::Function(f) => ExportItem::Function(ids.get_func(f)?),
                elements::Internal::Table(t) => ExportItem::Table(ids.get_table(t)?),
                elements::Internal::Memory(t) => ExportItem::Memory(ids.get_memory(t)?),
                elements::Internal::Global(t) => ExportItem::Global(ids.get_global(t)?),
            };
            let id = self.exports.arena.next_id();
            self.exports.arena.insert(Export {
                id,
                name: exp.field().to_string(),
                item,
            });
        }
        Ok(())
    }
}

impl Emit for ModuleExports {
    fn emit(&self, cx: &mut EmitContext) {
        // NB: exports are always considered used. They are the roots that the
        // used analysis searches out from.

        let mut exports = vec![];

        for (_id, exp) in self.arena.iter() {
            let export = exp.entry(cx.indices);
            exports.push(export);
        }

        if exports.is_empty() {
            return;
        }

        let exports = elements::ExportSection::with_entries(exports);
        let exports = elements::Section::Export(exports);
        cx.dst.sections_mut().push(exports);
    }
}
