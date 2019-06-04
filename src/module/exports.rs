//! Exported items in a wasm module.

use crate::emit::{Emit, EmitContext, Section};
use crate::tombstone_arena::{Id, Tombstone, TombstoneArena};
use crate::{FunctionId, GlobalId, MemoryId, Module, Result, TableId};

/// The id of an export.
pub type ExportId = Id<Export>;

/// A named item exported from the wasm.
#[derive(Clone, Debug)]
pub struct Export {
    id: ExportId,
    /// The name of this export.
    pub name: String,
    /// The item being exported.
    pub item: ExportItem,
}

impl Tombstone for Export {
    fn on_delete(&mut self) {
        self.name = String::new();
    }
}

impl Export {
    /// Returns the id of this export
    pub fn id(&self) -> ExportId {
        self.id
    }
}

/// An exported item.
#[derive(Copy, Clone, Debug)]
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

/// The set of exports in a module.
#[derive(Debug, Default)]
pub struct ModuleExports {
    /// The arena containing this module's exports.
    arena: TombstoneArena<Export>,
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

    /// Delete an export entry from this module.
    pub fn delete(&mut self, id: ExportId) {
        self.arena.delete(id);
    }

    /// Get a shared reference to this module's exports.
    pub fn iter(&self) -> impl Iterator<Item = &Export> {
        self.arena.iter().map(|(_, f)| f)
    }

    /// Add a new export to this module
    pub fn add(&mut self, name: &str, item: impl Into<ExportItem>) -> ExportId {
        self.arena.alloc_with_id(|id| Export {
            id,
            name: name.to_string(),
            item: item.into(),
        })
    }

    #[doc(hidden)]
    #[deprecated(note = "Use `ModuleExports::delete` instead")]
    pub fn remove_root(&mut self, id: ExportId) {
        self.delete(id);
    }
}

impl Module {
    /// Construct the export set for a wasm module.
    pub(crate) fn parse_exports(&mut self, section: wasmparser::ExportSectionReader) -> Result<()> {
        log::debug!("parse export section");
        use wasmparser::ExternalKind::*;

        for entry in section {
            let entry = entry?;
            let item = match entry.kind {
                Function => ExportItem::Function(self.indices_to_ids.get_func(entry.index)?),
                Table => ExportItem::Table(self.indices_to_ids.get_table(entry.index)?),
                Memory => ExportItem::Memory(self.indices_to_ids.get_memory(entry.index)?),
                Global => ExportItem::Global(self.indices_to_ids.get_global(entry.index)?),
            };
            self.exports.arena.alloc_with_id(|id| Export {
                id,
                name: entry.field.to_string(),
                item,
            });
        }
        Ok(())
    }
}

impl Emit for ModuleExports {
    fn emit(&self, cx: &mut EmitContext) {
        log::debug!("emit export section");
        // NB: exports are always considered used. They are the roots that the
        // used analysis searches out from.

        let count = self.iter().count();
        if count == 0 {
            return;
        }

        let mut cx = cx.start_section(Section::Export);
        cx.encoder.usize(count);

        for export in self.iter() {
            cx.encoder.str(&export.name);
            match export.item {
                ExportItem::Function(id) => {
                    let index = cx.indices.get_func_index(id);
                    cx.encoder.byte(0x00);
                    cx.encoder.u32(index);
                }
                ExportItem::Table(id) => {
                    let index = cx.indices.get_table_index(id);
                    cx.encoder.byte(0x01);
                    cx.encoder.u32(index);
                }
                ExportItem::Memory(id) => {
                    let index = cx.indices.get_memory_index(id);
                    cx.encoder.byte(0x02);
                    cx.encoder.u32(index);
                }
                ExportItem::Global(id) => {
                    let index = cx.indices.get_global_index(id);
                    cx.encoder.byte(0x03);
                    cx.encoder.u32(index);
                }
            }
        }
    }
}

impl From<MemoryId> for ExportItem {
    fn from(id: MemoryId) -> ExportItem {
        ExportItem::Memory(id)
    }
}

impl From<FunctionId> for ExportItem {
    fn from(id: FunctionId) -> ExportItem {
        ExportItem::Function(id)
    }
}

impl From<GlobalId> for ExportItem {
    fn from(id: GlobalId) -> ExportItem {
        ExportItem::Global(id)
    }
}

impl From<TableId> for ExportItem {
    fn from(id: TableId) -> ExportItem {
        ExportItem::Table(id)
    }
}
