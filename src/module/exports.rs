//! Exported items in a wasm module.

use super::globals::GlobalId;
use super::memories::MemoryId;
use super::tables::TableId;
use crate::emit::{Emit, EmitContext, Section};
use crate::error::Result;
use crate::module::functions::FunctionId;
use crate::module::Module;
use crate::parse::IndicesToIds;
use id_arena::{Arena, Id};

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
    arena: Arena<Export>,
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

    /// Add a new export to this module
    pub fn add(&mut self, name: &str, item: impl Into<ExportItem>) -> ExportId {
        self.arena.alloc_with_id(|id| Export {
            id,
            name: name.to_string(),
            item: item.into(),
        })
    }
}

impl Module {
    /// Construct the export set for a wasm module.
    pub(crate) fn parse_exports(
        &mut self,
        section: wasmparser::ExportSectionReader,
        ids: &IndicesToIds,
    ) -> Result<()> {
        log::debug!("parse export section");
        use wasmparser::ExternalKind::*;

        for entry in section {
            let entry = entry?;
            let item = match entry.kind {
                Function => ExportItem::Function(ids.get_func(entry.index)?),
                Table => ExportItem::Table(ids.get_table(entry.index)?),
                Memory => ExportItem::Memory(ids.get_memory(entry.index)?),
                Global => ExportItem::Global(ids.get_global(entry.index)?),
            };
            let id = self.exports.arena.next_id();
            self.exports.arena.alloc(Export {
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

        if self.arena.len() == 0 {
            return;
        }

        let mut cx = cx.start_section(Section::Export);
        cx.encoder.usize(self.arena.len());

        for (_id, export) in self.arena.iter() {
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
