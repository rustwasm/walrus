//! Exported items in a wasm module.

use crate::emit::{Emit, EmitContext, Section};
use crate::map::IdHashMap;
use crate::parse::IndicesToIds;
use crate::tombstone_arena::{Id, Tombstone, TombstoneArena};
use crate::{Function, Global, Memory, Table};
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
    fn_id_to_export_id: IdHashMap<Function, ExportId>,
    tbl_id_to_export_id: IdHashMap<Table, ExportId>,
    mem_id_to_export_id: IdHashMap<Memory, ExportId>,
    global_id_to_export_id: IdHashMap<Global, ExportId>,
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
        self.remove_mapping(id);
        self.arena.delete(id);
    }

    /// Get a shared reference to this module's exports.
    pub fn iter(&self) -> impl Iterator<Item = &Export> {
        self.arena.iter().map(|(_, f)| f)
    }

    /// Get a mutable reference to this module's exports.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Export> {
        self.arena.iter_mut().map(|(_, f)| f)
    }

    /// Add a new export to this module
    pub fn add(&mut self, name: &str, item: impl Into<ExportItem>) -> ExportId {
        let export_item: ExportItem = item.into();
        let export_id: ExportId = self.arena.alloc_with_id(|id| Export {
            id,
            name: name.to_string(),
            item: export_item,
        });
        self.insert_mapping(export_item, export_id);
        export_id
    }

    #[doc(hidden)]
    #[deprecated(note = "Use `ModuleExports::delete` instead")]
    pub fn remove_root(&mut self, id: ExportId) {
        self.delete(id);
    }

    fn insert_mapping(&mut self, item: ExportItem, id: ExportId) -> Option<ExportId> {
        match item {
            ExportItem::Function(f) => self.fn_id_to_export_id.insert(f, id),
            ExportItem::Table(t) => self.tbl_id_to_export_id.insert(t, id),
            ExportItem::Memory(m) => self.mem_id_to_export_id.insert(m, id),
            ExportItem::Global(g) => self.global_id_to_export_id.insert(g, id),
        }
    }

    fn remove_mapping(&mut self, id: ExportId) -> Option<ExportId> {
        let export = self.get_mut(id); // can throw
        match export.item {
            ExportItem::Function(f) => self.fn_id_to_export_id.remove(&f),
            ExportItem::Table(t) => self.tbl_id_to_export_id.remove(&t),
            ExportItem::Memory(m) => self.mem_id_to_export_id.remove(&m),
            ExportItem::Global(g) => self.global_id_to_export_id.remove(&g),
        }
    }

    /// Get a reference to a function export given its function id.
    pub fn get_exported_func(&self, f: FunctionId) -> Option<&Export> {
        self.fn_id_to_export_id.get(&f).map(|id| self.get(*id)) // self.get can throw
    }

    /// Get a reference to a table export given its table id.
    pub fn get_exported_table(&self, t: TableId) -> Option<&Export> {
        self.tbl_id_to_export_id.get(&t).map(|id| self.get(*id)) // self.get can throw
    }

    /// Get a reference to a memory export given its export id.
    pub fn get_exported_memory(&self, m: MemoryId) -> Option<&Export> {
        self.mem_id_to_export_id.get(&m).map(|id| self.get(*id)) // self.get can throw
    }

    /// Get a reference to a global export given its global id.
    pub fn get_exported_global(&self, g: GlobalId) -> Option<&Export> {
        self.global_id_to_export_id.get(&g).map(|id| self.get(*id)) // self.get can throw
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{FunctionBuilder, Module};

    #[test]
    fn get_exported_func() {
        let mut module = Module::default();
        let mut builder = FunctionBuilder::new(&mut module.types, &[], &[]);
        builder.func_body().i32_const(1234).drop();
        let function_id = builder.finish(vec![], &mut module.funcs);
        module.exports.add("dummy", function_id);

        let actual: Option<&Export> = module.exports.get_exported_func(function_id);

        let export: &Export = actual.expect("Expected Some(Export) got None");
        assert_eq!(export.name, "dummy");
        match export.item {
            ExportItem::Function(f) => assert_eq!(f, function_id),
            _ => panic!("Expected a Function"),
        }
    }

    #[test]
    fn get_exported_func_should_return_none_for_unknown_function_id() {
        let mut module = Module::default();
        let mut builder = FunctionBuilder::new(&mut module.types, &[], &[]);
        builder.func_body().i32_const(1234).drop();
        let function_id = builder.finish(vec![], &mut module.funcs);

        let actual: Option<&Export> = module.exports.get_exported_func(function_id);

        assert!(actual.is_none());
    }
}
