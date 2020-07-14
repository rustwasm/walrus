//! A wasm module's imports.

use crate::emit::{Emit, EmitContext, Section};
use crate::parse::IndicesToIds;
use crate::tombstone_arena::{Id, Tombstone, TombstoneArena};
use crate::{FunctionId, GlobalId, MemoryId, Result, TableId};
use crate::{Module, TypeId, ValType};

/// The id of an import.
pub type ImportId = Id<Import>;

/// A named item imported into the wasm.
#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Import {
    id: ImportId,
    /// The module name of this import.
    pub module: String,
    /// The name of this import.
    pub name: String,
    /// The kind of item being imported.
    pub kind: ImportKind,
}

impl Tombstone for Import {
    fn on_delete(&mut self) {
        self.module = String::new();
        self.name = String::new();
    }
}

impl Import {
    /// Get this import's identifier.
    pub fn id(&self) -> ImportId {
        self.id
    }
}

/// An imported item.
#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum ImportKind {
    /// An imported function.
    Function(FunctionId),
    /// An imported table.
    Table(TableId),
    /// An imported memory.
    Memory(MemoryId),
    /// An imported global.
    Global(GlobalId),
}

/// The set of imports in a module.
#[derive(Debug, Default)]
pub struct ModuleImports {
    arena: TombstoneArena<Import>,
}

impl ModuleImports {
    /// Gets a reference to an import given its id
    pub fn get(&self, id: ImportId) -> &Import {
        &self.arena[id]
    }

    /// Gets a reference to an import given its id
    pub fn get_mut(&mut self, id: ImportId) -> &mut Import {
        &mut self.arena[id]
    }

    /// Removes an import from this module.
    ///
    /// It is up to you to ensure that any potential references to the deleted
    /// import are also removed, eg `get_global` expressions.
    pub fn delete(&mut self, id: ImportId) {
        self.arena.delete(id);
    }

    /// Get a shared reference to this module's imports.
    pub fn iter(&self) -> impl Iterator<Item = &Import> {
        self.arena.iter().map(|(_, f)| f)
    }

    /// Get mutable references to this module's imports.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Import> {
        self.arena.iter_mut().map(|(_, f)| f)
    }

    /// Adds a new import to this module
    pub fn add(&mut self, module: &str, name: &str, kind: impl Into<ImportKind>) -> ImportId {
        self.arena.alloc_with_id(|id| Import {
            id,
            module: module.to_string(),
            name: name.to_string(),
            kind: kind.into(),
        })
    }

    /// Get the import with the given module and name
    pub fn find(&self, module: &str, name: &str) -> Option<ImportId> {
        let import = self
            .arena
            .iter()
            .find(|(_, import)| import.name == name && import.module == module);

        Some(import?.0)
    }
}

impl Module {
    /// Construct the import set for a wasm module.
    pub(crate) fn parse_imports(
        &mut self,
        section: wasmparser::ImportSectionReader,
        ids: &mut IndicesToIds,
    ) -> Result<()> {
        log::debug!("parse import section");
        for entry in section {
            let entry = entry?;
            match entry.ty {
                wasmparser::ImportSectionEntryType::Function(idx) => {
                    let ty = ids.get_type(idx)?;
                    let id = self.add_import_func(
                        entry.module,
                        entry.field.expect("module linking not supported"),
                        ty,
                    );
                    ids.push_func(id.0);
                }
                wasmparser::ImportSectionEntryType::Table(t) => {
                    let ty = ValType::parse(&t.element_type)?;
                    let id = self.add_import_table(
                        entry.module,
                        entry.field.expect("module linking not supported"),
                        t.limits.initial,
                        t.limits.maximum,
                        ty,
                    );
                    ids.push_table(id.0);
                }
                wasmparser::ImportSectionEntryType::Memory(m) => {
                    let id = self.add_import_memory(
                        entry.module,
                        entry.field.expect("module linking not supported"),
                        m.shared,
                        m.limits.initial,
                        m.limits.maximum,
                    );
                    ids.push_memory(id.0);
                }
                wasmparser::ImportSectionEntryType::Global(g) => {
                    let id = self.add_import_global(
                        entry.module,
                        entry.field.expect("module linking not supported"),
                        ValType::parse(&g.content_type)?,
                        g.mutable,
                    );
                    ids.push_global(id.0);
                }
                wasmparser::ImportSectionEntryType::Module(_)
                | wasmparser::ImportSectionEntryType::Instance(_) => {
                    unimplemented!("module linking not implemented");
                }
            }
        }

        Ok(())
    }

    /// Add an imported function to this module
    pub fn add_import_func(
        &mut self,
        module: &str,
        name: &str,
        ty: TypeId,
    ) -> (FunctionId, ImportId) {
        let import = self.imports.arena.next_id();
        let func = self.funcs.add_import(ty, import);
        self.imports.add(module, name, func);
        (func, import)
    }

    /// Add an imported memory to this module
    pub fn add_import_memory(
        &mut self,
        module: &str,
        name: &str,
        shared: bool,
        initial: u32,
        maximum: Option<u32>,
    ) -> (MemoryId, ImportId) {
        let import = self.imports.arena.next_id();
        let mem = self.memories.add_import(shared, initial, maximum, import);
        self.imports.add(module, name, mem);
        (mem, import)
    }

    /// Add an imported table to this module
    pub fn add_import_table(
        &mut self,
        module: &str,
        name: &str,
        initial: u32,
        max: Option<u32>,
        ty: ValType,
    ) -> (TableId, ImportId) {
        let import = self.imports.arena.next_id();
        let table = self.tables.add_import(initial, max, ty, import);
        self.imports.add(module, name, table);
        (table, import)
    }

    /// Add an imported global to this module
    pub fn add_import_global(
        &mut self,
        module: &str,
        name: &str,
        ty: ValType,
        mutable: bool,
    ) -> (GlobalId, ImportId) {
        let import = self.imports.arena.next_id();
        let global = self.globals.add_import(ty, mutable, import);
        self.imports.add(module, name, global);
        (global, import)
    }
}

impl Emit for ModuleImports {
    fn emit(&self, cx: &mut EmitContext) {
        log::debug!("emit import section");
        let count = self.iter().count();
        if count == 0 {
            return;
        }

        let mut cx = cx.start_section(Section::Import);
        cx.encoder.usize(count);

        for import in self.iter() {
            cx.encoder.str(&import.module);
            cx.encoder.str(&import.name);
            match import.kind {
                ImportKind::Function(id) => {
                    cx.encoder.byte(0x00);
                    cx.indices.push_func(id);
                    let ty = cx.module.funcs.get(id).ty();
                    let idx = cx.indices.get_type_index(ty);
                    cx.encoder.u32(idx);
                }
                ImportKind::Table(id) => {
                    cx.encoder.byte(0x01);
                    cx.indices.push_table(id);
                    cx.module.tables.get(id).emit(&mut cx);
                }
                ImportKind::Memory(id) => {
                    cx.encoder.byte(0x02);
                    cx.indices.push_memory(id);
                    cx.module.memories.get(id).emit(&mut cx);
                }
                ImportKind::Global(id) => {
                    cx.encoder.byte(0x03);
                    cx.indices.push_global(id);
                    cx.module.globals.get(id).emit(&mut cx);
                }
            }
        }
    }
}

impl From<MemoryId> for ImportKind {
    fn from(id: MemoryId) -> ImportKind {
        ImportKind::Memory(id)
    }
}

impl From<FunctionId> for ImportKind {
    fn from(id: FunctionId) -> ImportKind {
        ImportKind::Function(id)
    }
}

impl From<GlobalId> for ImportKind {
    fn from(id: GlobalId) -> ImportKind {
        ImportKind::Global(id)
    }
}

impl From<TableId> for ImportKind {
    fn from(id: TableId) -> ImportKind {
        ImportKind::Table(id)
    }
}
