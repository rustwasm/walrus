//! A wasm module's imports.

use anyhow::{bail, Context};

use crate::emit::{Emit, EmitContext};
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

    /// Retrieve an imported function by name, including the module in which it resides
    pub fn get_func_by_name(
        &self,
        module: impl AsRef<str>,
        name: impl AsRef<str>,
    ) -> Result<FunctionId> {
        self.iter()
            .find_map(|impt| match impt.kind {
                ImportKind::Function(fid)
                    if impt.module == module.as_ref() && impt.name == name.as_ref() =>
                {
                    Some(fid)
                }
                _ => None,
            })
            .with_context(|| format!("unable to find function export '{}'", name.as_ref()))
    }

    /// Retrieve an imported function by ID
    pub fn get_imported_func(&self, fid: FunctionId) -> Option<&Import> {
        self.arena.iter().find_map(|(_, import)| match import.kind {
            ImportKind::Function(id) if fid == id => Some(import),
            _ => None,
        })
    }

    /// Delete an imported function by name from this module.
    pub fn delete_func_by_name(
        &mut self,
        module: impl AsRef<str>,
        name: impl AsRef<str>,
    ) -> Result<()> {
        let fid = self
            .get_func_by_name(module, name.as_ref())
            .with_context(|| {
                format!("failed to find imported func with name [{}]", name.as_ref())
            })?;
        self.delete(
            self.get_imported_func(fid)
                .with_context(|| format!("failed to find imported func with ID [{fid:?}]"))?
                .id(),
        );
        Ok(())
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
                        t.initial,
                        t.maximum,
                        ty,
                    );
                    ids.push_table(id.0);
                }
                wasmparser::ImportSectionEntryType::Memory(m) => {
                    if m.memory64 {
                        bail!("64-bit memories not supported")
                    };
                    let id = self.add_import_memory(
                        entry.module,
                        entry.field.expect("module linking not supported"),
                        m.shared,
                        m.initial as u32,
                        m.maximum.map(|m| m as u32),
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
                    unimplemented!("component model not implemented");
                }
                wasmparser::ImportSectionEntryType::Tag(_) => {
                    unimplemented!("exception handling not implemented");
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

        let mut wasm_import_section = wasm_encoder::ImportSection::new();

        let count = self.iter().count();
        if count == 0 {
            return;
        }

        for import in self.iter() {
            wasm_import_section.import(
                &import.module,
                &import.name,
                match import.kind {
                    ImportKind::Function(id) => {
                        cx.indices.push_func(id);
                        let ty = cx.module.funcs.get(id).ty();
                        let idx = cx.indices.get_type_index(ty);
                        wasm_encoder::EntityType::Function(idx)
                    }
                    ImportKind::Table(id) => {
                        cx.indices.push_table(id);
                        let table = cx.module.tables.get(id);
                        wasm_encoder::EntityType::Table(wasm_encoder::TableType {
                            element_type: match table.element_ty {
                                ValType::Externref => wasm_encoder::RefType::EXTERNREF,
                                ValType::Funcref => wasm_encoder::RefType::FUNCREF,
                                _ => panic!("Unexpected table type"),
                            },
                            minimum: table.initial,
                            maximum: table.maximum,
                        })
                    }
                    ImportKind::Memory(id) => {
                        cx.indices.push_memory(id);
                        let mem = cx.module.memories.get(id);
                        wasm_encoder::EntityType::Memory(wasm_encoder::MemoryType {
                            minimum: mem.initial as u64,
                            maximum: mem.maximum.map(|v| v as u64),
                            memory64: false,
                            shared: mem.shared,
                        })
                    }
                    ImportKind::Global(id) => {
                        cx.indices.push_global(id);
                        let g = cx.module.globals.get(id);
                        wasm_encoder::EntityType::Global(wasm_encoder::GlobalType {
                            val_type: g.ty.to_wasmencoder_type(),
                            mutable: g.mutable,
                        })
                    }
                },
            );
        }

        cx.wasm_module.section(&wasm_import_section);
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{FunctionBuilder, Module};

    #[test]
    fn get_imported_func() {
        let mut module = Module::default();

        let mut builder = FunctionBuilder::new(&mut module.types, &[], &[]);
        builder.func_body().i32_const(1234).drop();
        let new_fn_id: FunctionId = builder.finish(vec![], &mut module.funcs);
        module.imports.add("mod", "dummy", new_fn_id);

        assert!(module.imports.get_imported_func(new_fn_id).is_some());
    }

    #[test]
    fn get_func_by_name() {
        let mut module = Module::default();

        let mut builder = FunctionBuilder::new(&mut module.types, &[], &[]);
        builder.func_body().i32_const(1234).drop();
        let new_fn_id: FunctionId = builder.finish(vec![], &mut module.funcs);
        module.imports.add("mod", "dummy", new_fn_id);

        assert!(module
            .imports
            .get_func_by_name("mod", "dummy")
            .is_ok_and(|fid| fid == new_fn_id));
    }
}
