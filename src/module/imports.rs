//! A wasm module's imports.

use crate::arena_set::ArenaSet;
use crate::emit::{Emit, EmitContext};
use crate::error::Result;
use crate::module::functions::FunctionId;
use crate::module::globals::GlobalId;
use crate::module::memories::MemoryId;
use crate::module::tables::{FunctionTable, TableId, TableKind};
use crate::module::Module;
use crate::parse::IndicesToIds;
use crate::ty::ValType;
use id_arena::Id;
use parity_wasm::elements;

/// The id of an import.
pub type ImportId = Id<Import>;

/// A named item imported into the wasm.
#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Import {
    /// The module name of this import.
    pub module: String,
    /// The name of this import.
    pub name: String,
    /// The kind of item being imported.
    pub kind: ImportKind,
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
    arena: ArenaSet<Import>,
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

    /// Get a shared reference to this module's imports.
    pub fn iter(&self) -> impl Iterator<Item = &Import> {
        self.arena.iter().map(|(_, f)| f)
    }
}

impl Module {
    /// Construct the import set for a wasm module.
    pub(crate) fn parse_imports(
        &mut self,
        section: &elements::ImportSection,
        ids: &mut IndicesToIds,
    ) -> Result<()> {
        for exp in section.entries() {
            let import = self.imports.arena.next_id();
            let kind = match *exp.external() {
                elements::External::Function(idx) => {
                    let ty = ids.get_type(idx)?;
                    let id = self.funcs.add_import(ty, import);
                    ids.push_func(id);
                    ImportKind::Function(id)
                }
                elements::External::Table(t) => {
                    let kind = match t.elem_type() {
                        elements::TableElementType::AnyFunc => {
                            TableKind::Function(FunctionTable::default())
                        }
                    };
                    let id = self.tables.add_import(
                        t.limits().initial(),
                        t.limits().maximum(),
                        kind,
                        import,
                    );
                    ids.push_table(id);
                    ImportKind::Table(id)
                }
                elements::External::Memory(m) => {
                    let id = self.memories.add_import(
                        m.limits().shared(),
                        m.limits().initial(),
                        m.limits().maximum(),
                        import,
                    );
                    ids.push_memory(id);
                    ImportKind::Memory(id)
                }
                elements::External::Global(g) => {
                    let id = self.globals.add_import(
                        ValType::from(&g.content_type()),
                        g.is_mutable(),
                        import,
                    );
                    ids.push_global(id);
                    ImportKind::Global(id)
                }
            };
            self.imports.arena.insert(Import {
                module: exp.module().to_string(),
                name: exp.field().to_string(),
                kind,
            });
        }

        Ok(())
    }
}

impl Emit for ModuleImports {
    fn emit(&self, cx: &mut EmitContext) {
        let mut imports = Vec::new();

        for (_id, import) in self.arena.iter() {
            let used = match import.kind {
                ImportKind::Function(id) => cx.used.funcs.contains(&id),
                ImportKind::Global(id) => cx.used.globals.contains(&id),
                ImportKind::Memory(id) => cx.used.memories.contains(&id),
                ImportKind::Table(id) => cx.used.tables.contains(&id),
            };
            if !used {
                continue;
            }

            let external = match import.kind {
                ImportKind::Function(id) => {
                    cx.indices.push_func(id);
                    let ty = cx.module.funcs.get(id).ty();
                    elements::External::Function(cx.indices.get_type_index(ty))
                }
                ImportKind::Global(id) => {
                    cx.indices.push_global(id);
                    let global = cx.module.globals.get(id);
                    let global = elements::GlobalType::new(global.ty.into(), global.mutable);
                    elements::External::Global(global)
                }
                ImportKind::Memory(id) => {
                    cx.indices.push_memory(id);
                    let memory = cx.module.memories.get(id);
                    let memory =
                        elements::MemoryType::new(memory.initial, memory.maximum, memory.shared);
                    elements::External::Memory(memory)
                }
                ImportKind::Table(id) => {
                    cx.indices.push_table(id);
                    let table = cx.module.tables.get(id);
                    let table = elements::TableType::new(table.initial, table.maximum);
                    elements::External::Table(table)
                }
            };
            let entry =
                elements::ImportEntry::new(import.module.clone(), import.name.clone(), external);
            imports.push(entry);
        }

        if !imports.is_empty() {
            let imports = elements::ImportSection::with_entries(imports);
            let imports = elements::Section::Import(imports);
            cx.dst.sections_mut().push(imports);
        }
    }
}
