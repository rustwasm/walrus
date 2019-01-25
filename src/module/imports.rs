//! A wasm module's imports.

use crate::arena_set::ArenaSet;
use crate::emit::{Emit, EmitContext, Section};
use crate::error::Result;
use crate::module::functions::FunctionId;
use crate::module::globals::GlobalId;
use crate::module::memories::MemoryId;
use crate::module::tables::{FunctionTable, TableId, TableKind};
use crate::module::Module;
use crate::parse::IndicesToIds;
use crate::ty::ValType;
use id_arena::Id;

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
        section: wasmparser::ImportSectionReader,
        ids: &mut IndicesToIds,
    ) -> Result<()> {
        log::debug!("parse import section");
        for entry in section {
            let entry = entry?;
            let import = self.imports.arena.next_id();
            let kind = match entry.ty {
                wasmparser::ImportSectionEntryType::Function(idx) => {
                    let ty = ids.get_type(idx)?;
                    let id = self.funcs.add_import(ty, import);
                    ids.push_func(id);
                    ImportKind::Function(id)
                }
                wasmparser::ImportSectionEntryType::Table(t) => {
                    let kind = match t.element_type {
                        wasmparser::Type::AnyFunc => TableKind::Function(FunctionTable::default()),
                        _ => failure::bail!("invalid table type"),
                    };
                    let id =
                        self.tables
                            .add_import(t.limits.initial, t.limits.maximum, kind, import);
                    ids.push_table(id);
                    ImportKind::Table(id)
                }
                wasmparser::ImportSectionEntryType::Memory(m) => {
                    let id = self.memories.add_import(
                        m.shared,
                        m.limits.initial,
                        m.limits.maximum,
                        import,
                    );
                    ids.push_memory(id);
                    ImportKind::Memory(id)
                }
                wasmparser::ImportSectionEntryType::Global(g) => {
                    let id = self.globals.add_import(
                        ValType::parse(&g.content_type)?,
                        g.mutable,
                        import,
                    );
                    ids.push_global(id);
                    ImportKind::Global(id)
                }
            };
            self.imports.arena.insert(Import {
                module: entry.module.to_string(),
                name: entry.field.to_string(),
                kind,
            });
        }

        Ok(())
    }
}

impl Emit for ModuleImports {
    fn emit(&self, cx: &mut EmitContext) {
        log::debug!("emit import section");
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
            imports.push(import);
        }
        if imports.len() == 0 {
            return;
        }

        let mut cx = cx.start_section(Section::Import);
        cx.encoder.usize(imports.len());

        for import in imports {
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
