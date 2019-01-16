//! A wasm module's imports.

use crate::arena_set::ArenaSet;
use crate::error::{Error, ErrorKind, Result};
use crate::module::emit::{Emit, IdsToIndices};
use crate::module::types::ModuleTypes;
use crate::passes::Used;
use crate::ty::{TypeId, ValType};
use failure::Fail;
use id_arena::Id;
use parity_wasm::elements;
use std::hash::{Hash, Hasher};
use std::mem;
use std::ops;

/// The id of an import.
pub type ImportId = Id<Import>;

/// A named item imported into the wasm.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Import {
    /// The module name of this import.
    pub module: String,
    /// The name of this import.
    pub name: String,
    /// The kind of item being imported.
    pub kind: ImportKind,
}

impl Import {
    fn entry(&self, indices: &IdsToIndices) -> elements::ImportEntry {
        let external = self.kind.external(indices);
        elements::ImportEntry::new(self.module.clone(), self.name.clone(), external)
    }
}

/// An imported item.
#[derive(Clone, Debug)]
pub enum ImportKind {
    /// An imported function.
    Function {
        /// The type of this imported function.
        ty: TypeId,
    },
    /// An imported table.
    Table {
        /// The size limits for this imported table.
        limits: elements::ResizableLimits,
        /// The type of this imported table's elements.
        ty: elements::TableElementType,
    },
    /// An imported memory.
    Memory {
        /// The size limits for this imported memory.
        limits: elements::ResizableLimits,
        /// Is this memory shared across threads?
        shared: bool,
    },
    /// An imported global.
    Global {
        /// The type of this imported global.
        ty: ValType,
        /// Whether this imported global is mutable or not.
        mutable: bool,
    },
}

impl PartialEq for ImportKind {
    fn eq(&self, rhs: &ImportKind) -> bool {
        use self::ImportKind::*;
        match (self, rhs) {
            (&Function { ty: t1 }, &Function { ty: t2 }) => t1 == t2,
            (
                &Table {
                    limits: ref l1,
                    ty: ref t1,
                },
                &Table {
                    limits: ref l2,
                    ty: ref t2,
                },
            ) => l1 == l2 && t1 == t2,
            (
                &Memory {
                    limits: ref l1,
                    shared: s1,
                },
                &Memory {
                    limits: ref l2,
                    shared: s2,
                },
            ) => l1 == l2 && s1 == s2,
            (
                &Global {
                    ty: t1,
                    mutable: m1,
                },
                &Global {
                    ty: t2,
                    mutable: m2,
                },
            ) => t1 == t2 && m1 == m2,
            _ => false,
        }
    }
}

impl Eq for ImportKind {}

impl Hash for ImportKind {
    fn hash<H: Hasher>(&self, state: &mut H) {
        use self::ImportKind::*;

        fn hash_limits<H: Hasher>(l: &elements::ResizableLimits, h: &mut H) {
            l.initial().hash(h);
            l.maximum().hash(h);
        }

        mem::discriminant(self).hash(state);
        match self {
            Function { ty } => {
                ty.hash(state);
            }
            Table { ref limits, ref ty } => {
                hash_limits(limits, state);
                // Don't hash `ty` for now because (a) parity-wasm doesn't
                // implement Hash for it, and (b) there is only a single table
                // type for wasm at the moment.
                //
                // ty.hash(state);
                let _ = ty;
            }
            Memory { ref limits, shared } => {
                hash_limits(limits, state);
                shared.hash(state);
            }
            Global { ty, mutable } => {
                ty.hash(state);
                mutable.hash(state);
            }
        }
    }
}

impl ImportKind {
    fn external(&self, indices: &IdsToIndices) -> elements::External {
        match self {
            ImportKind::Function { ty } => {
                let idx = indices.get_type_index(*ty);
                elements::External::Function(idx)
            }
            ImportKind::Table { limits, .. } => {
                let table = elements::TableType::new(limits.initial(), limits.maximum());
                elements::External::Table(table)
            }
            ImportKind::Memory { limits, shared } => {
                let mem = elements::MemoryType::new(limits.initial(), limits.maximum(), *shared);
                elements::External::Memory(mem)
            }
            ImportKind::Global { ty, mutable } => {
                let global = elements::GlobalType::new((*ty).into(), *mutable);
                elements::External::Global(global)
            }
        }
    }
}

/// The set of imports in a module.
#[derive(Debug, Default)]
pub struct ModuleImports {
    imports: ArenaSet<Import>,
}

impl ops::Deref for ModuleImports {
    type Target = ArenaSet<Import>;

    #[inline]
    fn deref(&self) -> &ArenaSet<Import> {
        &self.imports
    }
}

impl ops::DerefMut for ModuleImports {
    #[inline]
    fn deref_mut(&mut self) -> &mut ArenaSet<Import> {
        &mut self.imports
    }
}

impl ModuleImports {
    /// Construct the import set for a wasm module.
    pub fn parse(
        types: &ModuleTypes,
        import_section: &elements::ImportSection,
    ) -> Result<ModuleImports> {
        let mut imports = ArenaSet::with_capacity(import_section.entries().len());

        for exp in import_section.entries() {
            let kind = match *exp.external() {
                elements::External::Function(idx) => {
                    let ty = types.type_for_index(idx).ok_or_else(|| -> Error {
                        ErrorKind::InvalidWasm
                            .context("imported function refers to non-existant type")
                            .into()
                    })?;
                    ImportKind::Function { ty }
                }
                elements::External::Table(t) => ImportKind::Table {
                    limits: t.limits().clone(),
                    ty: t.elem_type(),
                },
                elements::External::Memory(m) => ImportKind::Memory {
                    limits: m.limits().clone(),
                    shared: m.limits().shared(),
                },
                elements::External::Global(g) => ImportKind::Global {
                    ty: ValType::from(&g.content_type()),
                    mutable: g.is_mutable(),
                },
            };
            imports.insert(Import {
                module: exp.module().to_string(),
                name: exp.field().to_string(),
                kind,
            });
        }

        Ok(ModuleImports { imports })
    }
}

impl Emit for ModuleImports {
    type Extra = ();

    fn emit(&self, _: &(), used: &Used, module: &mut elements::Module, indices: &mut IdsToIndices) {
        if used.imports.is_empty() {
            return;
        }

        let mut imports = Vec::with_capacity(used.imports.len());

        for (id, imp) in self.imports.iter() {
            if !used.imports.contains(&id) {
                continue;
            }

            indices.set_import_index(id, imports.len() as u32);
            imports.push(imp.entry(indices));
        }

        let imports = elements::ImportSection::with_entries(imports);
        let imports = elements::Section::Import(imports);
        module.sections_mut().push(imports);
    }
}
