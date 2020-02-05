//! Tables within a wasm module.

use crate::emit::{Emit, EmitContext, Section};
use crate::map::IdHashSet;
use crate::parse::IndicesToIds;
use crate::tombstone_arena::{Id, Tombstone, TombstoneArena};
use crate::{Element, ImportId, Module, Result, ValType};
use anyhow::bail;

/// The id of a table.
pub type TableId = Id<Table>;

/// A table in the wasm.
#[derive(Debug)]
pub struct Table {
    id: TableId,
    /// The initial size of this table
    pub initial: u32,
    /// The maximum size of this table
    pub maximum: Option<u32>,
    /// The type of the elements in this table
    pub element_ty: ValType,
    /// Whether or not this table is imported, and if so what imports it.
    pub import: Option<ImportId>,
    /// Active data segments that will be used to initialize this memory.
    pub elem_segments: IdHashSet<Element>,
}

impl Tombstone for Table {}

impl Table {
    /// Get this table's id.
    pub fn id(&self) -> TableId {
        self.id
    }
}

impl Emit for Table {
    fn emit(&self, cx: &mut EmitContext) {
        self.element_ty.emit(&mut cx.encoder);
        cx.encoder.byte(self.maximum.is_some() as u8);
        cx.encoder.u32(self.initial);
        if let Some(m) = self.maximum {
            cx.encoder.u32(m);
        }
    }
}

/// The set of tables in this module.
#[derive(Debug, Default)]
pub struct ModuleTables {
    /// The arena containing this module's tables.
    arena: TombstoneArena<Table>,
}

impl ModuleTables {
    /// Adds a new imported table to this list of tables
    pub fn add_import(
        &mut self,
        initial: u32,
        max: Option<u32>,
        element_ty: ValType,
        import: ImportId,
    ) -> TableId {
        let id = self.arena.next_id();
        self.arena.alloc(Table {
            id,
            initial,
            maximum: max,
            element_ty,
            import: Some(import),
            elem_segments: Default::default(),
        })
    }

    /// Construct a new table, that does not originate from any of the input
    /// wasm tables.
    pub fn add_local(&mut self, initial: u32, max: Option<u32>, element_ty: ValType) -> TableId {
        let id = self.arena.next_id();
        let id2 = self.arena.alloc(Table {
            id,
            initial,
            maximum: max,
            element_ty,
            import: None,
            elem_segments: Default::default(),
        });
        debug_assert_eq!(id, id2);
        id
    }

    /// Returns the actual table associated with an ID
    pub fn get(&self, table: TableId) -> &Table {
        &self.arena[table]
    }

    /// Returns the actual table associated with an ID
    pub fn get_mut(&mut self, table: TableId) -> &mut Table {
        &mut self.arena[table]
    }

    /// Removes a table from this module.
    ///
    /// It is up to you to ensure that any potential references to the deleted
    /// table are also removed, eg `call_indirect` expressions and exports, etc.
    pub fn delete(&mut self, id: TableId) {
        self.arena.delete(id);
    }

    /// Iterates over all tables in this section.
    pub fn iter(&self) -> impl Iterator<Item = &Table> {
        self.arena.iter().map(|p| p.1)
    }

    /// Finds a unique function table in a module.
    ///
    /// Modules produced by compilers like LLVM typically have one function
    /// table for indirect function calls. This function will look for a single
    /// function table inside this module, and return that if found. If no
    /// function tables are present `None` will be returned
    ///
    /// # Errors
    ///
    /// Returns an error if there are two function tables in this module
    pub fn main_function_table(&self) -> Result<Option<TableId>> {
        let mut tables = self.iter().filter(|t| t.element_ty == ValType::Funcref);
        let id = match tables.next() {
            Some(t) => t.id(),
            None => return Ok(None),
        };
        if tables.next().is_some() {
            bail!("module contains more than one function table");
        }
        Ok(Some(id))
    }

    /// Iterates over all tables in this section.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Table> {
        self.arena.iter_mut().map(|p| p.1)
    }
}

impl Module {
    /// Construct a new, empty set of tables for a module.
    pub(crate) fn parse_tables(
        &mut self,
        section: wasmparser::TableSectionReader,
        ids: &mut IndicesToIds,
    ) -> Result<()> {
        log::debug!("parse table section");
        for t in section {
            let t = t?;
            let id = self.tables.add_local(
                t.limits.initial,
                t.limits.maximum,
                ValType::parse(&t.element_type)?,
            );
            ids.push_table(id);
        }
        Ok(())
    }
}

impl Emit for ModuleTables {
    fn emit(&self, cx: &mut EmitContext) {
        log::debug!("emit table section");
        // Skip imported tables because those are emitted in the import section.
        let tables = self.iter().filter(|t| t.import.is_none()).count();
        if tables == 0 {
            return;
        }

        let mut cx = cx.start_section(Section::Table);
        cx.encoder.usize(tables);
        for table in self.iter().filter(|t| t.import.is_none()) {
            cx.indices.push_table(table.id());
            table.emit(&mut cx);
        }
    }
}
