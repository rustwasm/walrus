//! Tables within a wasm module.

use crate::module::emit::{Emit, IdsToIndices};
use crate::passes::Used;
use id_arena::{Arena, Id};
use parity_wasm::elements;
use std::collections::HashMap;

/// The id of a table.
pub type TableId = Id<Table>;

/// A table in the wasm.
#[derive(Debug)]
pub struct Table {
    id: TableId,
    /// The type of each element within this table.
    pub ty: elements::TableElementType,
    /// The size limits for this table.
    pub limits: elements::ResizableLimits,
}

impl Table {
    /// Get this table's id.
    pub fn id(&self) -> TableId {
        self.id
    }
}

/// The set of tables in this module.
#[derive(Debug, Default)]
pub struct ModuleTables {
    /// The arena containing this module's tables.
    pub arena: Arena<Table>,
    index_to_table_id: HashMap<u32, TableId>,
}

impl ModuleTables {
    /// Construct a new, empty set of tables for a module.
    pub fn parse(table_section: &elements::TableSection) -> ModuleTables {
        let capacity = table_section.entries().len();
        let mut tables = ModuleTables {
            arena: Arena::with_capacity(capacity),
            index_to_table_id: HashMap::with_capacity(capacity),
        };

        for (i, t) in table_section.entries().iter().enumerate() {
            tables.add_table_for_index(i as u32, t.elem_type(), t.limits().clone());
        }

        tables
    }

    /// Get the table for the given index in the input wasm, if any exists.
    pub fn table_for_index(&self, index: u32) -> Option<TableId> {
        self.index_to_table_id.get(&index).cloned()
    }

    /// Get or create the table for this function at the given index.
    fn add_table_for_index(
        &mut self,
        index: u32,
        ty: elements::TableElementType,
        limits: elements::ResizableLimits,
    ) -> TableId {
        assert!(!self.index_to_table_id.contains_key(&index));
        let id = self.new_table(ty, limits);
        self.index_to_table_id.insert(index, id);
        id
    }

    /// Construct a new table, that does not originate from any of the input
    /// wasm tables.
    pub fn new_table(
        &mut self,
        ty: elements::TableElementType,
        limits: elements::ResizableLimits,
    ) -> TableId {
        let id = self.arena.next_id();
        let id2 = self.arena.alloc(Table { id, ty, limits });
        debug_assert_eq!(id, id2);
        id
    }
}

impl Emit for ModuleTables {
    fn emit(&self, used: &Used, module: &mut elements::Module, indices: &mut IdsToIndices) {
        if used.tables.is_empty() {
            return;
        }

        let mut tables = Vec::with_capacity(used.tables.len());

        let import_count = module.import_count(elements::ImportCountType::Table) as u32;

        for (id, table) in &self.arena {
            if !used.tables.contains(&id) {
                continue;
            }

            indices.set_table_index(id, tables.len() as u32 + import_count);
            let table = elements::TableType::new(table.limits.initial(), table.limits.maximum());
            tables.push(table);
        }

        let tables = elements::TableSection::with_entries(tables);
        let tables = elements::Section::Table(tables);
        module.sections_mut().push(tables);
    }
}
