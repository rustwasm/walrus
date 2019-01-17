//! Tables within a wasm module.

use crate::module::emit::{Emit, IdsToIndices};
use crate::module::functions::FunctionId;
use crate::module::globals::GlobalId;
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
    /// The initial size of this table
    pub initial_size: u32,
    /// The maximum size of this table
    pub maximum_size: Option<u32>,
    /// Which kind of table this is
    pub kind: TableKind,
}

/// The kinds of tables that can exist
#[derive(Debug)]
pub enum TableKind {
    /// A table of `anyfunc` functions.
    ///
    /// Contains the initialization list for this table, if any.
    Function(FunctionTable),
}

/// Components of a table of functions (`anyfunc` table)
#[derive(Debug, Default)]
pub struct FunctionTable {
    /// Layout of this function table that we know of, or those elements which
    /// have constant initializers.
    pub elements: Vec<Option<FunctionId>>,

    /// Elements of this table which are relative to a global, typically
    /// imported.
    pub relative_elements: Vec<(GlobalId, Vec<FunctionId>)>,
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
        let id2 = self.arena.alloc(Table {
            id,
            initial_size: limits.initial(),
            maximum_size: limits.maximum(),
            kind: match ty {
                elements::TableElementType::AnyFunc => TableKind::Function(Default::default()),
            },
        });
        debug_assert_eq!(id, id2);
        id
    }

    /// Iterates over all tables in this section.
    pub fn iter(&self) -> impl Iterator<Item = &Table> {
        self.arena.iter().map(|p| p.1)
    }

    /// Returns the actual table associated with an ID
    pub fn get(&self, table: TableId) -> &Table {
        &self.arena[table]
    }

    /// Returns the actual table associated with an ID
    pub fn get_mut(&mut self, table: TableId) -> &mut Table {
        &mut self.arena[table]
    }
}

impl Emit for ModuleTables {
    type Extra = ();

    fn emit(&self, _: &(), used: &Used, module: &mut elements::Module, indices: &mut IdsToIndices) {
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
            let table = elements::TableType::new(table.initial_size, table.maximum_size);
            tables.push(table);
        }

        let tables = elements::TableSection::with_entries(tables);
        let tables = elements::Section::Table(tables);
        module.sections_mut().push(tables);
    }
}
