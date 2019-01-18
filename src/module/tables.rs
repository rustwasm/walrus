//! Tables within a wasm module.

use crate::emit::{Emit, EmitContext};
use crate::module::functions::FunctionId;
use crate::module::globals::GlobalId;
use crate::module::imports::ImportId;
use crate::module::Module;
use crate::parse::IndicesToIds;
use id_arena::{Arena, Id};
use parity_wasm::elements;

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
    /// Which kind of table this is
    pub kind: TableKind,
    /// Whether or not this table is imported, and if so what imports it.
    pub import: Option<ImportId>,
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
    arena: Arena<Table>,
}

impl ModuleTables {
    /// Adds a new imported table to this list of tables
    pub fn add_import(
        &mut self,
        initial: u32,
        max: Option<u32>,
        kind: TableKind,
        import: ImportId,
    ) -> TableId {
        let id = self.arena.next_id();
        self.arena.alloc(Table {
            id,
            initial: initial,
            maximum: max,
            kind,
            import: Some(import),
        })
    }

    /// Construct a new table, that does not originate from any of the input
    /// wasm tables.
    pub fn add_local(&mut self, initial: u32, max: Option<u32>, kind: TableKind) -> TableId {
        let id = self.arena.next_id();
        let id2 = self.arena.alloc(Table {
            id,
            initial: initial,
            maximum: max,
            kind,
            import: None,
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

    /// Iterates over all tables in this section.
    pub fn iter(&self) -> impl Iterator<Item = &Table> {
        self.arena.iter().map(|p| p.1)
    }
}

impl Module {
    /// Construct a new, empty set of tables for a module.
    pub(crate) fn parse_tables(
        &mut self,
        section: &elements::TableSection,
        ids: &mut IndicesToIds,
    ) {
        for t in section.entries() {
            let id = self.tables.add_local(
                t.limits().initial(),
                t.limits().maximum(),
                match t.elem_type() {
                    elements::TableElementType::AnyFunc => {
                        TableKind::Function(FunctionTable::default())
                    }
                },
            );
            ids.push_table(id);
        }
    }
}

impl Emit for ModuleTables {
    fn emit(&self, cx: &mut EmitContext) {
        let mut tables = Vec::with_capacity(cx.used.tables.len());

        for (id, table) in &self.arena {
            if !cx.used.tables.contains(&id) {
                continue;
            }
            if table.import.is_some() {
                continue; // already emitted in the imports section
            }

            cx.indices.push_table(id);
            let table = elements::TableType::new(table.initial, table.maximum);
            tables.push(table);
        }

        if !tables.is_empty() {
            let tables = elements::TableSection::with_entries(tables);
            let tables = elements::Section::Table(tables);
            cx.dst.sections_mut().push(tables);
        }
    }
}
