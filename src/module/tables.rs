//! Tables within a wasm module.

use crate::emit::{Emit, EmitContext, Section};
use crate::parse::IndicesToIds;
use crate::tombstone_arena::{Id, Tombstone, TombstoneArena};
use crate::{FunctionId, GlobalId, ImportId, Module, Result, ValType};

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

impl Tombstone for Table {}

/// The kinds of tables that can exist
#[derive(Debug)]
pub enum TableKind {
    /// A table of `anyfunc` functions.
    ///
    /// Contains the initialization list for this table, if any.
    Function(FunctionTable),

    /// A table of type `anyref` values
    Anyref(AnyrefTable),
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

/// Components of a table of `anyref`
#[derive(Debug, Default)]
pub struct AnyrefTable {
    // currently intentionally empty
}

impl Table {
    /// Get this table's id.
    pub fn id(&self) -> TableId {
        self.id
    }
}

impl Emit for Table {
    fn emit(&self, cx: &mut EmitContext) {
        match self.kind {
            TableKind::Function(_) => {
                cx.encoder.byte(0x70); // the `anyfunc` type
            }
            TableKind::Anyref(_) => ValType::Anyref.emit(&mut cx.encoder),
        }
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
        let mut tables = self.iter().filter_map(|t| match t.kind {
            TableKind::Function(_) => Some(t.id()),
            _ => None,
        });
        let id = match tables.next() {
            Some(id) => id,
            None => return Ok(None),
        };
        if tables.next().is_some() {
            failure::bail!("module contains more than one function table");
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
                match t.element_type {
                    wasmparser::Type::AnyFunc => TableKind::Function(FunctionTable::default()),
                    wasmparser::Type::AnyRef => TableKind::Anyref(AnyrefTable::default()),
                    _ => failure::bail!("invalid table type"),
                },
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
