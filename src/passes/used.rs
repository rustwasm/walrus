use crate::const_value::Const;
use crate::ir::*;
use crate::map::{IdHashMap, IdHashSet};
use crate::module::data::{Data, DataId};
use crate::module::elements::Element;
use crate::module::exports::{ExportId, ExportItem};
use crate::module::functions::{Function, FunctionId, FunctionKind, LocalFunction};
use crate::module::globals::{Global, GlobalId, GlobalKind};
use crate::module::imports::ImportKind;
use crate::module::memories::{Memory, MemoryId};
use crate::module::tables::{Table, TableId, TableKind};
use crate::module::Module;
use crate::ty::{Type, TypeId};

/// Finds the things within a module that are used.
///
/// This is useful for implementing something like a linker's `--gc-sections` so
/// that our emitted `.wasm` binaries are small and don't contain things that
/// are not used.
#[derive(Debug, Default)]
pub struct Used {
    /// The module's used tables.
    pub tables: IdHashSet<Table>,
    /// The module's used types.
    pub types: IdHashSet<Type>,
    /// The module's used functions.
    pub funcs: IdHashSet<Function>,
    /// The module's used globals.
    pub globals: IdHashSet<Global>,
    /// The module's used memories.
    pub memories: IdHashSet<Memory>,
    /// The module's used passive element segments.
    pub elements: IdHashSet<Element>,
    /// The module's used passive data segments.
    pub data: IdHashSet<Data>,
    /// Locals used within functions
    pub locals: IdHashMap<Function, IdHashSet<Local>>,
}

impl Used {
    /// Construct a new `Used` set for the given module.
    pub fn new<R>(module: &Module, roots: R) -> Used
    where
        R: IntoIterator<Item = ExportId>,
    {
        log::debug!("starting to calculate used set");
        let mut used = Used::default();
        let mut stack = UsedStack {
            used: &mut used,
            functions: Vec::new(),
            tables: Vec::new(),
            globals: Vec::new(),
            memories: Vec::new(),
        };

        for r in roots {
            match module.exports.get(r).item {
                ExportItem::Function(f) => stack.push_func(f),
                ExportItem::Table(t) => stack.push_table(t),
                ExportItem::Memory(m) => stack.push_memory(m),
                ExportItem::Global(g) => stack.push_global(g),
            }
        }
        if let Some(f) = module.start {
            stack.push_func(f);
        }

        // Initialization of imported memories or imported tables is a
        // side-effectful operation, so be sure to retain any tables/memories
        // that are imported and initialized, even if they aren't used.
        for import in module.imports.iter() {
            match import.kind {
                ImportKind::Memory(m) => {
                    let mem = module.memories.get(m);
                    if !mem.data.is_empty() {
                        stack.push_memory(m);
                    }
                }
                ImportKind::Table(t) => {
                    let table = module.tables.get(t);
                    match &table.kind {
                        TableKind::Function(init) => {
                            if !init.elements.is_empty() || !init.relative_elements.is_empty() {
                                stack.push_table(t);
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        // Iteratively visit all items until our stack is empty
        while stack.functions.len() > 0
            || stack.tables.len() > 0
            || stack.memories.len() > 0
            || stack.globals.len() > 0
        {
            while let Some(f) = stack.functions.pop() {
                let func = module.funcs.get(f);
                stack.used.types.insert(func.ty());

                match &func.kind {
                    FunctionKind::Local(func) => {
                        func.entry_block().visit(&mut UsedVisitor {
                            func,
                            stack: &mut stack,
                            id: f,
                        });
                    }
                    FunctionKind::Import(_) => {}
                    FunctionKind::Uninitialized(_) => unreachable!(),
                }
            }

            while let Some(t) = stack.tables.pop() {
                match &module.tables.get(t).kind {
                    TableKind::Function(list) => {
                        for id in list.elements.iter() {
                            if let Some(id) = id {
                                stack.push_func(*id);
                            }
                        }
                        for (global, list) in list.relative_elements.iter() {
                            stack.push_global(*global);
                            for id in list {
                                stack.push_func(*id);
                            }
                        }
                    }
                }
            }

            while let Some(t) = stack.globals.pop() {
                match &module.globals.get(t).kind {
                    GlobalKind::Import(_) => {}
                    GlobalKind::Local(Const::Global(global)) => {
                        stack.push_global(*global);
                    }
                    GlobalKind::Local(Const::Value(_)) => {}
                }
            }

            while let Some(t) = stack.memories.pop() {
                for global in module.memories.get(t).data.globals() {
                    stack.push_global(global);
                }
            }
        }

        used
    }
}

struct UsedStack<'a> {
    used: &'a mut Used,
    functions: Vec<FunctionId>,
    tables: Vec<TableId>,
    memories: Vec<MemoryId>,
    globals: Vec<GlobalId>,
}

impl UsedStack<'_> {
    fn push_func(&mut self, f: FunctionId) {
        if self.used.funcs.insert(f) {
            self.functions.push(f);
        }
    }

    fn push_table(&mut self, f: TableId) {
        if self.used.tables.insert(f) {
            self.tables.push(f);
        }
    }

    fn push_global(&mut self, f: GlobalId) {
        if self.used.globals.insert(f) {
            self.globals.push(f);
        }
    }

    fn push_memory(&mut self, f: MemoryId) {
        if self.used.memories.insert(f) {
            self.memories.push(f);
        }
    }
}

struct UsedVisitor<'a, 'b> {
    func: &'a LocalFunction,
    stack: &'a mut UsedStack<'b>,
    id: FunctionId,
}

impl<'expr> Visitor<'expr> for UsedVisitor<'expr, '_> {
    fn local_function(&self) -> &'expr LocalFunction {
        self.func
    }

    fn visit_function_id(&mut self, &func: &FunctionId) {
        self.stack.push_func(func);
    }

    fn visit_memory_id(&mut self, &m: &MemoryId) {
        self.stack.push_memory(m);
    }

    fn visit_global_id(&mut self, &g: &GlobalId) {
        self.stack.push_global(g);
    }

    fn visit_table_id(&mut self, &t: &TableId) {
        self.stack.push_table(t);
    }

    fn visit_type_id(&mut self, &t: &TypeId) {
        self.stack.used.types.insert(t);
    }

    fn visit_data_id(&mut self, &t: &DataId) {
        self.stack.used.data.insert(t);
    }

    fn visit_local_id(&mut self, &l: &LocalId) {
        self.stack
            .used
            .locals
            .entry(self.id)
            .or_insert_with(IdHashSet::default)
            .insert(l);
    }
}
