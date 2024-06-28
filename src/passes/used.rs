use crate::ir::*;
use crate::map::IdHashSet;
use crate::{ActiveDataLocation, Data, DataId, DataKind, Element, ExportItem, Function, InitExpr};
use crate::{ElementId, ElementItems, ElementKind, Module, Type, TypeId};
use crate::{FunctionId, FunctionKind, Global, GlobalId};
use crate::{GlobalKind, Memory, MemoryId, Table, TableId};

/// Set of all root used items in a wasm module.
#[derive(Debug, Default)]
pub struct Roots {
    tables: Vec<TableId>,
    funcs: Vec<FunctionId>,
    globals: Vec<GlobalId>,
    memories: Vec<MemoryId>,
    datas: Vec<DataId>,
    elements: Vec<ElementId>,
    used: Used,
}

impl Roots {
    /// Creates a new set of empty roots.
    pub fn new() -> Roots {
        Roots::default()
    }

    /// Adds a new function to the set of roots
    pub fn push_func(&mut self, func: FunctionId) -> &mut Roots {
        if self.used.funcs.insert(func) {
            log::trace!("function is used: {:?}", func);
            self.funcs.push(func);
        }
        self
    }

    /// Adds a new table to the set of roots
    pub fn push_table(&mut self, table: TableId) -> &mut Roots {
        if self.used.tables.insert(table) {
            log::trace!("table is used: {:?}", table);
            self.tables.push(table);
        }
        self
    }

    /// Adds a new memory to the set of roots
    pub fn push_memory(&mut self, memory: MemoryId) -> &mut Roots {
        if self.used.memories.insert(memory) {
            log::trace!("memory is used: {:?}", memory);
            self.memories.push(memory);
        }
        self
    }

    /// Adds a new global to the set of roots
    pub fn push_global(&mut self, global: GlobalId) -> &mut Roots {
        if self.used.globals.insert(global) {
            log::trace!("global is used: {:?}", global);
            self.globals.push(global);
        }
        self
    }

    fn push_data(&mut self, data: DataId) -> &mut Roots {
        if self.used.data.insert(data) {
            log::trace!("data is used: {:?}", data);
            self.datas.push(data);
        }
        self
    }

    fn push_element(&mut self, element: ElementId) -> &mut Roots {
        if self.used.elements.insert(element) {
            log::trace!("element is used: {:?}", element);
            self.elements.push(element);
        }
        self
    }
}

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
}

impl Used {
    /// Construct a new `Used` set for the given module.
    pub fn new(module: &Module) -> Used {
        log::debug!("starting to calculate used set");
        let mut stack = Roots::default();

        // All exports are roots
        for export in module.exports.iter() {
            match export.item {
                ExportItem::Function(f) => stack.push_func(f),
                ExportItem::Table(t) => stack.push_table(t),
                ExportItem::Memory(m) => stack.push_memory(m),
                ExportItem::Global(g) => stack.push_global(g),
            };
        }

        // The start function is an implicit root as well
        if let Some(f) = module.start {
            stack.push_func(f);
        }

        // Initialization of memories or tables is a side-effectful operation
        // because they can be out-of-bounds, so keep all active segments.
        for data in module.data.iter() {
            if let DataKind::Active { .. } = &data.kind {
                stack.push_data(data.id());
            }
        }
        for elem in module.elements.iter() {
            match elem.kind {
                // Active segments are rooted because they initialize imported
                // or exported tables. Declared segments can probably get gc'd
                // but for now we're conservative and we root them.
                ElementKind::Active { .. } | ElementKind::Declared => {
                    stack.push_element(elem.id());
                }
                ElementKind::Passive => {}
            }
        }

        // And finally ask custom sections for their roots
        for (_id, section) in module.customs.iter() {
            section.add_gc_roots(&mut stack);
        }

        // Iteratively visit all items until our stack is empty
        while stack.funcs.len() > 0
            || stack.tables.len() > 0
            || stack.memories.len() > 0
            || stack.globals.len() > 0
            || stack.datas.len() > 0
            || stack.elements.len() > 0
        {
            while let Some(f) = stack.funcs.pop() {
                let func = module.funcs.get(f);
                stack.used.types.insert(func.ty());

                match &func.kind {
                    FunctionKind::Local(func) => {
                        let mut visitor = UsedVisitor { stack: &mut stack };
                        dfs_in_order(&mut visitor, func, func.entry_block());
                    }
                    FunctionKind::Import(_) => {}
                    FunctionKind::Uninitialized(_) => unreachable!(),
                }
            }

            while let Some(t) = stack.tables.pop() {
                for elem in module.tables.get(t).elem_segments.iter() {
                    stack.push_element(*elem);
                }
            }

            while let Some(t) = stack.globals.pop() {
                match &module.globals.get(t).kind {
                    GlobalKind::Import(_) => {}
                    GlobalKind::Local(InitExpr::Global(global)) => {
                        stack.push_global(*global);
                    }
                    GlobalKind::Local(InitExpr::RefFunc(func)) => {
                        stack.push_func(*func);
                    }
                    GlobalKind::Local(InitExpr::Value(_))
                    | GlobalKind::Local(InitExpr::RefNull(_)) => {}
                }
            }

            while let Some(t) = stack.memories.pop() {
                for data in &module.memories.get(t).data_segments {
                    stack.push_data(*data);
                }
            }

            while let Some(d) = stack.datas.pop() {
                let d = module.data.get(d);
                if let DataKind::Active(a) = &d.kind {
                    stack.push_memory(a.memory);
                    if let ActiveDataLocation::Relative(g) = a.location {
                        stack.push_global(g);
                    }
                }
            }

            while let Some(e) = stack.elements.pop() {
                let e = module.elements.get(e);
                if let ElementItems::Functions(function_ids) = &e.items {
                    function_ids.iter().for_each(|f| {
                        stack.push_func(*f);
                    });
                }
                if let ElementItems::Expressions(crate::ValType::Funcref, items) = &e.items {
                    for item in items {
                        match item {
                            InitExpr::Global(g) => {
                                stack.push_global(*g);
                            }
                            InitExpr::RefFunc(f) => {
                                stack.push_func(*f);
                            }
                            _ => {}
                        }
                    }
                }
                if let ElementKind::Active { offset, table } = &e.kind {
                    if let InitExpr::Global(g) = offset {
                        stack.push_global(*g);
                    }
                    stack.push_table(*table);
                }
            }
        }

        // Wabt seems to have weird behavior where a `data` segment, if present
        // even if passive, requires a `memory` declaration. Our GC pass is
        // pretty aggressive and if you have a passive data segment and only
        // `data.drop` instructions you technically don't need the `memory`.
        // Let's keep `wabt` passing though and just say that if there are data
        // segments kept, but no memories, then we try to add the first memory,
        // if any, to the used set.
        if stack.used.data.len() > 0 && stack.used.memories.len() == 0 {
            if let Some(mem) = module.memories.iter().next() {
                stack.used.memories.insert(mem.id());
            }
        }

        stack.used
    }
}

struct UsedVisitor<'a> {
    stack: &'a mut Roots,
}

impl<'expr> Visitor<'expr> for UsedVisitor<'_> {
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

    fn visit_data_id(&mut self, &d: &DataId) {
        self.stack.push_data(d);
    }

    fn visit_element_id(&mut self, &e: &ElementId) {
        self.stack.push_element(e);
    }
}
