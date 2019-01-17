use crate::ir::*;
use crate::module::elements::ElementId;
use crate::module::exports::{ExportId, ExportItem};
use crate::module::functions::{FunctionId, FunctionKind, LocalFunction};
use crate::module::globals::GlobalId;
use crate::module::imports::ImportId;
use crate::module::memories::MemoryId;
use crate::module::tables::{TableId, TableKind};
use crate::module::Module;
use crate::ty::TypeId;
use std::collections::HashSet;

/// Finds the things within a module that are used.
///
/// This is useful for implementing something like a linker's `--gc-sections` so
/// that our emitted `.wasm` binaries are small and don't contain things that
/// are not used.
#[derive(Debug, Default)]
pub struct Used {
    /// The module's used imports.
    pub imports: HashSet<ImportId>,
    /// The module's used tables.
    pub tables: HashSet<TableId>,
    /// The module's used types.
    pub types: HashSet<TypeId>,
    /// The module's used functions.
    pub funcs: HashSet<FunctionId>,
    /// The module's used globals.
    pub globals: HashSet<GlobalId>,
    /// The module's used memories.
    pub memories: HashSet<MemoryId>,
    /// The module's used passive segments.
    pub elements: HashSet<ElementId>,
}

impl Used {
    /// Construct a new `Used` set for the given module.
    pub fn new<R>(module: &Module, roots: R) -> Used
    where
        R: IntoIterator<Item = ExportId>,
    {
        let mut used = Used::default();
        let mut stack = UsedStack {
            used: &mut used,
            functions: Vec::new(),
            tables: Vec::new(),
        };

        for r in roots {
            match module.exports.arena[r].item {
                ExportItem::Function(f) => stack.push_func(f),
                ExportItem::Table(t) => stack.push_table(t),
                ExportItem::Memory(m) => {
                    stack.used.memories.insert(m);
                }
                ExportItem::Global(g) => {
                    stack.used.globals.insert(g);
                }
            }
        }

        while stack.functions.len() > 0 || stack.tables.len() > 0 {
            while let Some(f) = stack.functions.pop() {
                let func = &module.funcs.arena[f];
                stack.used.types.insert(func.ty(&module));

                match &func.kind {
                    FunctionKind::Local(func) => {
                        func.entry_block().visit(&mut UsedVisitor {
                            func,
                            stack: &mut stack,
                        });
                    }
                    FunctionKind::Import(i) => {
                        stack.used.imports.insert(i.import);
                    }
                    FunctionKind::Uninitialized(_) => unreachable!(),
                }
            }

            while let Some(t) = stack.tables.pop() {
                let table = &module.tables.arena[t];
                match &table.kind {
                    TableKind::Function(list) => {
                        for id in list.elements.iter() {
                            if let Some(id) = id {
                                stack.push_func(*id);
                            }
                        }
                        for (global, list) in list.relative_elements.iter() {
                            stack.used.globals.insert(*global);
                            for id in list {
                                stack.push_func(*id);
                            }
                        }
                    }
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
}

struct UsedVisitor<'a, 'b> {
    func: &'a LocalFunction,
    stack: &'a mut UsedStack<'b>,
}

impl<'expr> Visitor<'expr> for UsedVisitor<'expr, '_> {
    fn local_function(&self) -> &'expr LocalFunction {
        self.func
    }

    fn visit_function_id(&mut self, &func: &FunctionId) {
        self.stack.push_func(func);
    }

    fn visit_memory_id(&mut self, &m: &MemoryId) {
        self.stack.used.memories.insert(m);
    }

    fn visit_global_id(&mut self, &g: &GlobalId) {
        self.stack.used.globals.insert(g);
    }

    fn visit_table_id(&mut self, &t: &TableId) {
        self.stack.push_table(t);
    }

    fn visit_type_id(&mut self, &t: &TypeId) {
        self.stack.used.types.insert(t);
    }
}
