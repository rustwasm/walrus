use crate::ir::*;
use crate::module::exports::{ExportId, ExportItem};
use crate::module::functions::{FunctionId, FunctionKind, LocalFunction};
use crate::module::globals::GlobalId;
use crate::module::imports::ImportId;
use crate::module::memories::MemoryId;
use crate::module::tables::TableId;
use crate::module::Module;
use crate::ty::TypeId;
use std::collections::HashSet;

/// Finds the things within a module that are used.
///
/// This is useful for implementing something like a linker's `--gc-sections` so
/// that our emitted `.wasm` binaries are small and don't contain things that
/// are not used.
#[derive(Debug)]
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
}

impl Used {
    /// Construct a new `Used` set for the given module.
    pub fn new<R>(module: &Module, roots: R) -> Used
    where
        R: IntoIterator<Item = ExportId>,
    {
        let mut stack = vec![];

        let mut used = Used {
            imports: HashSet::new(),
            tables: HashSet::new(),
            types: HashSet::new(),
            funcs: HashSet::new(),
            globals: HashSet::new(),
            memories: HashSet::new(),
        };

        for r in roots {
            match module.exports.arena[r].item {
                ExportItem::Function(f) => {
                    if used.funcs.insert(f) {
                        stack.push(f);
                    }
                }
                ExportItem::Table(t) => {
                    used.tables.insert(t);
                    // TODO: add all functions in an anyfunc table
                }
                ExportItem::Memory(m) => {
                    used.memories.insert(m);
                }
                ExportItem::Global(g) => {
                    used.globals.insert(g);
                }
            }
        }

        while let Some(f) = stack.pop() {
            let func = &module.funcs.arena[f];
            used.types.insert(func.ty(&module));

            match &func.kind {
                FunctionKind::Local(func) => {
                    let v = &mut UsedVisitor {
                        func,
                        used: &mut used,
                        stack: &mut stack,
                    };

                    func.entry_block().visit(v);
                }
                FunctionKind::Import(i) => {
                    used.imports.insert(i.import);
                }
                FunctionKind::Uninitialized(_) => unreachable!(),
            }
        }

        used
    }
}

struct UsedVisitor<'a> {
    func: &'a LocalFunction,
    used: &'a mut Used,
    stack: &'a mut Vec<FunctionId>,
}

impl UsedVisitor<'_> {
    fn mark(&mut self, f: FunctionId) {
        if self.used.funcs.insert(f) {
            self.stack.push(f);
        }
    }
}

impl<'expr> Visitor<'expr> for UsedVisitor<'expr> {
    fn local_function(&self) -> &'expr LocalFunction {
        self.func
    }

    fn visit_function_id(&mut self, &func: &FunctionId) {
        self.mark(func);
    }

    fn visit_memory_id(&mut self, &m: &MemoryId) {
        self.used.memories.insert(m);
    }

    fn visit_global_id(&mut self, &g: &GlobalId) {
        self.used.globals.insert(g);
    }

    fn visit_table_id(&mut self, &t: &TableId) {
        self.used.tables.insert(t);
        // TODO: need to recurse into table entries
    }
}
