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

            match func.kind {
                FunctionKind::Local(ref func) => {
                    let v = &mut UsedVisitor {
                        func,
                        used: &mut used,
                        stack: &mut stack,
                    };

                    v.visit(func.entry_block());
                }
                FunctionKind::Import(ref i) => {
                    used.imports.insert(i.import);
                }
                FunctionKind::Uninitialized => unreachable!(),
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

    fn visit<E>(&mut self, e: E)
    where
        E: Into<ExprId>,
    {
        self.func.exprs[e.into()].visit(self)
    }
}

impl Visitor for UsedVisitor<'_> {
    type Return = ();

    fn visit_block(&mut self, e: &Block) {
        e.exprs.iter().for_each(|e| self.visit(*e));
    }

    fn visit_call(&mut self, e: &Call) {
        self.mark(e.func);
        e.args.iter().for_each(|a| self.visit(*a));
    }

    fn visit_i32_add(&mut self, e: &I32Add) {
        self.visit(e.lhs);
        self.visit(e.rhs);
    }

    fn visit_i32_sub(&mut self, e: &I32Sub) {
        self.visit(e.lhs);
        self.visit(e.rhs);
    }

    fn visit_i32_mul(&mut self, e: &I32Mul) {
        self.visit(e.lhs);
        self.visit(e.rhs);
    }

    fn visit_i32_eqz(&mut self, e: &I32Eqz) {
        self.visit(e.expr);
    }

    fn visit_i32_popcnt(&mut self, e: &I32Popcnt) {
        self.visit(e.expr);
    }

    fn visit_select(&mut self, e: &Select) {
        self.visit(e.condition);
        self.visit(e.consequent);
        self.visit(e.alternative);
    }

    fn visit_unreachable(&mut self, _: &Unreachable) {}

    fn visit_br(&mut self, e: &Br) {
        e.args.iter().for_each(|e| self.visit(*e));
    }

    fn visit_br_if(&mut self, e: &BrIf) {
        e.args.iter().for_each(|e| self.visit(*e));
    }

    fn visit_if_else(&mut self, e: &IfElse) {
        self.visit(e.condition);
        self.visit(e.consequent);
        self.visit(e.alternative);
    }

    fn visit_br_table(&mut self, e: &BrTable) {
        self.visit(e.which);
        e.args.iter().for_each(|e| self.visit(*e));
    }

    fn visit_drop(&mut self, e: &Drop) {
        self.visit(e.expr);
    }

    fn visit_return(&mut self, e: &Return) {
        e.values.iter().for_each(|e| self.visit(*e));
    }

    fn visit_local_get(&mut self, _: &LocalGet) {}
    fn visit_local_set(&mut self, _: &LocalSet) {}
    fn visit_i32_const(&mut self, _: &I32Const) {}

    fn visit_memory_size(&mut self, m: &MemorySize) {
        self.used.memories.insert(m.memory);
    }
}
