//! All the locals used by functions in a wasm module.

use crate::ir::{Local, LocalId};
use crate::module::functions::FunctionId;
use crate::ty::ValType;
use id_arena::Arena;
use std::collections::HashMap;

/// The set of locals in each function in this module.
#[derive(Debug, Default)]
pub struct ModuleLocals {
    locals: Arena<Local>,
    func_and_index_to_local_id: HashMap<(FunctionId, u32), LocalId>,
}

impl ModuleLocals {
    /// Construct a new, empty set of locals for a module.
    pub fn new() -> ModuleLocals {
        ModuleLocals {
            locals: Arena::new(),
            func_and_index_to_local_id: HashMap::new(),
        }
    }

    /// Get or create the local for this function at the given index.
    pub fn local_for_function_and_index(
        &mut self,
        func: FunctionId,
        ty: ValType,
        index: u32,
    ) -> LocalId {
        let key = (func, index);
        if let Some(id) = self.func_and_index_to_local_id.get(&key) {
            return *id;
        }

        let id = self.new_local(ty);
        self.func_and_index_to_local_id.insert(key, id);
        id
    }

    /// Construct a new local, that does not originate from any of the input
    /// wasm locals.
    pub fn new_local(&mut self, ty: ValType) -> LocalId {
        let id = self.locals.next_id();
        let id2 = self.locals.alloc(Local::new(id, ty));
        debug_assert_eq!(id, id2);
        id
    }
}
