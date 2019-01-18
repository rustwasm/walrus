//! All the locals used by functions in a wasm module.

use crate::ir::{Local, LocalId};
use crate::ty::ValType;
use id_arena::Arena;

/// The set of locals in each function in this module.
#[derive(Debug, Default)]
pub struct ModuleLocals {
    arena: Arena<Local>,
}

impl ModuleLocals {
    /// Construct a new local, that does not originate from any of the input
    /// wasm locals.
    pub fn add(&mut self, ty: ValType) -> LocalId {
        let id = self.arena.next_id();
        let id2 = self.arena.alloc(Local::new(id, ty));
        debug_assert_eq!(id, id2);
        id
    }

    /// Get the local for an ID
    pub fn get(&self, id: LocalId) -> &Local {
        &self.arena[id]
    }

    /// Get the set of locals for this module.
    pub fn get_mut(&mut self, id: LocalId) -> &mut Local {
        &mut self.arena[id]
    }

    /// Get a shared reference to this module's globals.
    pub fn iter(&self) -> impl Iterator<Item = &Local> {
        self.arena.iter().map(|(_, f)| f)
    }
}
