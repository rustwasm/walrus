//! Globals within a wasm module.

use crate::const_value::Const;
use crate::error::Result;
use crate::module::emit::{Emit, IdsToIndices};
use crate::passes::Used;
use crate::ty::ValType;
use failure::ResultExt;
use id_arena::{Arena, Id};
use parity_wasm::elements;
use std::collections::HashMap;

/// The id of a global.
pub type GlobalId = Id<Global>;

/// A wasm global.
#[derive(Debug)]
pub struct Global {
    // NB: Not public so that it can't get out of sync with the arena this is
    // contained within.
    id: GlobalId,

    /// This global's type.
    pub ty: ValType,

    /// Whether this global is mutable or not.
    pub mutable: bool,

    /// The initialized constant value
    pub init: Const,
}

impl Global {
    /// Get this global's id.
    pub fn id(&self) -> GlobalId {
        self.id
    }
}

/// The set of globals in each function in this module.
#[derive(Debug, Default)]
pub struct ModuleGlobals {
    /// The arena where the globals are stored.
    pub arena: Arena<Global>,
    index_to_global_id: HashMap<u32, GlobalId>,
}

impl ModuleGlobals {
    /// Construct a new, empty set of globals for a module.
    pub fn parse(global_section: &elements::GlobalSection) -> Result<ModuleGlobals> {
        let capacity = global_section.entries().len();
        let mut globals = ModuleGlobals {
            arena: Arena::with_capacity(capacity),
            index_to_global_id: HashMap::with_capacity(capacity),
        };

        for (i, g) in global_section.entries().iter().enumerate() {
            globals.add_global_for_index(
                i as u32,
                ValType::from(&g.global_type().content_type()),
                g.global_type().is_mutable(),
                g.init_expr(),
            )?;
        }

        Ok(globals)
    }

    /// Get the global associated with the given index in the input wasm, if it
    /// exists.
    pub fn global_for_index(&self, index: u32) -> Option<GlobalId> {
        self.index_to_global_id.get(&index).cloned()
    }

    /// Create the global for this function at the given index.
    fn add_global_for_index(
        &mut self,
        index: u32,
        ty: ValType,
        mutable: bool,
        init_expr: &elements::InitExpr,
    ) -> Result<()> {
        assert!(!self.index_to_global_id.contains_key(&index));
        let id = self
            .new_global(ty, mutable, init_expr)
            .with_context(|_| format!("adding global {}", index))?;
        self.index_to_global_id.insert(index, id);
        Ok(())
    }

    /// Construct a new global, that does not originate from any of the input
    /// wasm globals.
    pub fn new_global(
        &mut self,
        ty: ValType,
        mutable: bool,
        init_expr: &elements::InitExpr,
    ) -> Result<GlobalId> {
        let id = self.arena.next_id();
        let id2 = self.arena.alloc(Global {
            id,
            ty,
            mutable,
            init: Const::eval(init_expr)?,
        });
        debug_assert_eq!(id, id2);
        Ok(id)
    }
}

impl Emit for ModuleGlobals {
    type Extra = ();

    fn emit(&self, _: &(), used: &Used, module: &mut elements::Module, indices: &mut IdsToIndices) {
        if used.globals.is_empty() {
            return;
        }

        let mut globals = Vec::with_capacity(used.globals.len());

        let import_count = module.import_count(elements::ImportCountType::Global) as u32;

        for (id, global) in &self.arena {
            if !used.globals.contains(&id) {
                continue;
            }

            indices.set_global_index(id, globals.len() as u32 + import_count);

            assert!(
                !global.mutable,
                "can't emit mutable globals yet; parity-wasm doesn't have a mutable parameter in \
                 its constructor"
            );

            let init_expr = global.init.emit_instructions(indices);

            let ty = elements::GlobalType::new(global.ty.into(), global.mutable);
            let global = elements::GlobalEntry::new(ty, init_expr);
            globals.push(global);
        }

        let globals = elements::GlobalSection::with_entries(globals);
        let globals = elements::Section::Global(globals);
        module.sections_mut().push(globals);
    }
}
