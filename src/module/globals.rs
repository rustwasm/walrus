//! Globals within a wasm module.

use crate::const_value::Const;
use crate::emit::{Emit, EmitContext};
use crate::error::Result;
use crate::module::imports::ImportId;
use crate::module::Module;
use crate::parse::IndicesToIds;
use crate::ty::ValType;
use id_arena::{Arena, Id};
use parity_wasm::elements;

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

    /// The kind of global this is
    pub kind: GlobalKind,
}

/// The different kinds of globals a wasm module can have
#[derive(Debug)]
pub enum GlobalKind {
    /// An imported global without a known initializer
    Import(ImportId),
    /// A locally declare global with the specified identifier
    Local(Const),
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
    arena: Arena<Global>,
}

impl ModuleGlobals {
    /// Adds a new imported global to this list.
    pub fn add_import(&mut self, ty: ValType, mutable: bool, import_id: ImportId) -> GlobalId {
        let id = self.arena.next_id();
        self.arena.alloc(Global {
            id,
            ty,
            mutable,
            kind: GlobalKind::Import(import_id),
        })
    }

    /// Construct a new global, that does not originate from any of the input
    /// wasm globals.
    pub fn add_local(&mut self, ty: ValType, mutable: bool, init: Const) -> GlobalId {
        let id = self.arena.next_id();
        self.arena.alloc(Global {
            id,
            ty,
            mutable,
            kind: GlobalKind::Local(init),
        })
    }

    /// Gets a reference to a memory given its id
    pub fn get(&self, id: GlobalId) -> &Global {
        &self.arena[id]
    }

    /// Gets a reference to a memory given its id
    pub fn get_mut(&mut self, id: GlobalId) -> &mut Global {
        &mut self.arena[id]
    }

    /// Get a shared reference to this module's globals.
    pub fn iter(&self) -> impl Iterator<Item = &Global> {
        self.arena.iter().map(|(_, f)| f)
    }
}

impl Module {
    /// Construct a new, empty set of globals for a module.
    pub(crate) fn parse_globals(
        &mut self,
        section: wasmparser::GlobalSectionReader,
        ids: &mut IndicesToIds,
    ) -> Result<()> {
        for g in section {
            let g = g?;
            let id = self.globals.add_local(
                ValType::parse(&g.ty.content_type)?,
                g.ty.mutable,
                Const::eval(&g.init_expr, ids)?,
            );
            ids.push_global(id);
        }
        Ok(())
    }
}

impl Emit for ModuleGlobals {
    fn emit(&self, cx: &mut EmitContext) {
        let mut globals = Vec::with_capacity(cx.used.globals.len());

        for (id, global) in &self.arena {
            if !cx.used.globals.contains(&id) {
                continue;
            }
            let init = match &global.kind {
                GlobalKind::Import(_) => continue, // emitted in import section
                GlobalKind::Local(init) => init,
            };

            cx.indices.push_global(id);

            let init_expr = init.emit_instructions(cx.indices);

            let ty = elements::GlobalType::new(global.ty.into(), global.mutable);
            let global = elements::GlobalEntry::new(ty, init_expr);
            globals.push(global);
        }

        if !globals.is_empty() {
            let globals = elements::GlobalSection::with_entries(globals);
            let globals = elements::Section::Global(globals);
            cx.dst.sections_mut().push(globals);
        }
    }
}
