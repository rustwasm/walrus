//! Globals within a wasm module.

use crate::emit::{Emit, EmitContext, Section};
use crate::parse::IndicesToIds;
use crate::tombstone_arena::{Id, TombstoneArena};
use crate::{ImportId, InitExpr, Module, Result, ValType};

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
    Local(InitExpr),
}

impl Global {
    /// Get this global's id.
    pub fn id(&self) -> GlobalId {
        self.id
    }
}

impl Emit for Global {
    fn emit(&self, cx: &mut EmitContext) {
        Emit::emit(&self.ty, cx);
        cx.encoder.byte(self.mutable as u8);
    }
}

/// The set of globals in each function in this module.
#[derive(Debug, Default)]
pub struct ModuleGlobals {
    /// The arena where the globals are stored.
    arena: TombstoneArena<Global>,
}

impl ModuleGlobals {
    /// Adds a new imported global to this list.
    pub fn add_import(&mut self, ty: ValType, mutable: bool, import_id: ImportId) -> GlobalId {
        self.arena.alloc_with_id(|id| Global {
            id,
            ty,
            mutable,
            kind: GlobalKind::Import(import_id),
        })
    }

    /// Construct a new global, that does not originate from any of the input
    /// wasm globals.
    pub fn add_local(&mut self, ty: ValType, mutable: bool, init: InitExpr) -> GlobalId {
        self.arena.alloc_with_id(|id| Global {
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
        log::debug!("parse global section");
        for g in section {
            let g = g?;
            let id = self.globals.add_local(
                ValType::parse(&g.ty.content_type)?,
                g.ty.mutable,
                InitExpr::eval(&g.init_expr, ids)?,
            );
            ids.push_global(id);
        }
        Ok(())
    }
}

impl Emit for ModuleGlobals {
    fn emit(&self, cx: &mut EmitContext) {
        log::debug!("emit global section");
        fn get_local<'a>(cx: &EmitContext, global: &'a Global) -> Option<&'a InitExpr> {
            // If it's imported we already emitted this in the import section
            if !cx.used.globals.contains(&global.id) {
                return None;
            }
            match &global.kind {
                GlobalKind::Import(_) => None,
                GlobalKind::Local(local) => Some(local),
            }
        }

        let globals = self
            .arena
            .iter()
            .filter(|(_id, global)| get_local(cx, global).is_some())
            .count();

        if globals == 0 {
            return;
        }

        let mut cx = cx.start_section(Section::Global);
        cx.encoder.usize(globals);
        for (id, global) in self.arena.iter() {
            if let Some(local) = get_local(&cx, global) {
                cx.indices.push_global(id);
                global.emit(&mut cx);
                local.emit(&mut cx);
            }
        }
    }
}
