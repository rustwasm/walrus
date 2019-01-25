//! Types in a wasm module.

use crate::arena_set::ArenaSet;
use crate::emit::{Emit, EmitContext, Section};
use crate::error::Result;
use crate::module::Module;
use crate::parse::IndicesToIds;
use crate::ty::{Type, TypeId, ValType};

/// The set of de-duplicated types within a module.
#[derive(Debug, Default)]
pub struct ModuleTypes {
    arena: ArenaSet<Type>,
}

impl ModuleTypes {
    /// Get a type associated with an ID
    pub fn get(&self, id: TypeId) -> &Type {
        &self.arena[id]
    }

    /// Get a type associated with an ID
    pub fn get_mut(&mut self, id: TypeId) -> &mut Type {
        &mut self.arena[id]
    }

    /// Get a shared reference to this module's types.
    pub fn iter(&self) -> impl Iterator<Item = &Type> {
        self.arena.iter().map(|(_, f)| f)
    }
}

impl Module {
    /// Construct the set of types within a module.
    pub(crate) fn parse_types(
        &mut self,
        section: wasmparser::TypeSectionReader,
        ids: &mut IndicesToIds,
    ) -> Result<()> {
        log::debug!("parsing type section");
        for ty in section {
            let fun_ty = ty?;
            let id = self.types.arena.next_id();
            let params = fun_ty
                .params
                .iter()
                .map(ValType::parse)
                .collect::<Result<Vec<_>>>()?
                .into_boxed_slice();
            let results = fun_ty
                .returns
                .iter()
                .map(ValType::parse)
                .collect::<Result<Vec<_>>>()?
                .into_boxed_slice();
            let id = self.types.arena.insert(Type::new(id, params, results));
            ids.push_type(id);
        }

        Ok(())
    }
}

impl Emit for ModuleTypes {
    fn emit(&self, cx: &mut EmitContext) {
        log::debug!("emitting type section");
        let ntypes = cx.used.types.len();
        if ntypes == 0 {
            return;
        }
        let mut cx = cx.start_section(Section::Type);
        cx.encoder.usize(ntypes);

        for (id, ty) in self.arena.iter() {
            if !cx.used.types.contains(&id) {
                continue;
            }
            cx.indices.push_type(id);
            ty.emit(&mut cx);
        }
    }
}
