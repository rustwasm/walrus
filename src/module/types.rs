//! Types in a wasm module.

use crate::arena_set::ArenaSet;
use crate::emit::{Emit, EmitContext, Section};
use crate::error::Result;
use crate::module::Module;
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

    /// Get a type ID by its name.
    ///
    /// This is currently only intended for in-memory modifications, and by
    /// default will always return `None` for a newly parsed module. A
    /// hypothetical future WAT text format to `walrus::Module` parser could
    /// preserve type names from the WAT.
    pub fn by_name(&self, name: &str) -> Option<TypeId> {
        self.arena.iter().find_map(|(id, ty)| {
            if ty.name.as_ref().map(|s| s.as_str()) == Some(name) {
                Some(id)
            } else {
                None
            }
        })
    }

    /// Get a shared reference to this module's types.
    pub fn iter(&self) -> impl Iterator<Item = &Type> {
        self.arena.iter().map(|(_, f)| f)
    }

    /// Removes a type from this module.
    ///
    /// It is up to you to ensure that any potential references to the deleted
    /// type are also removed, eg `call_indirect` expressions, function types,
    /// etc.
    pub fn delete(&mut self, ty: TypeId) {
        self.arena.remove(ty);
    }

    /// Add a new type to this module, and return its `Id`
    pub fn add(&mut self, params: &[ValType], results: &[ValType]) -> TypeId {
        let id = self.arena.next_id();
        self.arena.insert(Type::new(
            id,
            params.to_vec().into_boxed_slice(),
            results.to_vec().into_boxed_slice(),
        ))
    }
}

impl Module {
    /// Construct the set of types within a module.
    pub(crate) fn parse_types(&mut self, section: wasmparser::TypeSectionReader) -> Result<()> {
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
            self.indices_to_ids.push_type(id);
        }

        Ok(())
    }
}

impl Emit for ModuleTypes {
    fn emit(&self, cx: &mut EmitContext) {
        log::debug!("emitting type section");
        let ntypes = self.iter().count();
        if ntypes == 0 {
            return;
        }
        let mut cx = cx.start_section(Section::Type);
        cx.encoder.usize(ntypes);

        for (id, ty) in self.arena.iter() {
            cx.indices.push_type(id);
            ty.emit(&mut cx);
        }
    }
}
