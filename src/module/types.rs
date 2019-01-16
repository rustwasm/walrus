//! Types in a wasm module.

use crate::arena_set::ArenaSet;
use crate::emit::{Emit, EmitContext};
use crate::module::parse::IndicesToIds;
use crate::module::Module;
use crate::ty::{Type, TypeId, ValType};
use parity_wasm::elements;

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
    pub(crate) fn parse_types(&mut self, section: &elements::TypeSection, ids: &mut IndicesToIds) {
        for ty in section.types() {
            let id = self.types.arena.next_id();
            let fun_ty = match ty {
                elements::Type::Function(f) => f,
            };
            let params = fun_ty
                .params()
                .iter()
                .map(ValType::from)
                .collect::<Vec<_>>()
                .into_boxed_slice();
            let results = fun_ty
                .return_type()
                .iter()
                .map(ValType::from)
                .collect::<Vec<_>>()
                .into_boxed_slice();
            let id = self.types.arena.insert(Type::new(id, params, results));
            ids.push_type(id);
            // self.types.index_to_type_id.insert(i as u32, id);
        }
    }
}

impl Emit for ModuleTypes {
    fn emit(&self, cx: &mut EmitContext) {
        let mut types = Vec::with_capacity(cx.used.types.len());

        for (id, ty) in self.arena.iter() {
            if !cx.used.types.contains(&id) {
                continue;
            }
            cx.indices.push_type(id);

            let params: Vec<elements::ValueType> =
                ty.params().iter().cloned().map(Into::into).collect();

            let ret: Vec<elements::ValueType> =
                ty.results().iter().cloned().map(Into::into).collect();
            assert!(
                ret.len() <= 1,
                "multiple return values not supported yet; \
                 write a legalization pass to rewrite them into single value returns \
                 and store extra return values in globals."
            );
            let ret = if ret.is_empty() { None } else { Some(ret[0]) };

            types.push(elements::Type::Function(elements::FunctionType::new(
                params, ret,
            )));
        }

        if !types.is_empty() {
            let types = elements::TypeSection::with_types(types);
            let types = elements::Section::Type(types);
            cx.dst.sections_mut().push(types);
        }
    }
}
