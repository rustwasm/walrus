//! Types in a wasm module.

use crate::arena_set::ArenaSet;
use crate::module::emit::{Emit, IdsToIndices};
use crate::passes::Used;
use crate::ty::{Type, TypeId, ValType};
use parity_wasm::elements;
use std::collections::HashMap;

/// The set of de-duplicated types within a module.
#[derive(Debug, Default)]
pub struct ModuleTypes {
    types: ArenaSet<Type>,
    index_to_type_id: HashMap<u32, TypeId>,
}

impl ModuleTypes {
    /// Construct the set of types within a module.
    pub fn parse(type_section: &elements::TypeSection) -> ModuleTypes {
        let mut types = ArenaSet::with_capacity(type_section.types().len());
        let mut index_to_type_id = HashMap::with_capacity(type_section.types().len());

        for (i, ty) in type_section.types().iter().enumerate() {
            let id = types.next_id();
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
            let id = types.insert(Type::new(id, params, results));
            index_to_type_id.insert(i as u32, id);
        }

        ModuleTypes {
            types,
            index_to_type_id,
        }
    }

    /// Get the type id for the type at the given index in the original input
    /// wasm.
    pub fn type_for_index(&self, index: u32) -> Option<TypeId> {
        self.index_to_type_id.get(&index).cloned()
    }

    /// Get the set of types for this module.
    pub fn types(&self) -> &ArenaSet<Type> {
        &self.types
    }

    /// Get the set of types for this module.
    pub fn types_mut(&mut self) -> &mut ArenaSet<Type> {
        &mut self.types
    }
}

impl Emit for ModuleTypes {
    fn emit(&self, used: &Used, module: &mut elements::Module, indices: &mut IdsToIndices) {
        if used.types.is_empty() {
            return;
        }

        let mut types = Vec::with_capacity(used.types.len());

        for (id, ty) in self.types.iter() {
            if !used.types.contains(&id) {
                continue;
            }

            indices.set_type_index(id, types.len() as u32);

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

        let types = elements::TypeSection::with_types(types);
        let types = elements::Section::Type(types);
        module.sections_mut().push(types);
    }
}
