//! Traits and code for emitting high-level structures as low-level, raw wasm
//! structures. E.g. translating from globally unique identifiers down to the
//! raw wasm structure's index spaces. Currently "raw wasm structures" are
//! `parity_wasm::elements` types.

use crate::ir::LocalId;
use crate::module::elements::ElementId;
use crate::module::functions::FunctionId;
use crate::module::globals::GlobalId;
use crate::module::imports::ImportId;
use crate::module::memories::MemoryId;
use crate::module::tables::TableId;
use crate::passes::Used;
use crate::ty::TypeId;
use parity_wasm::elements;
use std::collections::HashMap;

/// Anything that can be lowered to raw wasm structures.
pub(crate) trait Emit {
    /// Extra data, if any, passed into `emit`.
    type Extra;

    /// Emit `self` into the given module.
    ///
    /// Anything that is not in the `used` set should not be emitted.
    fn emit(
        &self,
        extra: &Self::Extra,
        used: &Used,
        module: &mut elements::Module,
        indices: &mut IdsToIndices,
    );
}

/// Maps our high-level identifiers to the raw indices they end up emitted at.
///
/// As we lower to raw wasm structures, we cement various constructs' locations
/// in their respective index spaces. For example, a type with some id `A` ends
/// up being the `i^th` type emitted in the raw wasm type section. When a
/// function references that type, it needs to reference it by its `i` index
/// since the identifier `A` doesn't exist at the raw wasm level.
#[derive(Debug, Default)]
pub(crate) struct IdsToIndices {
    imports: HashMap<ImportId, u32>,
    tables: HashMap<TableId, u32>,
    types: HashMap<TypeId, u32>,
    funcs: HashMap<FunctionId, u32>,
    globals: HashMap<GlobalId, u32>,
    locals: HashMap<LocalId, u32>,
    memories: HashMap<MemoryId, u32>,
    elements: HashMap<ElementId, u32>,
}

macro_rules! define_get_set_index {
    ( $get_name:ident, $set_name:ident, $id_ty:ty, $member:ident ) => {
        impl IdsToIndices {
            /// Get the index for the given identifier.
            #[inline]
            #[allow(dead_code)] // not everything is used just yet
            pub(crate) fn $get_name(&self, id: $id_ty) -> u32 {
                self.$member.get(&id).cloned().expect(
                    "Should never try and get the index for an identifier that has not already had \
                     its index set. This means that either we are attempting to get the index of \
                     an unused identifier, or that we are emitting sections in the wrong order."
                )
            }

            /// Set the index for the given identifier.
            #[inline]
            pub(crate) fn $set_name(&mut self, id: $id_ty, index: u32) {
                let existing = self.$member.insert(id, index);
                assert!(existing.is_none(), "should never re-assign an index for an identifier");
            }
        }
    };
}

define_get_set_index!(get_import_index, set_import_index, ImportId, imports);
define_get_set_index!(get_table_index, set_table_index, TableId, tables);
define_get_set_index!(get_type_index, set_type_index, TypeId, types);
define_get_set_index!(get_func_index, set_func_index, FunctionId, funcs);
define_get_set_index!(get_global_index, set_global_index, GlobalId, globals);
define_get_set_index!(get_local_index, set_local_index, LocalId, locals);
define_get_set_index!(get_memory_index, set_memory_index, MemoryId, memories);
define_get_set_index!(get_element_index, set_element_index, ElementId, elements);
