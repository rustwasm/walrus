//! Traits and code for emitting high-level structures as low-level, raw wasm
//! structures. E.g. translating from globally unique identifiers down to the
//! raw wasm structure's index spaces. Currently "raw wasm structures" are
//! `parity_wasm::elements` types.

use crate::ir::LocalId;
use crate::module::data::DataId;
use crate::module::elements::ElementId;
use crate::module::functions::FunctionId;
use crate::module::globals::GlobalId;
use crate::module::memories::MemoryId;
use crate::module::tables::TableId;
use crate::module::Module;
use crate::passes::Used;
use crate::ty::TypeId;
use parity_wasm::elements;
use std::collections::HashMap;

pub struct EmitContext<'a> {
    pub module: &'a Module,
    pub used: &'a Used,
    pub indices: &'a mut IdsToIndices,
    pub dst: &'a mut elements::Module,
}

/// Anything that can be lowered to raw wasm structures.
pub trait Emit {
    /// Emit `self` into the given context.
    fn emit(&self, cx: &mut EmitContext);
}

/// Maps our high-level identifiers to the raw indices they end up emitted at.
///
/// As we lower to raw wasm structures, we cement various constructs' locations
/// in their respective index spaces. For example, a type with some id `A` ends
/// up being the `i^th` type emitted in the raw wasm type section. When a
/// function references that type, it needs to reference it by its `i` index
/// since the identifier `A` doesn't exist at the raw wasm level.
#[derive(Debug, Default)]
pub struct IdsToIndices {
    tables: HashMap<TableId, u32>,
    types: HashMap<TypeId, u32>,
    funcs: HashMap<FunctionId, u32>,
    globals: HashMap<GlobalId, u32>,
    locals: HashMap<LocalId, u32>,
    memories: HashMap<MemoryId, u32>,
    elements: HashMap<ElementId, u32>,
    data: HashMap<DataId, u32>,
}

macro_rules! define_get_push_index {
    ( $get_name:ident, $push_name:ident, $id_ty:ty, $member:ident ) => {
        impl IdsToIndices {
            /// Get the index for the given identifier.
            #[inline]
            #[allow(dead_code)] // not everything is used just yet
            pub fn $get_name(&self, id: $id_ty) -> u32 {
                self.$member.get(&id).cloned().expect(
                    "Should never try and get the index for an identifier that has not already had \
                     its index set. This means that either we are attempting to get the index of \
                     an unused identifier, or that we are emitting sections in the wrong order."
                )
            }

            /// Adds the given identifier to this set, assigning it the next
            /// available index.
            #[inline]
            pub fn $push_name(&mut self, id: $id_ty) {
                let idx = self.$member.len() as u32;
                self.$member.insert(id, idx);
            }
        }
    };
}

define_get_push_index!(get_table_index, push_table, TableId, tables);
define_get_push_index!(get_type_index, push_type, TypeId, types);
define_get_push_index!(get_func_index, push_func, FunctionId, funcs);
define_get_push_index!(get_global_index, push_global, GlobalId, globals);
define_get_push_index!(get_memory_index, push_memory, MemoryId, memories);
define_get_push_index!(get_element_index, push_element, ElementId, elements);
define_get_push_index!(get_data_index, push_data, DataId, data);

impl IdsToIndices {
    /// Get the index for the given identifier.
    #[inline]
    pub fn get_local_index(&self, id: LocalId) -> u32 {
        self.locals.get(&id).cloned().expect(
            "Should never try and get the index for an identifier that has not already had \
             its index set. This means that either we are attempting to get the index of \
             an unused identifier, or that we are emitting sections in the wrong order.",
        )
    }

    /// Adds the given identifier to this set, assigning it the next
    /// available index.
    #[inline]
    pub fn set_local_index(&mut self, id: LocalId, index: u32) {
        assert!(
            self.locals.insert(id, index).is_none(),
            "cannot set local index twice"
        );
    }
}
