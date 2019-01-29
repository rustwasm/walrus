use crate::error::Result;
use crate::ir::LocalId;
use crate::map::IdHashMap;
use crate::module::data::DataId;
use crate::module::elements::ElementId;
use crate::module::functions::{Function, FunctionId};
use crate::module::globals::GlobalId;
use crate::module::memories::MemoryId;
use crate::module::tables::TableId;
use crate::ty::TypeId;
use failure::bail;

#[derive(Debug, Default)]
pub struct IndicesToIds {
    tables: Vec<TableId>,
    types: Vec<TypeId>,
    funcs: Vec<FunctionId>,
    globals: Vec<GlobalId>,
    memories: Vec<MemoryId>,
    elements: Vec<ElementId>,
    data: Vec<DataId>,
    locals: IdHashMap<Function, Vec<LocalId>>,
}

macro_rules! define_push_get {
    ( $push:ident, $get:ident, $id_ty:ty, $member:ident ) => {
        impl IndicesToIds {
            /// Pushes a new local ID to map it to the next index internally
            pub fn $push(&mut self, id: $id_ty) {
                self.$member.push(id);
            }

            /// Gets the ID for a particular index
            pub fn $get(&self, index: u32) -> Result<$id_ty> {
                match self.$member.get(index as usize) {
                    Some(x) => Ok(*x),
                    None => bail!(
                        "index `{}` is out of bounds for {}",
                        index,
                        stringify!($member)
                    ),
                }
            }
        }
    };
}

define_push_get!(push_table, get_table, TableId, tables);
define_push_get!(push_type, get_type, TypeId, types);
define_push_get!(push_func, get_func, FunctionId, funcs);
define_push_get!(push_global, get_global, GlobalId, globals);
define_push_get!(push_memory, get_memory, MemoryId, memories);
define_push_get!(push_element, get_element, ElementId, elements);
define_push_get!(push_data, get_data, DataId, data);

impl IndicesToIds {
    /// Pushes a new local ID to map it to the next index internally
    pub fn push_local(&mut self, function: FunctionId, id: LocalId) {
        self.locals.entry(function).or_insert(Vec::new()).push(id);
    }

    /// Gets the ID for a particular index
    pub fn get_local(&self, function: FunctionId, index: u32) -> Result<LocalId> {
        let ret = self
            .locals
            .get(&function)
            .and_then(|list| list.get(index as usize));
        match ret {
            Some(x) => Ok(*x),
            None => bail!("index `{}` is out of bounds for local", index,),
        }
    }
}
