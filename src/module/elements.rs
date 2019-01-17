//! Table elements within a wasm module.

use crate::error::Result;
use crate::module::emit::{Emit, IdsToIndices};
use crate::module::functions::FunctionId;
use crate::module::tables::{ModuleTables, TableKind};
use crate::module::Module;
use crate::passes::Used;
use id_arena::{Arena, Id};
use parity_wasm::elements;
use std::collections::HashMap;

/// A passive element segment identifier
pub type ElementId = Id<Element>;

/// A passive segment which contains a list of functions
#[derive(Debug)]
pub struct Element {
    members: Vec<FunctionId>,
}

/// All element segments of a wasm module, used to initialize `anyfunc` tables,
/// used as function pointers.
#[derive(Debug, Default)]
pub struct ModuleElements {
    elements: Arena<Element>,
    index_to_element_id: HashMap<u32, ElementId>,
}

impl ModuleElements {
    /// Parses a raw was section into a fully-formed `ModuleElements` instance.
    pub fn parse(
        module: &mut Module,
        section: &elements::ElementSection,
    ) -> Result<ModuleElements> {
        let mut elements = Arena::new();
        let mut index_to_element_id = HashMap::new();

        for (i, segment) in section.entries().iter().enumerate() {
            if segment.passive() {
                let mut list = Vec::with_capacity(segment.members().len());
                for &func in segment.members() {
                    match module.funcs.function_for_index(func) {
                        Some(id) => list.push(id),
                        None => failure::bail!("invalid segment initialization"),
                    }
                }
                let id = elements.next_id();
                let ret = elements.alloc(Element { members: list });
                debug_assert_eq!(id, ret);
                index_to_element_id.insert(i as u32, id);
                continue;
            }

            let table = module
                .tables
                .table_for_index(segment.index())
                .ok_or_else(|| failure::format_err!("invalid table index"))?;
            let list = match &mut module.tables.get_mut(table).kind {
                TableKind::Function(list) => list,
            };

            let offset = segment.offset().as_ref().unwrap();
            if offset.code().len() != 2 {
                failure::bail!("invalid initialization expression");
            }
            match offset.code()[1] {
                elements::Instruction::End => {}
                _ => failure::bail!("invalid initialization expression"),
            }
            let offset = match offset.code()[0] {
                elements::Instruction::I32Const(n) => n as usize,
                _ => failure::bail!("invalid initialization expression"),
            };

            for (i, &func) in segment.members().iter().enumerate() {
                let id = match module.funcs.function_for_index(func) {
                    Some(i) => i,
                    None => failure::bail!("invalid segment initialization"),
                };
                while i + offset + 1 > list.len() {
                    list.push(None);
                }
                list[i + offset] = Some(id);
            }
        }

        Ok(ModuleElements {
            elements,
            index_to_element_id,
        })
    }

    /// Get the element for the given index in the input wasm, if any exists.
    pub fn element_for_index(&self, index: u32) -> Option<ElementId> {
        self.index_to_element_id.get(&index).cloned()
    }
}

impl Emit for ModuleElements {
    type Extra = ModuleTables;

    fn emit(
        &self,
        tables: &ModuleTables,
        used: &Used,
        module: &mut elements::Module,
        indices: &mut IdsToIndices,
    ) {
        let mut segments = Vec::new();

        // Sort table ids for a deterministic emission for now, eventually we
        // may want some sort of sorting heuristic here.
        let mut active = tables
            .iter()
            .filter(|t| used.tables.contains(&t.id()))
            .filter_map(|t| match &t.kind {
                TableKind::Function(list) => Some((t.id(), list)),
            })
            .collect::<Vec<_>>();
        active.sort();

        // Append segments as we find them for all table initializers. We can
        // skip initializers for unused tables, and othrewise we just want to
        // create an initializer for each contiguous chunk of function indices.
        for (table_id, initializer) in active {
            let table_index = indices.get_table_index(table_id);

            let mut add = |offset: usize, members: Vec<u32>| {
                let code = vec![
                    elements::Instruction::I32Const(offset as i32),
                    elements::Instruction::End,
                ];
                let init = elements::InitExpr::new(code);
                segments.push(elements::ElementSegment::new(
                    table_index,
                    Some(init),
                    members,
                    false,
                ));
            };

            let mut offset = 0;
            let mut cur = Vec::new();
            for (i, item) in initializer.iter().enumerate() {
                match item {
                    Some(item) => {
                        if cur.len() == 0 {
                            offset = i;
                        }
                        cur.push(indices.get_func_index(*item));
                    }
                    None => {
                        if cur.len() > 0 {
                            add(offset, cur);
                        }
                        cur = Vec::new();
                    }
                }
            }

            if cur.len() > 0 {
                add(offset, cur);
            }
        }

        // After all the active segments are added add passive segments next. We
        // may want to sort this more intelligently in the future. Othrewise
        // emitting a segment here is in general much simpler than above as we
        // know there are no holes.
        for (id, segment) in self.elements.iter() {
            if !used.elements.contains(&id) {
                continue;
            }
            let index = segments.len() as u32;
            indices.set_element_index(id, index);

            let members = segment
                .members
                .iter()
                .map(|id| indices.get_func_index(*id))
                .collect();
            segments.push(elements::ElementSegment::new(0, None, members, true));
        }

        if segments.len() > 0 {
            let elements = elements::ElementSection::with_entries(segments);
            let elements = elements::Section::Element(elements);
            module.sections_mut().push(elements);
        }
    }
}
