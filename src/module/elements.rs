//! Table elements within a wasm module.

use crate::const_value::Const;
use crate::emit::{Emit, EmitContext};
use crate::error::Result;
use crate::ir::Value;
use crate::module::functions::FunctionId;
use crate::module::tables::TableKind;
use crate::module::Module;
use crate::parse::IndicesToIds;
use crate::ty::ValType;
use failure::{bail, ResultExt};
use id_arena::{Arena, Id};
use parity_wasm::elements;

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
    arena: Arena<Element>,
}

impl ModuleElements {
    /// Get an element associated with an ID
    pub fn get(&self, id: ElementId) -> &Element {
        &self.arena[id]
    }

    /// Get an element associated with an ID
    pub fn get_mut(&mut self, id: ElementId) -> &mut Element {
        &mut self.arena[id]
    }

    /// Get a shared reference to this module's passive elements.
    pub fn iter(&self) -> impl Iterator<Item = &Element> {
        self.arena.iter().map(|(_, f)| f)
    }
}

impl Module {
    /// Parses a raw was section into a fully-formed `ModuleElements` instance.
    pub fn parse_elements(
        &mut self,
        section: &elements::ElementSection,
        ids: &mut IndicesToIds,
    ) -> Result<()> {
        for (i, segment) in section.entries().iter().enumerate() {
            if segment.passive() {
                let mut list = Vec::with_capacity(segment.members().len());
                for &func in segment.members() {
                    list.push(ids.get_func(func)?);
                }
                let id = self.elements.arena.next_id();
                self.elements.arena.alloc(Element { members: list });
                ids.push_element(id);
                continue;
            }

            let table = ids.get_table(segment.index())?;
            let table = match &mut self.tables.get_mut(table).kind {
                TableKind::Function(t) => t,
            };

            let functions = segment.members().iter().map(|func| ids.get_func(*func));

            let offset = segment.offset().as_ref().unwrap();
            let offset = Const::eval(offset, ids).with_context(|_e| format!("in segment {}", i))?;
            match offset {
                Const::Value(Value::I32(n)) => {
                    let offset = n as usize;
                    for (i, id) in functions.enumerate() {
                        while i + offset + 1 > table.elements.len() {
                            table.elements.push(None);
                        }
                        table.elements[i + offset] = Some(id?);
                    }
                }
                Const::Global(global) if self.globals.get(global).ty == ValType::I32 => {
                    let list = functions.collect::<Result<_>>()?;
                    table.relative_elements.push((global, list));
                }
                _ => bail!("non-i32 constant in segment {}", i),
            }
        }
        Ok(())
    }
}

impl Emit for ModuleElements {
    fn emit(&self, cx: &mut EmitContext) {
        let mut segments = Vec::new();

        // Sort table ids for a deterministic emission for now, eventually we
        // may want some sort of sorting heuristic here.
        let mut active = cx
            .module
            .tables
            .iter()
            .filter(|t| cx.used.tables.contains(&t.id()))
            .filter_map(|t| match &t.kind {
                TableKind::Function(list) => Some((t.id(), list)),
            })
            .collect::<Vec<_>>();
        active.sort_by_key(|pair| pair.0);

        // Append segments as we find them for all table initializers. We can
        // skip initializers for unused tables, and othrewise we just want to
        // create an initializer for each contiguous chunk of function indices.
        for (table_id, table) in active {
            let table_index = cx.indices.get_table_index(table_id);

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
            for (i, item) in table.elements.iter().enumerate() {
                match item {
                    Some(item) => {
                        if cur.len() == 0 {
                            offset = i;
                        }
                        cur.push(cx.indices.get_func_index(*item));
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

            for (global, list) in table.relative_elements.iter() {
                let init = Const::Global(*global).emit_instructions(cx.indices);
                let members = list.iter().map(|i| cx.indices.get_func_index(*i)).collect();
                segments.push(elements::ElementSegment::new(
                    table_index,
                    Some(init),
                    members,
                    false,
                ));
            }
        }

        // After all the active segments are added add passive segments next. We
        // may want to sort this more intelligently in the future. Othrewise
        // emitting a segment here is in general much simpler than above as we
        // know there are no holes.
        for (id, segment) in self.arena.iter() {
            if !cx.used.elements.contains(&id) {
                continue;
            }
            cx.indices.push_element(id);

            let members = segment
                .members
                .iter()
                .map(|id| cx.indices.get_func_index(*id))
                .collect();
            segments.push(elements::ElementSegment::new(0, None, members, true));
        }

        if segments.len() > 0 {
            let elements = elements::ElementSection::with_entries(segments);
            let elements = elements::Section::Element(elements);
            cx.dst.sections_mut().push(elements);
        }
    }
}
