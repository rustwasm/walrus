//! Table elements within a wasm module.

use crate::emit::{Emit, EmitContext, Section};
use crate::ir::Value;
use crate::parse::IndicesToIds;
use crate::{FunctionId, InitExpr, Module, Result, TableKind, ValType};
use failure::{bail, ResultExt};
use id_arena::{Arena, Id};

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
    pub(crate) fn parse_elements(
        &mut self,
        section: wasmparser::ElementSectionReader,
        ids: &mut IndicesToIds,
    ) -> Result<()> {
        log::debug!("parse element section");
        for (i, segment) in section.into_iter().enumerate() {
            let segment = segment?;

            match segment.kind {
                wasmparser::ElementKind::Passive(ty) => {
                    drop(ty);
                    bail!("passive element segments not supported yet");
                }
                wasmparser::ElementKind::Active {
                    table_index,
                    init_expr,
                } => {
                    let table = ids.get_table(table_index)?;
                    let table = match &mut self.tables.get_mut(table).kind {
                        TableKind::Function(t) => t,
                        TableKind::Anyref(_) => {
                            bail!("active anyref segments not supported yet");
                        }
                    };

                    let offset = InitExpr::eval(&init_expr, ids)
                        .with_context(|_e| format!("in segment {}", i))?;
                    let functions = segment.items.get_items_reader()?.into_iter().map(|func| {
                        let func = func?;
                        ids.get_func(func)
                    });

                    match offset {
                        InitExpr::Value(Value::I32(n)) => {
                            let offset = n as usize;
                            for (i, id) in functions.enumerate() {
                                while i + offset + 1 > table.elements.len() {
                                    table.elements.push(None);
                                }
                                table.elements[i + offset] = Some(id?);
                            }
                        }
                        InitExpr::Global(global) if self.globals.get(global).ty == ValType::I32 => {
                            let list = functions.collect::<Result<_>>()?;
                            table.relative_elements.push((global, list));
                        }
                        _ => bail!("non-i32 constant in segment {}", i),
                    }
                }
            }
        }
        Ok(())
    }
}

impl Emit for ModuleElements {
    fn emit(&self, cx: &mut EmitContext) {
        log::debug!("emit element section");
        // Sort table ids for a deterministic emission for now, eventually we
        // may want some sort of sorting heuristic here.
        let mut active = cx
            .module
            .tables
            .iter()
            .filter(|t| cx.used.tables.contains(&t.id()))
            .filter_map(|t| match &t.kind {
                TableKind::Function(list) => Some((t.id(), list)),
                TableKind::Anyref(_) => None,
            })
            .collect::<Vec<_>>();
        active.sort_by_key(|pair| pair.0);

        // Append segments as we find them for all table initializers. We can
        // skip initializers for unused tables, and otherwise we just want to
        // create an initializer for each contiguous chunk of function indices.
        let mut chunks = Vec::new();
        for (table_id, table) in active.iter() {
            let mut offset = 0;
            let mut len = 0;
            for (i, item) in table.elements.iter().enumerate() {
                if item.is_some() {
                    if len == 0 {
                        offset = i;
                    }
                    len += 1;
                } else {
                    if len > 0 {
                        chunks.push((table_id, table, offset, len));
                    }
                    len = 0;
                }
            }

            if len > 0 {
                chunks.push((table_id, table, offset, len));
            }
        }

        let passive = self
            .arena
            .iter()
            .filter(|(id, _)| cx.used.elements.contains(id))
            .count();
        let relative = active
            .iter()
            .map(|(_, table)| table.relative_elements.len())
            .sum::<usize>();
        let total = passive + relative + chunks.len();

        if total == 0 {
            return;
        }
        let mut cx = cx.start_section(Section::Element);
        cx.encoder.usize(total);

        // Emits the leading data for describing a table's index
        //
        // Note that much of this is in accordance with the
        // currently-in-progress bulk-memory proposal for WebAssembly.
        let active_table_header = |cx: &mut EmitContext, index: u32| {
            if index == 0 {
                cx.encoder.byte(0x00);
            } else {
                cx.encoder.byte(0x02);
                cx.encoder.u32(index);
            }
        };

        // Emit all contiguous chunks of functions pointers that are located at
        // constant offsets
        for (&id, table, offset, len) in chunks {
            let table_index = cx.indices.get_table_index(id);
            active_table_header(&mut cx, table_index);
            InitExpr::Value(Value::I32(offset as i32)).emit(&mut cx);
            cx.encoder.usize(len);
            for item in table.elements[offset..][..len].iter() {
                let index = cx.indices.get_func_index(item.unwrap());
                cx.encoder.u32(index);
            }
        }

        // Emit all chunks of function pointers that are located at relative
        // global offsets.
        for (id, table) in active.iter() {
            let table_index = cx.indices.get_table_index(*id);
            for (global, list) in table.relative_elements.iter() {
                active_table_header(&mut cx, table_index);
                InitExpr::Global(*global).emit(&mut cx);
                cx.encoder.usize(list.len());
                for func in list {
                    let index = cx.indices.get_func_index(*func);
                    cx.encoder.u32(index);
                }
            }
        }

        // After all the active segments are added add passive segments next. We
        // may want to sort this more intelligently in the future. Otherwise
        // emitting a segment here is in general much simpler than above as we
        // know there are no holes.
        for (id, segment) in self.arena.iter() {
            if !cx.used.elements.contains(&id) {
                continue;
            }
            cx.indices.push_element(id);
            drop((id, segment));
            // TODO: sync this with the upstream spec
            panic!(
                "encoding a passive element segment requires either \
                 `ref.null` or `ref.func` encodings, which aren't \
                 currently implemented"
            );
        }
    }
}
