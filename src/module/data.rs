//! Data segments within a wasm module.

use crate::const_value::Const;
use crate::emit::{Emit, EmitContext};
use crate::error::Result;
use crate::ir::Value;
use crate::module::Module;
use crate::parse::IndicesToIds;
use crate::ty::ValType;
use failure::{bail, ResultExt};
use id_arena::{Arena, Id};
use parity_wasm::elements;

/// A passive element segment identifier
pub type DataId = Id<Data>;

/// A passive data segment
#[derive(Debug)]
pub struct Data {
    value: Vec<u8>,
}

/// All passive data sections of a wasm module, used to initialize memories via
/// various instructions.
#[derive(Debug, Default)]
pub struct ModuleData {
    arena: Arena<Data>,
}

impl ModuleData {
    /// Get an element associated with an ID
    pub fn get(&self, id: DataId) -> &Data {
        &self.arena[id]
    }

    /// Get an element associated with an ID
    pub fn get_mut(&mut self, id: DataId) -> &mut Data {
        &mut self.arena[id]
    }

    /// Get a shared reference to this module's passive elements.
    pub fn iter(&self) -> impl Iterator<Item = &Data> {
        self.arena.iter().map(|(_, f)| f)
    }
}

impl Module {
    /// Parses a raw wasm section into a fully-formed `ModuleData` instance.
    pub fn parse_data(
        &mut self,
        section: &elements::DataSection,
        ids: &mut IndicesToIds,
    ) -> Result<()> {
        for (i, segment) in section.entries().iter().enumerate() {
            if segment.passive() {
                let id = self.data.arena.next_id();
                self.data.arena.alloc(Data {
                    value: segment.value().to_vec(),
                });
                ids.push_data(id);
                continue;
            }

            let memory = ids.get_memory(segment.index())?;
            let value = segment.value().to_vec();
            let memory = self.memories.get_mut(memory);

            let offset = segment.offset().as_ref().unwrap();
            let offset = Const::eval(offset, ids).with_context(|_e| format!("in segment {}", i))?;
            match offset {
                Const::Value(Value::I32(n)) => {
                    memory.data.add_absolute(n as u32, value);
                }
                Const::Global(global) if self.globals.get(global).ty == ValType::I32 => {
                    memory.data.add_relative(global, value);
                }
                _ => bail!("non-i32 constant in segment {}", i),
            }
        }
        Ok(())
    }
}

impl Emit for ModuleData {
    fn emit(&self, cx: &mut EmitContext) {
        let mut segments = Vec::new();

        // Sort table ids for a deterministic emission for now, eventually we
        // may want some sort of sorting heuristic here.
        let mut active = cx
            .module
            .memories
            .iter()
            .filter(|t| cx.used.memories.contains(&t.id()))
            .map(|m| (m.id(), m))
            .collect::<Vec<_>>();
        active.sort_by_key(|pair| pair.0);

        for (_memory_id, memory) in active {
            segments.extend(memory.emit_data(cx.indices));
        }

        // After all the active segments are added add passive segments next. We
        // may want to sort this more intelligently in the future. Othrewise
        // emitting a segment here is in general much simpler than above as we
        // know there are no holes.
        for (id, segment) in self.arena.iter() {
            if !cx.used.data.contains(&id) {
                continue;
            }
            cx.indices.push_data(id);
            segments.push(elements::DataSegment::new(
                0,
                None,
                segment.value.clone(),
                true,
            ));
        }

        if segments.len() > 0 {
            let elements = elements::DataSection::with_entries(segments);
            let elements = elements::Section::Data(elements);
            cx.dst.sections_mut().push(elements);
        }
    }
}
