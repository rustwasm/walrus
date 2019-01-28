//! Data segments within a wasm module.

use crate::const_value::Const;
use crate::emit::{Emit, EmitContext, Section};
use crate::error::Result;
use crate::ir::Value;
use crate::module::Module;
use crate::parse::IndicesToIds;
use crate::passes::Used;
use crate::ty::ValType;
use failure::{bail, ResultExt};
use id_arena::{Arena, Id};

/// A passive element segment identifier
pub type DataId = Id<Data>;

/// A passive data segment
#[derive(Debug)]
pub struct Data {
    id: DataId,
    /// The payload of this passive data segment
    pub value: Vec<u8>,
}

impl Data {
    /// Returns the id of this passive data segment
    pub fn id(&self) -> DataId {
        self.id
    }
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

    pub(crate) fn iter_used<'a>(&'a self, used: &'a Used) -> impl Iterator<Item = &'a Data> + 'a {
        self.iter().filter(move |data| used.data.contains(&data.id))
    }

    // Note that this is inaccordance with the upstream bulk memory proposal to
    // WebAssembly and isn't currently part of the WebAssembly standard.
    pub(crate) fn emit_data_count(&self, cx: &mut EmitContext) {
        let mut count = 0;

        // Assign indices before we start translating functions to ensure that
        // references to data have all been assigned
        for data in self.iter_used(cx.used) {
            cx.indices.push_data(data.id());
            count += 1;
        }

        if count != 0 {
            cx.start_section(Section::DataCount).encoder.usize(count);
        }
    }
}

impl Module {
    /// Parses a raw wasm section into a fully-formed `ModuleData` instance.
    pub(crate) fn parse_data(
        &mut self,
        section: wasmparser::DataSectionReader,
        ids: &mut IndicesToIds,
    ) -> Result<()> {
        log::debug!("parse data section");
        for (i, segment) in section.into_iter().enumerate() {
            let segment = segment?;

            // TODO: upstream passive support
            // if segment.passive() {
            //     let id = self.data.arena.next_id();
            //     self.data.arena.alloc(Data {
            //         value: segment.value().to_vec(),
            //     });
            //     ids.push_data(id);
            //     continue;
            // }

            let memory = ids.get_memory(segment.memory_index)?;
            let value = segment.data.to_vec();
            let memory = self.memories.get_mut(memory);

            let offset = Const::eval(&segment.init_expr, ids)
                .with_context(|_e| format!("in segment {}", i))?;
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
        log::debug!("emit data section");
        // Sort table ids for a deterministic emission for now, eventually we
        // may want some sort of sorting heuristic here.
        let mut active = cx
            .module
            .memories
            .iter()
            .filter(|m| cx.used.memories.contains(&m.id()))
            .flat_map(|memory| memory.emit_data().map(move |data| (memory.id(), data)))
            .collect::<Vec<_>>();
        active.sort_by_key(|pair| pair.0);
        let passive = self
            .arena
            .iter()
            .filter(|(id, _seg)| cx.used.data.contains(id))
            .count();

        if active.len() == 0 && passive == 0 {
            return;
        }

        let mut cx = cx.start_section(Section::Data);
        cx.encoder.usize(active.len() + passive);

        // The encodings here are with respect to the bulk memory proposal, but
        // should be backwards compatible with the current stable WebAssembly
        // spec so long as only memory 0 is used.
        for (id, (offset, data)) in active {
            let index = cx.indices.get_memory_index(id);
            if index == 0 {
                cx.encoder.byte(0x00);
            } else {
                cx.encoder.byte(0x02);
                cx.encoder.u32(index);
            }
            offset.emit(&mut cx);
            cx.encoder.bytes(data);
        }

        // After all the active segments are added add passive segments next. We
        // may want to sort this more intelligently in the future. Otherwise
        // emitting a segment here is in general much simpler than above as we
        // know there are no holes.
        for data in self.iter_used(cx.used) {
            cx.encoder.byte(0x01);
            cx.encoder.bytes(&data.value);
        }
    }
}
