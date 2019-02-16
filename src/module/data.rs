//! Data segments within a wasm module.

use crate::emit::{Emit, EmitContext, Section};
use crate::ir::Value;
use crate::parse::IndicesToIds;
use crate::passes::Used;
use crate::tombstone_arena::{Id, TombstoneArena};
use crate::{InitExpr, Module, Result, ValType};
use failure::{bail, ResultExt};

/// A passive element segment identifier
pub type DataId = Id<Data>;

/// A passive data segment
#[derive(Debug)]
pub struct Data {
    id: DataId,

    /// Initially set to `false` when reserving `Data` entries while parsing,
    /// and then read during validation to ensure that `memory.init`
    /// instructions only reference valid `Data` entries (ones that are actually
    /// passive).
    ///
    /// From a user-facing perspective, though, this is always `true` for all
    /// instances of `Data` as `Data` only makes sense as a passive segment.
    pub(crate) passive: bool,

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
    arena: TombstoneArena<Data>,
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

    /// Adds a new passive data segment with the specified contents
    pub fn add(&mut self, value: Vec<u8>) -> DataId {
        self.arena.alloc_with_id(|id| Data {
            id,
            value,
            passive: true,
        })
    }

    pub(crate) fn iter_used<'a>(&'a self, used: &'a Used) -> impl Iterator<Item = &'a Data> + 'a {
        self.iter().filter(move |data| used.data.contains(&data.id))
    }

    // Note that this is inaccordance with the upstream bulk memory proposal to
    // WebAssembly and isn't currently part of the WebAssembly standard.
    pub(crate) fn emit_data_count(&self, cx: &mut EmitContext) {
        let mut count = 0;

        // The first elements in the index space will be active segments, so
        // count all those first. Here we push an "invalid id", or one we know
        // isn't ever used, as the low data segments.
        for mem in cx.module.memories.iter_used(cx.used) {
            count += mem.emit_data().count();
        }

        // After the active data segments, assign indices to the passive data
        // segments.
        let mut any_passive = false;
        for data in self.iter_used(cx.used) {
            cx.indices.set_data_index(data.id(), count as u32);
            count += 1;
            any_passive = true;
        }

        if any_passive {
            cx.start_section(Section::DataCount).encoder.usize(count);
        }
    }
}

impl Module {
    /// Called when we see the data section section to create an id for all data
    /// indices
    ///
    /// Note that during function parsing all data indices less than `count` are
    /// considered valid, and it's only afterwards that we discover whether
    /// they're actually passive or not, and that property is checked during
    /// validation.
    pub(crate) fn reserve_data(&mut self, count: u32, ids: &mut IndicesToIds) {
        for _ in 0..count {
            ids.push_data(self.data.arena.alloc_with_id(|id| Data {
                id,
                passive: false, // this'll get set to `true` when parsing data
                value: Vec::new(),
            }));
        }
    }

    /// Parses a raw wasm section into a fully-formed `ModuleData` instance.
    pub(crate) fn parse_data(
        &mut self,
        section: wasmparser::DataSectionReader,
        ids: &IndicesToIds,
        data_count: Option<u32>,
    ) -> Result<()> {
        log::debug!("parse data section");
        if let Some(count) = data_count {
            if count != section.get_count() {
                bail!("data count section mismatches actual data section");
            }
        }
        for (i, segment) in section.into_iter().enumerate() {
            let segment = segment?;

            match segment.kind {
                wasmparser::DataKind::Passive => {
                    let id = ids.get_data(i as u32)?;
                    let data = self.data.get_mut(id);
                    data.value = segment.data.to_vec();
                    data.passive = true;
                }
                wasmparser::DataKind::Active {
                    memory_index,
                    init_expr,
                } => {
                    let memory = ids.get_memory(memory_index)?;
                    let value = segment.data.to_vec();
                    let memory = self.memories.get_mut(memory);

                    let offset = InitExpr::eval(&init_expr, ids)
                        .with_context(|_e| format!("in segment {}", i))?;
                    match offset {
                        InitExpr::Value(Value::I32(n)) => {
                            memory.data.add_absolute(n as u32, value);
                        }
                        InitExpr::Global(global) if self.globals.get(global).ty == ValType::I32 => {
                            memory.data.add_relative(global, value);
                        }
                        _ => bail!("non-i32 constant in segment {}", i),
                    }
                }
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
            .iter_used(cx.used)
            .flat_map(|memory| memory.emit_data().map(move |data| (memory.id(), data)))
            .collect::<Vec<_>>();
        active.sort_by_key(|pair| pair.0);
        let passive = self.iter_used(cx.used).count();

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
