//! Memories used in a wasm module.

use crate::const_value::Const;
use crate::emit::{Emit, EmitContext, IdsToIndices};
use crate::ir::Value;
use crate::module::globals::GlobalId;
use crate::module::imports::ImportId;
use crate::module::parse::IndicesToIds;
use crate::module::Module;
use id_arena::{Arena, Id};
use parity_wasm::elements;

/// The id of a memory.
pub type MemoryId = Id<Memory>;

/// A memory in the wasm.
#[derive(Debug)]
pub struct Memory {
    id: MemoryId,
    /// Is this memory shared?
    pub shared: bool,
    /// The initial page size for this memory
    pub initial: u32,
    /// The maximum page size for this memory
    pub maximum: Option<u32>,
    /// Whether or not this memory is imported, and if so from where
    pub import: Option<ImportId>,
    /// Data that will be used to initialize this memory chunk, with known
    /// static offsets
    pub data: MemoryData,
}

/// An abstraction for the initialization values of a `Memory`.
///
/// This houses all the data sections of a wasm executable that as associated
/// with this `Memory`.
#[derive(Debug, Default)]
pub struct MemoryData {
    absolute: Vec<(u32, Vec<u8>)>,
    relative: Vec<(GlobalId, Vec<u8>)>,
}

impl Memory {
    /// Return the id of this memory
    pub fn id(&self) -> MemoryId {
        self.id
    }

    pub(crate) fn emit_data<'a>(
        &'a self,
        indices: &'a IdsToIndices,
    ) -> impl Iterator<Item = elements::DataSegment> + 'a {
        let index = indices.get_memory_index(self.id);
        let absolute = self.data.absolute.iter().map(move |(pos, data)| {
            elements::DataSegment::new(
                index,
                Some(Const::Value(Value::I32(*pos as i32)).emit_instructions(indices)),
                data.to_vec(),
                false,
            )
        });
        let relative = self.data.relative.iter().map(move |(id, data)| {
            elements::DataSegment::new(
                index,
                Some(Const::Global(*id).emit_instructions(indices)),
                data.to_vec(),
                false,
            )
        });
        absolute.chain(relative)
    }
}

/// The set of memories in this module.
#[derive(Debug, Default)]
pub struct ModuleMemories {
    arena: Arena<Memory>,
}

impl ModuleMemories {
    /// Add an imported memory
    pub fn add_import(
        &mut self,
        shared: bool,
        initial: u32,
        maximum: Option<u32>,
        import: ImportId,
    ) -> MemoryId {
        let id = self.arena.next_id();
        let id2 = self.arena.alloc(Memory {
            id,
            shared,
            initial,
            maximum,
            import: Some(import),
            data: MemoryData::default(),
        });
        debug_assert_eq!(id, id2);
        id
    }

    /// Construct a new memory, that does not originate from any of the input
    /// wasm memories.
    pub fn add_local(&mut self, shared: bool, initial: u32, maximum: Option<u32>) -> MemoryId {
        let id = self.arena.next_id();
        let id2 = self.arena.alloc(Memory {
            id,
            shared,
            initial,
            maximum,
            import: None,
            data: MemoryData::default(),
        });
        debug_assert_eq!(id, id2);
        id
    }

    /// Gets a reference to a memory given its id
    pub fn get(&self, id: MemoryId) -> &Memory {
        &self.arena[id]
    }

    /// Gets a reference to a memory given its id
    pub fn get_mut(&mut self, id: MemoryId) -> &mut Memory {
        &mut self.arena[id]
    }

    /// Get a shared reference to this module's memories.
    pub fn iter(&self) -> impl Iterator<Item = &Memory> {
        self.arena.iter().map(|(_, f)| f)
    }
}

impl Module {
    /// Construct a new, empty set of memories for a module.
    pub(crate) fn parse_memories(
        &mut self,
        section: &elements::MemorySection,
        ids: &mut IndicesToIds,
    ) {
        for m in section.entries() {
            let id = self
                .memories
                .add_local(false, m.limits().initial(), m.limits().maximum());
            ids.push_memory(id);
        }
    }
}

impl Emit for ModuleMemories {
    fn emit(&self, cx: &mut EmitContext) {
        let mut memories = Vec::with_capacity(cx.used.memories.len());

        for (id, mem) in &self.arena {
            if !cx.used.memories.contains(&id) {
                continue;
            }
            if mem.import.is_some() {
                continue; // already emitted in the import section
            }

            cx.indices.push_memory(id);
            let memory = elements::MemoryType::new(mem.initial, mem.maximum, mem.shared);
            memories.push(memory);
        }

        if !memories.is_empty() {
            let memories = elements::MemorySection::with_entries(memories);
            let memories = elements::Section::Memory(memories);
            cx.dst.sections_mut().push(memories);
        }
    }
}

impl MemoryData {
    /// Adds a new chunk of data in this `ModuleData` at an absolute address
    pub fn add_absolute(&mut self, pos: u32, data: Vec<u8>) {
        self.absolute.push((pos, data));
    }

    /// Adds a new chunk of data in this `ModuleData` at a relative address
    pub fn add_relative(&mut self, id: GlobalId, data: Vec<u8>) {
        self.relative.push((id, data));
    }
}
