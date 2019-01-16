//! Memories used in a wasm module.

use crate::module::emit::{Emit, IdsToIndices};
use crate::passes::Used;
use id_arena::{Arena, Id};
use parity_wasm::elements;
use std::collections::HashMap;

/// The id of a memory.
pub type MemoryId = Id<Memory>;

/// A memory in the wasm.
#[derive(Debug)]
pub struct Memory {
    id: MemoryId,
    /// Is this memory shared?
    pub shared: bool,
    /// The size limits for this memory.
    pub limits: elements::ResizableLimits,
}

/// The set of memories in this module.
#[derive(Debug, Default)]
pub struct ModuleMemories {
    memories: Arena<Memory>,
    index_to_memory_id: HashMap<u32, MemoryId>,
}

impl ModuleMemories {
    /// Construct a new, empty set of memories for a module.
    pub fn parse(memory_section: &elements::MemorySection) -> ModuleMemories {
        let capacity = memory_section.entries().len();
        let mut memories = ModuleMemories {
            memories: Arena::with_capacity(capacity),
            index_to_memory_id: HashMap::with_capacity(capacity),
        };

        for (i, m) in memory_section.entries().iter().enumerate() {
            memories.add_memory_for_index(i as u32, false, m.limits().clone());
        }

        memories
    }

    /// Get the memory for the given index in the input wasm, if any exists.
    pub fn memory_for_index(&self, index: u32) -> Option<MemoryId> {
        self.index_to_memory_id.get(&index).cloned()
    }

    /// Create the memory for this function at the given index.
    fn add_memory_for_index(
        &mut self,
        index: u32,
        shared: bool,
        limits: elements::ResizableLimits,
    ) -> MemoryId {
        assert!(!self.index_to_memory_id.contains_key(&index));
        let id = self.new_memory(shared, limits);
        self.index_to_memory_id.insert(index, id);
        id
    }

    /// Construct a new memory, that does not originate from any of the input
    /// wasm memories.
    pub fn new_memory(&mut self, shared: bool, limits: elements::ResizableLimits) -> MemoryId {
        let id = self.memories.next_id();
        let id2 = self.memories.alloc(Memory { id, shared, limits });
        debug_assert_eq!(id, id2);
        id
    }
}

impl Emit for ModuleMemories {
    type Extra = ();

    fn emit(&self, _: &(), used: &Used, module: &mut elements::Module, indices: &mut IdsToIndices) {
        if used.memories.is_empty() {
            return;
        }

        let mut memories = Vec::with_capacity(used.memories.len());

        let import_count = module.import_count(elements::ImportCountType::Memory) as u32;

        for (id, mem) in &self.memories {
            if !used.memories.contains(&id) {
                continue;
            }

            indices.set_memory_index(id, memories.len() as u32 + import_count);
            let memory =
                elements::MemoryType::new(mem.limits.initial(), mem.limits.maximum(), mem.shared);
            memories.push(memory);
        }

        let memories = elements::MemorySection::with_entries(memories);
        let memories = elements::Section::Memory(memories);
        module.sections_mut().push(memories);
    }
}
