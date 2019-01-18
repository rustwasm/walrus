//! Memories used in a wasm module.

use crate::emit::{Emit, EmitContext};
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
