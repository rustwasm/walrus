//! Memories used in a wasm module.

use crate::emit::{Emit, EmitContext};
use crate::map::IdHashSet;
use crate::parse::IndicesToIds;
use crate::tombstone_arena::{Id, Tombstone, TombstoneArena};
use crate::{Data, ImportId, Module, Result};
use anyhow::bail;

/// The id of a memory.
pub type MemoryId = Id<Memory>;

/// A memory in the wasm.
#[derive(Debug)]
pub struct Memory {
    id: MemoryId,
    /// Is this memory shared?
    pub shared: bool,
    /// The initial page size for this memory.
    pub initial: u32,
    /// The maximum page size for this memory.
    pub maximum: Option<u32>,
    /// Whether or not this memory is imported, and if so from where.
    pub import: Option<ImportId>,
    /// Active data segments that will be used to initialize this memory.
    pub data_segments: IdHashSet<Data>,
    /// The name of this memory, used for debugging purposes in the `name`
    /// custom section.
    pub name: Option<String>,
}

impl Tombstone for Memory {
    fn on_delete(&mut self) {
        self.data_segments = Default::default();
    }
}

impl Memory {
    /// Return the id of this memory
    pub fn id(&self) -> MemoryId {
        self.id
    }
}

/// The set of memories in this module.
#[derive(Debug, Default)]
pub struct ModuleMemories {
    arena: TombstoneArena<Memory>,
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
            data_segments: Default::default(),
            name: None,
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
            data_segments: Default::default(),
            name: None,
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

    /// Removes a memory from this module.
    ///
    /// It is up to you to ensure that any potential references to the deleted
    /// memory are also removed, eg `mem.load` expressions and exports.
    pub fn delete(&mut self, id: MemoryId) {
        self.arena.delete(id);
    }

    /// Get a shared reference to this module's memories.
    pub fn iter(&self) -> impl Iterator<Item = &Memory> {
        self.arena.iter().map(|(_, f)| f)
    }

    /// Get a mutable reference to this module's memories.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Memory> {
        self.arena.iter_mut().map(|(_, f)| f)
    }

    /// Get the number of memories in this module
    pub fn len(&self) -> usize {
        self.arena.len()
    }
}

impl Module {
    /// Construct a new, empty set of memories for a module.
    pub(crate) fn parse_memories(
        &mut self,
        section: wasmparser::MemorySectionReader,
        ids: &mut IndicesToIds,
    ) -> Result<()> {
        log::debug!("parse memory section");
        for m in section {
            let m = m?;
            if m.memory64 {
                bail!("64-bit memories not supported")
            };
            let id =
                self.memories
                    .add_local(m.shared, m.initial as u32, m.maximum.map(|m| m as u32));
            ids.push_memory(id);
        }
        Ok(())
    }
}

impl Emit for ModuleMemories {
    fn emit(&self, cx: &mut EmitContext) {
        log::debug!("emit memory section");

        let mut wasm_memory_section = wasm_encoder::MemorySection::new();

        // imported memories are emitted earlier
        let memories = self.iter().filter(|m| m.import.is_none()).count();
        if memories == 0 {
            return;
        }

        for memory in self.iter().filter(|m| m.import.is_none()) {
            cx.indices.push_memory(memory.id());

            wasm_memory_section.memory(wasm_encoder::MemoryType {
                minimum: memory.initial as u64,
                maximum: memory.maximum.map(|v| v as u64),
                memory64: false,
                shared: memory.shared,
            });
        }

        cx.wasm_module.section(&wasm_memory_section);
    }
}

#[cfg(test)]
mod tests {
    use crate::Module;

    #[test]
    fn memories_len() {
        let mut module = Module::default();
        assert_eq!(module.memories.len(), 0);

        module.memories.add_local(false, 0, Some(1024));
        assert_eq!(module.memories.len(), 1);

        module.memories.add_local(true, 1024, Some(2048));
        assert_eq!(module.memories.len(), 2);
    }
}
