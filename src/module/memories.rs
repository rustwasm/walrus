//! Memories used in a wasm module.

use crate::emit::{Emit, EmitContext, Section};
use crate::map::IdHashSet;
use crate::parse::IndicesToIds;
use crate::tombstone_arena::{Id, Tombstone, TombstoneArena};
use crate::{Data, ImportId, Module, Result};

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

impl Emit for Memory {
    fn emit(&self, cx: &mut EmitContext) {
        if let Some(max) = self.maximum {
            cx.encoder.byte(if self.shared { 0x03 } else { 0x01 });
            cx.encoder.u32(self.initial);
            cx.encoder.u32(max);
        } else {
            cx.encoder.byte(0x00);
            cx.encoder.u32(self.initial);
        }
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
            let id = self
                .memories
                .add_local(m.shared, m.limits.initial, m.limits.maximum);
            ids.push_memory(id);
        }
        Ok(())
    }
}

impl Emit for ModuleMemories {
    fn emit(&self, cx: &mut EmitContext) {
        log::debug!("emit memory section");
        // imported memories are emitted earlier
        let memories = self.iter().filter(|m| m.import.is_none()).count();
        if memories == 0 {
            return;
        }

        let mut cx = cx.start_section(Section::Memory);
        cx.encoder.usize(memories);
        for memory in self.iter().filter(|m| m.import.is_none()) {
            cx.indices.push_memory(memory.id());
            memory.emit(&mut cx);
        }
    }
}
