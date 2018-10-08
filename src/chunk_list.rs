//! TODO

/// TODO
#[derive(Debug)]
pub struct ChunkList<'a, T: 'a> {
    head: Vec<T>,
    tail: Option<&'a ChunkList<'a, T>>,
}

impl<'a, T: 'a> ChunkList<'a, T> {
    /// Construct a new, empty chunk list.
    pub fn new() -> ChunkList<'a, T> {
        ChunkList {
            head: vec![],
            tail: None,
        }
    }

    /// Construct a new chunk list with the given elements.
    pub fn with_head(head: Vec<T>) -> ChunkList<'a, T> {
        ChunkList { head, tail: None }
    }

    /// Construct a new chunk list whose elements will be prepended in front of
    /// the given tail.
    pub fn with_tail(tail: &'a ChunkList<'a, T>) -> ChunkList<'a, T> {
        ChunkList {
            head: vec![],
            tail: Some(tail),
        }
    }

    /// Get a reference to the idx^th item in this chunk list.
    pub fn get(&self, mut idx: usize) -> Option<&T> {
        let mut chunk = self;
        while chunk.head.len() < idx {
            idx = idx - chunk.head.len();
            chunk = chunk.tail?;
        }
        chunk.head.get(idx)
    }
}
