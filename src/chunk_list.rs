//! TODO

use std::slice;

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

    /// Construct a new chunk list with the given head elements and tail elements.
    pub fn with_head_and_tail(head: Vec<T>, tail: &'a ChunkList<'a, T>) -> ChunkList<'a, T> {
        ChunkList {
            head,
            tail: Some(tail),
        }
    }

    /// Get a reference to the idx^th item in this chunk list.
    pub fn get(&self, mut idx: usize) -> Option<&T> {
        let mut chunk = self;
        while chunk.head.len() <= idx {
            idx = idx - chunk.head.len();
            chunk = chunk.tail?;
        }
        chunk.head.get(idx)
    }

    /// Get the total length of this chunk list.
    pub fn len(&self) -> usize {
        let mut chunk = self;
        let mut len = 0;
        loop {
            len += chunk.head.len();
            match chunk.tail {
                None => return len,
                Some(tail) => {
                    chunk = tail;
                }
            }
        }
    }

    /// Iterate over the items in this chunk list.
    pub fn iter<'me>(&'me self) -> Iter<'me, 'a, T> {
        Iter {
            iter: self.head.iter(),
            list: self,
        }
    }
}

/// An iterator over a chunk list.
#[derive(Clone, Debug)]
pub struct Iter<'a, 'b, T> {
    iter: slice::Iter<'a, T>,
    list: &'a ChunkList<'b, T>,
}

impl<'a, 'b, T> Iterator for Iter<'a, 'b, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<&'a T> {
        loop {
            if let Some(x) = self.iter.next() {
                return Some(x);
            }

            self.list = match self.list.tail {
                None => return None,
                Some(t) => t
            };
            self.iter = self.list.head.iter();
        }
    }
}
