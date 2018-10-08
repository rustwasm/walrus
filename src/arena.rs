//! TODO

use std::fmt;
use std::marker::PhantomData;
use std::ops;

/// TODO
pub struct Id<T> {
    idx: usize,
    _ty: PhantomData<*const T>,
}

impl<T> fmt::Debug for Id<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Id").field("idx", &self.idx).finish()
    }
}

impl<T> Copy for Id<T> {}

impl<T> Clone for Id<T> {
    #[inline]
    fn clone(&self) -> Id<T> {
        *self
    }
}

/// TODO
#[derive(Debug)]
pub struct Arena<T> {
    items: Vec<T>,
}

impl<T> Arena<T> {
    /// TODO
    pub fn new() -> Arena<T> {
        Arena { items: Vec::new() }
    }

    /// TODO
    pub fn alloc(&mut self, item: T) -> Id<T> {
        let idx = self.items.len();
        self.items.push(item);
        Id {
            idx,
            _ty: PhantomData,
        }
    }

    /// TODO
    pub fn get(&self, id: Id<T>) -> Option<&T> {
        self.items.get(id.idx)
    }

    /// TODO
    pub fn get_mut(&mut self, id: Id<T>) -> Option<&mut T> {
        self.items.get_mut(id.idx)
    }
}

impl<T> ops::Index<Id<T>> for Arena<T> {
    type Output = T;

    fn index(&self, id: Id<T>) -> &T {
        &self.items[id.idx]
    }
}

impl<T> ops::IndexMut<Id<T>> for Arena<T> {
    fn index_mut(&mut self, id: Id<T>) -> &mut T {
        &mut self.items[id.idx]
    }
}
