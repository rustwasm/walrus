//! TODO

use std::fmt;
use std::hash::{Hash, Hasher};
use std::iter;
use std::marker::PhantomData;
use std::ops;
use std::slice;

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

impl<T> PartialEq for Id<T> {
    fn eq(&self, rhs: &Self) -> bool {
        self.idx == rhs.idx
    }
}

impl<T> Eq for Id<T> {}

impl<T> Hash for Id<T> {
    fn hash<H: Hasher>(&self, h: &mut H) {
        self.idx.hash(h)
    }
}

impl<T> From<Id<T>> for usize {
    fn from(id: Id<T>) -> usize {
        id.idx
    }
}

impl<T> From<usize> for Id<T> {
    fn from(idx: usize) -> Id<T> {
        Id {
            idx,
            _ty: PhantomData,
        }
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

    /// Iterate over this arena's items and their ids.
    pub fn iter(&self) -> Iter<T> {
        IntoIterator::into_iter(self)
    }

    /// Get the length of this arena.
    pub fn len(&self) -> usize {
        self.items.len()
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

/// TODO
#[derive(Debug)]
pub struct Iter<'a, T: 'a> {
    iter: iter::Enumerate<slice::Iter<'a, T>>,
}

impl<'a, T: 'a> Iterator for Iter<'a, T> {
    type Item = (Id<T>, &'a T);
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|(idx, item)| {
            (
                Id {
                    idx,
                    _ty: PhantomData,
                },
                item,
            )
        })
    }
}

impl<'a, T> IntoIterator for &'a Arena<T> {
    type Item = (Id<T>, &'a T);
    type IntoIter = Iter<'a, T>;

    fn into_iter(self) -> Iter<'a, T> {
        Iter {
            iter: self.items.iter().enumerate(),
        }
    }
}
