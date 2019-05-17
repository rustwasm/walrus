//! Working with custom sections.

use crate::tombstone_arena::{Id, Tombstone, TombstoneArena};
use std::any::Any;
use std::borrow::Cow;
use std::fmt::Debug;
use std::marker::PhantomData;

// ModuleCustomSections

/// A trait for implementing [custom
/// sections](https://webassembly.github.io/spec/core/binary/modules.html#binary-customsec).
///
/// Custom sections are added to a `walrus::Module` via
/// `my_module.custom_sections.add(my_custom_section)`.
pub trait CustomSection: Any + Debug + Send + Sync {
    /// Get this custom section's name.
    ///
    /// For example ".debug_info" for one of the DWARF custom sections or "name"
    /// for the [names custom
    /// section](https://webassembly.github.io/spec/core/appendix/custom.html#name-section).
    fn name(&self) -> &str;

    /// Get the data payload for this custom section.
    ///
    /// This should *not* include the section header with id=0, the custom
    /// section's name, or the count of how many bytes are in the
    /// payload. `walrus` will handle these for you.
    fn data(&self) -> Cow<[u8]>;
}

// We have to have this so that we can convert `CustomSection` trait objects
// into `Any` trait objects.
trait CustomSectionAny: Any + CustomSection {
    fn impl_as_custom_section(&self) -> &dyn CustomSection;
    fn impl_into_any(self: Box<Self>) -> Box<dyn Any + 'static>;
    fn impl_as_any(&self) -> &dyn Any;
    fn impl_as_any_mut(&mut self) -> &mut dyn Any;
}

impl<T> CustomSectionAny for T
where
    T: CustomSection,
{
    fn impl_as_custom_section(&self) -> &dyn CustomSection {
        self
    }

    fn impl_into_any(self: Box<Self>) -> Box<dyn Any + 'static> {
        self
    }

    fn impl_as_any(&self) -> &dyn Any {
        self
    }

    fn impl_as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

impl dyn CustomSectionAny {
    fn into_any(self: Box<Self>) -> Box<dyn Any + 'static> {
        self.impl_into_any()
    }
    fn as_any(&self) -> &Any {
        self.impl_as_any()
    }
    fn as_any_mut(&mut self) -> &mut Any {
        self.impl_as_any_mut()
    }
}

/// A raw, unparsed custom section.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RawCustomSection {
    /// This custom section's name.
    pub name: String,

    /// This custom section's raw data.
    pub data: Vec<u8>,
}

impl CustomSection for RawCustomSection {
    fn name(&self) -> &str {
        &self.name
    }

    fn data(&self) -> Cow<[u8]> {
        self.data.as_slice().into()
    }
}

/// The id of some `CustomSection` instance in a `ModuleCustomSections`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CustomSectionId(Id<Option<Box<dyn CustomSectionAny>>>);

/// The id of a `CustomSection` instance with a statically-known type in a
/// `ModuleCustomSections`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypedCustomSectionId<T: CustomSection> {
    id: CustomSectionId,
    _phantom: PhantomData<T>,
}

impl<T> From<TypedCustomSectionId<T>> for CustomSectionId
where
    T: CustomSection,
{
    #[inline]
    fn from(a: TypedCustomSectionId<T>) -> Self {
        a.id
    }
}

impl Tombstone for Option<Box<dyn CustomSectionAny>> {
    fn on_delete(&mut self) {
        *self = None;
    }
}

/// A collection of custom sections inside a Wasm module.
///
/// To add parse and emit your own custom section:
///
/// * Define a `MyCustomSection` type to represent your custom section.
///
/// * Implement the `walrus::CustomSection` trait for your `MyCustomSection`
///   type.
///
/// * When working with a `walrus::Module` named `my_module`, use
///   `my_module.customs.take_raw("my_custom_section_name")` to take ownership
///   of the raw custom section if it is present in the Wasm module.
///
/// * Parse that into your own `MyCustomSection` type in whatever way is
///   appropriate.
///
/// * Do whatever kinds of inspection and manipulation of your `MyCustomSection`
///   type you need to do.
///
/// * Use `my_module.customs.add(my_custom_section)` to add the custom section
///   back into the module, so `walrus` can emit the processed/updated version
///   of the custom section.
#[derive(Debug, Default)]
pub struct ModuleCustomSections {
    arena: TombstoneArena<Option<Box<dyn CustomSectionAny>>>,
}

impl ModuleCustomSections {
    /// Add a new custom section to the module.
    pub fn add<T>(&mut self, custom_section: T) -> TypedCustomSectionId<T>
    where
        T: CustomSection,
    {
        let id = self
            .arena
            .alloc(Some(Box::new(custom_section) as Box<dyn CustomSectionAny>));
        TypedCustomSectionId {
            id: CustomSectionId(id),
            _phantom: PhantomData,
        }
    }

    /// Remove a custom section from the module.
    pub fn delete<I>(&mut self, id: I)
    where
        I: Into<CustomSectionId>,
    {
        let id = id.into().0;
        self.arena.delete(id);
    }

    /// Take a raw, unparsed custom section out of this module.
    pub fn take_raw(&mut self, name: &str) -> Option<RawCustomSection> {
        let id = self
            .arena
            .iter()
            .filter(|(_id, s)| {
                if let Some(s) = s {
                    s.as_any().is::<RawCustomSection>() && s.name() == name
                } else {
                    false
                }
            })
            .map(|(id, _)| id)
            .next()?;
        let section = self.arena[id].take().unwrap();
        self.arena.delete(id);
        let raw = section.into_any().downcast::<RawCustomSection>().unwrap();
        Some(*raw)
    }

    /// Get a shared reference to a custom section that is in this
    /// `ModuleCustomSections`.
    ///
    /// # Panics
    ///
    /// Panics if the custom section associated with the given `id` has been
    /// deleted or taken.
    pub fn get<T>(&self, id: TypedCustomSectionId<T>) -> &T
    where
        T: CustomSection,
    {
        self.try_get(id.into()).unwrap()
    }

    /// Try and get a shared reference to a custom section that is in this
    /// `ModuleCustomSections`.
    ///
    /// Returns `None` if the section associated with the given `id` has been
    /// taken or deleted, or if it is present but not an instance of `T`.
    pub fn try_get<T>(&self, id: CustomSectionId) -> Option<&T>
    where
        T: CustomSection,
    {
        self.arena
            .get(id.0)
            .and_then(|s| s.as_ref().unwrap().as_any().downcast_ref::<T>())
    }

    /// Get an exclusive reference to a custom section that is in this
    /// `ModuleCustomSections`.
    ///
    /// # Panics
    ///
    /// Panics if the custom section associated with the given `id` has been
    /// deleted or taken.
    pub fn get_mut<T>(&mut self, id: TypedCustomSectionId<T>) -> &mut T
    where
        T: CustomSection,
    {
        self.try_get_mut(id.into()).unwrap()
    }

    /// Try and get an exclusive reference to a custom section that is in this
    /// `ModuleCustomSections`.
    ///
    /// Returns `None` if the section associated with the given `id` has been
    /// taken or deleted, or if it is present but not an instance of `T`.
    pub fn try_get_mut<T>(&mut self, id: CustomSectionId) -> Option<&mut T>
    where
        T: CustomSection,
    {
        self.arena
            .get_mut(id.0)
            .and_then(|s| s.as_mut().unwrap().as_any_mut().downcast_mut::<T>())
    }

    /// Get an untyped, shared reference to a custom section that is in this
    /// `ModuleCustomSections`.
    ///
    /// # Panics
    ///
    /// Panics if the custom section associated with the given `id` has been
    /// deleted or taken.
    pub fn get_untyped(&self, id: CustomSectionId) -> &dyn CustomSection {
        self.try_get_untyped(id).unwrap()
    }

    /// Get an untyped, shared reference to a custom section that is in this
    /// `ModuleCustomSections`, or return `None` if the section has been deleted
    /// or taken.
    pub fn try_get_untyped(&self, id: CustomSectionId) -> Option<&dyn CustomSection> {
        self.arena
            .get(id.0)
            .map(|s| s.as_ref().unwrap().impl_as_custom_section())
    }

    /// Iterate over custom sections.
    pub fn iter(&self) -> impl Iterator<Item = (CustomSectionId, &dyn CustomSection)> {
        self.arena.iter().flat_map(|(id, s)| {
            if let Some(s) = s.as_ref() {
                Some((CustomSectionId(id), s.impl_as_custom_section()))
            } else {
                None
            }
        })
    }
}
