//! Working with custom sections.

use crate::tombstone_arena::{Id, Tombstone, TombstoneArena};
use std::any::Any;
use std::borrow::Cow;
use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};
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
#[doc(hidden)]
pub trait CustomSectionAny: Any + CustomSection {
    fn impl_as_custom_section(&self) -> &dyn CustomSection;
    fn impl_as_custom_section_mut(&mut self) -> &mut dyn CustomSection;
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

    fn impl_as_custom_section_mut(&mut self) -> &mut dyn CustomSection {
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

/// A common trait for custom section identifiers.
///
/// Used in the `ModuleCustomSections::get` family of methods to perform type
/// conversions from `dyn CustomSection` trait objects into the concrete
/// `Self::CustomSection` type instance.
///
/// You shouldn't implement this yourself. Instead use `TypedCustomSectionId<T>`
/// or `UntypedCustomSectionId`.
pub trait CustomSectionId {
    /// The concrete custom section type that this id gets out of a
    /// `ModuleCustomSections`.
    type CustomSection: ?Sized;

    #[doc(hidden)]
    fn into_inner_id(self) -> Id<Option<Box<dyn CustomSectionAny>>>;
    #[doc(hidden)]
    fn section(s: &dyn CustomSectionAny) -> Option<&Self::CustomSection>;
    #[doc(hidden)]
    fn section_mut(s: &mut dyn CustomSectionAny) -> Option<&mut Self::CustomSection>;
}

/// The id of some `CustomSection` instance in a `ModuleCustomSections`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UntypedCustomSectionId(Id<Option<Box<dyn CustomSectionAny>>>);

impl CustomSectionId for UntypedCustomSectionId {
    type CustomSection = dyn CustomSection;

    fn into_inner_id(self) -> Id<Option<Box<dyn CustomSectionAny>>> {
        self.0
    }

    fn section(s: &dyn CustomSectionAny) -> Option<&dyn CustomSection> {
        Some(s.impl_as_custom_section())
    }

    fn section_mut(s: &mut dyn CustomSectionAny) -> Option<&mut dyn CustomSection> {
        Some(s.impl_as_custom_section_mut())
    }
}

/// The id of a `CustomSection` instance with a statically-known type in a
/// `ModuleCustomSections`.
pub struct TypedCustomSectionId<T>
where
    T: CustomSection,
{
    id: UntypedCustomSectionId,
    _phantom: PhantomData<T>,
}

impl<T> Debug for TypedCustomSectionId<T>
where
    T: CustomSection,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_tuple("TypedCustomSectionId")
            .field(&self.id)
            .finish()
    }
}

impl<T> Clone for TypedCustomSectionId<T>
where
    T: CustomSection,
{
    fn clone(&self) -> Self {
        TypedCustomSectionId {
            id: self.id,
            _phantom: PhantomData,
        }
    }
}

impl<T> Copy for TypedCustomSectionId<T> where T: CustomSection {}

impl<T> PartialEq for TypedCustomSectionId<T>
where
    T: CustomSection,
{
    fn eq(&self, rhs: &Self) -> bool {
        self.id == rhs.id
    }
}

impl<T> Eq for TypedCustomSectionId<T> where T: CustomSection {}

impl<T> Hash for TypedCustomSectionId<T>
where
    T: CustomSection,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl<T> CustomSectionId for TypedCustomSectionId<T>
where
    T: CustomSection,
{
    type CustomSection = T;

    fn into_inner_id(self) -> Id<Option<Box<dyn CustomSectionAny>>> {
        self.id.0
    }

    fn section(s: &dyn CustomSectionAny) -> Option<&T> {
        s.as_any().downcast_ref::<T>()
    }

    fn section_mut(s: &mut dyn CustomSectionAny) -> Option<&mut T> {
        s.as_any_mut().downcast_mut::<T>()
    }
}

impl<T> From<TypedCustomSectionId<T>> for UntypedCustomSectionId
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
            id: UntypedCustomSectionId(id),
            _phantom: PhantomData,
        }
    }

    /// Remove a custom section from the module.
    pub fn delete<I>(&mut self, id: I)
    where
        I: Into<UntypedCustomSectionId>,
    {
        let id = id.into().0;
        self.arena.delete(id);
    }

    /// Take a raw, unparsed custom section out of this module.
    pub fn remove_raw(&mut self, name: &str) -> Option<RawCustomSection> {
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

    /// Try and get a shared reference to a custom section that is in this
    /// `ModuleCustomSections`.
    ///
    /// Returns `None` if the section associated with the given `id` has been
    /// taken or deleted, or if it is present but not an instance of
    /// `T::CustomSection`.
    pub fn get<T>(&self, id: T) -> Option<&T::CustomSection>
    where
        T: CustomSectionId,
    {
        self.arena
            .get(id.into_inner_id())
            .and_then(|s| T::section(&**s.as_ref().unwrap()))
    }

    /// Try and get an exclusive reference to a custom section that is in this
    /// `ModuleCustomSections`.
    ///
    /// Returns `None` if the section associated with the given `id` has been
    /// taken or deleted, or if it is present but not an instance of
    /// `T::CustomSection`.
    pub fn get_mut<T>(&mut self, id: T) -> Option<&mut T::CustomSection>
    where
        T: CustomSectionId,
    {
        self.arena
            .get_mut(id.into_inner_id())
            .and_then(|s| T::section_mut(&mut **s.as_mut().unwrap()))
    }

    /// Iterate over shared references to custom sections and their ids.
    pub fn iter(&self) -> impl Iterator<Item = (UntypedCustomSectionId, &dyn CustomSection)> {
        self.arena.iter().flat_map(|(id, s)| {
            if let Some(s) = s.as_ref() {
                Some((UntypedCustomSectionId(id), s.impl_as_custom_section()))
            } else {
                None
            }
        })
    }

    /// Iterate over exclusive references to custom sections and their ids.
    pub fn iter_mut(
        &mut self,
    ) -> impl Iterator<Item = (UntypedCustomSectionId, &mut dyn CustomSection)> {
        self.arena.iter_mut().flat_map(|(id, s)| {
            if let Some(s) = s.as_mut() {
                Some((UntypedCustomSectionId(id), s.impl_as_custom_section_mut()))
            } else {
                None
            }
        })
    }
}
