//! Utilities for emitting GraphViz dot files.

/// Render something into a GraphViz dot file.
pub trait Dot {
    /// Render as dot into the given string.
    fn dot(&self, out: &mut String);
}
