//! TODO

use std::io::{self, Write};

/// Render something into a GraphViz dot file.
pub trait Dot {
    /// Render as dot into the given writer.
    fn dot(&self, out: &mut Write) -> io::Result<()>;
}
