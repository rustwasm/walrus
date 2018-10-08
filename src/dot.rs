//! TODO

use std::io::{self, Write};

/// Render something into a GraphViz dot file.
pub trait Dot {
    /// Render as dot into the given writer.
    fn dot(&self, out: &mut Write) -> io::Result<()>;
}

/// Append a port to the given dot label.
#[derive(Debug)]
pub struct Port<'a, D: 'a + Dot>(pub &'a D, pub &'a str);

impl<'a, D: 'a + Dot> Dot for Port<'a, D> {
    fn dot(&self, out: &mut Write) -> io::Result<()> {
        self.0.dot(out)?;
        write!(out, ":{}", self.1)
    }
}
