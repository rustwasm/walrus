//! Error types and utilities.

use std::fmt;
pub use failure::Error;

/// Either `Ok(T)` or `Err(failure::Error)`.
pub type Result<T> = ::std::result::Result<T, failure::Error>;

/// A leaf wasm error type.
///
/// Just an enum with no further information. Extra diagnostics are attached via
/// failure's `context` method.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum ErrorKind {
    /// Given invalid input wasm.
    InvalidWasm,
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ErrorKind::InvalidWasm => {
                "The input WebAssembly is invalid".fmt(f)
            }
        }
    }
}

impl std::error::Error for ErrorKind {}
