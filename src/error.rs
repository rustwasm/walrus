//! Error types and utilities.

pub use failure::Error;
use failure::*;

/// Either `Ok(T)` or `Err(failure::Error)`.
pub type Result<T> = ::std::result::Result<T, failure::Error>;

/// A leaf wasm error type.
///
/// Just an enum with no further information. Extra diagnostics are attached via
/// failure's `context` method.
#[derive(Copy, Clone, Eq, PartialEq, Debug, Fail)]
pub enum ErrorKind {
    /// Given invalid input wasm.
    #[fail(display = "The input WebAssembly is invalid")]
    InvalidWasm,
}
