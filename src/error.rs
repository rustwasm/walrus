//! TODO

pub use failure::Error;
use failure::*;

/// TODO
pub type Result<T> = ::std::result::Result<T, failure::Error>;

/// TODO
#[derive(Copy, Clone, Eq, PartialEq, Debug, Fail)]
pub enum ErrorKind {
    /// TODO
    #[fail(display = "The WebAssembly is invalid")]
    InvalidWasm,
}
