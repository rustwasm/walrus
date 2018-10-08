//! TODO

use failure::*;
use std::fmt::{self, Display};

/// TODO
pub type Result<T> = ::std::result::Result<T, failure::Error>;

/// TODO
#[derive(Debug)]
pub struct Error {
    inner: Context<ErrorKind>,
}

impl Fail for Error {
    fn cause(&self) -> Option<&Fail> {
        self.inner.cause()
    }

    fn backtrace(&self) -> Option<&Backtrace> {
        self.inner.backtrace()
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(&self.inner, f)
    }
}

/// TODO
#[derive(Copy, Clone, Eq, PartialEq, Debug, Fail)]
pub enum ErrorKind {
    /// TODO
    #[fail(display = "The WebAssembly is invalid")]
    InvalidWasm,
}
