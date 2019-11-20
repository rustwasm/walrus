//! Passes over whole modules or individual functions.

pub mod gc;
mod used;
pub mod validate;
pub use self::used::Roots;
