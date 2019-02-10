//! Matching expressions.

use super::Expr;
use crate::LocalFunction;

// Re-export the custom derive-generated impls here, where it makes more sense
// to expose them.
pub use super::generated_matchers::*;

/// A trait to match an expression.
pub trait Matcher {
    /// Does this expression match?
    fn is_match(&self, func: &LocalFunction, expr: &Expr) -> bool;
}
