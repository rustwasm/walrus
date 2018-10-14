//! Control-flow and data-flow graph implementations for functions.

use super::super::ir::{BlockId, Expr, ExprId};
use super::Function;
use petgraph::visit;
use std::collections::HashSet;
use std::slice;

/// The control-flow graph between blocks in a function.
#[derive(Clone, Copy, Debug)]
pub struct ControlFlowGraph<'a> {
    func: &'a Function,
}

impl<'a> ControlFlowGraph<'a> {
    /// Construct the control-flow graph for the given function.
    pub(crate) fn new(func: &'a Function) -> ControlFlowGraph<'a> {
        ControlFlowGraph { func }
    }
}

impl<'a> visit::GraphBase for ControlFlowGraph<'a> {
    type EdgeId = ();
    type NodeId = BlockId;
}

impl<'a> visit::Visitable for ControlFlowGraph<'a> {
    type Map = HashSet<BlockId>;

    #[inline]
    fn visit_map(&self) -> Self::Map {
        HashSet::with_capacity(self.func.blocks.len())
    }

    #[inline]
    fn reset_map(&self, map: &mut Self::Map) {
        map.clear();
    }
}

/// TODO FITZGEN
#[derive(Clone, Debug)]
pub struct CfgNeighbors<'a> {
    func: &'a Function,
    inner: slice::Iter<'a, ExprId>,
    queued: Vec<BlockId>,
}

impl<'a> Iterator for CfgNeighbors<'a> {
    type Item = BlockId;

    fn next(&mut self) -> Option<BlockId> {
        if let Some(b) = self.queued.pop() {
            return Some(b);
        }

        loop {
            match self.inner.next().map(|id| &self.func.exprs[*id]) {
                None => return None,
                Some(Expr::Br { block, .. }) => return Some(*block),
                Some(Expr::BrIf { block, .. }) => return Some(*block),
                Some(Expr::BrTable {
                    blocks, default, ..
                }) => {
                    self.queued.extend(blocks.iter().rev().cloned());
                    return Some(*default);
                }
                Some(Expr::IfElse {
                    consequent,
                    alternative,
                    ..
                }) => {
                    self.queued.push(*alternative);
                    return Some(*consequent);
                }
                Some(_) => continue,
            }
        }
    }
}

impl<'a> visit::IntoNeighbors for &'a ControlFlowGraph<'a> {
    type Neighbors = CfgNeighbors<'a>;

    #[inline]
    fn neighbors(self, id: BlockId) -> CfgNeighbors<'a> {
        let inner = self.func.blocks[id].exprs.iter();
        CfgNeighbors {
            func: self.func,
            inner,
            queued: vec![],
        }
    }
}

impl<'a> visit::NodeCount for ControlFlowGraph<'a> {
    #[inline]
    fn node_count(&self) -> usize {
        self.func.blocks.len()
    }
}

/// The data-flow graph between expressions in a function.
#[derive(Clone, Copy, Debug)]
pub struct DataFlowGraph<'a> {
    func: &'a Function,
}
