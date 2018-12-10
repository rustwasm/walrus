//! TODO

pub mod matcher;

use crate::dot::Dot;
use crate::ty::ValType;
use id_arena::Id;
use std::io::{self, Write};
use walrus_derive::walrus_expr;

/// The id of a local.
pub type LocalId = Id<Local>;

/// A local variable or parameter.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Local {
    id: LocalId,
    ty: ValType,
}

impl Local {
    /// Construct a new local from the given id and type.
    pub fn new(id: LocalId, ty: ValType) -> Local {
        Local { id, ty }
    }

    /// Get this local's id that is unique across the whole module.
    pub fn id(&self) -> LocalId {
        self.id
    }

    /// Get this local's type.
    pub fn ty(&self) -> ValType {
        self.ty
    }
}

/// TODO
pub type ExprId = Id<Expr>;

impl Dot for ExprId {
    fn dot(&self, out: &mut Write) -> io::Result<()> {
        write!(out, "expr_{}", self.index())
    }
}

/// TODO
pub trait Ast: Into<Expr> {
    /// TODO
    type Id: Into<ExprId>;

    /// TODO
    fn new_id(id: ExprId) -> Self::Id;
}

/// Different kinds of blocks.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BlockKind {
    /// A `block` block.
    Block,

    /// A `loop` block.
    Loop,

    /// An `if` or `else` block.
    IfElse,

    /// The entry to a function.
    FunctionEntry,
}

/// TODO
#[walrus_expr]
#[derive(Clone, Debug)]
pub enum Expr {
    /// TODO
    Block {
        /// TODO
        diagnostic: &'static str,
        /// What kind of block is this?
        kind: BlockKind,
        /// TODO
        params: Box<[ValType]>,
        /// TODO
        results: Box<[ValType]>,
        /// TODO
        exprs: Vec<ExprId>,
    },

    /// `get_local n`
    GetLocal {
        /// The type of this local.
        ty: ValType,
        /// The local being got.
        local: LocalId,
    },

    /// `set_local n`
    SetLocal {
        /// The type of this local.
        ty: ValType,
        /// The local being set.
        local: LocalId,
        /// The value to set the local to.
        value: ExprId,
    },

    /// TODO
    I32Const {
        /// TODO
        value: i32,
    },

    /// TODO
    I32Add {
        /// TODO
        lhs: ExprId,
        /// TODO
        rhs: ExprId,
    },

    /// TODO
    I32Sub {
        /// TODO
        lhs: ExprId,
        /// TODO
        rhs: ExprId,
    },

    /// TODO
    I32Mul {
        /// TODO
        lhs: ExprId,
        /// TODO
        rhs: ExprId,
    },

    /// `i32.eqz`
    I32Eqz {
        /// TODO
        expr: ExprId,
    },

    /// `i32.popcnt`
    I32Popcnt {
        /// TODO
        expr: ExprId,
    },

    /// TODO
    Select {
        /// The condition.
        condition: ExprId,
        /// The value returned when the condition is true. Evaluated regardless
        /// if the condition is true.
        consequent: ExprId,
        /// The value returned when the condition is false. Evaluated regardless
        /// if the condition is false.
        alternative: ExprId,
    },

    /// TODO
    Unreachable {},

    /// TODO
    Br {
        /// The target block to branch to.
        block: BlockId,
        /// The arguments to the block.
        args: Box<[ExprId]>,
    },

    /// TODO
    BrIf {
        /// The condition for when to branch.
        condition: ExprId,
        /// The target block to branch to when the condition is met.
        block: BlockId,
        /// The arguments to the block.
        args: Box<[ExprId]>,
    },

    /// TODO
    IfElse {
        /// The condition.
        condition: ExprId,
        /// The block to execute when the condition is true.
        consequent: BlockId,
        /// The block to execute when the condition is false.
        alternative: BlockId,
    },

    /// TODO
    BrTable {
        /// The table index of which block to branch to.
        which: ExprId,
        /// The table of target blocks.
        blocks: Box<[BlockId]>,
        /// The block that is branched to by default when `which` is out of the
        /// table's bounds.
        default: BlockId,
        /// The arguments to the block.
        args: Box<[ExprId]>,
    },

    /// TODO
    Drop {
        /// TODO
        expr: ExprId,
    },

    /// TODO
    Return {
        /// The values being returned.
        values: Box<[ExprId]>,
    },
}

impl Expr {
    /// Are any instructions that follow this expression's instruction (within
    /// the current block) unreachable?
    ///
    /// Returns `true` for unconditional branches (`br`, `return`, etc...) and
    /// `unreachable`. Returns `false` for all other "normal" instructions
    /// (`i32.add`, etc...).
    pub fn following_instructions_are_unreachable(&self) -> bool {
        match *self {
            Expr::Unreachable(..) | Expr::Br(..) | Expr::BrTable(..) | Expr::Return(..) => true,

            // No `_` arm to make sure that we properly update this function as
            // we add support for new instructions.
            Expr::Block(..)
            | Expr::GetLocal(..)
            | Expr::SetLocal(..)
            | Expr::I32Const(..)
            | Expr::I32Add(..)
            | Expr::I32Sub(..)
            | Expr::I32Mul(..)
            | Expr::I32Eqz(..)
            | Expr::I32Popcnt(..)
            | Expr::Select(..)
            | Expr::BrIf(..)
            | Expr::IfElse(..)
            | Expr::Drop(..) => false,
        }
    }
}

impl Block {
    /// Construct a new block.
    pub fn new(
        diagnostic: &'static str,
        kind: BlockKind,
        params: Box<[ValType]>,
        results: Box<[ValType]>,
    ) -> Block {
        let exprs = vec![];
        Block {
            diagnostic,
            kind,
            params,
            results,
            exprs,
        }
    }
}
