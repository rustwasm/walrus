//! TODO

use super::ValType;
use crate::arena::Id;
use crate::dot::Dot;
use std::io::{self, Write};

/// TODO
pub type ExprId = Id<Expr>;

impl Dot for ExprId {
    fn dot(&self, out: &mut Write) -> io::Result<()> {
        let n: usize = (*self).into();
        write!(out, "expr_{}", n)
    }
}

/// TODO
pub type BlockId = Id<Block>;

impl Dot for BlockId {
    fn dot(&self, out: &mut Write) -> io::Result<()> {
        let n: usize = (*self).into();
        write!(out, "block_{}", n)
    }
}

/// TODO
#[derive(Debug)]
pub struct Block {
    pub(crate) exprs: Vec<ExprId>,
}

impl<'a> Dot for (BlockId, &'a Block) {
    fn dot(&self, out: &mut Write) -> io::Result<()> {
        self.0.dot(out)?;
        write!(
            out,
            " [shape=\"rect\", label=<<table border=\"0\" cellborder=\"0\"><tr><td><b><u>"
        )?;
        self.0.dot(out)?;
        write!(out, "</u></b></td></tr>")?;

        for (idx, expr) in self.1.exprs.iter().enumerate() {
            write!(out, "<tr><td port=\"e{}\">", idx)?;
            expr.dot(out)?;
            write!(out, "</td></tr>")?;
        }
        writeln!(out, "</table>>];")?;
        for (idx, expr) in self.1.exprs.iter().enumerate() {
            self.0.dot(out)?;
            write!(out, ":e{} -> ", idx)?;
            expr.dot(out)?;
            writeln!(out, ";")?;
        }
        Ok(())
    }
}

/// TODO
#[derive(Debug)]
pub enum Expr {
    /// `get_local n`
    GetLocal {
        /// The type of this local.
        ty: ValType,
        /// The n^th local.
        local: u32,
    },

    /// TODO
    I32Const(i32),

    /// TODO
    I32Add(ExprId, ExprId),

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
    Unreachable,

    /// TODO
    Phi,

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
    Loop(BlockId),

    /// TODO
    Drop(ExprId),
}

impl<'a> Dot for (ExprId, &'a Expr) {
    fn dot(&self, out: &mut Write) -> io::Result<()> {
        self.0.dot(out)?;
        write!(
            out,
            " [label=<<table cellborder=\"0\" border=\"0\"><tr><td><b><u>"
        )?;
        self.0.dot(out)?;
        write!(out, "</u></b></td></tr><tr><td><font face=\"monospace\">")?;
        let mut edges = vec![];

        fn edge<D: Dot, E: Dot>(edges: &mut Vec<String>, from: &D, to: &E, label: &str) {
            let mut e = Vec::new();
            from.dot(&mut e).unwrap();
            write!(&mut e, " -> ").unwrap();
            to.dot(&mut e).unwrap();
            write!(&mut e, " [label=\"{}\"];", label).unwrap();
            edges.push(String::from_utf8_lossy(&e).to_string());
        }

        match self.1 {
            Expr::GetLocal { ty, local } => write!(out, "get_local {} ;; ty = {}", local, ty)?,
            Expr::I32Const(n) => write!(out, "i32.const {}", n)?,
            Expr::I32Add(lhs, rhs) => {
                edge(&mut edges, &self.0, lhs, "lhs");
                edge(&mut edges, &self.0, rhs, "rhs");
                write!(out, "i32.add")?;
            }
            Expr::Select {
                condition,
                consequent,
                alternative,
            } => {
                edge(&mut edges, &self.0, condition, "condition");
                edge(&mut edges, &self.0, consequent, "consequent");
                edge(&mut edges, &self.0, alternative, "alternative");
                write!(out, "select")?;
            }

            Expr::Unreachable => write!(out, "unreachable")?,
            Expr::Phi => write!(out, "phi")?,
            Expr::Br { block, args } => {
                edge(&mut edges, &self.0, block, "block");
                for arg in args.iter() {
                    edge(&mut edges, &self.0, arg, "arg");
                }
                write!(out, "br ")?;
                block.dot(out)?;
            }
            Expr::BrIf {
                condition,
                block,
                args,
            } => {
                edge(&mut edges, &self.0, condition, "condition");
                edge(&mut edges, &self.0, block, "block");
                for arg in args.iter() {
                    edge(&mut edges, &self.0, arg, "arg");
                }
                write!(out, "br_if ")?;
                block.dot(out)?;
            }
            Expr::IfElse {
                condition,
                consequent,
                alternative,
            } => {
                edge(&mut edges, &self.0, condition, "condition");
                edge(&mut edges, &self.0, consequent, "consequent");
                edge(&mut edges, &self.0, alternative, "alternative");
                write!(out, "if_else ")?;
                consequent.dot(out)?;
                write!(out, " ")?;
                alternative.dot(out)?;
            }
            Expr::BrTable {
                which,
                blocks,
                default,
                args,
            } => {
                edge(&mut edges, &self.0, which, "which");
                for block in blocks.iter() {
                    edge(&mut edges, &self.0, block, "block");
                }
                edge(&mut edges, &self.0, default, "default");
                for arg in args.iter() {
                    edge(&mut edges, &self.0, arg, "arg");
                }
                write!(out, "br_table [")?;
                for (idx, block) in blocks.iter().enumerate() {
                    if idx != 0 {
                        write!(out, " ")?;
                    }
                    block.dot(out)?;
                }
                write!(out, "] ")?;
                default.dot(out)?;
            }
            Expr::Loop(block) => {
                edge(&mut edges, &self.0, block, "block");
                write!(out, "loop")?;
            }
            Expr::Drop(e) => {
                edge(&mut edges, &self.0, e, "e");
                write!(out, "drop")?;
            }
        }
        writeln!(out, "</font></td></tr></table>>];")?;
        for edge in edges {
            writeln!(out, "{}", edge)?;
        }
        Ok(())
    }
}
