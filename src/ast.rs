//! TODO

use super::ValType;
use crate::arena::Id;
use crate::dot::{Dot, Port};
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
    /// The kind of block this is and why it was created, for debugging
    /// purposes.
    pub(crate) kind: &'static str,
    pub(crate) params: Box<[ValType]>,
    pub(crate) exprs: Vec<ExprId>,
}

impl Block {
    /// Construct a new extended basic block.
    pub fn new(kind: &'static str, params: Box<[ValType]>) -> Block {
        Block {
            kind,
            params,
            exprs: vec![],
        }
    }
}

impl<'a> Dot for (BlockId, &'a Block) {
    fn dot(&self, out: &mut Write) -> io::Result<()> {
        self.0.dot(out)?;
        write!(
            out,
            " [shape=\"rect\", penwidth={}, label=<<table border=\"0\" cellborder=\"0\"><tr><td port=\"title\"><b><u>",
            if usize::from(self.0) <= 1 {
                "5"
            } else {
                "1"
            }
        )?;
        self.0.dot(out)?;
        write!(out, "</u></b> ({}) [", self.1.kind)?;
        for (idx, ty) in self.1.params.iter().enumerate() {
            if idx != 0 {
                write!(out, " ")?;
            }
            write!(out, "{}", ty)?;
        }
        write!(out, "]</td></tr>")?;
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

    /// `set_local n`
    SetLocal {
        /// The type of this local.
        ty: ValType,
        /// The n^th local.
        local: u32,
        /// The value to set the local to.
        value: ExprId,
    },

    /// TODO
    I32Const(i32),

    /// TODO
    I32Add(ExprId, ExprId),
    /// TODO
    I32Sub(ExprId, ExprId),

    /// TODO
    I32Mul(ExprId, ExprId),

    /// `i32.eqz`
    I32Eqz(ExprId),

    /// `i32.popcnt`
    I32Popcnt(ExprId),

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
    Drop(ExprId),

    /// TODO
    Return {
        /// The values being returned.
        values: Box<[ExprId]>,
    },
}

impl Expr {
    /// Is this expression some sort of unconditional jump?
    pub fn is_jump(&self) -> bool {
        match self {
            Expr::Unreachable
            | Expr::Br { .. }
            | Expr::IfElse { .. }
            | Expr::BrTable { .. }
            | Expr::Return { .. } => true,
            _ => false,
        }
    }
}

impl<'a> Dot for (ExprId, &'a Expr) {
    fn dot(&self, out: &mut Write) -> io::Result<()> {
        self.0.dot(out)?;
        write!(
            out,
            " [style=\"dotted\", label=<<table cellborder=\"0\" border=\"0\"><tr><td><b><u>"
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
            Expr::SetLocal { ty, local, value } => {
                edge(&mut edges, &self.0, value, "value");
                write!(out, "set_local {} ;; ty = {}", local, ty)?;
            }
            Expr::I32Const(n) => write!(out, "i32.const {}", n)?,
            Expr::I32Add(lhs, rhs) => {
                edge(&mut edges, &self.0, lhs, "lhs");
                edge(&mut edges, &self.0, rhs, "rhs");
                write!(out, "i32.add")?;
            }
            Expr::I32Sub(lhs, rhs) => {
                edge(&mut edges, &self.0, lhs, "lhs");
                edge(&mut edges, &self.0, rhs, "rhs");
                write!(out, "i32.sub")?;
            }
            Expr::I32Mul(lhs, rhs) => {
                edge(&mut edges, &self.0, lhs, "lhs");
                edge(&mut edges, &self.0, rhs, "rhs");
                write!(out, "i32.mul")?;
            }
            Expr::I32Eqz(e) => {
                edge(&mut edges, &self.0, e, "value");
                write!(out, "i32.eqz")?;
            }
            Expr::I32Popcnt(e) => {
                edge(&mut edges, &self.0, e, "value");
                write!(out, "i32.popcnt")?;
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
                edge(&mut edges, &self.0, &Port(block, "title"), "block");
                for arg in args.iter() {
                    edge(&mut edges, &self.0, arg, "arg");
                }
                write!(out, "br")?;
            }
            Expr::BrIf {
                condition,
                block,
                args,
            } => {
                edge(&mut edges, &self.0, condition, "condition");
                edge(&mut edges, &self.0, &Port(block, "title"), "block");
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
                edge(
                    &mut edges,
                    &self.0,
                    &Port(consequent, "title"),
                    "consequent",
                );
                edge(
                    &mut edges,
                    &self.0,
                    &Port(alternative, "title"),
                    "alternative",
                );
                write!(out, "if_else")?;
            }
            Expr::BrTable {
                which,
                blocks,
                default,
                args,
            } => {
                edge(&mut edges, &self.0, which, "which");
                for (idx, block) in blocks.iter().enumerate() {
                    edge(
                        &mut edges,
                        &self.0,
                        &Port(block, "title"),
                        &format!("block {}", idx),
                    );
                }
                edge(&mut edges, &self.0, &Port(default, "title"), "default");
                for arg in args.iter() {
                    edge(&mut edges, &self.0, arg, "arg");
                }
                write!(out, "br_table")?;
            }
            Expr::Drop(e) => {
                edge(&mut edges, &self.0, e, "value");
                write!(out, "drop")?;
            }
            Expr::Return { values } => {
                for (idx, v) in values.iter().enumerate() {
                    edge(&mut edges, &self.0, v, &format!("return value {}", idx));
                }
                write!(out, "return")?;
            }
        }
        writeln!(out, "</font></td></tr></table>>];")?;
        for edge in edges {
            writeln!(out, "{}", edge)?;
        }
        Ok(())
    }
}
