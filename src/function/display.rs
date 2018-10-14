//! Displaying IR.

use super::super::ast::{Block, Expr, ExprId};
use super::Function;
use std::fmt;

/// A trait for displaying our parsed IR.
pub trait DisplayIr {
    /// Extra context needed to display this thing.
    type Context;

    /// Display this IR into the given formatter.
    fn display_ir(&self, f: &mut fmt::Formatter, ctx: &Self::Context) -> fmt::Result;
}

impl DisplayIr for Function {
    type Context = ();

    fn display_ir(&self, f: &mut fmt::Formatter, _: &()) -> fmt::Result {
        writeln!(f, "func {{")?;
        for (i, (id, block)) in self.blocks.iter().enumerate() {
            if i != 0 {
                write!(f, "\n")?;
            }
            writeln!(f, "  ;; {}", block.kind)?;
            write!(f, "  block_{}(", usize::from(id))?;
            for (i, p) in block.params.iter().enumerate() {
                if i != 0 {
                    write!(f, " ")?;
                }
                write!(f, "{}", p)?;
            }
            writeln!(f, "):")?;
            block.display_ir(f, self)?;
        }
        writeln!(f, "}}")
    }
}

impl DisplayIr for Block {
    type Context = Function;

    fn display_ir(&self, f: &mut fmt::Formatter, func: &Function) -> fmt::Result {
        for expr in &self.exprs {
            write!(f, "    ")?;
            func.exprs.get(*expr).unwrap().display_ir(f, func)?;
            write!(f, "\n")?;
        }
        Ok(())
    }
}

fn binop(
    f: &mut fmt::Formatter,
    func: &Function,
    op: &str,
    lhs: ExprId,
    rhs: ExprId,
) -> fmt::Result {
    write!(f, "({} ", op)?;
    func.exprs.get(lhs).unwrap().display_ir(f, func)?;
    write!(f, " ")?;
    func.exprs.get(rhs).unwrap().display_ir(f, func)?;
    write!(f, ")")
}

fn unop(f: &mut fmt::Formatter, func: &Function, op: &str, e: ExprId) -> fmt::Result {
    write!(f, "({} ", op)?;
    func.exprs.get(e).unwrap().display_ir(f, func)?;
    write!(f, ")")
}

impl DisplayIr for Expr {
    type Context = Function;

    fn display_ir(&self, f: &mut fmt::Formatter, func: &Function) -> fmt::Result {
        match self {
            Expr::Br { block, args } => {
                write!(f, "(br block_{} (", usize::from(*block))?;
                for (i, a) in args.iter().enumerate() {
                    if i != 0 {
                        write!(f, " ")?;
                    }
                    func.exprs.get(*a).unwrap().display_ir(f, func)?;
                }
                write!(f, "))")?;
            }
            Expr::BrIf {
                condition,
                block,
                args,
            } => {
                write!(f, "(br_if ")?;
                func.exprs.get(*condition).unwrap().display_ir(f, func)?;
                write!(f, " block_{} (", usize::from(*block))?;
                for (i, a) in args.iter().enumerate() {
                    if i != 0 {
                        write!(f, " ")?;
                    }
                    func.exprs.get(*a).unwrap().display_ir(f, func)?;
                }
                write!(f, "))")?;
            }
            Expr::BrTable {
                which,
                blocks,
                default,
                args,
            } => {
                write!(f, "(br_table ")?;
                func.exprs.get(*which).unwrap().display_ir(f, func)?;
                write!(f, "[")?;
                for (i, b) in blocks.iter().enumerate() {
                    if i != 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "block_{}", usize::from(*b))?;
                }
                write!(f, "] block_{} (", usize::from(*default))?;
                for (i, a) in args.iter().enumerate() {
                    if i != 0 {
                        write!(f, " ")?;
                    }
                    func.exprs.get(*a).unwrap().display_ir(f, func)?;
                }
                write!(f, "))")?;
            }
            Expr::Drop(e) => {
                write!(f, "(drop ")?;
                func.exprs.get(*e).unwrap().display_ir(f, func)?;
                write!(f, ")")?;
            }
            Expr::GetLocal { ty: _, local } => {
                write!(f, "(get_local {})", local)?;
            }
            Expr::I32Add(lhs, rhs) => {
                binop(f, func, "i32.add", *lhs, *rhs)?;
            }
            Expr::I32Const(c) => {
                write!(f, "(i32.const {})", c)?;
            }
            Expr::I32Eqz(e) => {
                unop(f, func, "i32.eqz", *e)?;
            }
            Expr::I32Mul(lhs, rhs) => {
                binop(f, func, "i32.mul", *lhs, *rhs)?;
            }
            Expr::I32Popcnt(e) => {
                unop(f, func, "i32.popcnt", *e)?;
            }
            Expr::I32Sub(lhs, rhs) => {
                binop(f, func, "i32.sub", *lhs, *rhs)?;
            }
            Expr::IfElse {
                condition,
                consequent,
                alternative,
            } => {
                write!(f, "(if/else ")?;
                func.exprs.get(*condition).unwrap().display_ir(f, func)?;
                write!(
                    f,
                    " block_{} block_{})",
                    usize::from(*consequent),
                    usize::from(*alternative)
                )?;
            }
            Expr::Phi => {
                write!(f, "(phi)")?;
            }
            Expr::Return { values } => {
                write!(f, "(return (")?;
                for (i, v) in values.iter().enumerate() {
                    if i != 0 {
                        write!(f, " ")?;
                    }
                    func.exprs.get(*v).unwrap().display_ir(f, func)?;
                }
                write!(f, "))")?;
            }
            Expr::Select {
                condition,
                consequent,
                alternative,
            } => {
                write!(f, "(select ")?;
                func.exprs.get(*condition).unwrap().display_ir(f, func)?;
                write!(f, " ")?;
                func.exprs.get(*consequent).unwrap().display_ir(f, func)?;
                write!(f, " ")?;
                func.exprs.get(*alternative).unwrap().display_ir(f, func)?;
                write!(f, ")")?;
            }
            Expr::SetLocal {
                ty: _,
                value,
                local,
            } => {
                write!(f, "(set_local {} ", local)?;
                func.exprs.get(*value).unwrap().display_ir(f, func)?;
                write!(f, ")")?;
            }
            Expr::Unreachable => {
                write!(f, "(unreachable)")?;
            }
        }
        Ok(())
    }
}
