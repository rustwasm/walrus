//! Displaying IR.

use crate::ir::*;
use crate::module::functions::{Function, FunctionKind, ImportedFunction, LocalFunction};
use std::fmt::{self, Write};

/// A trait for displaying our parsed IR.
pub trait DisplayIr {
    /// Extra context needed to display this thing.
    type Context;

    /// Display this IR into the given formatter.
    fn display_ir(&self, f: &mut fmt::Formatter, ctx: &Self::Context, indent: usize)
        -> fmt::Result;
}

impl DisplayIr for Function {
    type Context = ();

    fn display_ir(&self, f: &mut fmt::Formatter, _: &(), indent: usize) -> fmt::Result {
        assert_eq!(indent, 0);
        match self.kind {
            FunctionKind::Import(ref i) => i.display_ir(f, &(), indent),
            FunctionKind::Local(ref l) => l.display_ir(f, &(), indent),
        }
    }
}

impl DisplayIr for ImportedFunction {
    type Context = ();

    fn display_ir(&self, f: &mut fmt::Formatter, _: &(), indent: usize) -> fmt::Result {
        assert_eq!(indent, 0);
        writeln!(f, "(import func)")
    }
}

impl DisplayIr for LocalFunction {
    type Context = ();

    fn display_ir(&self, f: &mut fmt::Formatter, _: &(), indent: usize) -> fmt::Result {
        assert_eq!(indent, 0);

        writeln!(f, "(func")?;
        let entry = self.entry_block();

        let mut visitor = DisplayExpr {
            func: self,
            f,
            indent: indent + 1,
            id: entry.into(),
        };
        self.exprs[entry.into()].visit(&mut visitor)?;

        writeln!(f, ")")
    }
}

struct DisplayExpr<'a, 'b, 'c> {
    func: &'a LocalFunction,
    f: &'b mut fmt::Formatter<'c>,
    indent: usize,
    id: ExprId,
}

impl DisplayExpr<'_, '_, '_> {
    fn indented(&mut self, s: &str) -> fmt::Result {
        for _ in 0..self.indent {
            write!(&mut self.f, "  ")?;
        }
        writeln!(self.f, "{}", s)
    }

    fn sexp(&mut self, op: &str, operands: &[ExprId]) -> fmt::Result {
        if operands.is_empty() && !op.contains(";") {
            return self.indented(&format!("({})", op));
        }

        self.indented(&format!("({}", op))?;
        self.indent += 1;
        for e in operands {
            self.visit(*e)?;
        }
        self.indent -= 1;
        self.indented(")")
    }

    fn list(&mut self, items: &[ExprId]) -> fmt::Result {
        if items.is_empty() {
            return self.indented("()");
        }
        self.indented("(")?;
        for i in items {
            self.visit(*i)?;
        }
        self.indented(")")
    }

    fn visit<E>(&mut self, e: E) -> fmt::Result
    where
        E: Into<ExprId>,
    {
        let e = e.into();
        let id = self.id;
        self.id = e;
        self.func.exprs[e].visit(self)?;
        self.id = id;
        Ok(())
    }
}

impl Visitor for DisplayExpr<'_, '_, '_> {
    type Return = fmt::Result;

    fn visit_block(&mut self, b: &Block) -> fmt::Result {
        let label = format!(
            "{} ;; e{}",
            match b.kind {
                BlockKind::IfElse | BlockKind::FunctionEntry | BlockKind::Block => "block",
                BlockKind::Loop => "loop",
            },
            self.id.index(),
        );
        self.sexp(&label, &b.exprs)
    }

    fn visit_get_local(&mut self, expr: &GetLocal) -> fmt::Result {
        self.indented(&format!("(get_local {})", expr.local.index()))
    }

    fn visit_set_local(&mut self, expr: &SetLocal) -> fmt::Result {
        self.indented("(set_local")?;
        self.indent += 1;
        self.indented(&format!("{}", expr.local.index()))?;
        self.visit(expr.value)?;
        self.indent -= 1;
        self.indented(")")
    }

    fn visit_i32_const(&mut self, expr: &I32Const) -> fmt::Result {
        self.indented(&format!("(i32.const {})", expr.value))
    }

    fn visit_i32_add(&mut self, expr: &I32Add) -> fmt::Result {
        self.sexp("i32.add", &[expr.lhs, expr.rhs])
    }

    fn visit_i32_sub(&mut self, expr: &I32Sub) -> fmt::Result {
        self.sexp("i32.sub", &[expr.lhs, expr.rhs])
    }

    fn visit_i32_mul(&mut self, expr: &I32Mul) -> fmt::Result {
        self.sexp("i32.mul", &[expr.lhs, expr.rhs])
    }

    fn visit_i32_eqz(&mut self, e: &I32Eqz) -> fmt::Result {
        self.sexp("i32.eqz", &[e.expr])
    }

    fn visit_i32_popcnt(&mut self, e: &I32Popcnt) -> fmt::Result {
        self.sexp("i32.popcnt", &[e.expr])
    }

    fn visit_select(&mut self, s: &Select) -> fmt::Result {
        self.sexp("select", &[s.condition, s.consequent, s.alternative])
    }

    fn visit_unreachable(&mut self, _: &Unreachable) -> fmt::Result {
        self.indented("unreachable")
    }

    fn visit_br(&mut self, br: &Br) -> fmt::Result {
        self.indented("(br")?;
        self.indent += 1;

        self.indented(&format!("e{} ;; block", {
            let b: ExprId = br.block.into();
            b.index()
        }))?;

        self.list(&br.args)?;

        self.indent -= 1;
        self.indented(")")
    }

    fn visit_br_if(&mut self, br: &BrIf) -> fmt::Result {
        self.indented("(br_if")?;
        self.indent += 1;

        self.indented(&format!("e{}", {
            let b: ExprId = br.block.into();
            b.index()
        }))?;

        self.visit(br.condition)?;
        self.list(&br.args)?;

        self.indent -= 1;
        self.indented(")")
    }

    fn visit_if_else(&mut self, expr: &IfElse) -> fmt::Result {
        self.sexp(
            "if",
            &[
                expr.condition,
                expr.consequent.into(),
                expr.alternative.into(),
            ],
        )
    }

    fn visit_br_table(&mut self, b: &BrTable) -> fmt::Result {
        self.indented("(br_table")?;
        self.indent += 1;

        self.visit(b.which)?;

        let default: ExprId = b.default.into();
        self.indented(&format!("e{} ;; default", default.index()))?;

        let mut blocks = "[".to_string();
        for (i, bl) in b.blocks.iter().cloned().enumerate() {
            if i > 0 {
                blocks.push(' ');
            }
            let bl: ExprId = bl.into();
            write!(&mut blocks, "e{}", bl.index()).unwrap();
        }
        blocks.push(']');
        self.indented(&blocks)?;

        self.list(&b.args)?;

        self.indent -= 1;
        self.indented(")")
    }

    fn visit_drop(&mut self, d: &Drop) -> fmt::Result {
        self.sexp("drop", &[d.expr])
    }

    fn visit_return(&mut self, r: &Return) -> fmt::Result {
        self.sexp("return", &r.values)
    }
}
