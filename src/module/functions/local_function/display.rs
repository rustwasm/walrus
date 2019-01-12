//! Displaying IR.

use crate::ir::*;
use crate::module::functions::{Function, FunctionKind, ImportedFunction, LocalFunction};
use std::fmt::Write;

/// A trait for displaying our parsed IR.
pub trait DisplayIr {
    /// Extra context needed to display this thing.
    type Context;

    /// Display this IR into the given formatter.
    fn display_ir(&self, f: &mut String, ctx: &Self::Context, indent: usize);
}

impl DisplayIr for Function {
    type Context = ();

    fn display_ir(&self, f: &mut String, _: &(), indent: usize) {
        assert_eq!(indent, 0);
        match self.kind {
            FunctionKind::Import(ref i) => i.display_ir(f, &(), indent),
            FunctionKind::Local(ref l) => l.display_ir(f, &(), indent),
            FunctionKind::Uninitialized(_) => unreachable!(),
        }
    }
}

impl DisplayIr for ImportedFunction {
    type Context = ();

    fn display_ir(&self, f: &mut String, _: &(), indent: usize) {
        assert_eq!(indent, 0);
        f.push_str("(import func)");
    }
}

impl DisplayIr for LocalFunction {
    type Context = ();

    fn display_ir(&self, f: &mut String, _: &(), indent: usize) {
        assert_eq!(indent, 0);

        f.push_str("(func\n");
        let entry = self.entry_block();

        let mut visitor = DisplayExpr {
            func: self,
            f,
            indent: indent + 1,
            id: entry.into(),
        };
        self.exprs[entry.into()].visit(&mut visitor);

        f.push_str(")");
    }
}

struct DisplayExpr<'a, 'b> {
    func: &'a LocalFunction,
    f: &'b mut String,
    indent: usize,
    id: ExprId,
}

impl DisplayExpr<'_, '_> {
    fn indented(&mut self, s: &str) {
        for _ in 0..self.indent {
            self.f.push_str("  ");
        }
        self.f.push_str(s);
        self.f.push('\n');
    }

    fn sexp(&mut self, op: &str, operands: &[ExprId]) {
        if operands.is_empty() && !op.contains(";") {
            return self.indented(&format!("({})", op));
        }

        self.indented(&format!("({}", op));
        self.indent += 1;
        for e in operands {
            self.visit(*e);
        }
        self.indent -= 1;
        self.indented(")")
    }

    fn list(&mut self, items: &[ExprId]) {
        if items.is_empty() {
            return self.indented("()");
        }
        self.indented("(");
        for i in items {
            self.visit(*i);
        }
        self.indented(")")
    }

    fn visit<E>(&mut self, e: E)
    where
        E: Into<ExprId>,
    {
        let e = e.into();
        let id = self.id;
        self.id = e;
        self.func.exprs[e].visit(self);
        self.id = id;
    }
}

impl<'expr> Visitor<'expr> for DisplayExpr<'expr, '_> {
    fn local_function(&self) -> &'expr LocalFunction {
        self.func
    }

    fn visit_block(&mut self, b: &Block) {
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

    fn visit_call(&mut self, e: &Call) {
        if e.args.is_empty() {
            return self.indented(&format!("(call {})", e.func.index()));
        }

        self.indented(&format!("(call {}", e.func.index()));
        self.indent += 1;
        for a in e.args.iter() {
            self.visit(*a);
        }
        self.indent -= 1;
        self.indented(")")
    }

    fn visit_local_get(&mut self, expr: &LocalGet) {
        self.indented(&format!("(local.get {})", expr.local.index()))
    }

    fn visit_local_set(&mut self, expr: &LocalSet) {
        self.indented("(local.set");
        self.indent += 1;
        self.indented(&format!("{}", expr.local.index()));
        self.visit(expr.value);
        self.indent -= 1;
        self.indented(")")
    }

    fn visit_global_get(&mut self, expr: &GlobalGet) {
        self.indented(&format!("(global.get {})", expr.global.index()))
    }

    fn visit_global_set(&mut self, expr: &GlobalSet) {
        self.indented("(global.set");
        self.indent += 1;
        self.indented(&format!("{}", expr.global.index()));
        self.visit(expr.value);
        self.indent -= 1;
        self.indented(")")
    }

    fn visit_const(&mut self, expr: &Const) {
        self.indented(&match expr.value {
            Value::I32(i) => format!("(i32.const {})", i),
            Value::I64(i) => format!("(i64.const {})", i),
            Value::F32(i) => format!("(f32.const {})", i),
            Value::F64(i) => format!("(f64.const {})", i),
            Value::V128(i) => format!("(v128.const {})", i),
        })
    }

    fn visit_i32_add(&mut self, expr: &I32Add) {
        self.sexp("i32.add", &[expr.lhs, expr.rhs])
    }

    fn visit_i32_sub(&mut self, expr: &I32Sub) {
        self.sexp("i32.sub", &[expr.lhs, expr.rhs])
    }

    fn visit_i32_mul(&mut self, expr: &I32Mul) {
        self.sexp("i32.mul", &[expr.lhs, expr.rhs])
    }

    fn visit_i32_eqz(&mut self, e: &I32Eqz) {
        self.sexp("i32.eqz", &[e.expr])
    }

    fn visit_i32_popcnt(&mut self, e: &I32Popcnt) {
        self.sexp("i32.popcnt", &[e.expr])
    }

    fn visit_select(&mut self, s: &Select) {
        self.sexp("select", &[s.condition, s.consequent, s.alternative])
    }

    fn visit_unreachable(&mut self, _: &Unreachable) {
        self.indented("unreachable")
    }

    fn visit_br(&mut self, br: &Br) {
        self.indented("(br");
        self.indent += 1;

        self.indented(&format!("e{} ;; block", {
            let b: ExprId = br.block.into();
            b.index()
        }));

        self.list(&br.args);

        self.indent -= 1;
        self.indented(")")
    }

    fn visit_br_if(&mut self, br: &BrIf) {
        self.indented("(br_if");
        self.indent += 1;

        self.indented(&format!("e{}", {
            let b: ExprId = br.block.into();
            b.index()
        }));

        self.visit(br.condition);
        self.list(&br.args);

        self.indent -= 1;
        self.indented(")")
    }

    fn visit_if_else(&mut self, expr: &IfElse) {
        self.sexp(
            "if",
            &[
                expr.condition,
                expr.consequent.into(),
                expr.alternative.into(),
            ],
        )
    }

    fn visit_br_table(&mut self, b: &BrTable) {
        self.indented("(br_table");
        self.indent += 1;

        self.visit(b.which);

        let default: ExprId = b.default.into();
        self.indented(&format!("e{} ;; default", default.index()));

        let mut blocks = "[".to_string();
        for (i, bl) in b.blocks.iter().cloned().enumerate() {
            if i > 0 {
                blocks.push(' ');
            }
            let bl: ExprId = bl.into();
            write!(&mut blocks, "e{}", bl.index()).unwrap();
        }
        blocks.push(']');
        self.indented(&blocks);

        self.list(&b.args);

        self.indent -= 1;
        self.indented(")")
    }

    fn visit_drop(&mut self, d: &Drop) {
        self.sexp("drop", &[d.expr])
    }

    fn visit_return(&mut self, r: &Return) {
        self.sexp("return", &r.values)
    }

    fn visit_memory_size(&mut self, m: &MemorySize) {
        self.indented(&format!("(memory.size {})", m.memory.index()))
    }
}
