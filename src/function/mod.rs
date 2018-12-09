//! TODO

mod context;
pub mod display;

use self::context::FunctionContext;
use super::dot::Dot;
use super::error::{ErrorKind, Result};
use super::validation_context::ValidationContext;
use super::ValType;
use crate::ir::*;
use failure::{Fail, ResultExt};
use id_arena::Arena;
use parity_wasm::elements::{self, Instruction};
use std::collections::HashSet;
use std::fmt;
use std::io::{self, Write};

/// TODO
#[derive(Debug)]
pub struct Function {
    pub(crate) exprs: Arena<Expr>,
    entry: Option<BlockId>,
    // TODO: provenance: ExprId -> offset in code section of the original
    // instruction
}

impl Function {
    /// TODO
    pub fn new(
        validation: &ValidationContext,
        types: &elements::TypeSection,
        func: &elements::Func,
        body: &elements::FuncBody,
    ) -> Result<Function> {
        let validation = validation.for_function(func, body)?;

        let ty = func.type_ref();
        let ty = &types.types().get(ty as usize).ok_or_else(|| {
            ErrorKind::InvalidWasm
                .context("function's type is an out-of-bounds reference into the types section")
        })?;
        let ty = match ty {
            elements::Type::Function(f) => f,
        };

        let mut func = Function {
            exprs: Arena::new(),
            entry: None,
        };

        let result: Vec<_> = ty
            .return_type()
            .as_ref()
            .into_iter()
            .map(ValType::from)
            .collect();

        let operands = &mut context::OperandStack::new();
        let controls = &mut context::ControlStack::new();

        let mut ctx = FunctionContext::new(&mut func, &validation, operands, controls);

        let entry = ctx.push_control("function entry", BlockKind::Block, vec![], result.clone());
        ctx.func.entry = Some(entry);
        validate_expression(&mut ctx, body.code().elements())?;

        debug_assert_eq!(ctx.operands.len(), result.len());
        debug_assert!(ctx.controls.is_empty());

        Ok(func)
    }

    fn alloc<T>(&mut self, val: T) -> T::Id
    where
        T: Ast,
    {
        let id = self.exprs.alloc(val.into());
        T::new_id(id)
    }

    /// Get the id of this function's entry block.
    pub fn entry_block(&self) -> BlockId {
        self.entry.unwrap()
    }

    /// Get the block associated with the given id.
    pub fn block(&self, block: BlockId) -> &Block {
        self.exprs[block.into()].unwrap_block()
    }

    /// Get the block associated with the given id.
    pub fn block_mut(&mut self, block: BlockId) -> &mut Block {
        self.exprs[block.into()].unwrap_block_mut()
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::display::DisplayIr;
        self.display_ir(f, &(), 0)
    }
}

impl Dot for Function {
    fn dot(&self, out: &mut Write) -> io::Result<()> {
        struct DotVisitor<'a, 'b> {
            out: &'a mut Write,
            func: &'b Function,
            id: ExprId,
            seen: HashSet<ExprId>,
        }

        impl DotVisitor<'_, '_> {
            fn write_id(&mut self, id: ExprId) -> io::Result<()> {
                write!(self.out, "e{}", id.index())
            }

            fn write_node_prologue(&mut self) -> io::Result<()> {
                self.write_id(self.id)?;
                write!(
                    self.out,
                    " [label=<<table cellborder=\"0\" border=\"0\"><tr><td><font face=\"monospace\">"
                )
            }

            fn write_node_epilogue(&mut self) -> io::Result<()> {
                writeln!(self.out, "</font></td></tr></table>>];")
            }

            fn node<S>(&mut self, label: S) -> io::Result<()>
            where
                S: AsRef<str>,
            {
                self.write_node_prologue()?;
                write!(self.out, "{}", label.as_ref())?;
                self.write_node_epilogue()
            }

            fn edge<E, S>(&mut self, to: E, label: S) -> io::Result<()>
            where
                E: Into<ExprId>,
                S: AsRef<str>,
            {
                let to = to.into();
                let label = label.as_ref();
                self.write_id(self.id)?;
                write!(self.out, " -> ")?;
                self.write_id(to)?;
                writeln!(self.out, " [label=\"{}\"];", label)?;
                self.visit_if_unseen(to)
            }

            fn visit_if_unseen<E>(&mut self, e: E) -> io::Result<()>
            where
                E: Into<ExprId>,
            {
                let e = e.into();
                if self.seen.insert(e) {
                    let id = self.id;
                    self.id = e;
                    self.func.exprs[e].visit(self)?;
                    self.id = id;
                    Ok(())
                } else {
                    Ok(())
                }
            }

            fn binop<S, L, R>(&mut self, label: S, lhs: L, rhs: R) -> io::Result<()>
            where
                S: AsRef<str>,
                L: Into<ExprId>,
                R: Into<ExprId>,
            {
                self.node(label)?;
                self.edge(lhs, "lhs")?;
                self.edge(rhs, "rhs")
            }

            fn unop<S, E>(&mut self, label: S, e: E) -> io::Result<()>
            where
                S: AsRef<str>,
                E: Into<ExprId>,
            {
                self.node(label)?;
                self.edge(e, "expr")
            }
        }

        impl Visitor for DotVisitor<'_, '_> {
            type Return = io::Result<()>;

            fn visit_block(&mut self, e: &Block) -> io::Result<()> {
                let kind = match e.kind {
                    BlockKind::Block => "block",
                    BlockKind::Loop => "loop",
                };
                self.node(format!("{} ({})", kind, e.diagnostic))?;
                for (i, e) in e.exprs.iter().enumerate() {
                    self.edge(*e, format!("exprs[{}]", i))?;
                }
                if let BlockKind::Loop = e.kind {
                    self.edge(self.id, "loop")?;
                }
                Ok(())
            }

            fn visit_get_local(&mut self, e: &GetLocal) -> io::Result<()> {
                self.node(format!("get_local {}", e.local))
            }

            fn visit_set_local(&mut self, e: &SetLocal) -> io::Result<()> {
                self.node(format!("set_local {}", e.local))?;
                self.edge(e.value, "value")
            }

            fn visit_i32_const(&mut self, e: &I32Const) -> io::Result<()> {
                self.node(format!("i32.const {}", e.value))
            }

            fn visit_i32_add(&mut self, e: &I32Add) -> io::Result<()> {
                self.binop("i32.add", e.lhs, e.rhs)
            }

            fn visit_i32_sub(&mut self, e: &I32Sub) -> io::Result<()> {
                self.binop("i32.sub", e.lhs, e.rhs)
            }

            fn visit_i32_mul(&mut self, e: &I32Mul) -> io::Result<()> {
                self.binop("i32.mul", e.lhs, e.rhs)
            }

            fn visit_i32_eqz(&mut self, e: &I32Eqz) -> io::Result<()> {
                self.unop("i32.eqz", e.expr)
            }

            fn visit_i32_popcnt(&mut self, e: &I32Popcnt) -> io::Result<()> {
                self.unop("i32.popcnt", e.expr)
            }

            fn visit_select(&mut self, e: &Select) -> io::Result<()> {
                self.node("select")?;
                self.edge(e.condition, "condition")?;
                self.edge(e.consequent, "consequent")?;
                self.edge(e.alternative, "alternative")
            }

            fn visit_unreachable(&mut self, _: &Unreachable) -> io::Result<()> {
                self.node("unreachable")
            }

            fn visit_br(&mut self, e: &Br) -> io::Result<()> {
                self.node("br")?;
                self.edge(e.block, "block")?;
                for (i, a) in e.args.iter().enumerate() {
                    self.edge(*a, format!("parameter[{}]", i))?;
                }
                Ok(())
            }

            fn visit_br_if(&mut self, e: &BrIf) -> io::Result<()> {
                let block: ExprId = e.block.into();
                self.node("br_if")?;
                self.edge(e.condition, "condition")?;
                self.edge(block, "block")?;
                for (i, a) in e.args.iter().enumerate() {
                    self.edge(*a, format!("parameter[{}]", i))?;
                }
                Ok(())
            }

            fn visit_if_else(&mut self, e: &IfElse) -> io::Result<()> {
                self.node("if/else")?;
                self.edge(e.condition, "condition")?;
                self.edge(e.consequent, "consequent")?;
                self.edge(e.alternative, "alternative")
            }

            fn visit_br_table(&mut self, e: &BrTable) -> io::Result<()> {
                self.node("br_table")?;
                self.edge(e.which, "which")?;
                for (i, b) in e.blocks.iter().enumerate() {
                    self.edge(*b, format!("block[{}]", i))?;
                }
                self.edge(e.default, "default block")
            }

            fn visit_drop(&mut self, e: &Drop) -> io::Result<()> {
                self.unop("drop", e.expr)
            }

            fn visit_return(&mut self, e: &Return) -> io::Result<()> {
                self.node("return")?;
                for (i, v) in e.values.iter().enumerate() {
                    self.edge(*v, format!("values[{}]", i))?;
                }
                Ok(())
            }
        }

        writeln!(out, "digraph {{")?;
        writeln!(out, "rankdir=LR;")?;

        let v = &mut DotVisitor {
            out,
            func: self,
            id: self.entry_block().into(),
            seen: HashSet::new(),
        };
        v.visit_if_unseen(self.entry_block())?;

        writeln!(v.out, "subgraph unreachable {{")?;
        for (id, _) in self.exprs.iter() {
            v.visit_if_unseen(id)?;
        }
        writeln!(v.out, "}}")?;
        writeln!(out, "}}")
    }
}

macro_rules! const_ {
    ($ctx:ident, $op:ident, $ty:ident, $val:expr) => {
        let expr = $ctx.func.alloc($op { value: $val });
        $ctx.push_operand(Some(ValType::$ty), expr);
    };
}

macro_rules! binop {
    ($ctx:ident, $op:ident, $ty:ident) => {
        let (_, rhs) = $ctx.pop_operand_expected(Some(ValType::$ty))?;
        let (_, lhs) = $ctx.pop_operand_expected(Some(ValType::$ty))?;
        let expr = $ctx.func.alloc($op { lhs, rhs });
        $ctx.push_operand(Some(ValType::$ty), expr);
    };
}

macro_rules! unop {
    ($ctx:ident, $op:ident, $ty:ident) => {
        let (_, expr) = $ctx.pop_operand_expected(Some(ValType::$ty))?;
        let expr = $ctx.func.alloc($op { expr });
        $ctx.push_operand(Some(ValType::$ty), expr);
    };
}

macro_rules! testop {
    ($ctx:ident, $op:ident, $ty:ident) => {
        let (_, expr) = $ctx.pop_operand_expected(Some(ValType::$ty))?;
        let expr = $ctx.func.alloc($op { expr });
        $ctx.push_operand(Some(ValType::I32), expr);
    };
}

fn validate_instruction_sequence<'a>(
    ctx: &mut FunctionContext,
    insts: &'a [Instruction],
    until: Instruction,
) -> Result<&'a [Instruction]> {
    let mut insts = insts;
    loop {
        match insts.first() {
            None => {
                return Err(ErrorKind::InvalidWasm
                    .context(format!("expected `{}`", until))
                    .into());
            }
            Some(inst) if inst == &until => return Ok(&insts[1..]),
            Some(_) => {
                insts = validate_instruction(ctx, insts)?;
            }
        }
    }
}

fn validate_expression(ctx: &mut FunctionContext, expr: &[Instruction]) -> Result<Vec<ExprId>> {
    let rest = validate_instruction_sequence(ctx, expr, Instruction::End)?;
    let exprs = validate_end(ctx)?;
    if rest.is_empty() {
        Ok(exprs)
    } else {
        Err(ErrorKind::InvalidWasm
            .context("trailing instructions after final `end`")
            .into())
    }
}

fn validate_end(ctx: &mut FunctionContext) -> Result<Vec<ExprId>> {
    let (results, exprs) = ctx.pop_control()?;
    ctx.push_operands(&results, &exprs);
    Ok(exprs)
}

fn validate_instruction<'a>(
    ctx: &mut FunctionContext,
    insts: &'a [Instruction],
) -> Result<&'a [Instruction]> {
    assert!(!insts.is_empty());
    match &insts[0] {
        Instruction::GetLocal(n) => {
            let ty = ctx.validation.local(*n).context("invalid get_local")?;
            let expr = ctx.func.alloc(GetLocal { ty, local: *n });
            ctx.push_operand(Some(ty), expr);
        }
        Instruction::SetLocal(n) => {
            let ty = ctx.validation.local(*n).context("invalid set_local")?;
            let (_, value) = ctx.pop_operand_expected(Some(ty))?;
            let expr = ctx.func.alloc(SetLocal {
                ty,
                local: *n,
                value,
            });
            ctx.add_to_current_frame_block(expr);
        }
        Instruction::I32Const(n) => {
            const_!(ctx, I32Const, I32, *n);
        }
        Instruction::I32Add => {
            binop!(ctx, I32Add, I32);
        }
        Instruction::I32Sub => {
            binop!(ctx, I32Sub, I32);
        }
        Instruction::I32Mul => {
            binop!(ctx, I32Mul, I32);
        }
        Instruction::I32Eqz => {
            testop!(ctx, I32Eqz, I32);
        }
        Instruction::I32Popcnt => {
            unop!(ctx, I32Popcnt, I32);
        }
        Instruction::Drop => {
            let (_, expr) = ctx.pop_operand()?;
            let expr = ctx.func.alloc(Drop { expr });
            ctx.add_to_current_frame_block(expr);
        }
        Instruction::Select => {
            let (_, condition) = ctx.pop_operand_expected(Some(ValType::I32))?;
            let (t1, consequent) = ctx.pop_operand()?;
            let (t2, alternative) = ctx.pop_operand_expected(t1)?;
            let expr = ctx.func.alloc(Select {
                condition,
                consequent,
                alternative,
            });
            ctx.push_operand(t2, expr);
        }
        Instruction::Return => {
            let expected: Vec<_> = ctx
                .validation
                .return_
                .iter()
                .flat_map(|b| ValType::from_block_ty(b))
                .collect();
            let values = ctx.pop_operands(&expected)?.into_boxed_slice();
            let expr = ctx.func.alloc(Return { values });
            ctx.unreachable(expr);
            ctx.add_to_current_frame_block(expr);
        }
        Instruction::Unreachable => {
            let expr = ctx.func.alloc(Unreachable {});
            ctx.unreachable(expr);
            ctx.add_to_current_frame_block(expr);
        }
        Instruction::Block(block_ty) => {
            let validation = ctx.validation.for_block(*block_ty);
            let mut ctx = ctx.nested(&validation);
            let params = ValType::from_block_ty(block_ty);
            let params_is_empty = params.is_empty();
            let block = ctx.push_control("block", BlockKind::Block, params.clone(), params);
            let rest = validate_instruction_sequence(&mut ctx, &insts[1..], Instruction::End)?;
            validate_end(&mut ctx)?;
            if params_is_empty {
                ctx.add_to_current_frame_block(block);
            }
            return Ok(rest);
        }
        Instruction::Loop(block_ty) => {
            let validation = ctx.validation.for_loop();
            let mut ctx = ctx.nested(&validation);
            let t = ValType::from_block_ty(block_ty);
            let t_is_empty = t.is_empty();
            let block = ctx.push_control("loop", BlockKind::Loop, vec![], t);
            let rest = validate_instruction_sequence(&mut ctx, &insts[1..], Instruction::End)?;
            validate_end(&mut ctx)?;
            if t_is_empty {
                ctx.add_to_current_frame_block(block);
            }
            return Ok(rest);
        }
        Instruction::If(block_ty) => {
            let validation = ctx.validation.for_if_else(*block_ty);
            let mut ctx = ctx.nested(&validation);

            let (_, condition) = ctx.pop_operand_expected(Some(ValType::I32))?;

            let ty = ValType::from_block_ty(block_ty);
            let consequent =
                ctx.push_control("consequent", BlockKind::Block, ty.clone(), ty.clone());

            let rest = validate_instruction_sequence(&mut ctx, &insts[1..], Instruction::Else)?;
            let (results, _values) = ctx.pop_control()?;

            let alternative =
                ctx.push_control("alternative", BlockKind::Block, results.clone(), results);

            let rest_rest = validate_instruction_sequence(&mut ctx, rest, Instruction::End)?;
            let (results, _values) = ctx.pop_control()?;

            let expr = ctx.func.alloc(IfElse {
                condition,
                consequent,
                alternative,
            });
            if results.is_empty() {
                ctx.add_to_current_frame_block(expr);
            } else {
                let exprs: Vec<_> = results.iter().map(|_| expr).collect();
                ctx.push_operands(&results, &exprs);
            }

            return Ok(rest_rest);
        }
        Instruction::End => {
            return Err(ErrorKind::InvalidWasm
                .context("unexpected `end` instruction")
                .into());
        }
        Instruction::Else => {
            return Err(ErrorKind::InvalidWasm
                .context("`else` without a leading `if`")
                .into());
        }
        Instruction::Br(n) => {
            ctx.validation
                .label(*n)
                .context("`br` to out-of-bounds block")?;

            let n = *n as usize;
            if ctx.controls.len() <= n {
                return Err(ErrorKind::InvalidWasm
                    .context("attempt to branch to out-of-bounds block")
                    .into());
            }

            let expected = ctx.control(n).label_types.clone();
            let args = ctx.pop_operands(&expected)?.into_boxed_slice();

            let to_block = ctx.control(n + 1).block;
            let expr = ctx.func.alloc(Br {
                block: to_block,
                args,
            });
            ctx.unreachable(expr);
            ctx.add_to_current_frame_block(expr);
        }
        Instruction::BrIf(n) => {
            ctx.validation
                .label(*n)
                .context("`br_if` to out-of-bounds block")?;

            let n = *n as usize;
            if ctx.controls.len() <= n {
                return Err(ErrorKind::InvalidWasm
                    .context("attempt to branch to out-of-bounds block")
                    .into());
            }

            let (_, condition) = ctx.pop_operand_expected(Some(ValType::I32))?;

            let expected = ctx.control(n).label_types.clone();
            let args = ctx.pop_operands(&expected)?.into_boxed_slice();

            let to_block = ctx.control(n + 1).block;
            let expr = ctx.func.alloc(BrIf {
                condition,
                block: to_block,
                args,
            });
            if expected.is_empty() {
                ctx.add_to_current_frame_block(expr);
            } else {
                let exprs: Vec<_> = expected.iter().map(|_| expr).collect();
                ctx.push_operands(&expected, &exprs);
            }
        }
        Instruction::BrTable(table) => {
            ctx.validation
                .label(table.default)
                .context("`br_table` with out-of-bounds default block")?;
            if ctx.controls.len() < table.default as usize {
                return Err(ErrorKind::InvalidWasm
                    .context(
                        "attempt to jump to an out-of-bounds block from the default table entry",
                    )
                    .into());
            }
            let default = ctx.control(table.default as usize + 1).block;

            let mut blocks = Vec::with_capacity(table.table.len());
            for n in table.table.iter() {
                ctx.validation
                    .label(*n)
                    .context("`br_table` with out-of-bounds block")?;
                let n = *n as usize;
                if ctx.controls.len() < n {
                    return Err(ErrorKind::InvalidWasm
                        .context("attempt to jump to an out-of-bounds block from a table entry")
                        .into());
                }
                if ctx.control(n).label_types != ctx.control(table.default as usize).label_types {
                    return Err(ErrorKind::InvalidWasm
                        .context(
                            "attempt to jump to block non-matching label types from a table entry",
                        )
                        .into());
                }
                blocks.push(ctx.control(n + 1).block);
            }
            let blocks = blocks.into_boxed_slice();

            let (_, which) = ctx.pop_operand_expected(Some(ValType::I32))?;

            let expected = ctx.control(table.default as usize).label_types.clone();

            let args = ctx.pop_operands(&expected)?.into_boxed_slice();
            let expr = ctx.func.alloc(BrTable {
                which,
                blocks,
                default,
                args,
            });

            ctx.unreachable(expr);
            ctx.add_to_current_frame_block(expr);
        }

        op => unimplemented!("Have not implemented support for opcode yet: {:?}", op),
    }

    Ok(&insts[1..])
}
