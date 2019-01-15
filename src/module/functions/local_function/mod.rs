//! Functions defined locally within a wasm module.

mod context;
mod emit;
pub mod display;

use std::mem;
use self::context::FunctionContext;
use super::FunctionId;
use crate::dot::Dot;
use crate::error::{ErrorKind, Result};
use crate::ir::matcher::{ConstMatcher, Matcher};
use crate::ir::*;
use crate::module::emit::IdsToIndices;
use crate::module::Module;
use crate::ty::{TypeId, ValType};
use crate::validation_context::ValidationContext;
use failure::{Fail, ResultExt, format_err, bail};
use id_arena::{Arena, Id};
use parity_wasm::elements::{self, Instruction};
use std::collections::{BTreeSet, HashSet};
use std::fmt;
use std::iter;

/// A function defined locally within the wasm module.
#[derive(Debug)]
pub struct LocalFunction {
    /// This function's type.
    pub ty: TypeId,

    /// The arena that contains this function's expressions.
    pub(crate) exprs: Arena<Expr>,

    /// The entry block for this function. Always `Some` after the constructor
    /// returns.
    entry: Option<BlockId>,
    //
    // TODO: provenance: ExprId -> offset in code section of the original
    // instruction. This will be necessary for preserving debug info.
}

impl LocalFunction {
    /// Construct a new `LocalFunction`.
    ///
    /// Validates the given function body and constructs the `Expr` IR at the
    /// same time.
    pub fn parse(
        module: &mut Module,
        id: FunctionId,
        ty: TypeId,
        validation: &ValidationContext,
        body: &elements::FuncBody,
    ) -> Result<LocalFunction> {
        let validation = validation.for_function(&module.types.types()[ty], body)?;

        let mut func = LocalFunction {
            ty,
            exprs: Arena::new(),
            entry: None,
        };

        let result: Vec<_> = module.types.types()[ty].results().iter().cloned().collect();
        let result = result.into_boxed_slice();
        let result_len = result.len();

        let operands = &mut context::OperandStack::new();
        let controls = &mut context::ControlStack::new();

        let mut ctx = FunctionContext::new(module, id, &mut func, &validation, operands, controls);

        let entry = ctx.push_control(BlockKind::FunctionEntry, vec![].into_boxed_slice(), result);
        ctx.func.entry = Some(entry);
        validate_expression(&mut ctx, body.code().elements())?;

        debug_assert_eq!(ctx.operands.len(), result_len);
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

    /// Get the size of this function, in number of expressions.
    pub fn size(&self) -> u64 {
        struct SizeVisitor<'a> {
            func: &'a LocalFunction,
            exprs: u64,
        }

        impl<'expr> Visitor<'expr> for SizeVisitor<'expr> {
            fn local_function(&self) -> &'expr LocalFunction {
                self.func
            }

            fn visit_expr(&mut self, e: &'expr Expr) {
                self.exprs += 1;
                e.visit(self);
            }
        }

        let mut v = SizeVisitor {
            func: self,
            exprs: 0,
        };
        self.entry_block().visit(&mut v);
        v.exprs
    }

    /// Is this function's body a [constant
    /// expression](https://webassembly.github.io/spec/core/valid/instructions.html#constant-expressions)?
    pub fn is_const(&self) -> bool {
        let entry = match &self.exprs[self.entry_block().into()] {
            Expr::Block(b) => b,
            _ => unreachable!(),
        };
        let matcher = ConstMatcher::new();
        entry
            .exprs
            .iter()
            .all(|e| matcher.is_match(self, &self.exprs[*e]))
    }

    /// Emit this function's compact locals declarations.
    pub(crate) fn emit_locals(&self, indices: &mut IdsToIndices) -> Vec<elements::Local> {
        struct LocalsVisitor<'a> {
            func: &'a LocalFunction,
            seen: HashSet<ExprId>,

            // NB: Use `BTreeSet` to make compilation deterministic (since we iterate
            // over these sets) so our tests aren't busted.
            i32s: BTreeSet<LocalId>,
            i64s: BTreeSet<LocalId>,
            f32s: BTreeSet<LocalId>,
            f64s: BTreeSet<LocalId>,
            v128s: BTreeSet<LocalId>,
        }

        impl LocalsVisitor<'_> {
            fn visit<E>(&mut self, e: E)
            where
                E: Into<ExprId>,
            {
                let id = e.into();
                if self.seen.insert(id) {
                    self.func.exprs[id].visit(self);
                }
            }

            fn insert_local(&mut self, ty: ValType, local: LocalId) {
                match ty {
                    ValType::I32 => {
                        self.i32s.insert(local);
                    }
                    ValType::I64 => {
                        self.i64s.insert(local);
                    }
                    ValType::F32 => {
                        self.f32s.insert(local);
                    }
                    ValType::F64 => {
                        self.f64s.insert(local);
                    }
                    ValType::V128 => {
                        self.v128s.insert(local);
                    }
                }
            }
        }

        impl<'expr> Visitor<'expr> for LocalsVisitor<'expr> {
            fn local_function(&self) -> &'expr LocalFunction {
                self.func
            }

            // FIXME(#10) should use `visit_local_id` instead
            fn visit_local_get(&mut self, e: &LocalGet) {
                self.insert_local(e.ty, e.local);
            }

            // FIXME(#10) should use `visit_local_id` instead
            fn visit_local_set(&mut self, e: &LocalSet) {
                self.insert_local(e.ty, e.local);
                self.visit(e.value);
            }

            fn visit_global_get(&mut self, _: &GlobalGet) {}

            fn visit_global_set(&mut self, e: &GlobalSet) {
                self.visit(e.value);
            }
        }

        let mut v = LocalsVisitor {
            func: self,
            seen: Default::default(),
            i32s: Default::default(),
            i64s: Default::default(),
            f32s: Default::default(),
            f64s: Default::default(),
            v128s: Default::default(),
        };
        v.visit(self.entry_block());

        let mut locals = Vec::with_capacity(5);
        let mut idx = 0;

        let LocalsVisitor {
            i32s,
            i64s,
            f32s,
            f64s,
            v128s,
            ..
        } = v;

        if !i32s.is_empty() {
            locals.push(elements::Local::new(
                i32s.len() as u32,
                elements::ValueType::I32,
            ));
            for l in i32s {
                indices.set_local_index(l, idx);
                idx += 1;
            }
        }

        if !i64s.is_empty() {
            locals.push(elements::Local::new(
                i64s.len() as u32,
                elements::ValueType::I64,
            ));
            for l in i64s {
                indices.set_local_index(l, idx);
                idx += 1;
            }
        }

        if !f32s.is_empty() {
            locals.push(elements::Local::new(
                f32s.len() as u32,
                elements::ValueType::F32,
            ));
            for l in f32s {
                indices.set_local_index(l, idx);
                idx += 1;
            }
        }

        if !f64s.is_empty() {
            locals.push(elements::Local::new(
                f64s.len() as u32,
                elements::ValueType::F64,
            ));
            for l in f64s {
                indices.set_local_index(l, idx);
                idx += 1;
            }
        }

        if !v128s.is_empty() {
            locals.push(elements::Local::new(
                v128s.len() as u32,
                elements::ValueType::V128,
            ));
            for l in v128s {
                indices.set_local_index(l, idx);
                idx += 1;
            }
        }

        locals
    }

    /// Emit this function's instruction sequence.
    pub(crate) fn emit_instructions(&self, indices: &IdsToIndices) -> Vec<elements::Instruction> {
        emit::run(self, indices)
    }
}

impl fmt::Display for LocalFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::display::DisplayIr;
        let mut dst = String::new();
        self.display_ir(&mut dst, &(), 0);
        dst.fmt(f)
    }
}

impl Dot for LocalFunction {
    fn dot(&self, out: &mut String) {
        out.push_str("digraph {\n");
        out.push_str("rankdir=LR;\n");

        let v = &mut DotExpr {
            out,
            func: self,
            id: self.entry_block().into(),
            needs_close: false,
        };
        v.out.push_str("subgraph unreachable {\n");
        v.visit_block_id(&self.entry_block());
        // self.entry_block().visit(v);
        // for (id, _) in self.exprs.iter() {
        //     id.visit(v);
        // }
        v.close_previous();
        v.out.push_str("}\n");
        out.push_str("}\n");
    }
}

pub(crate) struct DotExpr<'a, 'b> {
    pub(crate) out: &'a mut String,
    pub(crate) func: &'b LocalFunction,
    id: ExprId,
    needs_close: bool,
}

impl DotExpr<'_, '_> {
    pub(crate) fn expr_id(&mut self, id: ExprId) {
        self.close_previous();
        let prev = mem::replace(&mut self.id, id);
        id.dot(self.out);
        self.out.push_str(
            " [label=<<table cellborder=\"0\" border=\"0\"><tr><td><font face=\"monospace\">"
        );
        self.needs_close = true;
        id.visit(self);
        self.close_previous();
        self.id = prev;
    }

    pub(crate) fn id<T>(&mut self, id: Id<T>) {
        self.out.push_str(" ");
        self.out.push_str(&id.index().to_string());
    }

    fn close_previous(&mut self) {
        if self.needs_close {
            self.out.push_str("</font></td></tr></table>>];\n")
        }
        self.needs_close = false;
    }

    pub(crate) fn edge<E, S>(&mut self, to: E, label: S)
    where
        E: Into<ExprId>,
        S: AsRef<str>,
    {
        self._edge(to.into(), label.as_ref())
    }

    fn _edge(&mut self, to: ExprId, label: &str) {
        self.close_previous();
        self.id.dot(self.out);
        self.out.push_str(" -> ");
        to.dot(self.out);
        self.out.push_str(&format!(" [label=\"{}\"];\n", label));
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
        Instruction::Call(idx) => {
            let fun_ty = ctx.validation.funcs.get(*idx as usize).ok_or_else(|| {
                ErrorKind::InvalidWasm
                    .context(format!("`call` instruction with invalid index {}", idx))
            })?;
            let func = ctx.module.funcs.function_for_index(*idx).expect(
                "if the validation context has a function for this index, then our module \
                 functions should too",
            );
            let param_tys: Vec<ValType> = fun_ty.params().iter().map(Into::into).collect();
            let args = ctx.pop_operands(&param_tys)?.into_boxed_slice();
            let expr = ctx.func.alloc(Call { func, args });
            let result_tys: Vec<ValType> = fun_ty.return_type().iter().map(Into::into).collect();
            let result_exprs: Vec<_> = iter::repeat(expr).take(result_tys.len()).collect();
            ctx.push_operands(&result_tys, &result_exprs);
        }
        Instruction::GetLocal(n) => {
            let ty = ctx.validation.local(*n).context("invalid get_local")?;
            let local = ctx
                .module
                .locals
                .local_for_function_and_index(ctx.func_id, ty, *n);
            let expr = ctx.func.alloc(LocalGet { ty, local });
            ctx.push_operand(Some(ty), expr);
        }
        Instruction::SetLocal(n) => {
            let ty = ctx.validation.local(*n).context("invalid local.set")?;
            let (_, value) = ctx.pop_operand_expected(Some(ty))?;
            let local = ctx
                .module
                .locals
                .local_for_function_and_index(ctx.func_id, ty, *n);
            let expr = ctx.func.alloc(LocalSet { ty, local, value });
            ctx.add_to_current_frame_block(expr);
        }
        Instruction::GetGlobal(n) => {
            let global = ctx.module.globals.global_for_index(*n)
                .ok_or_else(|| format_err!("invalid global.get index"))?;
            let ty = ctx.module.globals.arena[global].ty;
            let expr = ctx.func.alloc(GlobalGet { global });
            ctx.push_operand(Some(ty), expr);
        }
        Instruction::SetGlobal(n) => {
            let global = ctx.module.globals.global_for_index(*n)
                .ok_or_else(|| format_err!("invalid global.get index"))?;
            let ty = ctx.module.globals.arena[global].ty;
            let (_, value) = ctx.pop_operand_expected(Some(ty))?;
            let expr = ctx.func.alloc(GlobalSet { global, value });
            ctx.add_to_current_frame_block(expr);
        }
        Instruction::I32Const(n) => {
            const_!(ctx, Const, I32, Value::I32(*n));
        }
        Instruction::I64Const(n) => {
            const_!(ctx, Const, I64, Value::I64(*n));
        }
        Instruction::F32Const(n) => {
            const_!(ctx, Const, F32, Value::F32(f32::from_bits(*n)));
        }
        Instruction::F64Const(n) => {
            const_!(ctx, Const, F64, Value::F64(f64::from_bits(*n)));
        }
        Instruction::V128Const(n) => {
            let val = ((n[0] as u128) << 0)
                | ((n[1] as u128) << 8)
                | ((n[2] as u128) << 16)
                | ((n[3] as u128) << 24)
                | ((n[4] as u128) << 32)
                | ((n[5] as u128) << 40)
                | ((n[6] as u128) << 48)
                | ((n[7] as u128) << 56)
                | ((n[8] as u128) << 64)
                | ((n[9] as u128) << 72)
                | ((n[10] as u128) << 80)
                | ((n[11] as u128) << 88)
                | ((n[12] as u128) << 96)
                | ((n[13] as u128) << 104)
                | ((n[14] as u128) << 112)
                | ((n[15] as u128) << 120);
            const_!(ctx, Const, V128, Value::V128(val));
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
                .flat_map(|b| b.iter().cloned())
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
            let validation = ctx.validation.for_block(ValType::from_block_ty(block_ty));
            let mut ctx = ctx.nested(&validation);
            let params = ValType::from_block_ty(block_ty);
            let params_is_empty = params.is_empty();
            let block = ctx.push_control(BlockKind::Block, params.clone(), params);
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
            let block = ctx.push_control(BlockKind::Loop, vec![].into_boxed_slice(), t);
            let rest = validate_instruction_sequence(&mut ctx, &insts[1..], Instruction::End)?;
            validate_end(&mut ctx)?;
            if t_is_empty {
                ctx.add_to_current_frame_block(block);
            }
            return Ok(rest);
        }
        Instruction::If(block_ty) => {
            let validation = ctx.validation.for_if_else(ValType::from_block_ty(block_ty));
            let mut ctx = ctx.nested(&validation);

            let (_, condition) = ctx.pop_operand_expected(Some(ValType::I32))?;

            let ty = ValType::from_block_ty(block_ty);
            let consequent = ctx.push_control(BlockKind::IfElse, ty.clone(), ty.clone());

            let rest = validate_instruction_sequence(&mut ctx, &insts[1..], Instruction::Else)?;
            let (results, _values) = ctx.pop_control()?;

            let alternative = ctx.push_control(BlockKind::IfElse, results.clone(), results);

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

        Instruction::CurrentMemory(mem) => {
            let memory = match ctx.module.memories.memory_for_index(*mem as u32) {
                Some(id) => id,
                None => bail!("memory {} is out of bounds", mem),
            };
            let expr = ctx.func.alloc(MemorySize { memory });
            ctx.push_operand(Some(ValType::I32), expr);
        }

        op => bail!("Have not implemented support for opcode yet: {:?}", op),
    }

    Ok(&insts[1..])
}
