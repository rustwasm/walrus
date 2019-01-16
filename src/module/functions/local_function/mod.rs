//! Functions defined locally within a wasm module.

mod context;
pub mod display;
mod emit;

use self::context::FunctionContext;
use super::FunctionId;
use crate::dot::Dot;
use crate::emit::IdsToIndices;
use crate::error::{ErrorKind, Result};
use crate::ir::matcher::{ConstMatcher, Matcher};
use crate::ir::*;
use crate::module::locals::ModuleLocals;
use crate::module::parse::IndicesToIds;
use crate::module::Module;
use crate::ty::{TypeId, ValType};
use crate::validation_context::ValidationContext;
use failure::{bail, Fail, ResultExt};
use id_arena::{Arena, Id};
use parity_wasm::elements::{self, Instruction};
use std::collections::{BTreeMap, HashSet};
use std::fmt;
use std::iter;
use std::mem;

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
        indices: &IndicesToIds,
        id: FunctionId,
        ty: TypeId,
        validation: &ValidationContext,
        body: &elements::FuncBody,
    ) -> Result<LocalFunction> {
        let validation = validation.for_function(&module.types.get(ty), body)?;

        let mut func = LocalFunction {
            ty,
            exprs: Arena::new(),
            entry: None,
        };

        let result: Vec<_> = module.types.get(ty).results().iter().cloned().collect();
        let result = result.into_boxed_slice();
        let result_len = result.len();

        let operands = &mut context::OperandStack::new();
        let controls = &mut context::ControlStack::new();

        let mut ctx = FunctionContext::new(
            module,
            indices,
            id,
            &mut func,
            &validation,
            operands,
            controls,
        );

        let entry = ctx.push_control(BlockKind::FunctionEntry, vec![].into_boxed_slice(), result);
        ctx.func.entry = Some(entry);
        validate_expression(&mut ctx, body.code().elements(), entry.into())?;

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
    pub(crate) fn emit_locals(
        &self,
        locals: &ModuleLocals,
        indices: &mut IdsToIndices,
    ) -> Vec<elements::Local> {
        struct LocalsVisitor<'a> {
            func: &'a LocalFunction,
            locals: &'a ModuleLocals,
            seen: HashSet<LocalId>,
            // NB: Use `BTreeMap` to make compilation deterministic
            ty_to_locals: BTreeMap<ValType, Vec<LocalId>>,
        }

        impl<'expr> Visitor<'expr> for LocalsVisitor<'expr> {
            fn local_function(&self) -> &'expr LocalFunction {
                self.func
            }

            fn visit_local_id(&mut self, &id: &LocalId) {
                if self.seen.insert(id) {
                    let ty = self.locals.locals()[id].ty();
                    self.ty_to_locals.entry(ty).or_insert(Vec::new()).push(id);
                }
            }
        }

        let mut v = LocalsVisitor {
            func: self,
            locals,
            seen: HashSet::new(),
            ty_to_locals: BTreeMap::new(),
        };
        self.entry_block().visit(&mut v);

        let mut ret = Vec::with_capacity(5);
        let mut idx = 0;

        for (ty, locals) in v.ty_to_locals {
            let element_ty = match ty {
                ValType::I32 => elements::ValueType::I32,
                ValType::I64 => elements::ValueType::I64,
                ValType::F32 => elements::ValueType::F32,
                ValType::F64 => elements::ValueType::F64,
                ValType::V128 => elements::ValueType::V128,
            };
            ret.push(elements::Local::new(locals.len() as u32, element_ty));
            for l in locals {
                indices.set_local_index(l, idx);
                idx += 1;
            }
        }

        return ret;
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
        self.entry_block().visit(v);
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
            " [label=<<table cellborder=\"0\" border=\"0\"><tr><td><font face=\"monospace\">",
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

fn validate_expression(
    ctx: &mut FunctionContext,
    expr: &[Instruction],
    container: ExprId,
) -> Result<Vec<ExprId>> {
    let rest = validate_instruction_sequence(ctx, expr, Instruction::End)?;
    let exprs = validate_end(ctx, container)?;
    if rest.is_empty() {
        Ok(exprs)
    } else {
        Err(ErrorKind::InvalidWasm
            .context("trailing instructions after final `end`")
            .into())
    }
}

fn validate_end(ctx: &mut FunctionContext, expr: ExprId) -> Result<Vec<ExprId>> {
    let (results, exprs) = ctx.pop_control()?;
    ctx.push_operands(&results, &exprs, expr);
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
            let func = ctx.indices.get_func(*idx).context("invalid call")?;
            let param_tys: Vec<ValType> = fun_ty.params().iter().map(Into::into).collect();
            let args = ctx.pop_operands(&param_tys)?.into_boxed_slice();
            let expr = ctx.func.alloc(Call { func, args });
            let result_tys: Vec<ValType> = fun_ty.return_type().iter().map(Into::into).collect();
            let result_exprs: Vec<_> = iter::repeat(expr).take(result_tys.len()).collect();
            ctx.push_operands(&result_tys, &result_exprs, expr.into());
        }
        Instruction::CallIndirect(type_idx, table_idx) => {
            let type_id = ctx
                .indices
                .get_type(*type_idx)
                .context("invalid call_indirect")?;
            let ty = ctx.module.types.get(type_id).clone();
            let table = ctx
                .indices
                .get_table(*table_idx as u32)
                .context("invalid call_indirect")?;
            let (_, func) = ctx.pop_operand_expected(Some(ValType::I32))?;
            let args = ctx.pop_operands(ty.params())?.into_boxed_slice();
            let expr = ctx.func.alloc(CallIndirect {
                table,
                ty: type_id,
                func,
                args,
            });
            let result_exprs: Vec<_> = iter::repeat(expr).take(ty.results().len()).collect();
            ctx.push_operands(ty.results(), &result_exprs, expr.into());
        }
        Instruction::GetLocal(n) => {
            let ty = ctx.validation.local(*n).context("invalid get_local")?;
            let local = ctx
                .module
                .locals
                .local_for_function_and_index(ctx.func_id, ty, *n);
            let expr = ctx.func.alloc(LocalGet { local });
            ctx.push_operand(Some(ty), expr);
        }
        Instruction::SetLocal(n) => {
            let ty = ctx.validation.local(*n).context("invalid local.set")?;
            let (_, value) = ctx.pop_operand_expected(Some(ty))?;
            let local = ctx
                .module
                .locals
                .local_for_function_and_index(ctx.func_id, ty, *n);
            let expr = ctx.func.alloc(LocalSet { local, value });
            ctx.add_to_current_frame_block(expr);
        }
        Instruction::GetGlobal(n) => {
            let global = ctx.indices.get_global(*n).context("invalid global.get")?;
            let ty = ctx.module.globals.get(global).ty;
            let expr = ctx.func.alloc(GlobalGet { global });
            ctx.push_operand(Some(ty), expr);
        }
        Instruction::SetGlobal(n) => {
            let global = ctx.indices.get_global(*n).context("invalid global.set")?;
            let ty = ctx.module.globals.get(global).ty;
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
            let results = ValType::from_block_ty(block_ty);
            let block = ctx.push_control(BlockKind::Block, results.clone(), results);
            let rest = validate_instruction_sequence(&mut ctx, &insts[1..], Instruction::End)?;
            validate_end(&mut ctx, block.into())?;
            return Ok(rest);
        }
        Instruction::Loop(block_ty) => {
            let validation = ctx.validation.for_loop();
            let mut ctx = ctx.nested(&validation);
            let t = ValType::from_block_ty(block_ty);
            let block = ctx.push_control(BlockKind::Loop, vec![].into_boxed_slice(), t);
            let rest = validate_instruction_sequence(&mut ctx, &insts[1..], Instruction::End)?;
            validate_end(&mut ctx, block.into())?;
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
            let exprs: Vec<_> = results.iter().map(|_| expr).collect();
            ctx.push_operands(&results, &exprs, expr.into());

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
            let exprs: Vec<_> = expected.iter().map(|_| expr).collect();
            ctx.push_operands(&expected, &exprs, expr.into());
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
            let memory = ctx.indices.get_memory(*mem as u32)?;
            let expr = ctx.func.alloc(MemorySize { memory });
            ctx.push_operand(Some(ValType::I32), expr);
        }

        op => bail!("Have not implemented support for opcode yet: {:?}", op),
    }

    Ok(&insts[1..])
}
