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
use crate::module::Module;
use crate::parse::IndicesToIds;
use crate::ty::{TypeId, ValType};
use crate::validation_context::ValidationContext;
use failure::{bail, Fail, ResultExt};
use id_arena::{Arena, Id};
use parity_wasm::elements::{self, Instruction};
use std::collections::{BTreeMap, HashSet};
use std::fmt;
use std::mem;

/// A function defined locally within the wasm module.
#[derive(Debug)]
pub struct LocalFunction {
    /// This function's type.
    pub ty: TypeId,

    /// The arena that contains this function's expressions.
    pub(crate) exprs: Arena<Expr>,

    args: Vec<LocalId>,

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
        indices: &mut IndicesToIds,
        id: FunctionId,
        ty: TypeId,
        validation: &ValidationContext,
        body: &elements::FuncBody,
    ) -> Result<LocalFunction> {
        let validation = validation.for_function(&module.types.get(ty));

        // First up, implicitly add locals for all function arguments. We also
        // record these in the function itself for later processing.
        let mut args = Vec::new();
        for ty in module.types.get(ty).params() {
            let local_id = module.locals.add(*ty);
            indices.push_local(id, local_id);
            args.push(local_id);
        }

        // Next up, process all function body locals
        for local in body.locals() {
            let ty = ValType::from(&local.value_type());
            for _ in 0..local.count() {
                let local_id = module.locals.add(ty);
                indices.push_local(id, local_id);
            }
        }

        let mut func = LocalFunction {
            ty,
            exprs: Arena::new(),
            args,
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

        let entry = ctx.push_control(BlockKind::FunctionEntry, result.clone(), result);
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
            args: HashSet<LocalId>,
        }

        impl<'expr> Visitor<'expr> for LocalsVisitor<'expr> {
            fn local_function(&self) -> &'expr LocalFunction {
                self.func
            }

            fn visit_local_id(&mut self, &id: &LocalId) {
                if !self.seen.insert(id) {
                    return; // already seen? no more work to do
                }
                if self.args.contains(&id) {
                    return; // is this an argument? we'll handle that separately
                }
                let ty = self.locals.get(id).ty();
                self.ty_to_locals.entry(ty).or_insert(Vec::new()).push(id);
            }
        }

        let mut v = LocalsVisitor {
            func: self,
            locals,
            seen: HashSet::new(),
            ty_to_locals: BTreeMap::new(),
            args: self.args.iter().cloned().collect(),
        };
        self.entry_block().visit(&mut v);

        // First up allocate indices to the arguments of the function. These
        // arguments get the first few indexes in the local index space, and are
        // unconditionally used.
        let mut idx = 0;
        for &arg in self.args.iter() {
            indices.set_local_index(arg, idx);
            idx += 1;
        }

        // Next up assign chunks of locals all at once as we see them.
        let mut ret = Vec::with_capacity(5);
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

fn validate_instruction_sequence<'a>(
    ctx: &mut FunctionContext,
    insts: &'a [Instruction],
    until: Instruction,
) -> Result<&'a [Instruction]> {
    validate_instruction_sequence_until(ctx, insts, |i| *i == until)
}

fn validate_instruction_sequence_until<'a>(
    ctx: &mut FunctionContext,
    insts: &'a [Instruction],
    mut until: impl FnMut(&Instruction) -> bool,
) -> Result<&'a [Instruction]> {
    let mut insts = insts;
    loop {
        match insts.first() {
            None => bail!("unexpected end of instructions"),
            Some(inst) => {
                if until(inst) {
                    return Ok(&insts[1..]);
                }
                insts = validate_instruction(ctx, insts)?;
            }
        }
    }
}

fn validate_expression(ctx: &mut FunctionContext, expr: &[Instruction]) -> Result<BlockId> {
    let rest = validate_instruction_sequence(ctx, expr, Instruction::End)?;
    let block = validate_end(ctx)?;
    if rest.is_empty() {
        Ok(block)
    } else {
        Err(ErrorKind::InvalidWasm
            .context("trailing instructions after final `end`")
            .into())
    }
}

fn validate_end(ctx: &mut FunctionContext) -> Result<BlockId> {
    let (results, block) = ctx.pop_control()?;
    ctx.push_operands(&results, block.into());
    Ok(block)
}

fn validate_instruction<'a>(
    ctx: &mut FunctionContext,
    insts: &'a [Instruction],
) -> Result<&'a [Instruction]> {
    use ValType::*;

    let const_ = |ctx: &mut FunctionContext, ty, value| {
        let expr = ctx.func.alloc(Const { value });
        ctx.push_operand(Some(ty), expr);
    };

    let one_op = |ctx: &mut FunctionContext, input, output, op| -> Result<()> {
        let (_, expr) = ctx.pop_operand_expected(Some(input))?;
        let expr = ctx.func.alloc(Unop { op, expr });
        ctx.push_operand(Some(output), expr);
        Ok(())
    };
    let two_ops = |ctx: &mut FunctionContext, input, output, op| -> Result<()> {
        let (_, rhs) = ctx.pop_operand_expected(Some(input))?;
        let (_, lhs) = ctx.pop_operand_expected(Some(input))?;
        let expr = ctx.func.alloc(Binop { op, lhs, rhs });
        ctx.push_operand(Some(output), expr);
        Ok(())
    };

    let binop = |ctx: &mut FunctionContext, ty, op| -> Result<()> { two_ops(ctx, ty, ty, op) };

    let unop = |ctx: &mut FunctionContext, ty, op| -> Result<()> { one_op(ctx, ty, ty, op) };

    let testop =
        |ctx: &mut FunctionContext, ty, op| -> Result<()> { one_op(ctx, ty, ValType::I32, op) };

    let relop =
        |ctx: &mut FunctionContext, ty, op| -> Result<()> { two_ops(ctx, ty, ValType::I32, op) };

    assert!(!insts.is_empty());
    match &insts[0] {
        Instruction::Call(idx) => {
            let fun_id = ctx.indices.get_func(*idx).context("invalid call")?;
            let ty_id = ctx.module.funcs.get(fun_id).ty();
            let fun_ty = ctx.module.types.get(ty_id).clone();
            let func = ctx.indices.get_func(*idx).context("invalid call")?;
            let mut args = ctx.pop_operands(fun_ty.params())?.into_boxed_slice();
            args.reverse();
            let expr = ctx.func.alloc(Call { func, args });
            ctx.push_operands(fun_ty.results(), expr.into());
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
            let mut args = ctx.pop_operands(ty.params())?.into_boxed_slice();
            args.reverse();
            let expr = ctx.func.alloc(CallIndirect {
                table,
                ty: type_id,
                func,
                args,
            });
            ctx.push_operands(ty.results(), expr.into());
        }
        Instruction::GetLocal(n) => {
            let local = ctx.indices.get_local(ctx.func_id, *n)?;
            let ty = ctx.module.locals.get(local).ty();
            let expr = ctx.func.alloc(LocalGet { local });
            ctx.push_operand(Some(ty), expr);
        }
        Instruction::SetLocal(n) => {
            let local = ctx.indices.get_local(ctx.func_id, *n)?;
            let ty = ctx.module.locals.get(local).ty();
            let (_, value) = ctx.pop_operand_expected(Some(ty))?;
            let expr = ctx.func.alloc(LocalSet { local, value });
            ctx.add_to_current_frame_block(expr);
        }
        Instruction::TeeLocal(n) => {
            let local = ctx.indices.get_local(ctx.func_id, *n)?;
            let ty = ctx.module.locals.get(local).ty();
            let (_, value) = ctx.pop_operand_expected(Some(ty))?;
            let expr = ctx.func.alloc(LocalTee { local, value });
            ctx.push_operand(Some(ty), expr);
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
            const_(ctx, I32, Value::I32(*n));
        }
        Instruction::I64Const(n) => {
            const_(ctx, I64, Value::I64(*n));
        }
        Instruction::F32Const(n) => {
            const_(ctx, F32, Value::F32(f32::from_bits(*n)));
        }
        Instruction::F64Const(n) => {
            const_(ctx, F64, Value::F64(f64::from_bits(*n)));
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
            const_(ctx, V128, Value::V128(val));
        }

        Instruction::I32Eqz => testop(ctx, I32, UnaryOp::I32Eqz)?,
        Instruction::I32Eq => relop(ctx, I32, BinaryOp::I32Eq)?,
        Instruction::I32Ne => relop(ctx, I32, BinaryOp::I32Ne)?,
        Instruction::I32LtS => relop(ctx, I32, BinaryOp::I32LtS)?,
        Instruction::I32LtU => relop(ctx, I32, BinaryOp::I32LtU)?,
        Instruction::I32GtS => relop(ctx, I32, BinaryOp::I32GtS)?,
        Instruction::I32GtU => relop(ctx, I32, BinaryOp::I32GtU)?,
        Instruction::I32LeS => relop(ctx, I32, BinaryOp::I32LeS)?,
        Instruction::I32LeU => relop(ctx, I32, BinaryOp::I32LeU)?,
        Instruction::I32GeS => relop(ctx, I32, BinaryOp::I32GeS)?,
        Instruction::I32GeU => relop(ctx, I32, BinaryOp::I32GeU)?,

        Instruction::I64Eqz => testop(ctx, I64, UnaryOp::I64Eqz)?,
        Instruction::I64Eq => relop(ctx, I64, BinaryOp::I64Eq)?,
        Instruction::I64Ne => relop(ctx, I64, BinaryOp::I64Ne)?,
        Instruction::I64LtS => relop(ctx, I64, BinaryOp::I64LtS)?,
        Instruction::I64LtU => relop(ctx, I64, BinaryOp::I64LtU)?,
        Instruction::I64GtS => relop(ctx, I64, BinaryOp::I64GtS)?,
        Instruction::I64GtU => relop(ctx, I64, BinaryOp::I64GtU)?,
        Instruction::I64LeS => relop(ctx, I64, BinaryOp::I64LeS)?,
        Instruction::I64LeU => relop(ctx, I64, BinaryOp::I64LeU)?,
        Instruction::I64GeS => relop(ctx, I64, BinaryOp::I64GeS)?,
        Instruction::I64GeU => relop(ctx, I64, BinaryOp::I64GeU)?,

        Instruction::F32Eq => relop(ctx, F32, BinaryOp::F32Eq)?,
        Instruction::F32Ne => relop(ctx, F32, BinaryOp::F32Ne)?,
        Instruction::F32Lt => relop(ctx, F32, BinaryOp::F32Lt)?,
        Instruction::F32Gt => relop(ctx, F32, BinaryOp::F32Gt)?,
        Instruction::F32Le => relop(ctx, F32, BinaryOp::F32Le)?,
        Instruction::F32Ge => relop(ctx, F32, BinaryOp::F32Ge)?,

        Instruction::F64Eq => relop(ctx, F64, BinaryOp::F64Eq)?,
        Instruction::F64Ne => relop(ctx, F64, BinaryOp::F64Ne)?,
        Instruction::F64Lt => relop(ctx, F64, BinaryOp::F64Lt)?,
        Instruction::F64Gt => relop(ctx, F64, BinaryOp::F64Gt)?,
        Instruction::F64Le => relop(ctx, F64, BinaryOp::F64Le)?,
        Instruction::F64Ge => relop(ctx, F64, BinaryOp::F64Ge)?,

        Instruction::I32Clz => unop(ctx, I32, UnaryOp::I32Clz)?,
        Instruction::I32Ctz => unop(ctx, I32, UnaryOp::I32Ctz)?,
        Instruction::I32Popcnt => unop(ctx, I32, UnaryOp::I32Popcnt)?,
        Instruction::I32Add => binop(ctx, I32, BinaryOp::I32Add)?,
        Instruction::I32Sub => binop(ctx, I32, BinaryOp::I32Sub)?,
        Instruction::I32Mul => binop(ctx, I32, BinaryOp::I32Mul)?,
        Instruction::I32DivS => binop(ctx, I32, BinaryOp::I32DivS)?,
        Instruction::I32DivU => binop(ctx, I32, BinaryOp::I32DivU)?,
        Instruction::I32RemS => binop(ctx, I32, BinaryOp::I32RemS)?,
        Instruction::I32RemU => binop(ctx, I32, BinaryOp::I32RemU)?,
        Instruction::I32And => binop(ctx, I32, BinaryOp::I32And)?,
        Instruction::I32Or => binop(ctx, I32, BinaryOp::I32Or)?,
        Instruction::I32Xor => binop(ctx, I32, BinaryOp::I32Xor)?,
        Instruction::I32Shl => binop(ctx, I32, BinaryOp::I32Shl)?,
        Instruction::I32ShrS => binop(ctx, I32, BinaryOp::I32ShrS)?,
        Instruction::I32ShrU => binop(ctx, I32, BinaryOp::I32ShrU)?,
        Instruction::I32Rotl => binop(ctx, I32, BinaryOp::I32Rotl)?,
        Instruction::I32Rotr => binop(ctx, I32, BinaryOp::I32Rotr)?,

        Instruction::I64Clz => unop(ctx, I64, UnaryOp::I64Clz)?,
        Instruction::I64Ctz => unop(ctx, I64, UnaryOp::I64Ctz)?,
        Instruction::I64Popcnt => unop(ctx, I64, UnaryOp::I64Popcnt)?,
        Instruction::I64Add => binop(ctx, I64, BinaryOp::I64Add)?,
        Instruction::I64Sub => binop(ctx, I64, BinaryOp::I64Sub)?,
        Instruction::I64Mul => binop(ctx, I64, BinaryOp::I64Mul)?,
        Instruction::I64DivS => binop(ctx, I64, BinaryOp::I64DivS)?,
        Instruction::I64DivU => binop(ctx, I64, BinaryOp::I64DivU)?,
        Instruction::I64RemS => binop(ctx, I64, BinaryOp::I64RemS)?,
        Instruction::I64RemU => binop(ctx, I64, BinaryOp::I64RemU)?,
        Instruction::I64And => binop(ctx, I64, BinaryOp::I64And)?,
        Instruction::I64Or => binop(ctx, I64, BinaryOp::I64Or)?,
        Instruction::I64Xor => binop(ctx, I64, BinaryOp::I64Xor)?,
        Instruction::I64Shl => binop(ctx, I64, BinaryOp::I64Shl)?,
        Instruction::I64ShrS => binop(ctx, I64, BinaryOp::I64ShrS)?,
        Instruction::I64ShrU => binop(ctx, I64, BinaryOp::I64ShrU)?,
        Instruction::I64Rotl => binop(ctx, I64, BinaryOp::I64Rotl)?,
        Instruction::I64Rotr => binop(ctx, I64, BinaryOp::I64Rotr)?,

        Instruction::F32Abs => unop(ctx, F32, UnaryOp::F32Abs)?,
        Instruction::F32Neg => unop(ctx, F32, UnaryOp::F32Neg)?,
        Instruction::F32Ceil => unop(ctx, F32, UnaryOp::F32Ceil)?,
        Instruction::F32Floor => unop(ctx, F32, UnaryOp::F32Floor)?,
        Instruction::F32Trunc => unop(ctx, F32, UnaryOp::F32Trunc)?,
        Instruction::F32Nearest => unop(ctx, F32, UnaryOp::F32Nearest)?,
        Instruction::F32Sqrt => unop(ctx, F32, UnaryOp::F32Sqrt)?,
        Instruction::F32Add => binop(ctx, F32, BinaryOp::F32Add)?,
        Instruction::F32Sub => binop(ctx, F32, BinaryOp::F32Sub)?,
        Instruction::F32Mul => binop(ctx, F32, BinaryOp::F32Mul)?,
        Instruction::F32Div => binop(ctx, F32, BinaryOp::F32Div)?,
        Instruction::F32Min => binop(ctx, F32, BinaryOp::F32Min)?,
        Instruction::F32Max => binop(ctx, F32, BinaryOp::F32Max)?,
        Instruction::F32Copysign => binop(ctx, F32, BinaryOp::F32Copysign)?,

        Instruction::F64Abs => unop(ctx, F64, UnaryOp::F64Abs)?,
        Instruction::F64Neg => unop(ctx, F64, UnaryOp::F64Neg)?,
        Instruction::F64Ceil => unop(ctx, F64, UnaryOp::F64Ceil)?,
        Instruction::F64Floor => unop(ctx, F64, UnaryOp::F64Floor)?,
        Instruction::F64Trunc => unop(ctx, F64, UnaryOp::F64Trunc)?,
        Instruction::F64Nearest => unop(ctx, F64, UnaryOp::F64Nearest)?,
        Instruction::F64Sqrt => unop(ctx, F64, UnaryOp::F64Sqrt)?,
        Instruction::F64Add => binop(ctx, F64, BinaryOp::F64Add)?,
        Instruction::F64Sub => binop(ctx, F64, BinaryOp::F64Sub)?,
        Instruction::F64Mul => binop(ctx, F64, BinaryOp::F64Mul)?,
        Instruction::F64Div => binop(ctx, F64, BinaryOp::F64Div)?,
        Instruction::F64Min => binop(ctx, F64, BinaryOp::F64Min)?,
        Instruction::F64Max => binop(ctx, F64, BinaryOp::F64Max)?,
        Instruction::F64Copysign => binop(ctx, F64, BinaryOp::F64Copysign)?,

        Instruction::I32WrapI64 => one_op(ctx, I64, I32, UnaryOp::I32WrapI64)?,
        Instruction::I32TruncSF32 => one_op(ctx, F32, I32, UnaryOp::I32TruncSF32)?,
        Instruction::I32TruncUF32 => one_op(ctx, F32, I32, UnaryOp::I32TruncUF32)?,
        Instruction::I32TruncSF64 => one_op(ctx, F64, I32, UnaryOp::I32TruncSF64)?,
        Instruction::I32TruncUF64 => one_op(ctx, F64, I32, UnaryOp::I32TruncUF64)?,

        Instruction::I64ExtendSI32 => one_op(ctx, I32, I64, UnaryOp::I64ExtendSI32)?,
        Instruction::I64ExtendUI32 => one_op(ctx, I32, I64, UnaryOp::I64ExtendUI32)?,
        Instruction::I64TruncSF32 => one_op(ctx, F32, I64, UnaryOp::I64TruncSF32)?,
        Instruction::I64TruncUF32 => one_op(ctx, F32, I64, UnaryOp::I64TruncUF32)?,
        Instruction::I64TruncSF64 => one_op(ctx, F64, I64, UnaryOp::I64TruncSF64)?,
        Instruction::I64TruncUF64 => one_op(ctx, F64, I64, UnaryOp::I64TruncUF64)?,

        Instruction::F32ConvertSI32 => one_op(ctx, I32, F32, UnaryOp::F32ConvertSI32)?,
        Instruction::F32ConvertUI32 => one_op(ctx, I32, F32, UnaryOp::F32ConvertUI32)?,
        Instruction::F32ConvertSI64 => one_op(ctx, I64, F32, UnaryOp::F32ConvertSI64)?,
        Instruction::F32ConvertUI64 => one_op(ctx, I64, F32, UnaryOp::F32ConvertUI64)?,
        Instruction::F32DemoteF64 => one_op(ctx, F64, F32, UnaryOp::F32DemoteF64)?,

        Instruction::F64ConvertSI32 => one_op(ctx, I32, F64, UnaryOp::F64ConvertSI32)?,
        Instruction::F64ConvertUI32 => one_op(ctx, I32, F64, UnaryOp::F64ConvertUI32)?,
        Instruction::F64ConvertSI64 => one_op(ctx, I64, F64, UnaryOp::F64ConvertSI64)?,
        Instruction::F64ConvertUI64 => one_op(ctx, I64, F64, UnaryOp::F64ConvertUI64)?,
        Instruction::F64PromoteF32 => one_op(ctx, F32, F64, UnaryOp::F64PromoteF32)?,

        Instruction::I32ReinterpretF32 => one_op(ctx, F32, I32, UnaryOp::I32ReinterpretF32)?,
        Instruction::I64ReinterpretF64 => one_op(ctx, F64, I64, UnaryOp::I64ReinterpretF64)?,
        Instruction::F32ReinterpretI32 => one_op(ctx, I32, F32, UnaryOp::F32ReinterpretI32)?,
        Instruction::F64ReinterpretI64 => one_op(ctx, I64, F64, UnaryOp::F64ReinterpretI64)?,

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
        }
        Instruction::Unreachable => {
            let expr = ctx.func.alloc(Unreachable {});
            ctx.unreachable(expr);
        }
        Instruction::Block(block_ty) => {
            let validation = ctx.validation.for_block(ValType::from_block_ty(block_ty));
            let mut ctx = ctx.nested(&validation);
            let results = ValType::from_block_ty(block_ty);
            ctx.push_control(BlockKind::Block, results.clone(), results);
            let rest = validate_instruction_sequence(&mut ctx, &insts[1..], Instruction::End)?;
            validate_end(&mut ctx)?;
            return Ok(rest);
        }
        Instruction::Loop(block_ty) => {
            let validation = ctx.validation.for_loop();
            let mut ctx = ctx.nested(&validation);
            let t = ValType::from_block_ty(block_ty);
            ctx.push_control(BlockKind::Loop, vec![].into_boxed_slice(), t);
            let rest = validate_instruction_sequence(&mut ctx, &insts[1..], Instruction::End)?;
            validate_end(&mut ctx)?;
            return Ok(rest);
        }
        Instruction::If(block_ty) => {
            let validation = ctx.validation.for_if_else(ValType::from_block_ty(block_ty));
            let mut ctx = ctx.nested(&validation);

            let (_, condition) = ctx.pop_operand_expected(Some(ValType::I32))?;

            let ty = ValType::from_block_ty(block_ty);
            let consequent = ctx.push_control(BlockKind::IfElse, ty.clone(), ty.clone());

            let mut else_found = false;
            let mut rest = validate_instruction_sequence_until(&mut ctx, &insts[1..], |i| {
                if *i == Instruction::Else {
                    else_found = true;
                    true
                } else {
                    *i == Instruction::End
                }
            })?;
            let (results, _block) = ctx.pop_control()?;

            let alternative = ctx.push_control(BlockKind::IfElse, results.clone(), results);

            if else_found {
                rest = validate_instruction_sequence(&mut ctx, rest, Instruction::End)?;
            }
            let (results, _block) = ctx.pop_control()?;

            let expr = ctx.func.alloc(IfElse {
                condition,
                consequent,
                alternative,
            });
            ctx.push_operands(&results, expr.into());

            return Ok(rest);
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

            let to_block = ctx.control(n).block;
            let expr = ctx.func.alloc(Br {
                block: to_block,
                args,
            });
            ctx.unreachable(expr);
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

            let to_block = ctx.control(n).block;
            let expr = ctx.func.alloc(BrIf {
                condition,
                block: to_block,
                args,
            });
            ctx.push_operands(&expected, expr.into());
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
            let default = ctx.control(table.default as usize).block;

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
                blocks.push(ctx.control(n).block);
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
        }

        Instruction::CurrentMemory(mem) => {
            let memory = ctx.indices.get_memory(*mem as u32)?;
            let expr = ctx.func.alloc(MemorySize { memory });
            ctx.push_operand(Some(ValType::I32), expr);
        }
        Instruction::GrowMemory(mem) => {
            let (_, pages) = ctx.pop_operand_expected(Some(ValType::I32))?;
            let memory = ctx.indices.get_memory(*mem as u32)?;
            let expr = ctx.func.alloc(MemoryGrow { memory, pages });
            ctx.push_operand(Some(ValType::I32), expr);
        }

        Instruction::Nop => {}

        op => bail!("Have not implemented support for opcode yet: {:?}", op),
    }

    Ok(&insts[1..])
}
