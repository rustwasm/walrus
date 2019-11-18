//! Functions defined locally within a wasm module.

mod context;
mod emit;

use self::context::ValidationContext;
use crate::emit::IdsToIndices;
use crate::encode::Encoder;
use crate::ir::*;
use crate::map::{IdHashMap, IdHashSet};
use crate::parse::IndicesToIds;
use crate::{
    Data, DataId, FunctionBuilder, FunctionId, Module, Result, TableKind, TypeId, ValType,
};
use anyhow::{bail, Context};
use std::collections::BTreeMap;
use wasmparser::Operator;

/// A function defined locally within the wasm module.
#[derive(Debug)]
pub struct LocalFunction {
    /// All of this function's instructions, contained in the arena.
    builder: FunctionBuilder,

    /// Arguments to this function, and the locals that they're assigned to.
    pub args: Vec<LocalId>,
    //
    // TODO: provenance: (InstrSeqId, usize) -> offset in code section of the
    // original instruction. This will be necessary for preserving debug info.
}

impl LocalFunction {
    /// Creates a new definition of a local function from its components.
    pub(crate) fn new(args: Vec<LocalId>, builder: FunctionBuilder) -> LocalFunction {
        LocalFunction { args, builder }
    }

    /// Construct a new `LocalFunction`.
    ///
    /// Validates the given function body and constructs the `Instr` IR at the
    /// same time.
    pub(crate) fn parse(
        module: &Module,
        indices: &IndicesToIds,
        id: FunctionId,
        ty: TypeId,
        args: Vec<LocalId>,
        mut body: wasmparser::OperatorsReader,
        on_instr_pos: Option<&(dyn Fn(&usize) -> InstrLocId + Sync + Send + 'static)>,
    ) -> Result<LocalFunction> {
        let mut func = LocalFunction {
            builder: FunctionBuilder::without_entry(ty),
            args,
        };

        let result: Vec<_> = module.types.get(ty).results().iter().cloned().collect();
        let result = result.into_boxed_slice();
        let result_len = result.len();

        let operands = &mut context::OperandStack::new();
        let controls = &mut context::ControlStack::new();

        let mut ctx = ValidationContext::new(module, indices, id, &mut func, operands, controls);

        let ty = module.types.find_for_function_entry(&result).expect(
            "the function entry type should have already been created before parsing the body",
        );
        let entry = ctx.push_control_with_ty(BlockKind::FunctionEntry, ty);
        ctx.func.builder.entry = Some(entry);
        while !body.eof() {
            let (inst, pos) = body.read_with_offset()?;
            let loc = if let Some(ref on_instr_pos) = on_instr_pos {
                on_instr_pos(&pos)
            } else {
                InstrLocId::new(pos as u32)
            };
            validate_instruction(&mut ctx, inst, loc)?;
        }
        if !ctx.controls.is_empty() {
            bail!("function failed to end with `end`");
        }

        debug_assert_eq!(ctx.operands.len(), result_len);
        debug_assert!(ctx.controls.is_empty());

        Ok(func)
    }

    /// Get this function's type.
    #[inline]
    pub fn ty(&self) -> TypeId {
        self.builder.ty
    }

    pub(crate) fn add_block(
        &mut self,
        make_block: impl FnOnce(InstrSeqId) -> InstrSeq,
    ) -> InstrSeqId {
        self.builder.arena.alloc_with_id(make_block)
    }

    /// Get the id of this function's entry block.
    pub fn entry_block(&self) -> InstrSeqId {
        self.builder.entry.unwrap()
    }

    /// Get the block associated with the given id.
    pub fn block(&self, id: InstrSeqId) -> &InstrSeq {
        &self.builder.arena[id]
    }

    /// Get the block associated with the given id.
    pub fn block_mut(&mut self, id: InstrSeqId) -> &mut InstrSeq {
        &mut self.builder.arena[id]
    }

    /// Get access to a `FunctionBuilder` to continue adding instructions to
    /// this function.
    pub fn builder(&self) -> &FunctionBuilder {
        &self.builder
    }

    /// Get access to a `FunctionBuilder` to continue adding instructions to
    /// this function.
    pub fn builder_mut(&mut self) -> &mut FunctionBuilder {
        &mut self.builder
    }

    /// Get the size of this function, in number of instructions.
    pub fn size(&self) -> u64 {
        let mut v = SizeVisitor::default();
        dfs_in_order(&mut v, self, self.entry_block());
        return v.size;

        #[derive(Default)]
        struct SizeVisitor {
            size: u64,
        }

        impl<'instr> Visitor<'instr> for SizeVisitor {
            fn start_instr_seq(&mut self, seq: &'instr InstrSeq) {
                self.size += seq.len() as u64;
            }
        }
    }

    /// Is this function's body a [constant
    /// instruction](https://webassembly.github.io/spec/core/valid/instructions.html#constant-instructions)?
    pub fn is_const(&self) -> bool {
        self.block(self.entry_block())
            .instrs
            .iter()
            .all(|(e, _)| e.is_const())
    }

    /// Collect the set of data segments that are used in this function via
    /// `memory.init` or `data.drop` instructions.
    pub fn used_data_segments(&self) -> IdHashSet<Data> {
        let mut visitor = DataSegmentsVisitor::default();
        dfs_in_order(&mut visitor, self, self.entry_block());
        return visitor.segments;

        #[derive(Default)]
        struct DataSegmentsVisitor {
            segments: IdHashSet<Data>,
        }

        impl<'a> Visitor<'a> for DataSegmentsVisitor {
            fn visit_data_id(&mut self, id: &DataId) {
                self.segments.insert(*id);
            }
        }
    }

    fn used_locals(&self) -> IdHashSet<Local> {
        let mut locals = Used::default();
        dfs_in_order(&mut locals, self, self.entry_block());
        return locals.locals;

        #[derive(Default)]
        struct Used {
            locals: IdHashSet<Local>,
        }

        impl<'a> Visitor<'a> for Used {
            fn visit_local_id(&mut self, id: &LocalId) {
                self.locals.insert(*id);
            }
        }
    }

    /// Emit this function's compact locals declarations.
    pub(crate) fn emit_locals(
        &self,
        module: &Module,
        encoder: &mut Encoder,
    ) -> (IdHashSet<Local>, IdHashMap<Local, u32>) {
        let used_set = self.used_locals();
        let mut used_locals = used_set.iter().cloned().collect::<Vec<_>>();
        // Sort to ensure we assign local indexes deterministically, and
        // everything is distinct so we can use a faster unstable sort.
        used_locals.sort_unstable();

        // NB: Use `BTreeMap` to make compilation deterministic by emitting
        // types in the same order
        let mut ty_to_locals = BTreeMap::new();
        let args = self.args.iter().cloned().collect::<IdHashSet<_>>();

        // Partition all locals by their type as we'll create at most one entry
        // for each type. Skip all arguments to the function because they're
        // handled separately.
        for local in used_locals.iter() {
            if !args.contains(local) {
                let ty = module.locals.get(*local).ty();
                ty_to_locals.entry(ty).or_insert_with(Vec::new).push(*local);
            }
        }

        let mut local_map = IdHashMap::default();
        local_map.reserve(used_locals.len());

        // Allocate an index to all the function arguments, as these are all
        // unconditionally used and are implicit locals in wasm.
        let mut idx = 0;
        for &arg in self.args.iter() {
            local_map.insert(arg, idx);
            idx += 1;
        }

        // Assign an index to all remaining locals
        for (_, locals) in ty_to_locals.iter() {
            for l in locals {
                local_map.insert(*l, idx);
                idx += 1;
            }
        }

        // Use our type map to emit a compact representation of all locals now
        encoder.usize(ty_to_locals.len());
        for (ty, locals) in ty_to_locals.iter() {
            encoder.usize(locals.len());
            ty.emit(encoder);
        }

        (used_set, local_map)
    }

    /// Emit this function's instruction sequence.
    pub(crate) fn emit_instructions(
        &self,
        indices: &IdsToIndices,
        local_indices: &IdHashMap<Local, u32>,
        dst: &mut Encoder,
        map: Option<&mut Vec<(InstrLocId, usize)>>,
    ) {
        emit::run(self, indices, local_indices, dst, map)
    }
}

fn block_result_tys(
    ctx: &ValidationContext,
    ty: wasmparser::TypeOrFuncType,
) -> Result<Box<[ValType]>> {
    match ty {
        wasmparser::TypeOrFuncType::Type(ty) => ValType::from_wasmparser_type(ty).map(Into::into),
        wasmparser::TypeOrFuncType::FuncType(idx) => {
            let ty = ctx.indices.get_type(idx)?;
            Ok(ctx.module.types.results(ty).into())
        }
    }
}

fn block_param_tys(
    ctx: &ValidationContext,
    ty: wasmparser::TypeOrFuncType,
) -> Result<Box<[ValType]>> {
    match ty {
        wasmparser::TypeOrFuncType::Type(_) => Ok([][..].into()),
        wasmparser::TypeOrFuncType::FuncType(idx) => {
            let ty = ctx.indices.get_type(idx)?;
            Ok(ctx.module.types.params(ty).into())
        }
    }
}

fn validate_instruction<'context>(
    ctx: &'context mut ValidationContext,
    inst: Operator,
    loc: InstrLocId,
) -> Result<()> {
    use crate::ir::ExtendedLoad::*;
    use crate::ValType::*;

    log::trace!("validate instruction: {:?}", inst);

    let const_ = |ctx: &mut ValidationContext, ty, value| {
        ctx.alloc_instr(Const { value }, loc);
        ctx.push_operand(Some(ty));
    };

    let one_op = |ctx: &mut ValidationContext, input, output, op| -> Result<()> {
        ctx.pop_operand_expected(Some(input))?;
        ctx.alloc_instr(Unop { op }, loc);
        ctx.push_operand(Some(output));
        Ok(())
    };
    let two_ops = |ctx: &mut ValidationContext, lhs, rhs, output, op| -> Result<()> {
        ctx.pop_operand_expected(Some(rhs))?;
        ctx.pop_operand_expected(Some(lhs))?;
        ctx.alloc_instr(Binop { op }, loc);
        ctx.push_operand(Some(output));
        Ok(())
    };

    let binop = |ctx, ty, op| two_ops(ctx, ty, ty, ty, op);
    let unop = |ctx, ty, op| one_op(ctx, ty, ty, op);
    let testop = |ctx, ty, op| one_op(ctx, ty, I32, op);
    let relop = |ctx, ty, op| two_ops(ctx, ty, ty, I32, op);

    let mem_arg = |arg: &wasmparser::MemoryImmediate| -> Result<MemArg> {
        if arg.flags >= 32 {
            bail!("invalid alignment");
        }
        Ok(MemArg {
            align: 1 << (arg.flags as i32),
            offset: arg.offset,
        })
    };

    let load = |ctx: &mut ValidationContext, arg, ty, kind| -> Result<()> {
        ctx.pop_operand_expected(Some(I32))?;
        let memory = ctx.indices.get_memory(0)?;
        let arg = mem_arg(&arg)?;
        ctx.alloc_instr(Load { arg, kind, memory }, loc);
        ctx.push_operand(Some(ty));
        Ok(())
    };

    let store = |ctx: &mut ValidationContext, arg, ty, kind| -> Result<()> {
        ctx.pop_operand_expected(Some(ty))?;
        ctx.pop_operand_expected(Some(I32))?;
        let memory = ctx.indices.get_memory(0)?;
        let arg = mem_arg(&arg)?;
        ctx.alloc_instr(Store { arg, kind, memory }, loc);
        Ok(())
    };

    let atomicrmw = |ctx: &mut ValidationContext, arg, ty, op, width| -> Result<()> {
        ctx.pop_operand_expected(Some(ty))?;
        ctx.pop_operand_expected(Some(I32))?;
        let memory = ctx.indices.get_memory(0)?;
        let arg = mem_arg(&arg)?;
        ctx.alloc_instr(
            AtomicRmw {
                arg,
                memory,
                op,
                width,
            },
            loc,
        );
        ctx.push_operand(Some(ty));
        Ok(())
    };

    let cmpxchg = |ctx: &mut ValidationContext, arg, ty, width| -> Result<()> {
        ctx.pop_operand_expected(Some(ty))?;
        ctx.pop_operand_expected(Some(ty))?;
        ctx.pop_operand_expected(Some(I32))?;
        let memory = ctx.indices.get_memory(0)?;
        let arg = mem_arg(&arg)?;
        ctx.alloc_instr(Cmpxchg { arg, memory, width }, loc);
        ctx.push_operand(Some(ty));
        Ok(())
    };

    let load_splat = |ctx: &mut ValidationContext, arg, kind| -> Result<()> {
        ctx.pop_operand_expected(Some(I32))?;
        let memory = ctx.indices.get_memory(0)?;
        let arg = mem_arg(&arg)?;
        ctx.alloc_instr(LoadSplat { memory, arg, kind }, loc);
        ctx.push_operand(Some(V128));
        Ok(())
    };
    match inst {
        Operator::Call { function_index } => {
            let func = ctx
                .indices
                .get_func(function_index)
                .context("invalid call")?;
            let ty_id = ctx.module.funcs.get(func).ty();
            let fun_ty = ctx.module.types.get(ty_id);
            ctx.pop_operands(fun_ty.params())?;
            ctx.alloc_instr(Call { func }, loc);
            ctx.push_operands(fun_ty.results());
        }
        Operator::CallIndirect { index, table_index } => {
            let type_id = ctx
                .indices
                .get_type(index)
                .context("invalid call_indirect")?;
            let ty = ctx.module.types.get(type_id);
            let table = ctx
                .indices
                .get_table(table_index)
                .context("invalid call_indirect")?;
            ctx.pop_operand_expected(Some(I32))?;
            ctx.pop_operands(ty.params())?;
            ctx.alloc_instr(CallIndirect { table, ty: type_id }, loc);
            ctx.push_operands(ty.results());
        }
        Operator::LocalGet { local_index } => {
            let local = ctx.indices.get_local(ctx.func_id, local_index)?;
            let ty = ctx.module.locals.get(local).ty();
            ctx.alloc_instr(LocalGet { local }, loc);
            ctx.push_operand(Some(ty));
        }
        Operator::LocalSet { local_index } => {
            let local = ctx.indices.get_local(ctx.func_id, local_index)?;
            let ty = ctx.module.locals.get(local).ty();
            ctx.pop_operand_expected(Some(ty))?;
            ctx.alloc_instr(LocalSet { local }, loc);
        }
        Operator::LocalTee { local_index } => {
            let local = ctx.indices.get_local(ctx.func_id, local_index)?;
            let ty = ctx.module.locals.get(local).ty();
            ctx.pop_operand_expected(Some(ty))?;
            ctx.alloc_instr(LocalTee { local }, loc);
            ctx.push_operand(Some(ty));
        }
        Operator::GlobalGet { global_index } => {
            let global = ctx
                .indices
                .get_global(global_index)
                .context("invalid global.get")?;
            let ty = ctx.module.globals.get(global).ty;
            ctx.alloc_instr(GlobalGet { global }, loc);
            ctx.push_operand(Some(ty));
        }
        Operator::GlobalSet { global_index } => {
            let global = ctx
                .indices
                .get_global(global_index)
                .context("invalid global.set")?;
            let ty = ctx.module.globals.get(global).ty;
            ctx.pop_operand_expected(Some(ty))?;
            ctx.alloc_instr(GlobalSet { global }, loc);
        }
        Operator::I32Const { value } => const_(ctx, I32, Value::I32(value)),
        Operator::I64Const { value } => const_(ctx, I64, Value::I64(value)),
        Operator::F32Const { value } => {
            const_(ctx, F32, Value::F32(f32::from_bits(value.bits())));
        }
        Operator::F64Const { value } => {
            const_(ctx, F64, Value::F64(f64::from_bits(value.bits())));
        }
        Operator::V128Const { value } => {
            let n = value.bytes();
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
        Operator::I32Eqz => testop(ctx, I32, UnaryOp::I32Eqz)?,
        Operator::I32Eq => relop(ctx, I32, BinaryOp::I32Eq)?,
        Operator::I32Ne => relop(ctx, I32, BinaryOp::I32Ne)?,
        Operator::I32LtS => relop(ctx, I32, BinaryOp::I32LtS)?,
        Operator::I32LtU => relop(ctx, I32, BinaryOp::I32LtU)?,
        Operator::I32GtS => relop(ctx, I32, BinaryOp::I32GtS)?,
        Operator::I32GtU => relop(ctx, I32, BinaryOp::I32GtU)?,
        Operator::I32LeS => relop(ctx, I32, BinaryOp::I32LeS)?,
        Operator::I32LeU => relop(ctx, I32, BinaryOp::I32LeU)?,
        Operator::I32GeS => relop(ctx, I32, BinaryOp::I32GeS)?,
        Operator::I32GeU => relop(ctx, I32, BinaryOp::I32GeU)?,

        Operator::I64Eqz => testop(ctx, I64, UnaryOp::I64Eqz)?,
        Operator::I64Eq => relop(ctx, I64, BinaryOp::I64Eq)?,
        Operator::I64Ne => relop(ctx, I64, BinaryOp::I64Ne)?,
        Operator::I64LtS => relop(ctx, I64, BinaryOp::I64LtS)?,
        Operator::I64LtU => relop(ctx, I64, BinaryOp::I64LtU)?,
        Operator::I64GtS => relop(ctx, I64, BinaryOp::I64GtS)?,
        Operator::I64GtU => relop(ctx, I64, BinaryOp::I64GtU)?,
        Operator::I64LeS => relop(ctx, I64, BinaryOp::I64LeS)?,
        Operator::I64LeU => relop(ctx, I64, BinaryOp::I64LeU)?,
        Operator::I64GeS => relop(ctx, I64, BinaryOp::I64GeS)?,
        Operator::I64GeU => relop(ctx, I64, BinaryOp::I64GeU)?,

        Operator::F32Eq => relop(ctx, F32, BinaryOp::F32Eq)?,
        Operator::F32Ne => relop(ctx, F32, BinaryOp::F32Ne)?,
        Operator::F32Lt => relop(ctx, F32, BinaryOp::F32Lt)?,
        Operator::F32Gt => relop(ctx, F32, BinaryOp::F32Gt)?,
        Operator::F32Le => relop(ctx, F32, BinaryOp::F32Le)?,
        Operator::F32Ge => relop(ctx, F32, BinaryOp::F32Ge)?,

        Operator::F64Eq => relop(ctx, F64, BinaryOp::F64Eq)?,
        Operator::F64Ne => relop(ctx, F64, BinaryOp::F64Ne)?,
        Operator::F64Lt => relop(ctx, F64, BinaryOp::F64Lt)?,
        Operator::F64Gt => relop(ctx, F64, BinaryOp::F64Gt)?,
        Operator::F64Le => relop(ctx, F64, BinaryOp::F64Le)?,
        Operator::F64Ge => relop(ctx, F64, BinaryOp::F64Ge)?,

        Operator::I32Clz => unop(ctx, I32, UnaryOp::I32Clz)?,
        Operator::I32Ctz => unop(ctx, I32, UnaryOp::I32Ctz)?,
        Operator::I32Popcnt => unop(ctx, I32, UnaryOp::I32Popcnt)?,
        Operator::I32Add => binop(ctx, I32, BinaryOp::I32Add)?,
        Operator::I32Sub => binop(ctx, I32, BinaryOp::I32Sub)?,
        Operator::I32Mul => binop(ctx, I32, BinaryOp::I32Mul)?,
        Operator::I32DivS => binop(ctx, I32, BinaryOp::I32DivS)?,
        Operator::I32DivU => binop(ctx, I32, BinaryOp::I32DivU)?,
        Operator::I32RemS => binop(ctx, I32, BinaryOp::I32RemS)?,
        Operator::I32RemU => binop(ctx, I32, BinaryOp::I32RemU)?,
        Operator::I32And => binop(ctx, I32, BinaryOp::I32And)?,
        Operator::I32Or => binop(ctx, I32, BinaryOp::I32Or)?,
        Operator::I32Xor => binop(ctx, I32, BinaryOp::I32Xor)?,
        Operator::I32Shl => binop(ctx, I32, BinaryOp::I32Shl)?,
        Operator::I32ShrS => binop(ctx, I32, BinaryOp::I32ShrS)?,
        Operator::I32ShrU => binop(ctx, I32, BinaryOp::I32ShrU)?,
        Operator::I32Rotl => binop(ctx, I32, BinaryOp::I32Rotl)?,
        Operator::I32Rotr => binop(ctx, I32, BinaryOp::I32Rotr)?,

        Operator::I64Clz => unop(ctx, I64, UnaryOp::I64Clz)?,
        Operator::I64Ctz => unop(ctx, I64, UnaryOp::I64Ctz)?,
        Operator::I64Popcnt => unop(ctx, I64, UnaryOp::I64Popcnt)?,
        Operator::I64Add => binop(ctx, I64, BinaryOp::I64Add)?,
        Operator::I64Sub => binop(ctx, I64, BinaryOp::I64Sub)?,
        Operator::I64Mul => binop(ctx, I64, BinaryOp::I64Mul)?,
        Operator::I64DivS => binop(ctx, I64, BinaryOp::I64DivS)?,
        Operator::I64DivU => binop(ctx, I64, BinaryOp::I64DivU)?,
        Operator::I64RemS => binop(ctx, I64, BinaryOp::I64RemS)?,
        Operator::I64RemU => binop(ctx, I64, BinaryOp::I64RemU)?,
        Operator::I64And => binop(ctx, I64, BinaryOp::I64And)?,
        Operator::I64Or => binop(ctx, I64, BinaryOp::I64Or)?,
        Operator::I64Xor => binop(ctx, I64, BinaryOp::I64Xor)?,
        Operator::I64Shl => binop(ctx, I64, BinaryOp::I64Shl)?,
        Operator::I64ShrS => binop(ctx, I64, BinaryOp::I64ShrS)?,
        Operator::I64ShrU => binop(ctx, I64, BinaryOp::I64ShrU)?,
        Operator::I64Rotl => binop(ctx, I64, BinaryOp::I64Rotl)?,
        Operator::I64Rotr => binop(ctx, I64, BinaryOp::I64Rotr)?,

        Operator::F32Abs => unop(ctx, F32, UnaryOp::F32Abs)?,
        Operator::F32Neg => unop(ctx, F32, UnaryOp::F32Neg)?,
        Operator::F32Ceil => unop(ctx, F32, UnaryOp::F32Ceil)?,
        Operator::F32Floor => unop(ctx, F32, UnaryOp::F32Floor)?,
        Operator::F32Trunc => unop(ctx, F32, UnaryOp::F32Trunc)?,
        Operator::F32Nearest => unop(ctx, F32, UnaryOp::F32Nearest)?,
        Operator::F32Sqrt => unop(ctx, F32, UnaryOp::F32Sqrt)?,
        Operator::F32Add => binop(ctx, F32, BinaryOp::F32Add)?,
        Operator::F32Sub => binop(ctx, F32, BinaryOp::F32Sub)?,
        Operator::F32Mul => binop(ctx, F32, BinaryOp::F32Mul)?,
        Operator::F32Div => binop(ctx, F32, BinaryOp::F32Div)?,
        Operator::F32Min => binop(ctx, F32, BinaryOp::F32Min)?,
        Operator::F32Max => binop(ctx, F32, BinaryOp::F32Max)?,
        Operator::F32Copysign => binop(ctx, F32, BinaryOp::F32Copysign)?,

        Operator::F64Abs => unop(ctx, F64, UnaryOp::F64Abs)?,
        Operator::F64Neg => unop(ctx, F64, UnaryOp::F64Neg)?,
        Operator::F64Ceil => unop(ctx, F64, UnaryOp::F64Ceil)?,
        Operator::F64Floor => unop(ctx, F64, UnaryOp::F64Floor)?,
        Operator::F64Trunc => unop(ctx, F64, UnaryOp::F64Trunc)?,
        Operator::F64Nearest => unop(ctx, F64, UnaryOp::F64Nearest)?,
        Operator::F64Sqrt => unop(ctx, F64, UnaryOp::F64Sqrt)?,
        Operator::F64Add => binop(ctx, F64, BinaryOp::F64Add)?,
        Operator::F64Sub => binop(ctx, F64, BinaryOp::F64Sub)?,
        Operator::F64Mul => binop(ctx, F64, BinaryOp::F64Mul)?,
        Operator::F64Div => binop(ctx, F64, BinaryOp::F64Div)?,
        Operator::F64Min => binop(ctx, F64, BinaryOp::F64Min)?,
        Operator::F64Max => binop(ctx, F64, BinaryOp::F64Max)?,
        Operator::F64Copysign => binop(ctx, F64, BinaryOp::F64Copysign)?,

        Operator::I32WrapI64 => one_op(ctx, I64, I32, UnaryOp::I32WrapI64)?,
        Operator::I32TruncF32S => one_op(ctx, F32, I32, UnaryOp::I32TruncSF32)?,
        Operator::I32TruncF32U => one_op(ctx, F32, I32, UnaryOp::I32TruncUF32)?,
        Operator::I32TruncF64S => one_op(ctx, F64, I32, UnaryOp::I32TruncSF64)?,
        Operator::I32TruncF64U => one_op(ctx, F64, I32, UnaryOp::I32TruncUF64)?,

        Operator::I64ExtendI32S => one_op(ctx, I32, I64, UnaryOp::I64ExtendSI32)?,
        Operator::I64ExtendI32U => one_op(ctx, I32, I64, UnaryOp::I64ExtendUI32)?,
        Operator::I64TruncF32S => one_op(ctx, F32, I64, UnaryOp::I64TruncSF32)?,
        Operator::I64TruncF32U => one_op(ctx, F32, I64, UnaryOp::I64TruncUF32)?,
        Operator::I64TruncF64S => one_op(ctx, F64, I64, UnaryOp::I64TruncSF64)?,
        Operator::I64TruncF64U => one_op(ctx, F64, I64, UnaryOp::I64TruncUF64)?,

        Operator::F32ConvertI32S => one_op(ctx, I32, F32, UnaryOp::F32ConvertSI32)?,
        Operator::F32ConvertI32U => one_op(ctx, I32, F32, UnaryOp::F32ConvertUI32)?,
        Operator::F32ConvertI64S => one_op(ctx, I64, F32, UnaryOp::F32ConvertSI64)?,
        Operator::F32ConvertI64U => one_op(ctx, I64, F32, UnaryOp::F32ConvertUI64)?,
        Operator::F32DemoteF64 => one_op(ctx, F64, F32, UnaryOp::F32DemoteF64)?,

        Operator::F64ConvertI32S => one_op(ctx, I32, F64, UnaryOp::F64ConvertSI32)?,
        Operator::F64ConvertI32U => one_op(ctx, I32, F64, UnaryOp::F64ConvertUI32)?,
        Operator::F64ConvertI64S => one_op(ctx, I64, F64, UnaryOp::F64ConvertSI64)?,
        Operator::F64ConvertI64U => one_op(ctx, I64, F64, UnaryOp::F64ConvertUI64)?,
        Operator::F64PromoteF32 => one_op(ctx, F32, F64, UnaryOp::F64PromoteF32)?,

        Operator::I32ReinterpretF32 => one_op(ctx, F32, I32, UnaryOp::I32ReinterpretF32)?,
        Operator::I64ReinterpretF64 => one_op(ctx, F64, I64, UnaryOp::I64ReinterpretF64)?,
        Operator::F32ReinterpretI32 => one_op(ctx, I32, F32, UnaryOp::F32ReinterpretI32)?,
        Operator::F64ReinterpretI64 => one_op(ctx, I64, F64, UnaryOp::F64ReinterpretI64)?,

        Operator::I32Extend8S => one_op(ctx, I32, I32, UnaryOp::I32Extend8S)?,
        Operator::I32Extend16S => one_op(ctx, I32, I32, UnaryOp::I32Extend16S)?,
        Operator::I64Extend8S => one_op(ctx, I64, I64, UnaryOp::I64Extend8S)?,
        Operator::I64Extend16S => one_op(ctx, I64, I64, UnaryOp::I64Extend16S)?,
        Operator::I64Extend32S => one_op(ctx, I64, I64, UnaryOp::I64Extend32S)?,

        Operator::Drop => {
            ctx.pop_operand()?;
            ctx.alloc_instr(Drop {}, loc);
        }
        Operator::Select => {
            ctx.pop_operand_expected(Some(I32))?;
            let t1 = ctx.pop_operand()?;
            let t2 = ctx.pop_operand_expected(t1)?;
            ctx.alloc_instr(Select {}, loc);
            ctx.push_operand(t2);
        }
        Operator::Return => {
            let fn_ty = ctx.module.funcs.get(ctx.func_id).ty();
            let expected = ctx.module.types.get(fn_ty).results();
            ctx.pop_operands(expected)?;
            ctx.alloc_instr(Return {}, loc);
            ctx.unreachable();
        }
        Operator::Unreachable => {
            ctx.alloc_instr(Unreachable {}, loc);
            ctx.unreachable();
        }
        Operator::Block { ty } => {
            let param_tys = block_param_tys(ctx, ty)?;
            let result_tys = block_result_tys(ctx, ty)?;
            ctx.pop_operands(&param_tys)?;
            let seq = ctx.push_control(BlockKind::Block, param_tys, result_tys)?;
            ctx.alloc_instr_in_control(1, Block { seq }, loc)?;
        }
        Operator::Loop { ty } => {
            let result_tys = block_result_tys(ctx, ty)?;
            let param_tys = block_param_tys(ctx, ty)?;
            ctx.pop_operands(&param_tys)?;
            let seq = ctx.push_control(BlockKind::Loop, param_tys, result_tys)?;
            ctx.alloc_instr_in_control(1, Loop { seq }, loc)?;
        }
        Operator::If { ty } => {
            let result_tys = block_result_tys(ctx, ty)?;
            let param_tys = block_param_tys(ctx, ty)?;

            ctx.pop_operand_expected(Some(I32))?;
            ctx.pop_operands(&param_tys)?;

            let consequent = ctx.push_control(BlockKind::If, param_tys, result_tys)?;
            ctx.if_else.push(context::IfElseState {
                consequent,
                alternative: None,
            });
        }
        Operator::End => {
            let (frame, _block) = ctx.pop_control()?;

            // If we just finished an if/else block then the actual
            // instruction which produces the value will be an `IfElse` node,
            // not the block itself. Do some postprocessing here to create
            // such a node.
            match frame.kind {
                BlockKind::If | BlockKind::Else => {
                    let context::IfElseState {
                        consequent,
                        alternative,
                    } = ctx.if_else.pop().unwrap();

                    let alternative = match alternative {
                        Some(alt) => {
                            debug_assert_eq!(frame.kind, BlockKind::Else);
                            alt
                        }
                        None => {
                            debug_assert_eq!(frame.kind, BlockKind::If);
                            let alternative = ctx.push_control(
                                BlockKind::Else,
                                frame.start_types.clone(),
                                frame.end_types.clone(),
                            )?;
                            ctx.pop_control()?;
                            alternative
                        }
                    };

                    ctx.alloc_instr(
                        IfElse {
                            consequent,
                            alternative,
                        },
                        loc,
                    );
                }
                _ => {}
            }

            ctx.push_operands(&frame.end_types);
        }
        Operator::Else => {
            let (frame, _consequent) = ctx.pop_control()?;
            // An `else` instruction is only valid immediately inside an if/else
            // block which is denoted by the `IfElse` block kind.
            match frame.kind {
                BlockKind::If => {}
                _ => bail!("`else` without a leading `if`"),
            }

            // But we still need to parse the alternative block, so allocate the
            // block here to parse.
            let alternative =
                ctx.push_control(BlockKind::Else, frame.start_types, frame.end_types)?;
            let last = ctx.if_else.last_mut().unwrap();
            if last.alternative.is_some() {
                bail!("`else` without a leading `if`")
            }
            last.alternative = Some(alternative);
        }
        Operator::Br { relative_depth } => {
            let n = relative_depth as usize;
            let expected = ctx.control(n)?.label_types().to_vec();
            ctx.pop_operands(&expected)?;

            let block = ctx.control(n)?.block;
            ctx.alloc_instr(Br { block }, loc);
            ctx.unreachable();
        }
        Operator::BrIf { relative_depth } => {
            let n = relative_depth as usize;
            ctx.pop_operand_expected(Some(I32))?;

            let expected = ctx.control(n)?.label_types().to_vec();
            ctx.pop_operands(&expected)?;

            let block = ctx.control(n)?.block;
            ctx.alloc_instr(BrIf { block }, loc);
            ctx.push_operands(&expected);
        }
        Operator::BrTable { table } => {
            let len = table.len();
            let mut blocks = Vec::with_capacity(len);
            let mut label_types = None;
            let label_types_ref = &mut label_types;
            let mut iter = table.into_iter();
            let mut next = || {
                let n = match iter.next() {
                    Some(n) => n,
                    None => bail!("malformed `br_table"),
                };
                let control = ctx.control(n as usize)?;
                match label_types_ref {
                    None => *label_types_ref = Some(control.label_types().to_vec()),
                    Some(n) => {
                        if &n[..] != control.label_types() {
                            bail!("br_table jump with non-uniform label types")
                        }
                    }
                }
                Ok(control.block)
            };
            for _ in 0..len {
                blocks.push(next()?);
            }
            let default = next()?;
            if iter.next().is_some() {
                bail!("malformed `br_table`");
            }

            let blocks = blocks.into_boxed_slice();
            let expected = label_types.unwrap().clone();
            ctx.pop_operand_expected(Some(I32))?;

            ctx.pop_operands(&expected)?;
            ctx.alloc_instr(BrTable { blocks, default }, loc);

            ctx.unreachable();
        }

        Operator::MemorySize { reserved } => {
            if reserved != 0 {
                bail!("reserved byte isn't zero");
            }
            let memory = ctx.indices.get_memory(0)?;
            ctx.alloc_instr(MemorySize { memory }, loc);
            ctx.push_operand(Some(I32));
        }
        Operator::MemoryGrow { reserved } => {
            if reserved != 0 {
                bail!("reserved byte isn't zero");
            }
            ctx.pop_operand_expected(Some(I32))?;
            let memory = ctx.indices.get_memory(0)?;
            ctx.alloc_instr(MemoryGrow { memory }, loc);
            ctx.push_operand(Some(I32));
        }
        Operator::MemoryInit { segment } => {
            ctx.pop_operand_expected(Some(I32))?;
            ctx.pop_operand_expected(Some(I32))?;
            ctx.pop_operand_expected(Some(I32))?;
            let memory = ctx.indices.get_memory(0)?;
            let data = ctx.indices.get_data(segment)?;
            ctx.alloc_instr(MemoryInit { memory, data }, loc);
        }
        Operator::DataDrop { segment } => {
            let data = ctx.indices.get_data(segment)?;
            ctx.alloc_instr(DataDrop { data }, loc);
        }
        Operator::MemoryCopy => {
            ctx.pop_operand_expected(Some(I32))?;
            ctx.pop_operand_expected(Some(I32))?;
            ctx.pop_operand_expected(Some(I32))?;
            let memory = ctx.indices.get_memory(0)?;
            ctx.alloc_instr(
                MemoryCopy {
                    src: memory,
                    dst: memory,
                },
                loc,
            );
        }
        Operator::MemoryFill => {
            ctx.pop_operand_expected(Some(I32))?;
            ctx.pop_operand_expected(Some(I32))?;
            ctx.pop_operand_expected(Some(I32))?;
            let memory = ctx.indices.get_memory(0)?;
            ctx.alloc_instr(MemoryFill { memory }, loc);
        }

        Operator::Nop => {}

        Operator::I32Load { memarg } => load(ctx, memarg, I32, LoadKind::I32 { atomic: false })?,
        Operator::I64Load { memarg } => load(ctx, memarg, I64, LoadKind::I64 { atomic: false })?,
        Operator::F32Load { memarg } => load(ctx, memarg, F32, LoadKind::F32)?,
        Operator::F64Load { memarg } => load(ctx, memarg, F64, LoadKind::F64)?,
        Operator::V128Load { memarg } => load(ctx, memarg, V128, LoadKind::V128)?,
        Operator::I32Load8S { memarg } => {
            load(ctx, memarg, I32, LoadKind::I32_8 { kind: SignExtend })?
        }
        Operator::I32Load8U { memarg } => {
            load(ctx, memarg, I32, LoadKind::I32_8 { kind: ZeroExtend })?
        }
        Operator::I32Load16S { memarg } => {
            load(ctx, memarg, I32, LoadKind::I32_16 { kind: SignExtend })?
        }
        Operator::I32Load16U { memarg } => {
            load(ctx, memarg, I32, LoadKind::I32_16 { kind: ZeroExtend })?
        }
        Operator::I64Load8S { memarg } => {
            load(ctx, memarg, I64, LoadKind::I64_8 { kind: SignExtend })?
        }
        Operator::I64Load8U { memarg } => {
            load(ctx, memarg, I64, LoadKind::I64_8 { kind: ZeroExtend })?
        }
        Operator::I64Load16S { memarg } => {
            load(ctx, memarg, I64, LoadKind::I64_16 { kind: SignExtend })?
        }
        Operator::I64Load16U { memarg } => {
            load(ctx, memarg, I64, LoadKind::I64_16 { kind: ZeroExtend })?
        }
        Operator::I64Load32S { memarg } => {
            load(ctx, memarg, I64, LoadKind::I64_32 { kind: SignExtend })?
        }
        Operator::I64Load32U { memarg } => {
            load(ctx, memarg, I64, LoadKind::I64_32 { kind: ZeroExtend })?
        }

        Operator::I32Store { memarg } => store(ctx, memarg, I32, StoreKind::I32 { atomic: false })?,
        Operator::I64Store { memarg } => store(ctx, memarg, I64, StoreKind::I64 { atomic: false })?,
        Operator::F32Store { memarg } => store(ctx, memarg, F32, StoreKind::F32)?,
        Operator::F64Store { memarg } => store(ctx, memarg, F64, StoreKind::F64)?,
        Operator::V128Store { memarg } => store(ctx, memarg, V128, StoreKind::V128)?,
        Operator::I32Store8 { memarg } => {
            store(ctx, memarg, I32, StoreKind::I32_8 { atomic: false })?
        }
        Operator::I32Store16 { memarg } => {
            store(ctx, memarg, I32, StoreKind::I32_16 { atomic: false })?
        }
        Operator::I64Store8 { memarg } => {
            store(ctx, memarg, I64, StoreKind::I64_8 { atomic: false })?
        }
        Operator::I64Store16 { memarg } => {
            store(ctx, memarg, I64, StoreKind::I64_16 { atomic: false })?
        }
        Operator::I64Store32 { memarg } => {
            store(ctx, memarg, I64, StoreKind::I64_32 { atomic: false })?
        }

        Operator::AtomicFence { flags } => {
            if flags != 0 {
                bail!("fence with nonzero flags not supported yet");
            }
            ctx.alloc_instr(AtomicFence {}, loc);
        }

        Operator::I32AtomicLoad { memarg } => {
            load(ctx, memarg, I32, LoadKind::I32 { atomic: true })?
        }
        Operator::I64AtomicLoad { memarg } => {
            load(ctx, memarg, I64, LoadKind::I64 { atomic: true })?
        }
        Operator::I32AtomicLoad8U { memarg } => load(
            ctx,
            memarg,
            I32,
            LoadKind::I32_8 {
                kind: ZeroExtendAtomic,
            },
        )?,
        Operator::I32AtomicLoad16U { memarg } => load(
            ctx,
            memarg,
            I32,
            LoadKind::I32_16 {
                kind: ZeroExtendAtomic,
            },
        )?,
        Operator::I64AtomicLoad8U { memarg } => load(
            ctx,
            memarg,
            I64,
            LoadKind::I64_8 {
                kind: ZeroExtendAtomic,
            },
        )?,
        Operator::I64AtomicLoad16U { memarg } => load(
            ctx,
            memarg,
            I64,
            LoadKind::I64_16 {
                kind: ZeroExtendAtomic,
            },
        )?,
        Operator::I64AtomicLoad32U { memarg } => load(
            ctx,
            memarg,
            I64,
            LoadKind::I64_32 {
                kind: ZeroExtendAtomic,
            },
        )?,

        Operator::I32AtomicStore { memarg } => {
            store(ctx, memarg, I32, StoreKind::I32 { atomic: true })?
        }
        Operator::I64AtomicStore { memarg } => {
            store(ctx, memarg, I64, StoreKind::I64 { atomic: true })?
        }
        Operator::I32AtomicStore8 { memarg } => {
            store(ctx, memarg, I32, StoreKind::I32_8 { atomic: true })?
        }
        Operator::I32AtomicStore16 { memarg } => {
            store(ctx, memarg, I32, StoreKind::I32_16 { atomic: true })?
        }
        Operator::I64AtomicStore8 { memarg } => {
            store(ctx, memarg, I64, StoreKind::I64_8 { atomic: true })?
        }
        Operator::I64AtomicStore16 { memarg } => {
            store(ctx, memarg, I64, StoreKind::I64_16 { atomic: true })?
        }
        Operator::I64AtomicStore32 { memarg } => {
            store(ctx, memarg, I64, StoreKind::I64_32 { atomic: true })?
        }

        Operator::I32AtomicRmwAdd { memarg } => {
            atomicrmw(ctx, memarg, I32, AtomicOp::Add, AtomicWidth::I32)?;
        }
        Operator::I64AtomicRmwAdd { memarg } => {
            atomicrmw(ctx, memarg, I64, AtomicOp::Add, AtomicWidth::I64)?;
        }
        Operator::I32AtomicRmw8AddU { memarg } => {
            atomicrmw(ctx, memarg, I32, AtomicOp::Add, AtomicWidth::I32_8)?;
        }
        Operator::I32AtomicRmw16AddU { memarg } => {
            atomicrmw(ctx, memarg, I32, AtomicOp::Add, AtomicWidth::I32_16)?;
        }
        Operator::I64AtomicRmw8AddU { memarg } => {
            atomicrmw(ctx, memarg, I64, AtomicOp::Add, AtomicWidth::I64_8)?;
        }
        Operator::I64AtomicRmw16AddU { memarg } => {
            atomicrmw(ctx, memarg, I64, AtomicOp::Add, AtomicWidth::I64_16)?;
        }
        Operator::I64AtomicRmw32AddU { memarg } => {
            atomicrmw(ctx, memarg, I64, AtomicOp::Add, AtomicWidth::I64_32)?;
        }

        Operator::I32AtomicRmwSub { memarg } => {
            atomicrmw(ctx, memarg, I32, AtomicOp::Sub, AtomicWidth::I32)?;
        }
        Operator::I64AtomicRmwSub { memarg } => {
            atomicrmw(ctx, memarg, I64, AtomicOp::Sub, AtomicWidth::I64)?;
        }
        Operator::I32AtomicRmw8SubU { memarg } => {
            atomicrmw(ctx, memarg, I32, AtomicOp::Sub, AtomicWidth::I32_8)?;
        }
        Operator::I32AtomicRmw16SubU { memarg } => {
            atomicrmw(ctx, memarg, I32, AtomicOp::Sub, AtomicWidth::I32_16)?;
        }
        Operator::I64AtomicRmw8SubU { memarg } => {
            atomicrmw(ctx, memarg, I64, AtomicOp::Sub, AtomicWidth::I64_8)?;
        }
        Operator::I64AtomicRmw16SubU { memarg } => {
            atomicrmw(ctx, memarg, I64, AtomicOp::Sub, AtomicWidth::I64_16)?;
        }
        Operator::I64AtomicRmw32SubU { memarg } => {
            atomicrmw(ctx, memarg, I64, AtomicOp::Sub, AtomicWidth::I64_32)?;
        }

        Operator::I32AtomicRmwAnd { memarg } => {
            atomicrmw(ctx, memarg, I32, AtomicOp::And, AtomicWidth::I32)?;
        }
        Operator::I64AtomicRmwAnd { memarg } => {
            atomicrmw(ctx, memarg, I64, AtomicOp::And, AtomicWidth::I64)?;
        }
        Operator::I32AtomicRmw8AndU { memarg } => {
            atomicrmw(ctx, memarg, I32, AtomicOp::And, AtomicWidth::I32_8)?;
        }
        Operator::I32AtomicRmw16AndU { memarg } => {
            atomicrmw(ctx, memarg, I32, AtomicOp::And, AtomicWidth::I32_16)?;
        }
        Operator::I64AtomicRmw8AndU { memarg } => {
            atomicrmw(ctx, memarg, I64, AtomicOp::And, AtomicWidth::I64_8)?;
        }
        Operator::I64AtomicRmw16AndU { memarg } => {
            atomicrmw(ctx, memarg, I64, AtomicOp::And, AtomicWidth::I64_16)?;
        }
        Operator::I64AtomicRmw32AndU { memarg } => {
            atomicrmw(ctx, memarg, I64, AtomicOp::And, AtomicWidth::I64_32)?;
        }

        Operator::I32AtomicRmwOr { memarg } => {
            atomicrmw(ctx, memarg, I32, AtomicOp::Or, AtomicWidth::I32)?;
        }
        Operator::I64AtomicRmwOr { memarg } => {
            atomicrmw(ctx, memarg, I64, AtomicOp::Or, AtomicWidth::I64)?;
        }
        Operator::I32AtomicRmw8OrU { memarg } => {
            atomicrmw(ctx, memarg, I32, AtomicOp::Or, AtomicWidth::I32_8)?;
        }
        Operator::I32AtomicRmw16OrU { memarg } => {
            atomicrmw(ctx, memarg, I32, AtomicOp::Or, AtomicWidth::I32_16)?;
        }
        Operator::I64AtomicRmw8OrU { memarg } => {
            atomicrmw(ctx, memarg, I64, AtomicOp::Or, AtomicWidth::I64_8)?;
        }
        Operator::I64AtomicRmw16OrU { memarg } => {
            atomicrmw(ctx, memarg, I64, AtomicOp::Or, AtomicWidth::I64_16)?;
        }
        Operator::I64AtomicRmw32OrU { memarg } => {
            atomicrmw(ctx, memarg, I64, AtomicOp::Or, AtomicWidth::I64_32)?;
        }

        Operator::I32AtomicRmwXor { memarg } => {
            atomicrmw(ctx, memarg, I32, AtomicOp::Xor, AtomicWidth::I32)?;
        }
        Operator::I64AtomicRmwXor { memarg } => {
            atomicrmw(ctx, memarg, I64, AtomicOp::Xor, AtomicWidth::I64)?;
        }
        Operator::I32AtomicRmw8XorU { memarg } => {
            atomicrmw(ctx, memarg, I32, AtomicOp::Xor, AtomicWidth::I32_8)?;
        }
        Operator::I32AtomicRmw16XorU { memarg } => {
            atomicrmw(ctx, memarg, I32, AtomicOp::Xor, AtomicWidth::I32_16)?;
        }
        Operator::I64AtomicRmw8XorU { memarg } => {
            atomicrmw(ctx, memarg, I64, AtomicOp::Xor, AtomicWidth::I64_8)?;
        }
        Operator::I64AtomicRmw16XorU { memarg } => {
            atomicrmw(ctx, memarg, I64, AtomicOp::Xor, AtomicWidth::I64_16)?;
        }
        Operator::I64AtomicRmw32XorU { memarg } => {
            atomicrmw(ctx, memarg, I64, AtomicOp::Xor, AtomicWidth::I64_32)?;
        }

        Operator::I32AtomicRmwXchg { memarg } => {
            atomicrmw(ctx, memarg, I32, AtomicOp::Xchg, AtomicWidth::I32)?;
        }
        Operator::I64AtomicRmwXchg { memarg } => {
            atomicrmw(ctx, memarg, I64, AtomicOp::Xchg, AtomicWidth::I64)?;
        }
        Operator::I32AtomicRmw8XchgU { memarg } => {
            atomicrmw(ctx, memarg, I32, AtomicOp::Xchg, AtomicWidth::I32_8)?;
        }
        Operator::I32AtomicRmw16XchgU { memarg } => {
            atomicrmw(ctx, memarg, I32, AtomicOp::Xchg, AtomicWidth::I32_16)?;
        }
        Operator::I64AtomicRmw8XchgU { memarg } => {
            atomicrmw(ctx, memarg, I64, AtomicOp::Xchg, AtomicWidth::I64_8)?;
        }
        Operator::I64AtomicRmw16XchgU { memarg } => {
            atomicrmw(ctx, memarg, I64, AtomicOp::Xchg, AtomicWidth::I64_16)?;
        }
        Operator::I64AtomicRmw32XchgU { memarg } => {
            atomicrmw(ctx, memarg, I64, AtomicOp::Xchg, AtomicWidth::I64_32)?;
        }

        Operator::I32AtomicRmwCmpxchg { memarg } => {
            cmpxchg(ctx, memarg, I32, AtomicWidth::I32)?;
        }
        Operator::I64AtomicRmwCmpxchg { memarg } => {
            cmpxchg(ctx, memarg, I64, AtomicWidth::I64)?;
        }
        Operator::I32AtomicRmw8CmpxchgU { memarg } => {
            cmpxchg(ctx, memarg, I32, AtomicWidth::I32_8)?;
        }
        Operator::I32AtomicRmw16CmpxchgU { memarg } => {
            cmpxchg(ctx, memarg, I32, AtomicWidth::I32_16)?;
        }
        Operator::I64AtomicRmw8CmpxchgU { memarg } => {
            cmpxchg(ctx, memarg, I64, AtomicWidth::I64_8)?;
        }
        Operator::I64AtomicRmw16CmpxchgU { memarg } => {
            cmpxchg(ctx, memarg, I64, AtomicWidth::I64_16)?;
        }
        Operator::I64AtomicRmw32CmpxchgU { memarg } => {
            cmpxchg(ctx, memarg, I64, AtomicWidth::I64_32)?;
        }
        Operator::AtomicNotify { ref memarg } => {
            ctx.pop_operand_expected(Some(I32))?;
            ctx.pop_operand_expected(Some(I32))?;
            let memory = ctx.indices.get_memory(0)?;
            ctx.alloc_instr(
                AtomicNotify {
                    memory,
                    arg: mem_arg(memarg)?,
                },
                loc,
            );
            ctx.push_operand(Some(I32));
        }
        Operator::I32AtomicWait { ref memarg } | Operator::I64AtomicWait { ref memarg } => {
            let (ty, sixty_four) = match inst {
                Operator::I32AtomicWait { .. } => (I32, false),
                _ => (I64, true),
            };
            ctx.pop_operand_expected(Some(I64))?;
            ctx.pop_operand_expected(Some(ty))?;
            ctx.pop_operand_expected(Some(I32))?;
            let memory = ctx.indices.get_memory(0)?;
            ctx.alloc_instr(
                AtomicWait {
                    sixty_four,
                    memory,
                    arg: mem_arg(memarg)?,
                },
                loc,
            );
            ctx.push_operand(Some(I32));
        }

        Operator::TableGet { table } => {
            let table = ctx.indices.get_table(table)?;
            ctx.pop_operand_expected(Some(I32))?;
            ctx.alloc_instr(TableGet { table }, loc);
            ctx.push_operand(Some(Anyref));
        }
        Operator::TableSet { table } => {
            let table = ctx.indices.get_table(table)?;
            let expected_ty = match ctx.module.tables.get(table).kind {
                TableKind::Anyref(_) => Anyref,
                TableKind::Function(_) => bail!("cannot set function table yet"),
            };
            ctx.pop_operand_expected(Some(expected_ty))?;
            ctx.pop_operand_expected(Some(I32))?;
            ctx.alloc_instr(TableSet { table }, loc);
        }
        Operator::TableGrow { table } => {
            let table = ctx.indices.get_table(table)?;
            let expected_ty = match ctx.module.tables.get(table).kind {
                TableKind::Anyref(_) => Anyref,
                TableKind::Function(_) => bail!("cannot grow function table yet"),
            };
            ctx.pop_operand_expected(Some(I32))?;
            ctx.pop_operand_expected(Some(expected_ty))?;
            ctx.alloc_instr(TableGrow { table }, loc);
            ctx.push_operand(Some(I32));
        }
        Operator::TableSize { table } => {
            let table = ctx.indices.get_table(table)?;
            ctx.alloc_instr(TableSize { table }, loc);
            ctx.push_operand(Some(I32));
        }
        Operator::TableFill { table } => {
            let table = ctx.indices.get_table(table)?;
            let expected_ty = match ctx.module.tables.get(table).kind {
                TableKind::Anyref(_) => Anyref,
                TableKind::Function(_) => bail!("cannot set function table yet"),
            };
            ctx.pop_operand_expected(Some(I32))?;
            ctx.pop_operand_expected(Some(expected_ty))?;
            ctx.pop_operand_expected(Some(I32))?;
            ctx.alloc_instr(TableFill { table }, loc);
        }
        Operator::RefNull => {
            ctx.alloc_instr(RefNull {}, loc);
            ctx.push_operand(Some(Anyref));
        }
        Operator::RefIsNull => {
            ctx.pop_operand_expected(Some(Anyref))?;
            ctx.alloc_instr(RefIsNull {}, loc);
            ctx.push_operand(Some(I32));
        }
        Operator::RefFunc { function_index }=> {
            let func = ctx
                .indices
                .get_func(function_index)
                .context("invalid call")?;
            ctx.alloc_instr(RefFunc { func }, loc);
            ctx.push_operand(Some(Anyref));
        }

        Operator::V8x16Swizzle => {
            ctx.pop_operand_expected(Some(V128))?;
            ctx.pop_operand_expected(Some(V128))?;
            ctx.alloc_instr(V128Swizzle {}, loc);
            ctx.push_operand(Some(V128));
        }

        Operator::V8x16Shuffle { lanes } => {
            ctx.pop_operand_expected(Some(V128))?;
            ctx.pop_operand_expected(Some(V128))?;
            ctx.alloc_instr(V128Shuffle { indices: lanes }, loc);
            ctx.push_operand(Some(V128));
        }

        Operator::I8x16Splat => one_op(ctx, I32, V128, UnaryOp::I8x16Splat)?,
        Operator::I8x16ExtractLaneS { lane: idx } => {
            one_op(ctx, V128, I32, UnaryOp::I8x16ExtractLaneS { idx })?
        }
        Operator::I8x16ExtractLaneU { lane: idx } => {
            one_op(ctx, V128, I32, UnaryOp::I8x16ExtractLaneU { idx })?
        }
        Operator::I8x16ReplaceLane { lane: idx } => {
            two_ops(ctx, V128, I32, V128, BinaryOp::I8x16ReplaceLane { idx })?
        }
        Operator::I16x8Splat => one_op(ctx, I32, V128, UnaryOp::I16x8Splat)?,
        Operator::I16x8ExtractLaneS { lane: idx } => {
            one_op(ctx, V128, I32, UnaryOp::I16x8ExtractLaneS { idx })?
        }
        Operator::I16x8ExtractLaneU { lane: idx } => {
            one_op(ctx, V128, I32, UnaryOp::I16x8ExtractLaneU { idx })?
        }
        Operator::I16x8ReplaceLane { lane: idx } => {
            two_ops(ctx, V128, I32, V128, BinaryOp::I16x8ReplaceLane { idx })?
        }
        Operator::I32x4Splat => one_op(ctx, I32, V128, UnaryOp::I32x4Splat)?,
        Operator::I32x4ExtractLane { lane: idx } => {
            one_op(ctx, V128, I32, UnaryOp::I32x4ExtractLane { idx })?
        }
        Operator::I32x4ReplaceLane { lane: idx } => {
            two_ops(ctx, V128, I32, V128, BinaryOp::I32x4ReplaceLane { idx })?
        }
        Operator::I64x2Splat => one_op(ctx, I64, V128, UnaryOp::I64x2Splat)?,
        Operator::I64x2ExtractLane { lane: idx } => {
            one_op(ctx, V128, I64, UnaryOp::I64x2ExtractLane { idx })?
        }
        Operator::I64x2ReplaceLane { lane: idx } => {
            two_ops(ctx, V128, I64, V128, BinaryOp::I64x2ReplaceLane { idx })?
        }
        Operator::F32x4Splat => one_op(ctx, F32, V128, UnaryOp::F32x4Splat)?,
        Operator::F32x4ExtractLane { lane: idx } => {
            one_op(ctx, V128, F32, UnaryOp::F32x4ExtractLane { idx })?
        }
        Operator::F32x4ReplaceLane { lane: idx } => {
            two_ops(ctx, V128, F32, V128, BinaryOp::F32x4ReplaceLane { idx })?
        }
        Operator::F64x2Splat => one_op(ctx, F64, V128, UnaryOp::F64x2Splat)?,
        Operator::F64x2ExtractLane { lane: idx } => {
            one_op(ctx, V128, F64, UnaryOp::F64x2ExtractLane { idx })?
        }
        Operator::F64x2ReplaceLane { lane: idx } => {
            two_ops(ctx, V128, F64, V128, BinaryOp::F64x2ReplaceLane { idx })?
        }

        Operator::I8x16Eq => binop(ctx, V128, BinaryOp::I8x16Eq)?,
        Operator::I8x16Ne => binop(ctx, V128, BinaryOp::I8x16Ne)?,
        Operator::I8x16LtS => binop(ctx, V128, BinaryOp::I8x16LtS)?,
        Operator::I8x16LtU => binop(ctx, V128, BinaryOp::I8x16LtU)?,
        Operator::I8x16GtS => binop(ctx, V128, BinaryOp::I8x16GtS)?,
        Operator::I8x16GtU => binop(ctx, V128, BinaryOp::I8x16GtU)?,
        Operator::I8x16LeS => binop(ctx, V128, BinaryOp::I8x16LeS)?,
        Operator::I8x16LeU => binop(ctx, V128, BinaryOp::I8x16LeU)?,
        Operator::I8x16GeS => binop(ctx, V128, BinaryOp::I8x16GeS)?,
        Operator::I8x16GeU => binop(ctx, V128, BinaryOp::I8x16GeU)?,
        Operator::I16x8Eq => binop(ctx, V128, BinaryOp::I16x8Eq)?,
        Operator::I16x8Ne => binop(ctx, V128, BinaryOp::I16x8Ne)?,
        Operator::I16x8LtS => binop(ctx, V128, BinaryOp::I16x8LtS)?,
        Operator::I16x8LtU => binop(ctx, V128, BinaryOp::I16x8LtU)?,
        Operator::I16x8GtS => binop(ctx, V128, BinaryOp::I16x8GtS)?,
        Operator::I16x8GtU => binop(ctx, V128, BinaryOp::I16x8GtU)?,
        Operator::I16x8LeS => binop(ctx, V128, BinaryOp::I16x8LeS)?,
        Operator::I16x8LeU => binop(ctx, V128, BinaryOp::I16x8LeU)?,
        Operator::I16x8GeS => binop(ctx, V128, BinaryOp::I16x8GeS)?,
        Operator::I16x8GeU => binop(ctx, V128, BinaryOp::I16x8GeU)?,
        Operator::I32x4Eq => binop(ctx, V128, BinaryOp::I32x4Eq)?,
        Operator::I32x4Ne => binop(ctx, V128, BinaryOp::I32x4Ne)?,
        Operator::I32x4LtS => binop(ctx, V128, BinaryOp::I32x4LtS)?,
        Operator::I32x4LtU => binop(ctx, V128, BinaryOp::I32x4LtU)?,
        Operator::I32x4GtS => binop(ctx, V128, BinaryOp::I32x4GtS)?,
        Operator::I32x4GtU => binop(ctx, V128, BinaryOp::I32x4GtU)?,
        Operator::I32x4LeS => binop(ctx, V128, BinaryOp::I32x4LeS)?,
        Operator::I32x4LeU => binop(ctx, V128, BinaryOp::I32x4LeU)?,
        Operator::I32x4GeS => binop(ctx, V128, BinaryOp::I32x4GeS)?,
        Operator::I32x4GeU => binop(ctx, V128, BinaryOp::I32x4GeU)?,
        Operator::F32x4Eq => binop(ctx, V128, BinaryOp::F32x4Eq)?,
        Operator::F32x4Ne => binop(ctx, V128, BinaryOp::F32x4Ne)?,
        Operator::F32x4Lt => binop(ctx, V128, BinaryOp::F32x4Lt)?,
        Operator::F32x4Gt => binop(ctx, V128, BinaryOp::F32x4Gt)?,
        Operator::F32x4Le => binop(ctx, V128, BinaryOp::F32x4Le)?,
        Operator::F32x4Ge => binop(ctx, V128, BinaryOp::F32x4Ge)?,
        Operator::F64x2Eq => binop(ctx, V128, BinaryOp::F64x2Eq)?,
        Operator::F64x2Ne => binop(ctx, V128, BinaryOp::F64x2Ne)?,
        Operator::F64x2Lt => binop(ctx, V128, BinaryOp::F64x2Lt)?,
        Operator::F64x2Gt => binop(ctx, V128, BinaryOp::F64x2Gt)?,
        Operator::F64x2Le => binop(ctx, V128, BinaryOp::F64x2Le)?,
        Operator::F64x2Ge => binop(ctx, V128, BinaryOp::F64x2Ge)?,

        Operator::V128Not => unop(ctx, V128, UnaryOp::V128Not)?,
        Operator::V128And => binop(ctx, V128, BinaryOp::V128And)?,
        Operator::V128Or => binop(ctx, V128, BinaryOp::V128Or)?,
        Operator::V128Xor => binop(ctx, V128, BinaryOp::V128Xor)?,

        Operator::V128Bitselect => {
            ctx.pop_operand_expected(Some(V128))?;
            ctx.pop_operand_expected(Some(V128))?;
            ctx.pop_operand_expected(Some(V128))?;
            ctx.alloc_instr(V128Bitselect {}, loc);
            ctx.push_operand(Some(V128));
        }

        Operator::I8x16Neg => unop(ctx, V128, UnaryOp::I8x16Neg)?,
        Operator::I8x16AnyTrue => one_op(ctx, V128, I32, UnaryOp::I8x16AnyTrue)?,
        Operator::I8x16AllTrue => one_op(ctx, V128, I32, UnaryOp::I8x16AllTrue)?,
        Operator::I8x16Shl => two_ops(ctx, V128, I32, V128, BinaryOp::I8x16Shl)?,
        Operator::I8x16ShrS => two_ops(ctx, V128, I32, V128, BinaryOp::I8x16ShrS)?,
        Operator::I8x16ShrU => two_ops(ctx, V128, I32, V128, BinaryOp::I8x16ShrU)?,
        Operator::I8x16Add => binop(ctx, V128, BinaryOp::I8x16Add)?,
        Operator::I8x16AddSaturateS => binop(ctx, V128, BinaryOp::I8x16AddSaturateS)?,
        Operator::I8x16AddSaturateU => binop(ctx, V128, BinaryOp::I8x16AddSaturateU)?,
        Operator::I8x16Sub => binop(ctx, V128, BinaryOp::I8x16Sub)?,
        Operator::I8x16SubSaturateS => binop(ctx, V128, BinaryOp::I8x16SubSaturateS)?,
        Operator::I8x16SubSaturateU => binop(ctx, V128, BinaryOp::I8x16SubSaturateU)?,
        Operator::I8x16Mul => binop(ctx, V128, BinaryOp::I8x16Mul)?,

        Operator::I16x8Neg => unop(ctx, V128, UnaryOp::I16x8Neg)?,
        Operator::I16x8AnyTrue => one_op(ctx, V128, I32, UnaryOp::I16x8AnyTrue)?,
        Operator::I16x8AllTrue => one_op(ctx, V128, I32, UnaryOp::I16x8AllTrue)?,
        Operator::I16x8Shl => two_ops(ctx, V128, I32, V128, BinaryOp::I16x8Shl)?,
        Operator::I16x8ShrS => two_ops(ctx, V128, I32, V128, BinaryOp::I16x8ShrS)?,
        Operator::I16x8ShrU => two_ops(ctx, V128, I32, V128, BinaryOp::I16x8ShrU)?,
        Operator::I16x8Add => binop(ctx, V128, BinaryOp::I16x8Add)?,
        Operator::I16x8AddSaturateS => binop(ctx, V128, BinaryOp::I16x8AddSaturateS)?,
        Operator::I16x8AddSaturateU => binop(ctx, V128, BinaryOp::I16x8AddSaturateU)?,
        Operator::I16x8Sub => binop(ctx, V128, BinaryOp::I16x8Sub)?,
        Operator::I16x8SubSaturateS => binop(ctx, V128, BinaryOp::I16x8SubSaturateS)?,
        Operator::I16x8SubSaturateU => binop(ctx, V128, BinaryOp::I16x8SubSaturateU)?,
        Operator::I16x8Mul => binop(ctx, V128, BinaryOp::I16x8Mul)?,

        Operator::I32x4Neg => unop(ctx, V128, UnaryOp::I32x4Neg)?,
        Operator::I32x4AnyTrue => one_op(ctx, V128, I32, UnaryOp::I32x4AnyTrue)?,
        Operator::I32x4AllTrue => one_op(ctx, V128, I32, UnaryOp::I32x4AllTrue)?,
        Operator::I32x4Shl => two_ops(ctx, V128, I32, V128, BinaryOp::I32x4Shl)?,
        Operator::I32x4ShrS => two_ops(ctx, V128, I32, V128, BinaryOp::I32x4ShrS)?,
        Operator::I32x4ShrU => two_ops(ctx, V128, I32, V128, BinaryOp::I32x4ShrU)?,
        Operator::I32x4Add => binop(ctx, V128, BinaryOp::I32x4Add)?,
        Operator::I32x4Sub => binop(ctx, V128, BinaryOp::I32x4Sub)?,
        Operator::I32x4Mul => binop(ctx, V128, BinaryOp::I32x4Mul)?,

        Operator::I64x2Neg => unop(ctx, V128, UnaryOp::I64x2Neg)?,
        Operator::I64x2AnyTrue => one_op(ctx, V128, I32, UnaryOp::I64x2AnyTrue)?,
        Operator::I64x2AllTrue => one_op(ctx, V128, I32, UnaryOp::I64x2AllTrue)?,
        Operator::I64x2Shl => two_ops(ctx, V128, I32, V128, BinaryOp::I64x2Shl)?,
        Operator::I64x2ShrS => two_ops(ctx, V128, I32, V128, BinaryOp::I64x2ShrS)?,
        Operator::I64x2ShrU => two_ops(ctx, V128, I32, V128, BinaryOp::I64x2ShrU)?,
        Operator::I64x2Add => binop(ctx, V128, BinaryOp::I64x2Add)?,
        Operator::I64x2Sub => binop(ctx, V128, BinaryOp::I64x2Sub)?,

        Operator::F32x4Abs => unop(ctx, V128, UnaryOp::F32x4Abs)?,
        Operator::F32x4Neg => unop(ctx, V128, UnaryOp::F32x4Neg)?,
        Operator::F32x4Sqrt => unop(ctx, V128, UnaryOp::F32x4Sqrt)?,
        Operator::F32x4Add => binop(ctx, V128, BinaryOp::F32x4Add)?,
        Operator::F32x4Sub => binop(ctx, V128, BinaryOp::F32x4Sub)?,
        Operator::F32x4Mul => binop(ctx, V128, BinaryOp::F32x4Mul)?,
        Operator::F32x4Div => binop(ctx, V128, BinaryOp::F32x4Div)?,
        Operator::F32x4Min => binop(ctx, V128, BinaryOp::F32x4Min)?,
        Operator::F32x4Max => binop(ctx, V128, BinaryOp::F32x4Max)?,

        Operator::F64x2Abs => unop(ctx, V128, UnaryOp::F64x2Abs)?,
        Operator::F64x2Neg => unop(ctx, V128, UnaryOp::F64x2Neg)?,
        Operator::F64x2Sqrt => unop(ctx, V128, UnaryOp::F64x2Sqrt)?,
        Operator::F64x2Add => binop(ctx, V128, BinaryOp::F64x2Add)?,
        Operator::F64x2Sub => binop(ctx, V128, BinaryOp::F64x2Sub)?,
        Operator::F64x2Mul => binop(ctx, V128, BinaryOp::F64x2Mul)?,
        Operator::F64x2Div => binop(ctx, V128, BinaryOp::F64x2Div)?,
        Operator::F64x2Min => binop(ctx, V128, BinaryOp::F64x2Min)?,
        Operator::F64x2Max => binop(ctx, V128, BinaryOp::F64x2Max)?,

        Operator::I32x4TruncSatF32x4S => unop(ctx, V128, UnaryOp::I32x4TruncSF32x4Sat)?,
        Operator::I32x4TruncSatF32x4U => unop(ctx, V128, UnaryOp::I32x4TruncUF32x4Sat)?,
        Operator::I64x2TruncSatF64x2S => unop(ctx, V128, UnaryOp::I64x2TruncSF64x2Sat)?,
        Operator::I64x2TruncSatF64x2U => unop(ctx, V128, UnaryOp::I64x2TruncUF64x2Sat)?,
        Operator::F32x4ConvertI32x4S => unop(ctx, V128, UnaryOp::F32x4ConvertSI32x4)?,
        Operator::F32x4ConvertI32x4U => unop(ctx, V128, UnaryOp::F32x4ConvertUI32x4)?,
        Operator::F64x2ConvertI64x2S => unop(ctx, V128, UnaryOp::F64x2ConvertSI64x2)?,
        Operator::F64x2ConvertI64x2U => unop(ctx, V128, UnaryOp::F64x2ConvertUI64x2)?,

        Operator::I32TruncSatF32S => one_op(ctx, F32, I32, UnaryOp::I32TruncSSatF32)?,
        Operator::I32TruncSatF32U => one_op(ctx, F32, I32, UnaryOp::I32TruncUSatF32)?,
        Operator::I32TruncSatF64S => one_op(ctx, F64, I32, UnaryOp::I32TruncSSatF64)?,
        Operator::I32TruncSatF64U => one_op(ctx, F64, I32, UnaryOp::I32TruncUSatF64)?,
        Operator::I64TruncSatF32S => one_op(ctx, F32, I64, UnaryOp::I64TruncSSatF32)?,
        Operator::I64TruncSatF32U => one_op(ctx, F32, I64, UnaryOp::I64TruncUSatF32)?,
        Operator::I64TruncSatF64S => one_op(ctx, F64, I64, UnaryOp::I64TruncSSatF64)?,
        Operator::I64TruncSatF64U => one_op(ctx, F64, I64, UnaryOp::I64TruncUSatF64)?,

        Operator::V8x16LoadSplat { memarg } => load_splat(ctx, memarg, LoadSplatKind::I8)?,
        Operator::V16x8LoadSplat { memarg } => load_splat(ctx, memarg, LoadSplatKind::I16)?,
        Operator::V32x4LoadSplat { memarg } => load_splat(ctx, memarg, LoadSplatKind::I32)?,
        Operator::V64x2LoadSplat { memarg } => load_splat(ctx, memarg, LoadSplatKind::I64)?,

        op @ Operator::TableInit { .. }
        | op @ Operator::ElemDrop { .. }
        | op @ Operator::TableCopy => {
            bail!("Have not implemented support for opcode yet: {:?}", op)
        }
    }
    Ok(())
}
