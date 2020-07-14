//! Functions defined locally within a wasm module.

mod context;
mod emit;

use self::context::ValidationContext;
use crate::emit::IdsToIndices;
use crate::encode::Encoder;
use crate::ir::*;
use crate::map::{IdHashMap, IdHashSet};
use crate::parse::IndicesToIds;
use crate::{Data, DataId, FunctionBuilder, FunctionId, Module, Result, TypeId, ValType};
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
        mut validator: wasmparser::FuncValidator,
    ) -> Result<LocalFunction> {
        let mut func = LocalFunction {
            builder: FunctionBuilder::without_entry(ty),
            args,
        };

        let result: Vec<_> = module.types.get(ty).results().iter().cloned().collect();
        let result = result.into_boxed_slice();

        let controls = &mut context::ControlStack::new();

        let mut ctx = ValidationContext::new(module, indices, id, &mut func, controls);

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
            validator.op(pos, &inst)?;
            append_instruction(&mut ctx, inst, loc);
        }
        validator.finish()?;

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

fn append_instruction<'context>(
    ctx: &'context mut ValidationContext,
    inst: Operator,
    loc: InstrLocId,
) {
    // NB. there's a lot of `unwrap()` here in this function, and that's because
    // the `Operator` was validated above to already be valid, so everything
    // should succeed.
    use crate::ir::ExtendedLoad::*;

    log::trace!("validate instruction: {:?}", inst);

    let const_ = |ctx: &mut ValidationContext, value| {
        ctx.alloc_instr(Const { value }, loc);
    };

    let unop = |ctx: &mut ValidationContext, op| {
        ctx.alloc_instr(Unop { op }, loc);
    };
    let binop = |ctx: &mut ValidationContext, op| {
        ctx.alloc_instr(Binop { op }, loc);
    };

    let mem_arg = |arg: &wasmparser::MemoryImmediate| -> MemArg {
        MemArg {
            align: 1 << (arg.flags as i32),
            offset: arg.offset,
        }
    };

    let load = |ctx: &mut ValidationContext, arg, kind| {
        let memory = ctx.indices.get_memory(0).unwrap();
        let arg = mem_arg(&arg);
        ctx.alloc_instr(Load { arg, kind, memory }, loc);
    };

    let store = |ctx: &mut ValidationContext, arg, kind| {
        let memory = ctx.indices.get_memory(0).unwrap();
        let arg = mem_arg(&arg);
        ctx.alloc_instr(Store { arg, kind, memory }, loc);
    };

    let atomicrmw = |ctx: &mut ValidationContext, arg, op, width| {
        let memory = ctx.indices.get_memory(0).unwrap();
        let arg = mem_arg(&arg);
        ctx.alloc_instr(
            AtomicRmw {
                arg,
                memory,
                op,
                width,
            },
            loc,
        );
    };

    let cmpxchg = |ctx: &mut ValidationContext, arg, width| {
        let memory = ctx.indices.get_memory(0).unwrap();
        let arg = mem_arg(&arg);
        ctx.alloc_instr(Cmpxchg { arg, memory, width }, loc);
    };

    let load_simd = |ctx: &mut ValidationContext, arg, kind| {
        let memory = ctx.indices.get_memory(0).unwrap();
        let arg = mem_arg(&arg);
        ctx.alloc_instr(LoadSimd { memory, arg, kind }, loc);
    };
    match inst {
        Operator::Call { function_index } => {
            let func = ctx.indices.get_func(function_index).unwrap();
            ctx.alloc_instr(Call { func }, loc);
        }
        Operator::CallIndirect { index, table_index } => {
            let type_id = ctx.indices.get_type(index).unwrap();
            let table = ctx.indices.get_table(table_index).unwrap();
            ctx.alloc_instr(CallIndirect { table, ty: type_id }, loc);
        }
        Operator::LocalGet { local_index } => {
            let local = ctx.indices.get_local(ctx.func_id, local_index).unwrap();
            ctx.alloc_instr(LocalGet { local }, loc);
        }
        Operator::LocalSet { local_index } => {
            let local = ctx.indices.get_local(ctx.func_id, local_index).unwrap();
            ctx.alloc_instr(LocalSet { local }, loc);
        }
        Operator::LocalTee { local_index } => {
            let local = ctx.indices.get_local(ctx.func_id, local_index).unwrap();
            ctx.alloc_instr(LocalTee { local }, loc);
        }
        Operator::GlobalGet { global_index } => {
            let global = ctx.indices.get_global(global_index).unwrap();
            ctx.alloc_instr(GlobalGet { global }, loc);
        }
        Operator::GlobalSet { global_index } => {
            let global = ctx.indices.get_global(global_index).unwrap();
            ctx.alloc_instr(GlobalSet { global }, loc);
        }
        Operator::I32Const { value } => const_(ctx, Value::I32(value)),
        Operator::I64Const { value } => const_(ctx, Value::I64(value)),
        Operator::F32Const { value } => const_(ctx, Value::F32(f32::from_bits(value.bits()))),
        Operator::F64Const { value } => const_(ctx, Value::F64(f64::from_bits(value.bits()))),
        Operator::V128Const { value } => {
            let val = crate::init_expr::v128_to_u128(&value);
            const_(ctx, Value::V128(val))
        }
        Operator::I32Eqz => unop(ctx, UnaryOp::I32Eqz),
        Operator::I32Eq => binop(ctx, BinaryOp::I32Eq),
        Operator::I32Ne => binop(ctx, BinaryOp::I32Ne),
        Operator::I32LtS => binop(ctx, BinaryOp::I32LtS),
        Operator::I32LtU => binop(ctx, BinaryOp::I32LtU),
        Operator::I32GtS => binop(ctx, BinaryOp::I32GtS),
        Operator::I32GtU => binop(ctx, BinaryOp::I32GtU),
        Operator::I32LeS => binop(ctx, BinaryOp::I32LeS),
        Operator::I32LeU => binop(ctx, BinaryOp::I32LeU),
        Operator::I32GeS => binop(ctx, BinaryOp::I32GeS),
        Operator::I32GeU => binop(ctx, BinaryOp::I32GeU),

        Operator::I64Eqz => unop(ctx, UnaryOp::I64Eqz),
        Operator::I64Eq => binop(ctx, BinaryOp::I64Eq),
        Operator::I64Ne => binop(ctx, BinaryOp::I64Ne),
        Operator::I64LtS => binop(ctx, BinaryOp::I64LtS),
        Operator::I64LtU => binop(ctx, BinaryOp::I64LtU),
        Operator::I64GtS => binop(ctx, BinaryOp::I64GtS),
        Operator::I64GtU => binop(ctx, BinaryOp::I64GtU),
        Operator::I64LeS => binop(ctx, BinaryOp::I64LeS),
        Operator::I64LeU => binop(ctx, BinaryOp::I64LeU),
        Operator::I64GeS => binop(ctx, BinaryOp::I64GeS),
        Operator::I64GeU => binop(ctx, BinaryOp::I64GeU),

        Operator::F32Eq => binop(ctx, BinaryOp::F32Eq),
        Operator::F32Ne => binop(ctx, BinaryOp::F32Ne),
        Operator::F32Lt => binop(ctx, BinaryOp::F32Lt),
        Operator::F32Gt => binop(ctx, BinaryOp::F32Gt),
        Operator::F32Le => binop(ctx, BinaryOp::F32Le),
        Operator::F32Ge => binop(ctx, BinaryOp::F32Ge),

        Operator::F64Eq => binop(ctx, BinaryOp::F64Eq),
        Operator::F64Ne => binop(ctx, BinaryOp::F64Ne),
        Operator::F64Lt => binop(ctx, BinaryOp::F64Lt),
        Operator::F64Gt => binop(ctx, BinaryOp::F64Gt),
        Operator::F64Le => binop(ctx, BinaryOp::F64Le),
        Operator::F64Ge => binop(ctx, BinaryOp::F64Ge),

        Operator::I32Clz => unop(ctx, UnaryOp::I32Clz),
        Operator::I32Ctz => unop(ctx, UnaryOp::I32Ctz),
        Operator::I32Popcnt => unop(ctx, UnaryOp::I32Popcnt),
        Operator::I32Add => binop(ctx, BinaryOp::I32Add),
        Operator::I32Sub => binop(ctx, BinaryOp::I32Sub),
        Operator::I32Mul => binop(ctx, BinaryOp::I32Mul),
        Operator::I32DivS => binop(ctx, BinaryOp::I32DivS),
        Operator::I32DivU => binop(ctx, BinaryOp::I32DivU),
        Operator::I32RemS => binop(ctx, BinaryOp::I32RemS),
        Operator::I32RemU => binop(ctx, BinaryOp::I32RemU),
        Operator::I32And => binop(ctx, BinaryOp::I32And),
        Operator::I32Or => binop(ctx, BinaryOp::I32Or),
        Operator::I32Xor => binop(ctx, BinaryOp::I32Xor),
        Operator::I32Shl => binop(ctx, BinaryOp::I32Shl),
        Operator::I32ShrS => binop(ctx, BinaryOp::I32ShrS),
        Operator::I32ShrU => binop(ctx, BinaryOp::I32ShrU),
        Operator::I32Rotl => binop(ctx, BinaryOp::I32Rotl),
        Operator::I32Rotr => binop(ctx, BinaryOp::I32Rotr),

        Operator::I64Clz => unop(ctx, UnaryOp::I64Clz),
        Operator::I64Ctz => unop(ctx, UnaryOp::I64Ctz),
        Operator::I64Popcnt => unop(ctx, UnaryOp::I64Popcnt),
        Operator::I64Add => binop(ctx, BinaryOp::I64Add),
        Operator::I64Sub => binop(ctx, BinaryOp::I64Sub),
        Operator::I64Mul => binop(ctx, BinaryOp::I64Mul),
        Operator::I64DivS => binop(ctx, BinaryOp::I64DivS),
        Operator::I64DivU => binop(ctx, BinaryOp::I64DivU),
        Operator::I64RemS => binop(ctx, BinaryOp::I64RemS),
        Operator::I64RemU => binop(ctx, BinaryOp::I64RemU),
        Operator::I64And => binop(ctx, BinaryOp::I64And),
        Operator::I64Or => binop(ctx, BinaryOp::I64Or),
        Operator::I64Xor => binop(ctx, BinaryOp::I64Xor),
        Operator::I64Shl => binop(ctx, BinaryOp::I64Shl),
        Operator::I64ShrS => binop(ctx, BinaryOp::I64ShrS),
        Operator::I64ShrU => binop(ctx, BinaryOp::I64ShrU),
        Operator::I64Rotl => binop(ctx, BinaryOp::I64Rotl),
        Operator::I64Rotr => binop(ctx, BinaryOp::I64Rotr),

        Operator::F32Abs => unop(ctx, UnaryOp::F32Abs),
        Operator::F32Neg => unop(ctx, UnaryOp::F32Neg),
        Operator::F32Ceil => unop(ctx, UnaryOp::F32Ceil),
        Operator::F32Floor => unop(ctx, UnaryOp::F32Floor),
        Operator::F32Trunc => unop(ctx, UnaryOp::F32Trunc),
        Operator::F32Nearest => unop(ctx, UnaryOp::F32Nearest),
        Operator::F32Sqrt => unop(ctx, UnaryOp::F32Sqrt),
        Operator::F32Add => binop(ctx, BinaryOp::F32Add),
        Operator::F32Sub => binop(ctx, BinaryOp::F32Sub),
        Operator::F32Mul => binop(ctx, BinaryOp::F32Mul),
        Operator::F32Div => binop(ctx, BinaryOp::F32Div),
        Operator::F32Min => binop(ctx, BinaryOp::F32Min),
        Operator::F32Max => binop(ctx, BinaryOp::F32Max),
        Operator::F32Copysign => binop(ctx, BinaryOp::F32Copysign),

        Operator::F64Abs => unop(ctx, UnaryOp::F64Abs),
        Operator::F64Neg => unop(ctx, UnaryOp::F64Neg),
        Operator::F64Ceil => unop(ctx, UnaryOp::F64Ceil),
        Operator::F64Floor => unop(ctx, UnaryOp::F64Floor),
        Operator::F64Trunc => unop(ctx, UnaryOp::F64Trunc),
        Operator::F64Nearest => unop(ctx, UnaryOp::F64Nearest),
        Operator::F64Sqrt => unop(ctx, UnaryOp::F64Sqrt),
        Operator::F64Add => binop(ctx, BinaryOp::F64Add),
        Operator::F64Sub => binop(ctx, BinaryOp::F64Sub),
        Operator::F64Mul => binop(ctx, BinaryOp::F64Mul),
        Operator::F64Div => binop(ctx, BinaryOp::F64Div),
        Operator::F64Min => binop(ctx, BinaryOp::F64Min),
        Operator::F64Max => binop(ctx, BinaryOp::F64Max),
        Operator::F64Copysign => binop(ctx, BinaryOp::F64Copysign),

        Operator::I32WrapI64 => unop(ctx, UnaryOp::I32WrapI64),
        Operator::I32TruncF32S => unop(ctx, UnaryOp::I32TruncSF32),
        Operator::I32TruncF32U => unop(ctx, UnaryOp::I32TruncUF32),
        Operator::I32TruncF64S => unop(ctx, UnaryOp::I32TruncSF64),
        Operator::I32TruncF64U => unop(ctx, UnaryOp::I32TruncUF64),

        Operator::I64ExtendI32S => unop(ctx, UnaryOp::I64ExtendSI32),
        Operator::I64ExtendI32U => unop(ctx, UnaryOp::I64ExtendUI32),
        Operator::I64TruncF32S => unop(ctx, UnaryOp::I64TruncSF32),
        Operator::I64TruncF32U => unop(ctx, UnaryOp::I64TruncUF32),
        Operator::I64TruncF64S => unop(ctx, UnaryOp::I64TruncSF64),
        Operator::I64TruncF64U => unop(ctx, UnaryOp::I64TruncUF64),

        Operator::F32ConvertI32S => unop(ctx, UnaryOp::F32ConvertSI32),
        Operator::F32ConvertI32U => unop(ctx, UnaryOp::F32ConvertUI32),
        Operator::F32ConvertI64S => unop(ctx, UnaryOp::F32ConvertSI64),
        Operator::F32ConvertI64U => unop(ctx, UnaryOp::F32ConvertUI64),
        Operator::F32DemoteF64 => unop(ctx, UnaryOp::F32DemoteF64),

        Operator::F64ConvertI32S => unop(ctx, UnaryOp::F64ConvertSI32),
        Operator::F64ConvertI32U => unop(ctx, UnaryOp::F64ConvertUI32),
        Operator::F64ConvertI64S => unop(ctx, UnaryOp::F64ConvertSI64),
        Operator::F64ConvertI64U => unop(ctx, UnaryOp::F64ConvertUI64),
        Operator::F64PromoteF32 => unop(ctx, UnaryOp::F64PromoteF32),

        Operator::I32ReinterpretF32 => unop(ctx, UnaryOp::I32ReinterpretF32),
        Operator::I64ReinterpretF64 => unop(ctx, UnaryOp::I64ReinterpretF64),
        Operator::F32ReinterpretI32 => unop(ctx, UnaryOp::F32ReinterpretI32),
        Operator::F64ReinterpretI64 => unop(ctx, UnaryOp::F64ReinterpretI64),

        Operator::I32Extend8S => unop(ctx, UnaryOp::I32Extend8S),
        Operator::I32Extend16S => unop(ctx, UnaryOp::I32Extend16S),
        Operator::I64Extend8S => unop(ctx, UnaryOp::I64Extend8S),
        Operator::I64Extend16S => unop(ctx, UnaryOp::I64Extend16S),
        Operator::I64Extend32S => unop(ctx, UnaryOp::I64Extend32S),

        Operator::Drop => ctx.alloc_instr(Drop {}, loc),
        Operator::Select => ctx.alloc_instr(Select { ty: None }, loc),
        Operator::TypedSelect { ty } => {
            let ty = ValType::parse(&ty).unwrap();
            ctx.alloc_instr(Select { ty: Some(ty) }, loc);
        }
        Operator::Return => {
            ctx.alloc_instr(Return {}, loc);
            ctx.unreachable();
        }
        Operator::Unreachable => {
            ctx.alloc_instr(Unreachable {}, loc);
            ctx.unreachable();
        }
        Operator::Block { ty } => {
            let param_tys = block_param_tys(ctx, ty).unwrap();
            let result_tys = block_result_tys(ctx, ty).unwrap();
            let seq = ctx
                .push_control(BlockKind::Block, param_tys, result_tys)
                .unwrap();
            ctx.alloc_instr_in_control(1, Block { seq }, loc).unwrap();
        }
        Operator::Loop { ty } => {
            let result_tys = block_result_tys(ctx, ty).unwrap();
            let param_tys = block_param_tys(ctx, ty).unwrap();
            let seq = ctx
                .push_control(BlockKind::Loop, param_tys, result_tys)
                .unwrap();
            ctx.alloc_instr_in_control(1, Loop { seq }, loc).unwrap();
        }
        Operator::If { ty } => {
            let result_tys = block_result_tys(ctx, ty).unwrap();
            let param_tys = block_param_tys(ctx, ty).unwrap();

            let consequent = ctx
                .push_control(BlockKind::If, param_tys, result_tys)
                .unwrap();
            ctx.if_else.push(context::IfElseState {
                consequent,
                alternative: None,
            });
        }
        Operator::End => {
            let (frame, _block) = ctx.pop_control().unwrap();

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
                            let alternative = ctx
                                .push_control(
                                    BlockKind::Else,
                                    frame.start_types.clone(),
                                    frame.end_types.clone(),
                                )
                                .unwrap();
                            ctx.pop_control().unwrap();
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
        }
        Operator::Else => {
            let (frame, _consequent) = ctx.pop_control().unwrap();
            // An `else` instruction is only valid immediately inside an if/else
            // block which is denoted by the `IfElse` block kind.
            match frame.kind {
                BlockKind::If => {}
                _ => panic!("`else` without a leading `if`"),
            }

            // But we still need to parse the alternative block, so allocate the
            // block here to parse.
            let alternative = ctx
                .push_control(BlockKind::Else, frame.start_types, frame.end_types)
                .unwrap();
            let last = ctx.if_else.last_mut().unwrap();
            if last.alternative.is_some() {
                panic!("`else` without a leading `if`")
            }
            last.alternative = Some(alternative);
        }
        Operator::Br { relative_depth } => {
            let n = relative_depth as usize;
            let block = ctx.control(n).unwrap().block;
            ctx.alloc_instr(Br { block }, loc);
            ctx.unreachable();
        }
        Operator::BrIf { relative_depth } => {
            let n = relative_depth as usize;
            let block = ctx.control(n).unwrap().block;
            ctx.alloc_instr(BrIf { block }, loc);
        }

        Operator::BrTable { table } => {
            let mut blocks = Vec::with_capacity(table.len());
            let (labels, default) = table.read_table().unwrap();
            let default = ctx.control(default as usize).unwrap();

            let default = default.block;
            for label in labels.iter() {
                let control = ctx.control(*label as usize).unwrap();
                blocks.push(control.block);
            }
            let blocks = blocks.into_boxed_slice();
            ctx.alloc_instr(BrTable { blocks, default }, loc);
            ctx.unreachable();
        }

        Operator::MemorySize { reserved: _ } => {
            let memory = ctx.indices.get_memory(0).unwrap();
            ctx.alloc_instr(MemorySize { memory }, loc);
        }
        Operator::MemoryGrow { reserved: _ } => {
            let memory = ctx.indices.get_memory(0).unwrap();
            ctx.alloc_instr(MemoryGrow { memory }, loc);
        }
        Operator::MemoryInit { segment } => {
            let memory = ctx.indices.get_memory(0).unwrap();
            let data = ctx.indices.get_data(segment).unwrap();
            ctx.alloc_instr(MemoryInit { memory, data }, loc);
        }
        Operator::DataDrop { segment } => {
            let data = ctx.indices.get_data(segment).unwrap();
            ctx.alloc_instr(DataDrop { data }, loc);
        }
        Operator::MemoryCopy => {
            let memory = ctx.indices.get_memory(0).unwrap();
            ctx.alloc_instr(
                MemoryCopy {
                    src: memory,
                    dst: memory,
                },
                loc,
            );
        }
        Operator::MemoryFill => {
            let memory = ctx.indices.get_memory(0).unwrap();
            ctx.alloc_instr(MemoryFill { memory }, loc);
        }

        Operator::Nop => {}

        Operator::I32Load { memarg } => load(ctx, memarg, LoadKind::I32 { atomic: false }),
        Operator::I64Load { memarg } => load(ctx, memarg, LoadKind::I64 { atomic: false }),
        Operator::F32Load { memarg } => load(ctx, memarg, LoadKind::F32),
        Operator::F64Load { memarg } => load(ctx, memarg, LoadKind::F64),
        Operator::V128Load { memarg } => load(ctx, memarg, LoadKind::V128),
        Operator::I32Load8S { memarg } => load(ctx, memarg, LoadKind::I32_8 { kind: SignExtend }),
        Operator::I32Load8U { memarg } => load(ctx, memarg, LoadKind::I32_8 { kind: ZeroExtend }),
        Operator::I32Load16S { memarg } => load(ctx, memarg, LoadKind::I32_16 { kind: SignExtend }),
        Operator::I32Load16U { memarg } => load(ctx, memarg, LoadKind::I32_16 { kind: ZeroExtend }),
        Operator::I64Load8S { memarg } => load(ctx, memarg, LoadKind::I64_8 { kind: SignExtend }),
        Operator::I64Load8U { memarg } => load(ctx, memarg, LoadKind::I64_8 { kind: ZeroExtend }),
        Operator::I64Load16S { memarg } => load(ctx, memarg, LoadKind::I64_16 { kind: SignExtend }),
        Operator::I64Load16U { memarg } => load(ctx, memarg, LoadKind::I64_16 { kind: ZeroExtend }),
        Operator::I64Load32S { memarg } => load(ctx, memarg, LoadKind::I64_32 { kind: SignExtend }),
        Operator::I64Load32U { memarg } => load(ctx, memarg, LoadKind::I64_32 { kind: ZeroExtend }),

        Operator::I32Store { memarg } => store(ctx, memarg, StoreKind::I32 { atomic: false }),
        Operator::I64Store { memarg } => store(ctx, memarg, StoreKind::I64 { atomic: false }),
        Operator::F32Store { memarg } => store(ctx, memarg, StoreKind::F32),
        Operator::F64Store { memarg } => store(ctx, memarg, StoreKind::F64),
        Operator::V128Store { memarg } => store(ctx, memarg, StoreKind::V128),
        Operator::I32Store8 { memarg } => store(ctx, memarg, StoreKind::I32_8 { atomic: false }),
        Operator::I32Store16 { memarg } => store(ctx, memarg, StoreKind::I32_16 { atomic: false }),
        Operator::I64Store8 { memarg } => store(ctx, memarg, StoreKind::I64_8 { atomic: false }),
        Operator::I64Store16 { memarg } => store(ctx, memarg, StoreKind::I64_16 { atomic: false }),
        Operator::I64Store32 { memarg } => store(ctx, memarg, StoreKind::I64_32 { atomic: false }),

        Operator::AtomicFence { flags: _ } => ctx.alloc_instr(AtomicFence {}, loc),

        Operator::I32AtomicLoad { memarg } => load(ctx, memarg, LoadKind::I32 { atomic: true }),
        Operator::I64AtomicLoad { memarg } => load(ctx, memarg, LoadKind::I64 { atomic: true }),
        Operator::I32AtomicLoad8U { memarg } => load(
            ctx,
            memarg,
            LoadKind::I32_8 {
                kind: ZeroExtendAtomic,
            },
        ),
        Operator::I32AtomicLoad16U { memarg } => load(
            ctx,
            memarg,
            LoadKind::I32_16 {
                kind: ZeroExtendAtomic,
            },
        ),
        Operator::I64AtomicLoad8U { memarg } => load(
            ctx,
            memarg,
            LoadKind::I64_8 {
                kind: ZeroExtendAtomic,
            },
        ),
        Operator::I64AtomicLoad16U { memarg } => load(
            ctx,
            memarg,
            LoadKind::I64_16 {
                kind: ZeroExtendAtomic,
            },
        ),
        Operator::I64AtomicLoad32U { memarg } => load(
            ctx,
            memarg,
            LoadKind::I64_32 {
                kind: ZeroExtendAtomic,
            },
        ),

        Operator::I32AtomicStore { memarg } => store(ctx, memarg, StoreKind::I32 { atomic: true }),
        Operator::I64AtomicStore { memarg } => store(ctx, memarg, StoreKind::I64 { atomic: true }),
        Operator::I32AtomicStore8 { memarg } => {
            store(ctx, memarg, StoreKind::I32_8 { atomic: true })
        }
        Operator::I32AtomicStore16 { memarg } => {
            store(ctx, memarg, StoreKind::I32_16 { atomic: true })
        }
        Operator::I64AtomicStore8 { memarg } => {
            store(ctx, memarg, StoreKind::I64_8 { atomic: true })
        }
        Operator::I64AtomicStore16 { memarg } => {
            store(ctx, memarg, StoreKind::I64_16 { atomic: true })
        }
        Operator::I64AtomicStore32 { memarg } => {
            store(ctx, memarg, StoreKind::I64_32 { atomic: true })
        }

        Operator::I32AtomicRmwAdd { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::Add, AtomicWidth::I32);
        }
        Operator::I64AtomicRmwAdd { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::Add, AtomicWidth::I64);
        }
        Operator::I32AtomicRmw8AddU { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::Add, AtomicWidth::I32_8);
        }
        Operator::I32AtomicRmw16AddU { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::Add, AtomicWidth::I32_16);
        }
        Operator::I64AtomicRmw8AddU { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::Add, AtomicWidth::I64_8);
        }
        Operator::I64AtomicRmw16AddU { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::Add, AtomicWidth::I64_16);
        }
        Operator::I64AtomicRmw32AddU { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::Add, AtomicWidth::I64_32);
        }

        Operator::I32AtomicRmwSub { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::Sub, AtomicWidth::I32);
        }
        Operator::I64AtomicRmwSub { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::Sub, AtomicWidth::I64);
        }
        Operator::I32AtomicRmw8SubU { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::Sub, AtomicWidth::I32_8);
        }
        Operator::I32AtomicRmw16SubU { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::Sub, AtomicWidth::I32_16);
        }
        Operator::I64AtomicRmw8SubU { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::Sub, AtomicWidth::I64_8);
        }
        Operator::I64AtomicRmw16SubU { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::Sub, AtomicWidth::I64_16);
        }
        Operator::I64AtomicRmw32SubU { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::Sub, AtomicWidth::I64_32);
        }

        Operator::I32AtomicRmwAnd { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::And, AtomicWidth::I32);
        }
        Operator::I64AtomicRmwAnd { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::And, AtomicWidth::I64);
        }
        Operator::I32AtomicRmw8AndU { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::And, AtomicWidth::I32_8);
        }
        Operator::I32AtomicRmw16AndU { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::And, AtomicWidth::I32_16);
        }
        Operator::I64AtomicRmw8AndU { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::And, AtomicWidth::I64_8);
        }
        Operator::I64AtomicRmw16AndU { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::And, AtomicWidth::I64_16);
        }
        Operator::I64AtomicRmw32AndU { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::And, AtomicWidth::I64_32);
        }

        Operator::I32AtomicRmwOr { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::Or, AtomicWidth::I32);
        }
        Operator::I64AtomicRmwOr { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::Or, AtomicWidth::I64);
        }
        Operator::I32AtomicRmw8OrU { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::Or, AtomicWidth::I32_8);
        }
        Operator::I32AtomicRmw16OrU { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::Or, AtomicWidth::I32_16);
        }
        Operator::I64AtomicRmw8OrU { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::Or, AtomicWidth::I64_8);
        }
        Operator::I64AtomicRmw16OrU { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::Or, AtomicWidth::I64_16);
        }
        Operator::I64AtomicRmw32OrU { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::Or, AtomicWidth::I64_32);
        }

        Operator::I32AtomicRmwXor { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::Xor, AtomicWidth::I32);
        }
        Operator::I64AtomicRmwXor { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::Xor, AtomicWidth::I64);
        }
        Operator::I32AtomicRmw8XorU { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::Xor, AtomicWidth::I32_8);
        }
        Operator::I32AtomicRmw16XorU { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::Xor, AtomicWidth::I32_16);
        }
        Operator::I64AtomicRmw8XorU { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::Xor, AtomicWidth::I64_8);
        }
        Operator::I64AtomicRmw16XorU { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::Xor, AtomicWidth::I64_16);
        }
        Operator::I64AtomicRmw32XorU { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::Xor, AtomicWidth::I64_32);
        }

        Operator::I32AtomicRmwXchg { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::Xchg, AtomicWidth::I32);
        }
        Operator::I64AtomicRmwXchg { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::Xchg, AtomicWidth::I64);
        }
        Operator::I32AtomicRmw8XchgU { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::Xchg, AtomicWidth::I32_8);
        }
        Operator::I32AtomicRmw16XchgU { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::Xchg, AtomicWidth::I32_16);
        }
        Operator::I64AtomicRmw8XchgU { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::Xchg, AtomicWidth::I64_8);
        }
        Operator::I64AtomicRmw16XchgU { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::Xchg, AtomicWidth::I64_16);
        }
        Operator::I64AtomicRmw32XchgU { memarg } => {
            atomicrmw(ctx, memarg, AtomicOp::Xchg, AtomicWidth::I64_32);
        }

        Operator::I32AtomicRmwCmpxchg { memarg } => {
            cmpxchg(ctx, memarg, AtomicWidth::I32);
        }
        Operator::I64AtomicRmwCmpxchg { memarg } => {
            cmpxchg(ctx, memarg, AtomicWidth::I64);
        }
        Operator::I32AtomicRmw8CmpxchgU { memarg } => {
            cmpxchg(ctx, memarg, AtomicWidth::I32_8);
        }
        Operator::I32AtomicRmw16CmpxchgU { memarg } => {
            cmpxchg(ctx, memarg, AtomicWidth::I32_16);
        }
        Operator::I64AtomicRmw8CmpxchgU { memarg } => {
            cmpxchg(ctx, memarg, AtomicWidth::I64_8);
        }
        Operator::I64AtomicRmw16CmpxchgU { memarg } => {
            cmpxchg(ctx, memarg, AtomicWidth::I64_16);
        }
        Operator::I64AtomicRmw32CmpxchgU { memarg } => {
            cmpxchg(ctx, memarg, AtomicWidth::I64_32);
        }
        Operator::AtomicNotify { ref memarg } => {
            let memory = ctx.indices.get_memory(0).unwrap();
            ctx.alloc_instr(
                AtomicNotify {
                    memory,
                    arg: mem_arg(memarg),
                },
                loc,
            );
        }
        Operator::I32AtomicWait { ref memarg } | Operator::I64AtomicWait { ref memarg } => {
            let sixty_four = match inst {
                Operator::I32AtomicWait { .. } => false,
                _ => true,
            };
            let memory = ctx.indices.get_memory(0).unwrap();
            ctx.alloc_instr(
                AtomicWait {
                    sixty_four,
                    memory,
                    arg: mem_arg(memarg),
                },
                loc,
            );
        }

        Operator::TableGet { table } => {
            let table = ctx.indices.get_table(table).unwrap();
            ctx.alloc_instr(TableGet { table }, loc);
        }
        Operator::TableSet { table } => {
            let table = ctx.indices.get_table(table).unwrap();
            ctx.alloc_instr(TableSet { table }, loc);
        }
        Operator::TableGrow { table } => {
            let table = ctx.indices.get_table(table).unwrap();
            ctx.alloc_instr(TableGrow { table }, loc);
        }
        Operator::TableSize { table } => {
            let table = ctx.indices.get_table(table).unwrap();
            ctx.alloc_instr(TableSize { table }, loc);
        }
        Operator::TableFill { table } => {
            let table = ctx.indices.get_table(table).unwrap();
            ctx.alloc_instr(TableFill { table }, loc);
        }
        Operator::RefNull { ty } => {
            let ty = ValType::parse(&ty).unwrap();
            ctx.alloc_instr(RefNull { ty }, loc);
        }
        Operator::RefIsNull => {
            ctx.alloc_instr(RefIsNull {}, loc);
        }
        Operator::RefFunc { function_index } => {
            let func = ctx.indices.get_func(function_index).unwrap();
            ctx.alloc_instr(RefFunc { func }, loc);
        }

        Operator::V8x16Swizzle => {
            ctx.alloc_instr(V128Swizzle {}, loc);
        }

        Operator::V8x16Shuffle { lanes } => {
            ctx.alloc_instr(V128Shuffle { indices: lanes }, loc);
        }

        Operator::I8x16Splat => unop(ctx, UnaryOp::I8x16Splat),
        Operator::I8x16ExtractLaneS { lane: idx } => unop(ctx, UnaryOp::I8x16ExtractLaneS { idx }),
        Operator::I8x16ExtractLaneU { lane: idx } => unop(ctx, UnaryOp::I8x16ExtractLaneU { idx }),
        Operator::I8x16ReplaceLane { lane: idx } => binop(ctx, BinaryOp::I8x16ReplaceLane { idx }),
        Operator::I16x8Splat => unop(ctx, UnaryOp::I16x8Splat),
        Operator::I16x8ExtractLaneS { lane: idx } => unop(ctx, UnaryOp::I16x8ExtractLaneS { idx }),
        Operator::I16x8ExtractLaneU { lane: idx } => unop(ctx, UnaryOp::I16x8ExtractLaneU { idx }),
        Operator::I16x8ReplaceLane { lane: idx } => binop(ctx, BinaryOp::I16x8ReplaceLane { idx }),
        Operator::I32x4Splat => unop(ctx, UnaryOp::I32x4Splat),
        Operator::I32x4ExtractLane { lane: idx } => unop(ctx, UnaryOp::I32x4ExtractLane { idx }),
        Operator::I32x4ReplaceLane { lane: idx } => binop(ctx, BinaryOp::I32x4ReplaceLane { idx }),
        Operator::I64x2Splat => unop(ctx, UnaryOp::I64x2Splat),
        Operator::I64x2ExtractLane { lane: idx } => unop(ctx, UnaryOp::I64x2ExtractLane { idx }),
        Operator::I64x2ReplaceLane { lane: idx } => binop(ctx, BinaryOp::I64x2ReplaceLane { idx }),
        Operator::F32x4Splat => unop(ctx, UnaryOp::F32x4Splat),
        Operator::F32x4ExtractLane { lane: idx } => unop(ctx, UnaryOp::F32x4ExtractLane { idx }),
        Operator::F32x4ReplaceLane { lane: idx } => binop(ctx, BinaryOp::F32x4ReplaceLane { idx }),
        Operator::F64x2Splat => unop(ctx, UnaryOp::F64x2Splat),
        Operator::F64x2ExtractLane { lane: idx } => unop(ctx, UnaryOp::F64x2ExtractLane { idx }),
        Operator::F64x2ReplaceLane { lane: idx } => binop(ctx, BinaryOp::F64x2ReplaceLane { idx }),

        Operator::I8x16Eq => binop(ctx, BinaryOp::I8x16Eq),
        Operator::I8x16Ne => binop(ctx, BinaryOp::I8x16Ne),
        Operator::I8x16LtS => binop(ctx, BinaryOp::I8x16LtS),
        Operator::I8x16LtU => binop(ctx, BinaryOp::I8x16LtU),
        Operator::I8x16GtS => binop(ctx, BinaryOp::I8x16GtS),
        Operator::I8x16GtU => binop(ctx, BinaryOp::I8x16GtU),
        Operator::I8x16LeS => binop(ctx, BinaryOp::I8x16LeS),
        Operator::I8x16LeU => binop(ctx, BinaryOp::I8x16LeU),
        Operator::I8x16GeS => binop(ctx, BinaryOp::I8x16GeS),
        Operator::I8x16GeU => binop(ctx, BinaryOp::I8x16GeU),
        Operator::I16x8Eq => binop(ctx, BinaryOp::I16x8Eq),
        Operator::I16x8Ne => binop(ctx, BinaryOp::I16x8Ne),
        Operator::I16x8LtS => binop(ctx, BinaryOp::I16x8LtS),
        Operator::I16x8LtU => binop(ctx, BinaryOp::I16x8LtU),
        Operator::I16x8GtS => binop(ctx, BinaryOp::I16x8GtS),
        Operator::I16x8GtU => binop(ctx, BinaryOp::I16x8GtU),
        Operator::I16x8LeS => binop(ctx, BinaryOp::I16x8LeS),
        Operator::I16x8LeU => binop(ctx, BinaryOp::I16x8LeU),
        Operator::I16x8GeS => binop(ctx, BinaryOp::I16x8GeS),
        Operator::I16x8GeU => binop(ctx, BinaryOp::I16x8GeU),
        Operator::I32x4Eq => binop(ctx, BinaryOp::I32x4Eq),
        Operator::I32x4Ne => binop(ctx, BinaryOp::I32x4Ne),
        Operator::I32x4LtS => binop(ctx, BinaryOp::I32x4LtS),
        Operator::I32x4LtU => binop(ctx, BinaryOp::I32x4LtU),
        Operator::I32x4GtS => binop(ctx, BinaryOp::I32x4GtS),
        Operator::I32x4GtU => binop(ctx, BinaryOp::I32x4GtU),
        Operator::I32x4LeS => binop(ctx, BinaryOp::I32x4LeS),
        Operator::I32x4LeU => binop(ctx, BinaryOp::I32x4LeU),
        Operator::I32x4GeS => binop(ctx, BinaryOp::I32x4GeS),
        Operator::I32x4GeU => binop(ctx, BinaryOp::I32x4GeU),
        Operator::F32x4Eq => binop(ctx, BinaryOp::F32x4Eq),
        Operator::F32x4Ne => binop(ctx, BinaryOp::F32x4Ne),
        Operator::F32x4Lt => binop(ctx, BinaryOp::F32x4Lt),
        Operator::F32x4Gt => binop(ctx, BinaryOp::F32x4Gt),
        Operator::F32x4Le => binop(ctx, BinaryOp::F32x4Le),
        Operator::F32x4Ge => binop(ctx, BinaryOp::F32x4Ge),
        Operator::F64x2Eq => binop(ctx, BinaryOp::F64x2Eq),
        Operator::F64x2Ne => binop(ctx, BinaryOp::F64x2Ne),
        Operator::F64x2Lt => binop(ctx, BinaryOp::F64x2Lt),
        Operator::F64x2Gt => binop(ctx, BinaryOp::F64x2Gt),
        Operator::F64x2Le => binop(ctx, BinaryOp::F64x2Le),
        Operator::F64x2Ge => binop(ctx, BinaryOp::F64x2Ge),

        Operator::V128Not => unop(ctx, UnaryOp::V128Not),
        Operator::V128And => binop(ctx, BinaryOp::V128And),
        Operator::V128AndNot => binop(ctx, BinaryOp::V128AndNot),
        Operator::V128Or => binop(ctx, BinaryOp::V128Or),
        Operator::V128Xor => binop(ctx, BinaryOp::V128Xor),

        Operator::V128Bitselect => ctx.alloc_instr(V128Bitselect {}, loc),

        Operator::I8x16Abs => unop(ctx, UnaryOp::I8x16Abs),
        Operator::I8x16Neg => unop(ctx, UnaryOp::I8x16Neg),
        Operator::I8x16AnyTrue => unop(ctx, UnaryOp::I8x16AnyTrue),
        Operator::I8x16AllTrue => unop(ctx, UnaryOp::I8x16AllTrue),
        Operator::I8x16Shl => binop(ctx, BinaryOp::I8x16Shl),
        Operator::I8x16ShrS => binop(ctx, BinaryOp::I8x16ShrS),
        Operator::I8x16ShrU => binop(ctx, BinaryOp::I8x16ShrU),
        Operator::I8x16Add => binop(ctx, BinaryOp::I8x16Add),
        Operator::I8x16AddSaturateS => binop(ctx, BinaryOp::I8x16AddSaturateS),
        Operator::I8x16AddSaturateU => binop(ctx, BinaryOp::I8x16AddSaturateU),
        Operator::I8x16Sub => binop(ctx, BinaryOp::I8x16Sub),
        Operator::I8x16SubSaturateS => binop(ctx, BinaryOp::I8x16SubSaturateS),
        Operator::I8x16SubSaturateU => binop(ctx, BinaryOp::I8x16SubSaturateU),

        Operator::I16x8Abs => unop(ctx, UnaryOp::I16x8Abs),
        Operator::I16x8Neg => unop(ctx, UnaryOp::I16x8Neg),
        Operator::I16x8AnyTrue => unop(ctx, UnaryOp::I16x8AnyTrue),
        Operator::I16x8AllTrue => unop(ctx, UnaryOp::I16x8AllTrue),
        Operator::I16x8Shl => binop(ctx, BinaryOp::I16x8Shl),
        Operator::I16x8ShrS => binop(ctx, BinaryOp::I16x8ShrS),
        Operator::I16x8ShrU => binop(ctx, BinaryOp::I16x8ShrU),
        Operator::I16x8Add => binop(ctx, BinaryOp::I16x8Add),
        Operator::I16x8AddSaturateS => binop(ctx, BinaryOp::I16x8AddSaturateS),
        Operator::I16x8AddSaturateU => binop(ctx, BinaryOp::I16x8AddSaturateU),
        Operator::I16x8Sub => binop(ctx, BinaryOp::I16x8Sub),
        Operator::I16x8SubSaturateS => binop(ctx, BinaryOp::I16x8SubSaturateS),
        Operator::I16x8SubSaturateU => binop(ctx, BinaryOp::I16x8SubSaturateU),
        Operator::I16x8Mul => binop(ctx, BinaryOp::I16x8Mul),

        Operator::I32x4Abs => unop(ctx, UnaryOp::I32x4Abs),
        Operator::I32x4Neg => unop(ctx, UnaryOp::I32x4Neg),
        Operator::I32x4AnyTrue => unop(ctx, UnaryOp::I32x4AnyTrue),
        Operator::I32x4AllTrue => unop(ctx, UnaryOp::I32x4AllTrue),
        Operator::I32x4Shl => binop(ctx, BinaryOp::I32x4Shl),
        Operator::I32x4ShrS => binop(ctx, BinaryOp::I32x4ShrS),
        Operator::I32x4ShrU => binop(ctx, BinaryOp::I32x4ShrU),
        Operator::I32x4Add => binop(ctx, BinaryOp::I32x4Add),
        Operator::I32x4Sub => binop(ctx, BinaryOp::I32x4Sub),
        Operator::I32x4Mul => binop(ctx, BinaryOp::I32x4Mul),

        Operator::I64x2Neg => unop(ctx, UnaryOp::I64x2Neg),
        Operator::I64x2Shl => binop(ctx, BinaryOp::I64x2Shl),
        Operator::I64x2ShrS => binop(ctx, BinaryOp::I64x2ShrS),
        Operator::I64x2ShrU => binop(ctx, BinaryOp::I64x2ShrU),
        Operator::I64x2Add => binop(ctx, BinaryOp::I64x2Add),
        Operator::I64x2Sub => binop(ctx, BinaryOp::I64x2Sub),
        Operator::I64x2Mul => binop(ctx, BinaryOp::I64x2Mul),

        Operator::F32x4Abs => unop(ctx, UnaryOp::F32x4Abs),
        Operator::F32x4Neg => unop(ctx, UnaryOp::F32x4Neg),
        Operator::F32x4Sqrt => unop(ctx, UnaryOp::F32x4Sqrt),
        Operator::F32x4Add => binop(ctx, BinaryOp::F32x4Add),
        Operator::F32x4Sub => binop(ctx, BinaryOp::F32x4Sub),
        Operator::F32x4Mul => binop(ctx, BinaryOp::F32x4Mul),
        Operator::F32x4Div => binop(ctx, BinaryOp::F32x4Div),
        Operator::F32x4Min => binop(ctx, BinaryOp::F32x4Min),
        Operator::F32x4Max => binop(ctx, BinaryOp::F32x4Max),

        Operator::F64x2Abs => unop(ctx, UnaryOp::F64x2Abs),
        Operator::F64x2Neg => unop(ctx, UnaryOp::F64x2Neg),
        Operator::F64x2Sqrt => unop(ctx, UnaryOp::F64x2Sqrt),
        Operator::F64x2Add => binop(ctx, BinaryOp::F64x2Add),
        Operator::F64x2Sub => binop(ctx, BinaryOp::F64x2Sub),
        Operator::F64x2Mul => binop(ctx, BinaryOp::F64x2Mul),
        Operator::F64x2Div => binop(ctx, BinaryOp::F64x2Div),
        Operator::F64x2Min => binop(ctx, BinaryOp::F64x2Min),
        Operator::F64x2Max => binop(ctx, BinaryOp::F64x2Max),

        Operator::I32x4TruncSatF32x4S => unop(ctx, UnaryOp::I32x4TruncSatF32x4S),
        Operator::I32x4TruncSatF32x4U => unop(ctx, UnaryOp::I32x4TruncSatF32x4U),
        Operator::F32x4ConvertI32x4S => unop(ctx, UnaryOp::F32x4ConvertI32x4S),
        Operator::F32x4ConvertI32x4U => unop(ctx, UnaryOp::F32x4ConvertI32x4U),

        Operator::I32TruncSatF32S => unop(ctx, UnaryOp::I32TruncSSatF32),
        Operator::I32TruncSatF32U => unop(ctx, UnaryOp::I32TruncUSatF32),
        Operator::I32TruncSatF64S => unop(ctx, UnaryOp::I32TruncSSatF64),
        Operator::I32TruncSatF64U => unop(ctx, UnaryOp::I32TruncUSatF64),
        Operator::I64TruncSatF32S => unop(ctx, UnaryOp::I64TruncSSatF32),
        Operator::I64TruncSatF32U => unop(ctx, UnaryOp::I64TruncUSatF32),
        Operator::I64TruncSatF64S => unop(ctx, UnaryOp::I64TruncSSatF64),
        Operator::I64TruncSatF64U => unop(ctx, UnaryOp::I64TruncUSatF64),

        Operator::V8x16LoadSplat { memarg } => load_simd(ctx, memarg, LoadSimdKind::Splat8),
        Operator::V16x8LoadSplat { memarg } => load_simd(ctx, memarg, LoadSimdKind::Splat16),
        Operator::V32x4LoadSplat { memarg } => load_simd(ctx, memarg, LoadSimdKind::Splat32),
        Operator::V64x2LoadSplat { memarg } => load_simd(ctx, memarg, LoadSimdKind::Splat64),

        Operator::I8x16NarrowI16x8S => binop(ctx, BinaryOp::I8x16NarrowI16x8S),
        Operator::I8x16NarrowI16x8U => binop(ctx, BinaryOp::I8x16NarrowI16x8U),
        Operator::I16x8NarrowI32x4S => binop(ctx, BinaryOp::I16x8NarrowI32x4S),
        Operator::I16x8NarrowI32x4U => binop(ctx, BinaryOp::I16x8NarrowI32x4U),
        Operator::I16x8WidenLowI8x16S => unop(ctx, UnaryOp::I16x8WidenLowI8x16S),
        Operator::I16x8WidenLowI8x16U => unop(ctx, UnaryOp::I16x8WidenLowI8x16U),
        Operator::I16x8WidenHighI8x16S => unop(ctx, UnaryOp::I16x8WidenHighI8x16S),
        Operator::I16x8WidenHighI8x16U => unop(ctx, UnaryOp::I16x8WidenHighI8x16U),
        Operator::I32x4WidenLowI16x8S => unop(ctx, UnaryOp::I32x4WidenLowI16x8S),
        Operator::I32x4WidenLowI16x8U => unop(ctx, UnaryOp::I32x4WidenLowI16x8U),
        Operator::I32x4WidenHighI16x8S => unop(ctx, UnaryOp::I32x4WidenHighI16x8S),
        Operator::I32x4WidenHighI16x8U => unop(ctx, UnaryOp::I32x4WidenHighI16x8U),
        Operator::I16x8Load8x8S { memarg } => load_simd(ctx, memarg, LoadSimdKind::I16x8Load8x8S),
        Operator::I16x8Load8x8U { memarg } => load_simd(ctx, memarg, LoadSimdKind::I16x8Load8x8U),
        Operator::I32x4Load16x4S { memarg } => load_simd(ctx, memarg, LoadSimdKind::I32x4Load16x4S),
        Operator::I32x4Load16x4U { memarg } => load_simd(ctx, memarg, LoadSimdKind::I32x4Load16x4U),
        Operator::I64x2Load32x2S { memarg } => load_simd(ctx, memarg, LoadSimdKind::I64x2Load32x2S),
        Operator::I64x2Load32x2U { memarg } => load_simd(ctx, memarg, LoadSimdKind::I64x2Load32x2U),
        Operator::I8x16RoundingAverageU => binop(ctx, BinaryOp::I8x16RoundingAverageU),
        Operator::I16x8RoundingAverageU => binop(ctx, BinaryOp::I16x8RoundingAverageU),

        Operator::I8x16MinS => binop(ctx, BinaryOp::I8x16MinS),
        Operator::I8x16MinU => binop(ctx, BinaryOp::I8x16MinU),
        Operator::I8x16MaxS => binop(ctx, BinaryOp::I8x16MaxS),
        Operator::I8x16MaxU => binop(ctx, BinaryOp::I8x16MaxU),
        Operator::I16x8MinS => binop(ctx, BinaryOp::I16x8MinS),
        Operator::I16x8MinU => binop(ctx, BinaryOp::I16x8MinU),
        Operator::I16x8MaxS => binop(ctx, BinaryOp::I16x8MaxS),
        Operator::I16x8MaxU => binop(ctx, BinaryOp::I16x8MaxU),
        Operator::I32x4MinS => binop(ctx, BinaryOp::I32x4MinS),
        Operator::I32x4MinU => binop(ctx, BinaryOp::I32x4MinU),
        Operator::I32x4MaxS => binop(ctx, BinaryOp::I32x4MaxS),
        Operator::I32x4MaxU => binop(ctx, BinaryOp::I32x4MaxU),

        Operator::I8x16Bitmask => unop(ctx, UnaryOp::I8x16Bitmask),
        Operator::I16x8Bitmask => unop(ctx, UnaryOp::I16x8Bitmask),
        Operator::I32x4Bitmask => unop(ctx, UnaryOp::I32x4Bitmask),

        Operator::TableCopy {
            src_table,
            dst_table,
        } => {
            let src = ctx.indices.get_table(src_table).unwrap();
            let dst = ctx.indices.get_table(dst_table).unwrap();
            ctx.alloc_instr(TableCopy { src, dst }, loc);
        }

        Operator::TableInit { segment, table } => {
            let elem = ctx.indices.get_element(segment).unwrap();
            let table = ctx.indices.get_table(table).unwrap();
            ctx.alloc_instr(TableInit { elem, table }, loc);
        }

        Operator::ElemDrop { segment } => {
            let elem = ctx.indices.get_element(segment).unwrap();
            ctx.alloc_instr(ElemDrop { elem }, loc);
        }

        Operator::ReturnCall { .. } | Operator::ReturnCallIndirect { .. } => {
            unimplemented!("not supported");
        }
    }
}
