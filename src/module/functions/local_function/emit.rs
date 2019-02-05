use crate::emit::IdsToIndices;
use crate::encode::Encoder;
use crate::ir::*;
use crate::map::IdHashMap;
use crate::module::functions::LocalFunction;
use crate::module::memories::MemoryId;
use crate::ty::ValType;

pub(crate) fn run(
    func: &LocalFunction,
    indices: &IdsToIndices,
    local_indices: &IdHashMap<Local, u32>,
    encoder: &mut Encoder,
) {
    let mut v = Emit {
        func,
        indices,
        id: func.entry_block().into(),
        blocks: vec![],
        encoder,
        local_indices,
    };
    v.visit(func.entry_block());
}

struct Emit<'a, 'b> {
    // The function we are visiting.
    func: &'a LocalFunction,

    // The id of the current expression.
    id: ExprId,

    // Needed so we can map locals to their indices.
    indices: &'a IdsToIndices,
    local_indices: &'a IdHashMap<Local, u32>,

    // Stack of blocks that we are currently emitting instructions for. A branch
    // is only valid if its target is one of these blocks.
    blocks: Vec<BlockId>,

    // The instruction sequence we are building up to emit.
    encoder: &'a mut Encoder<'b>,
}

impl Emit<'_, '_> {
    fn visit<E>(&mut self, e: E)
    where
        E: Into<ExprId>,
    {
        self.visit_expr_id(e.into())
    }

    fn visit_expr_id(&mut self, id: ExprId) {
        use self::Expr::*;

        let old = self.id;
        self.id = id;

        match &self.func.exprs[id] {
            Const(e) => e.value.emit(self.encoder),
            Block(e) => self.visit_block(e),
            BrTable(e) => self.visit_br_table(e),
            IfElse(e) => self.visit_if_else(e),

            Drop(e) => {
                self.visit(e.expr);
                self.encoder.byte(0x1a); // drop
            }

            Return(e) => {
                for x in e.values.iter() {
                    self.visit(*x);
                }
                self.encoder.byte(0x0f); // return
            }

            WithSideEffects(e) => {
                self.visit(e.value);
                for x in e.side_effects.iter() {
                    self.visit(*x);
                }
            }

            MemorySize(e) => {
                let idx = self.indices.get_memory_index(e.memory);
                self.encoder.byte(0x3f); // memory.size
                self.encoder.u32(idx);
            }

            MemoryGrow(e) => {
                self.visit(e.pages);
                let idx = self.indices.get_memory_index(e.memory);
                self.encoder.byte(0x40); // memory.grow
                self.encoder.u32(idx);
            }

            MemoryInit(e) => {
                self.visit(e.memory_offset);
                self.visit(e.data_offset);
                self.visit(e.len);
                self.encoder.raw(&[0xfc, 0x08]); // memory.init
                let idx = self.indices.get_data_index(e.data);
                self.encoder.u32(idx);
                let idx = self.indices.get_memory_index(e.memory);
                assert_eq!(idx, 0);
                self.encoder.u32(idx);
            }

            DataDrop(e) => {
                self.encoder.raw(&[0xfc, 0x09]); // data.drop
                let idx = self.indices.get_data_index(e.data);
                self.encoder.u32(idx);
            }

            MemoryCopy(e) => {
                self.visit(e.dst_offset);
                self.visit(e.src_offset);
                self.visit(e.len);
                self.encoder.raw(&[0xfc, 0x0a]); // memory.copy
                let idx = self.indices.get_memory_index(e.src);
                assert_eq!(idx, 0);
                self.encoder.u32(idx);
                let idx = self.indices.get_memory_index(e.dst);
                assert_eq!(idx, 0);
                self.encoder.u32(idx);
            }

            MemoryFill(e) => {
                self.visit(e.offset);
                self.visit(e.value);
                self.visit(e.len);
                self.encoder.raw(&[0xfc, 0x0b]); // memory.fill
                let idx = self.indices.get_memory_index(e.memory);
                assert_eq!(idx, 0);
                self.encoder.u32(idx);
            }

            Binop(e) => {
                use BinaryOp::*;

                self.visit(e.lhs);
                self.visit(e.rhs);
                let opcode = match e.op {
                    I32Eq => 0x46,
                    I32Ne => 0x47,
                    I32LtS => 0x48,
                    I32LtU => 0x49,
                    I32GtS => 0x4a,
                    I32GtU => 0x4b,
                    I32LeS => 0x4c,
                    I32LeU => 0x4d,
                    I32GeS => 0x4e,
                    I32GeU => 0x4f,

                    I64Eq => 0x51,
                    I64Ne => 0x52,
                    I64LtS => 0x53,
                    I64LtU => 0x54,
                    I64GtS => 0x55,
                    I64GtU => 0x56,
                    I64LeS => 0x57,
                    I64LeU => 0x58,
                    I64GeS => 0x59,
                    I64GeU => 0x5a,

                    F32Eq => 0x5b,
                    F32Ne => 0x5c,
                    F32Lt => 0x5d,
                    F32Gt => 0x5e,
                    F32Le => 0x5f,
                    F32Ge => 0x60,

                    F64Eq => 0x61,
                    F64Ne => 0x62,
                    F64Lt => 0x63,
                    F64Gt => 0x64,
                    F64Le => 0x65,
                    F64Ge => 0x66,

                    I32Add => 0x6a,
                    I32Sub => 0x6b,
                    I32Mul => 0x6c,
                    I32DivS => 0x6d,
                    I32DivU => 0x6e,
                    I32RemS => 0x6f,
                    I32RemU => 0x70,
                    I32And => 0x71,
                    I32Or => 0x72,
                    I32Xor => 0x73,
                    I32Shl => 0x74,
                    I32ShrS => 0x75,
                    I32ShrU => 0x76,
                    I32Rotl => 0x77,
                    I32Rotr => 0x78,

                    I64Add => 0x7c,
                    I64Sub => 0x7d,
                    I64Mul => 0x7e,
                    I64DivS => 0x7f,
                    I64DivU => 0x80,
                    I64RemS => 0x81,
                    I64RemU => 0x82,
                    I64And => 0x83,
                    I64Or => 0x84,
                    I64Xor => 0x85,
                    I64Shl => 0x86,
                    I64ShrS => 0x87,
                    I64ShrU => 0x88,
                    I64Rotl => 0x89,
                    I64Rotr => 0x8a,

                    F32Add => 0x92,
                    F32Sub => 0x93,
                    F32Mul => 0x94,
                    F32Div => 0x95,
                    F32Min => 0x96,
                    F32Max => 0x97,
                    F32Copysign => 0x98,

                    F64Add => 0xa0,
                    F64Sub => 0xa1,
                    F64Mul => 0xa2,
                    F64Div => 0xa3,
                    F64Min => 0xa4,
                    F64Max => 0xa5,
                    F64Copysign => 0xa6,
                };
                self.encoder.byte(opcode);
            }

            Unop(e) => {
                use UnaryOp::*;

                self.visit(e.expr);
                let opcode = match e.op {
                    I32Eqz => 0x45,
                    I32Clz => 0x67,
                    I32Ctz => 0x68,
                    I32Popcnt => 0x69,

                    I64Eqz => 0x50,
                    I64Clz => 0x79,
                    I64Ctz => 0x7a,
                    I64Popcnt => 0x7b,

                    F32Abs => 0x8b,
                    F32Neg => 0x8c,
                    F32Ceil => 0x8d,
                    F32Floor => 0x8e,
                    F32Trunc => 0x8f,
                    F32Nearest => 0x90,
                    F32Sqrt => 0x91,

                    F64Abs => 0x99,
                    F64Neg => 0x9a,
                    F64Ceil => 0x9b,
                    F64Floor => 0x9c,
                    F64Trunc => 0x9d,
                    F64Nearest => 0x9e,
                    F64Sqrt => 0x9f,

                    I32WrapI64 => 0xa7,
                    I32TruncSF32 => 0xa8,
                    I32TruncUF32 => 0xa9,
                    I32TruncSF64 => 0xaa,
                    I32TruncUF64 => 0xab,
                    I64ExtendSI32 => 0xac,
                    I64ExtendUI32 => 0xad,
                    I64TruncSF32 => 0xae,
                    I64TruncUF32 => 0xaf,
                    I64TruncSF64 => 0xb0,
                    I64TruncUF64 => 0xb1,

                    F32ConvertSI32 => 0xb2,
                    F32ConvertUI32 => 0xb3,
                    F32ConvertSI64 => 0xb4,
                    F32ConvertUI64 => 0xb5,
                    F32DemoteF64 => 0xb6,
                    F64ConvertSI32 => 0xb7,
                    F64ConvertUI32 => 0xb8,
                    F64ConvertSI64 => 0xb9,
                    F64ConvertUI64 => 0xba,
                    F64PromoteF32 => 0xbb,

                    I32ReinterpretF32 => 0xbc,
                    I64ReinterpretF64 => 0xbd,
                    F32ReinterpretI32 => 0xbe,
                    F64ReinterpretI64 => 0xbf,

                    I32Extend8S => 0xc0,
                    I32Extend16S => 0xc1,
                    I64Extend8S => 0xc2,
                    I64Extend16S => 0xc3,
                    I64Extend32S => 0xc4,
                };
                self.encoder.byte(opcode);
            }

            Select(e) => {
                self.visit(e.alternative);
                self.visit(e.consequent);
                self.visit(e.condition);
                self.encoder.byte(0x1b); // select
            }

            Unreachable(_) => {
                self.encoder.byte(0x00); // unreachable
            }

            Br(e) => {
                for x in e.args.iter() {
                    self.visit(*x);
                }
                let target = self.branch_target(e.block);
                self.encoder.byte(0x0c); // br
                self.encoder.u32(target);
            }

            BrIf(e) => {
                for x in e.args.iter() {
                    self.visit(*x);
                }
                self.visit(e.condition);
                let target = self.branch_target(e.block);
                self.encoder.byte(0x0d); // br_if
                self.encoder.u32(target);
            }

            Call(e) => {
                for x in e.args.iter() {
                    self.visit(*x);
                }
                let idx = self.indices.get_func_index(e.func);
                self.encoder.byte(0x10); // call
                self.encoder.u32(idx);
            }

            CallIndirect(e) => {
                for x in e.args.iter() {
                    self.visit(*x);
                }
                self.visit(e.func);
                let idx = self.indices.get_type_index(e.ty);
                let table = self.indices.get_table_index(e.table);
                self.encoder.byte(0x11); // call_indirect
                self.encoder.u32(idx);
                self.encoder.u32(table);
            }

            LocalGet(e) => {
                let idx = self.local_indices[&e.local];
                self.encoder.byte(0x20); // local.get
                self.encoder.u32(idx);
            }

            LocalSet(e) => {
                self.visit(e.value);
                let idx = self.local_indices[&e.local];
                self.encoder.byte(0x21); // local.set
                self.encoder.u32(idx);
            }

            LocalTee(e) => {
                self.visit(e.value);
                let idx = self.local_indices[&e.local];
                self.encoder.byte(0x22); // local.tee
                self.encoder.u32(idx);
            }

            GlobalGet(e) => {
                let idx = self.indices.get_global_index(e.global);
                self.encoder.byte(0x23); // global.get
                self.encoder.u32(idx);
            }

            GlobalSet(e) => {
                self.visit(e.value);
                let idx = self.indices.get_global_index(e.global);
                self.encoder.byte(0x24); // global.set
                self.encoder.u32(idx);
            }

            Load(e) => {
                use ExtendedLoad::*;
                use LoadKind::*;
                self.visit(e.address);
                match e.kind {
                    I32 { atomic: false } => self.encoder.byte(0x28), // i32.load
                    I32 { atomic: true } => self.encoder.raw(&[0xfe, 0x10]), // i32.atomic.load
                    I64 { atomic: false } => self.encoder.byte(0x29), // i64.load
                    I64 { atomic: true } => self.encoder.raw(&[0xfe, 0x11]), // i64.atomic.load
                    F32 => self.encoder.byte(0x2a),                   // f32.load
                    F64 => self.encoder.byte(0x2b),                   // f64.load
                    V128 => self.encoder.raw(&[0xfd, 0x00]),
                    I32_8 { kind: SignExtend } => self.encoder.byte(0x2c),
                    I32_8 { kind: ZeroExtend } => self.encoder.byte(0x2d),
                    I32_8 {
                        kind: ZeroExtendAtomic,
                    } => self.encoder.raw(&[0xfe, 0x12]),
                    I32_16 { kind: SignExtend } => self.encoder.byte(0x2e),
                    I32_16 { kind: ZeroExtend } => self.encoder.byte(0x2f),
                    I32_16 {
                        kind: ZeroExtendAtomic,
                    } => self.encoder.raw(&[0xfe, 0x13]),
                    I64_8 { kind: SignExtend } => self.encoder.byte(0x30),
                    I64_8 { kind: ZeroExtend } => self.encoder.byte(0x31),
                    I64_8 {
                        kind: ZeroExtendAtomic,
                    } => self.encoder.raw(&[0xfe, 0x14]),
                    I64_16 { kind: SignExtend } => self.encoder.byte(0x32),
                    I64_16 { kind: ZeroExtend } => self.encoder.byte(0x33),
                    I64_16 {
                        kind: ZeroExtendAtomic,
                    } => self.encoder.raw(&[0xfe, 0x15]),
                    I64_32 { kind: SignExtend } => self.encoder.byte(0x34),
                    I64_32 { kind: ZeroExtend } => self.encoder.byte(0x35),
                    I64_32 {
                        kind: ZeroExtendAtomic,
                    } => self.encoder.raw(&[0xfe, 0x16]),
                }
                self.memarg(e.memory, &e.arg);
            }

            Store(e) => {
                use StoreKind::*;
                self.visit(e.address);
                self.visit(e.value);
                match e.kind {
                    I32 { atomic: false } => self.encoder.byte(0x36), // i32.store
                    I32 { atomic: true } => self.encoder.raw(&[0xfe, 0x17]), // i32.atomic.store
                    I64 { atomic: false } => self.encoder.byte(0x37), // i64.store
                    I64 { atomic: true } => self.encoder.raw(&[0xfe, 0x18]), // i64.atomic.store
                    F32 => self.encoder.byte(0x38),                   // f32.store
                    F64 => self.encoder.byte(0x39),                   // f64.store
                    V128 => self.encoder.raw(&[0xfd, 0x01]),          // v128.store
                    I32_8 { atomic: false } => self.encoder.byte(0x3a), // i32.store8
                    I32_8 { atomic: true } => self.encoder.raw(&[0xfe, 0x19]), // i32.atomic.store8
                    I32_16 { atomic: false } => self.encoder.byte(0x3b), // i32.store16
                    I32_16 { atomic: true } => self.encoder.raw(&[0xfe, 0x1a]), // i32.atomic.store16
                    I64_8 { atomic: false } => self.encoder.byte(0x3c),         // i64.store8
                    I64_8 { atomic: true } => self.encoder.raw(&[0xfe, 0x1b]),  // i64.atomic.store8
                    I64_16 { atomic: false } => self.encoder.byte(0x3d),        // i64.store16
                    I64_16 { atomic: true } => self.encoder.raw(&[0xfe, 0x1c]), // i64.atomic.store16
                    I64_32 { atomic: false } => self.encoder.byte(0x3e),        // i64.store32
                    I64_32 { atomic: true } => self.encoder.raw(&[0xfe, 0x1d]), // i64.atomic.store32
                }
                self.memarg(e.memory, &e.arg);
            }

            AtomicRmw(e) => {
                use AtomicOp::*;
                use AtomicWidth::*;

                self.visit(e.address);
                self.visit(e.value);

                self.encoder.byte(0xfe);
                self.encoder.byte(match (e.op, e.width) {
                    (Add, I32) => 0x1e,
                    (Add, I64) => 0x1f,
                    (Add, I32_8) => 0x20,
                    (Add, I32_16) => 0x21,
                    (Add, I64_8) => 0x22,
                    (Add, I64_16) => 0x23,
                    (Add, I64_32) => 0x24,

                    (Sub, I32) => 0x25,
                    (Sub, I64) => 0x26,
                    (Sub, I32_8) => 0x27,
                    (Sub, I32_16) => 0x28,
                    (Sub, I64_8) => 0x29,
                    (Sub, I64_16) => 0x2a,
                    (Sub, I64_32) => 0x2b,

                    (And, I32) => 0x2c,
                    (And, I64) => 0x2d,
                    (And, I32_8) => 0x2e,
                    (And, I32_16) => 0x2f,
                    (And, I64_8) => 0x30,
                    (And, I64_16) => 0x31,
                    (And, I64_32) => 0x32,

                    (Or, I32) => 0x33,
                    (Or, I64) => 0x34,
                    (Or, I32_8) => 0x35,
                    (Or, I32_16) => 0x36,
                    (Or, I64_8) => 0x37,
                    (Or, I64_16) => 0x38,
                    (Or, I64_32) => 0x39,

                    (Xor, I32) => 0x3a,
                    (Xor, I64) => 0x3b,
                    (Xor, I32_8) => 0x3c,
                    (Xor, I32_16) => 0x3d,
                    (Xor, I64_8) => 0x3e,
                    (Xor, I64_16) => 0x3f,
                    (Xor, I64_32) => 0x40,

                    (Xchg, I32) => 0x41,
                    (Xchg, I64) => 0x42,
                    (Xchg, I32_8) => 0x43,
                    (Xchg, I32_16) => 0x44,
                    (Xchg, I64_8) => 0x45,
                    (Xchg, I64_16) => 0x46,
                    (Xchg, I64_32) => 0x47,
                });

                self.memarg(e.memory, &e.arg);
            }

            Cmpxchg(e) => {
                use AtomicWidth::*;

                self.visit(e.address);
                self.visit(e.expected);
                self.visit(e.replacement);

                self.encoder.byte(0xfe);
                self.encoder.byte(match e.width {
                    I32 => 0x48,
                    I64 => 0x49,
                    I32_8 => 0x4a,
                    I32_16 => 0x4b,
                    I64_8 => 0x4c,
                    I64_16 => 0x4d,
                    I64_32 => 0x4e,
                });

                self.memarg(e.memory, &e.arg);
            }

            AtomicNotify(e) => {
                self.visit(e.address);
                self.visit(e.count);

                self.encoder.byte(0xfe);
                self.encoder.byte(0x00);
                self.memarg(e.memory, &e.arg);
            }

            AtomicWait(e) => {
                self.visit(e.address);
                self.visit(e.expected);
                self.visit(e.timeout);

                self.encoder.byte(0xfe);
                self.encoder.byte(if e.sixty_four { 0x02 } else { 0x01 });
                self.memarg(e.memory, &e.arg);
            }
        }

        self.id = old;
    }

    fn branch_target(&self, block: BlockId) -> u32 {
        self.blocks.iter().rev().position(|b| *b == block).expect(
            "attempt to branch to invalid block; bad transformation pass introduced bad branching?",
        ) as u32
    }

    fn visit_block(&mut self, e: &Block) {
        self.blocks.push(Block::new_id(self.id));

        match e.kind {
            BlockKind::Block => {
                self.encoder.byte(0x02); // block
                self.block_type(&e.results);
            }
            BlockKind::Loop => {
                self.encoder.byte(0x03); // loop
                self.block_type(&e.results);
            }
            BlockKind::FunctionEntry | BlockKind::IfElse => {}
        }

        for x in &e.exprs {
            self.visit(*x);
        }

        match e.kind {
            BlockKind::Block | BlockKind::Loop | BlockKind::FunctionEntry => {
                self.encoder.byte(0x0b); // end
            }
            BlockKind::IfElse => {}
        }

        self.blocks.pop();
    }

    fn visit_if_else(&mut self, e: &IfElse) {
        self.visit(e.condition);

        self.encoder.byte(0x04); // if
        let consequent = self.func.block(e.consequent);
        self.block_type(&consequent.results);

        self.visit(e.consequent);

        // TODO: don't emit `else` for empty else blocks
        self.encoder.byte(0x05); // else
        self.visit(e.alternative);

        self.encoder.byte(0x0b); // end
    }

    fn visit_br_table(&mut self, e: &BrTable) {
        for x in e.args.iter() {
            self.visit(*x);
        }
        self.visit(e.which);

        self.encoder.byte(0x0e); // br_table
        self.encoder.usize(e.blocks.len());
        for b in e.blocks.iter() {
            let target = self.branch_target(*b);
            self.encoder.u32(target);
        }
        let default = self.branch_target(e.default);
        self.encoder.u32(default);
    }

    fn block_type(&mut self, ty: &[ValType]) {
        match ty.len() {
            0 => self.encoder.byte(0x40),
            1 => ty[0].emit(self.encoder),
            _ => panic!(
                "multiple return values not yet supported; write a transformation to \
                 rewrite them into single value returns"
            ),
        }
    }

    fn memarg(&mut self, id: MemoryId, arg: &MemArg) {
        assert_eq!(self.indices.get_memory_index(id), 0);
        self.encoder.u32(arg.align.trailing_zeros());
        self.encoder.u32(arg.offset);
    }
}
