use crate::emit::IdsToIndices;
use crate::encode::Encoder;
use crate::ir::*;
use crate::map::IdHashMap;
use crate::module::functions::LocalFunction;
use crate::module::memories::MemoryId;

pub(crate) fn run(
    func: &LocalFunction,
    indices: &IdsToIndices,
    local_indices: &IdHashMap<Local, u32>,
    encoder: &mut Encoder,
    map: Option<&mut Vec<(InstrLocId, usize)>>,
) {
    let v = &mut Emit {
        indices,
        blocks: vec![],
        block_kinds: vec![BlockKind::FunctionEntry],
        encoder,
        local_indices,
        map,
    };
    dfs_in_order(v, func, func.entry_block());

    debug_assert!(v.blocks.is_empty());
    debug_assert!(v.block_kinds.is_empty());
}

struct Emit<'a, 'b> {
    // Needed so we can map locals to their indices.
    indices: &'a IdsToIndices,
    local_indices: &'a IdHashMap<Local, u32>,

    // Stack of blocks that we are currently emitting instructions for. A branch
    // is only valid if its target is one of these blocks. See also the
    // `branch_target` method.
    blocks: Vec<InstrSeqId>,

    // The kinds of blocks we have on the stack. This is parallel to `blocks`
    // 99% of the time, except we push a new block kind in `visit_instr`, before
    // we push the block in `start_instr_seq`, because this is when we know the
    // kind.
    block_kinds: Vec<BlockKind>,

    // The instruction sequence we are building up to emit.
    encoder: &'a mut Encoder<'b>,

    // Encoded ExprId -> offset map.
    map: Option<&'a mut Vec<(InstrLocId, usize)>>,
}

impl<'instr> Visitor<'instr> for Emit<'_, '_> {
    fn start_instr_seq(&mut self, seq: &'instr InstrSeq) {
        self.blocks.push(seq.id());
        debug_assert_eq!(self.blocks.len(), self.block_kinds.len());

        match self.block_kinds.last().unwrap() {
            BlockKind::Block => {
                self.encoder.byte(0x02); // block
                self.block_type(seq.ty);
            }
            BlockKind::Loop => {
                self.encoder.byte(0x03); // loop
                self.block_type(seq.ty);
            }
            BlockKind::If => {
                self.encoder.byte(0x04); // if
                self.block_type(seq.ty);
            }
            // Function entries are implicitly started, and don't need any
            // opcode to start them. `Else` blocks are started when `If` blocks
            // end in an `else` opcode, which we handle in `end_instr_seq`
            // below.
            BlockKind::FunctionEntry | BlockKind::Else => {}
        }
    }

    fn end_instr_seq(&mut self, seq: &'instr InstrSeq) {
        let popped_block = self.blocks.pop();
        debug_assert_eq!(popped_block, Some(seq.id()));

        let popped_kind = self.block_kinds.pop();
        debug_assert!(popped_kind.is_some());

        debug_assert_eq!(self.blocks.len(), self.block_kinds.len());

        if let BlockKind::If = popped_kind.unwrap() {
            // We're about to visit the `else` block, so push its kind.
            //
            // TODO: don't emit `else` for empty else blocks
            self.block_kinds.push(BlockKind::Else);
            self.encoder.byte(0x05); // else
        } else {
            self.encoder.byte(0x0b); // end
        }
    }

    fn visit_instr(&mut self, instr: &'instr Instr, instr_loc: &'instr InstrLocId) {
        use self::Instr::*;

        if let Some(map) = self.map.as_mut() {
            let pos = self.encoder.pos();
            // Save the encoded_at position for the specified ExprId.
            map.push((instr_loc.clone(), pos));
        }

        match instr {
            Block(_) => self.block_kinds.push(BlockKind::Block),
            Loop(_) => self.block_kinds.push(BlockKind::Loop),

            // Push the `if` block kind, and then when we finish encoding the
            // `if` block, we'll pop the `if` kind and push the `else`
            // kind. This allows us to maintain the `self.blocks.len() ==
            // self.block_kinds.len()` invariant.
            IfElse(_) => self.block_kinds.push(BlockKind::If),

            BrTable(e) => {
                self.encoder.byte(0x0e); // br_table
                self.encoder.usize(e.blocks.len());
                for b in e.blocks.iter() {
                    let target = self.branch_target(*b);
                    self.encoder.u32(target);
                }
                let default = self.branch_target(e.default);
                self.encoder.u32(default);
            }

            Const(e) => e.value.emit(self.encoder),
            Drop(_) => self.encoder.byte(0x1a),   // drop
            Return(_) => self.encoder.byte(0x0f), // return

            MemorySize(e) => {
                let idx = self.indices.get_memory_index(e.memory);
                self.encoder.byte(0x3f); // memory.size
                self.encoder.u32(idx);
            }

            MemoryGrow(e) => {
                let idx = self.indices.get_memory_index(e.memory);
                self.encoder.byte(0x40); // memory.grow
                self.encoder.u32(idx);
            }

            MemoryInit(e) => {
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
                self.encoder.raw(&[0xfc, 0x0a]); // memory.copy
                let idx = self.indices.get_memory_index(e.src);
                assert_eq!(idx, 0);
                self.encoder.u32(idx);
                let idx = self.indices.get_memory_index(e.dst);
                assert_eq!(idx, 0);
                self.encoder.u32(idx);
            }

            MemoryFill(e) => {
                self.encoder.raw(&[0xfc, 0x0b]); // memory.fill
                let idx = self.indices.get_memory_index(e.memory);
                assert_eq!(idx, 0);
                self.encoder.u32(idx);
            }

            Binop(e) => {
                use crate::ir::BinaryOp::*;

                match e.op {
                    I32Eq => self.encoder.byte(0x46),
                    I32Ne => self.encoder.byte(0x47),
                    I32LtS => self.encoder.byte(0x48),
                    I32LtU => self.encoder.byte(0x49),
                    I32GtS => self.encoder.byte(0x4a),
                    I32GtU => self.encoder.byte(0x4b),
                    I32LeS => self.encoder.byte(0x4c),
                    I32LeU => self.encoder.byte(0x4d),
                    I32GeS => self.encoder.byte(0x4e),
                    I32GeU => self.encoder.byte(0x4f),

                    I64Eq => self.encoder.byte(0x51),
                    I64Ne => self.encoder.byte(0x52),
                    I64LtS => self.encoder.byte(0x53),
                    I64LtU => self.encoder.byte(0x54),
                    I64GtS => self.encoder.byte(0x55),
                    I64GtU => self.encoder.byte(0x56),
                    I64LeS => self.encoder.byte(0x57),
                    I64LeU => self.encoder.byte(0x58),
                    I64GeS => self.encoder.byte(0x59),
                    I64GeU => self.encoder.byte(0x5a),

                    F32Eq => self.encoder.byte(0x5b),
                    F32Ne => self.encoder.byte(0x5c),
                    F32Lt => self.encoder.byte(0x5d),
                    F32Gt => self.encoder.byte(0x5e),
                    F32Le => self.encoder.byte(0x5f),
                    F32Ge => self.encoder.byte(0x60),

                    F64Eq => self.encoder.byte(0x61),
                    F64Ne => self.encoder.byte(0x62),
                    F64Lt => self.encoder.byte(0x63),
                    F64Gt => self.encoder.byte(0x64),
                    F64Le => self.encoder.byte(0x65),
                    F64Ge => self.encoder.byte(0x66),

                    I32Add => self.encoder.byte(0x6a),
                    I32Sub => self.encoder.byte(0x6b),
                    I32Mul => self.encoder.byte(0x6c),
                    I32DivS => self.encoder.byte(0x6d),
                    I32DivU => self.encoder.byte(0x6e),
                    I32RemS => self.encoder.byte(0x6f),
                    I32RemU => self.encoder.byte(0x70),
                    I32And => self.encoder.byte(0x71),
                    I32Or => self.encoder.byte(0x72),
                    I32Xor => self.encoder.byte(0x73),
                    I32Shl => self.encoder.byte(0x74),
                    I32ShrS => self.encoder.byte(0x75),
                    I32ShrU => self.encoder.byte(0x76),
                    I32Rotl => self.encoder.byte(0x77),
                    I32Rotr => self.encoder.byte(0x78),

                    I64Add => self.encoder.byte(0x7c),
                    I64Sub => self.encoder.byte(0x7d),
                    I64Mul => self.encoder.byte(0x7e),
                    I64DivS => self.encoder.byte(0x7f),
                    I64DivU => self.encoder.byte(0x80),
                    I64RemS => self.encoder.byte(0x81),
                    I64RemU => self.encoder.byte(0x82),
                    I64And => self.encoder.byte(0x83),
                    I64Or => self.encoder.byte(0x84),
                    I64Xor => self.encoder.byte(0x85),
                    I64Shl => self.encoder.byte(0x86),
                    I64ShrS => self.encoder.byte(0x87),
                    I64ShrU => self.encoder.byte(0x88),
                    I64Rotl => self.encoder.byte(0x89),
                    I64Rotr => self.encoder.byte(0x8a),

                    F32Add => self.encoder.byte(0x92),
                    F32Sub => self.encoder.byte(0x93),
                    F32Mul => self.encoder.byte(0x94),
                    F32Div => self.encoder.byte(0x95),
                    F32Min => self.encoder.byte(0x96),
                    F32Max => self.encoder.byte(0x97),
                    F32Copysign => self.encoder.byte(0x98),

                    F64Add => self.encoder.byte(0xa0),
                    F64Sub => self.encoder.byte(0xa1),
                    F64Mul => self.encoder.byte(0xa2),
                    F64Div => self.encoder.byte(0xa3),
                    F64Min => self.encoder.byte(0xa4),
                    F64Max => self.encoder.byte(0xa5),
                    F64Copysign => self.encoder.byte(0xa6),

                    I8x16ReplaceLane { idx } => self.encoder.raw(&[0xfd, 0x17, idx]),
                    I16x8ReplaceLane { idx } => self.encoder.raw(&[0xfd, 0x1a, idx]),
                    I32x4ReplaceLane { idx } => self.encoder.raw(&[0xfd, 0x1c, idx]),
                    I64x2ReplaceLane { idx } => self.encoder.raw(&[0xfd, 0x1e, idx]),
                    F32x4ReplaceLane { idx } => self.encoder.raw(&[0xfd, 0x20, idx]),
                    F64x2ReplaceLane { idx } => self.encoder.raw(&[0xfd, 0x22, idx]),

                    I8x16Eq => self.simd(0x23),
                    I8x16Ne => self.simd(0x24),
                    I8x16LtS => self.simd(0x25),
                    I8x16LtU => self.simd(0x26),
                    I8x16GtS => self.simd(0x27),
                    I8x16GtU => self.simd(0x28),
                    I8x16LeS => self.simd(0x29),
                    I8x16LeU => self.simd(0x2a),
                    I8x16GeS => self.simd(0x2b),
                    I8x16GeU => self.simd(0x2c),

                    I16x8Eq => self.simd(0x2d),
                    I16x8Ne => self.simd(0x2e),
                    I16x8LtS => self.simd(0x2f),
                    I16x8LtU => self.simd(0x30),
                    I16x8GtS => self.simd(0x31),
                    I16x8GtU => self.simd(0x32),
                    I16x8LeS => self.simd(0x33),
                    I16x8LeU => self.simd(0x34),
                    I16x8GeS => self.simd(0x35),
                    I16x8GeU => self.simd(0x36),

                    I32x4Eq => self.simd(0x37),
                    I32x4Ne => self.simd(0x38),
                    I32x4LtS => self.simd(0x39),
                    I32x4LtU => self.simd(0x3a),
                    I32x4GtS => self.simd(0x3b),
                    I32x4GtU => self.simd(0x3c),
                    I32x4LeS => self.simd(0x3d),
                    I32x4LeU => self.simd(0x3e),
                    I32x4GeS => self.simd(0x3f),
                    I32x4GeU => self.simd(0x40),

                    F32x4Eq => self.simd(0x41),
                    F32x4Ne => self.simd(0x42),
                    F32x4Lt => self.simd(0x43),
                    F32x4Gt => self.simd(0x44),
                    F32x4Le => self.simd(0x45),
                    F32x4Ge => self.simd(0x46),

                    F64x2Eq => self.simd(0x47),
                    F64x2Ne => self.simd(0x48),
                    F64x2Lt => self.simd(0x49),
                    F64x2Gt => self.simd(0x4a),
                    F64x2Le => self.simd(0x4b),
                    F64x2Ge => self.simd(0x4c),

                    V128And => self.simd(0x4e),
                    V128AndNot => self.simd(0x4f),
                    V128Or => self.simd(0x50),
                    V128Xor => self.simd(0x51),

                    I8x16NarrowI16x8S => self.simd(0x65),
                    I8x16NarrowI16x8U => self.simd(0x66),
                    I8x16Shl => self.simd(0x6b),
                    I8x16ShrS => self.simd(0x6c),
                    I8x16ShrU => self.simd(0x6d),
                    I8x16Add => self.simd(0x6e),
                    I8x16AddSaturateS => self.simd(0x6f),
                    I8x16AddSaturateU => self.simd(0x70),
                    I8x16Sub => self.simd(0x71),
                    I8x16SubSaturateS => self.simd(0x72),
                    I8x16SubSaturateU => self.simd(0x73),
                    I8x16MinS => self.simd(0x76),
                    I8x16MinU => self.simd(0x77),
                    I8x16MaxS => self.simd(0x78),
                    I8x16MaxU => self.simd(0x79),
                    I8x16RoundingAverageU => self.simd(0x7b),

                    I16x8NarrowI32x4S => self.simd(0x85),
                    I16x8NarrowI32x4U => self.simd(0x86),
                    I16x8Shl => self.simd(0x8b),
                    I16x8ShrS => self.simd(0x8c),
                    I16x8ShrU => self.simd(0x8d),
                    I16x8Add => self.simd(0x8e),
                    I16x8AddSaturateS => self.simd(0x8f),
                    I16x8AddSaturateU => self.simd(0x90),
                    I16x8Sub => self.simd(0x91),
                    I16x8SubSaturateS => self.simd(0x92),
                    I16x8SubSaturateU => self.simd(0x93),
                    I16x8Mul => self.simd(0x95),
                    I16x8MinS => self.simd(0x96),
                    I16x8MinU => self.simd(0x97),
                    I16x8MaxS => self.simd(0x98),
                    I16x8MaxU => self.simd(0x99),
                    I16x8RoundingAverageU => self.simd(0x9b),

                    I32x4Shl => self.simd(0xab),
                    I32x4ShrS => self.simd(0xac),
                    I32x4ShrU => self.simd(0xad),
                    I32x4Add => self.simd(0xae),
                    I32x4Sub => self.simd(0xb1),
                    I32x4Mul => self.simd(0xb5),
                    I32x4MinS => self.simd(0xb6),
                    I32x4MinU => self.simd(0xb7),
                    I32x4MaxS => self.simd(0xb8),
                    I32x4MaxU => self.simd(0xb9),

                    I64x2Shl => self.simd(0xcb),
                    I64x2ShrS => self.simd(0xcc),
                    I64x2ShrU => self.simd(0xcd),
                    I64x2Add => self.simd(0xce),
                    I64x2Sub => self.simd(0xd1),
                    I64x2Mul => self.simd(0xd5),

                    F32x4Add => self.simd(0xe4),
                    F32x4Sub => self.simd(0xe5),
                    F32x4Mul => self.simd(0xe6),
                    F32x4Div => self.simd(0xe7),
                    F32x4Min => self.simd(0xe8),
                    F32x4Max => self.simd(0xe9),

                    F64x2Add => self.simd(0xf0),
                    F64x2Sub => self.simd(0xf1),
                    F64x2Mul => self.simd(0xf2),
                    F64x2Div => self.simd(0xf3),
                    F64x2Min => self.simd(0xf4),
                    F64x2Max => self.simd(0xf5),
                }
            }

            Unop(e) => {
                use crate::ir::UnaryOp::*;

                match e.op {
                    I32Eqz => self.encoder.byte(0x45),
                    I32Clz => self.encoder.byte(0x67),
                    I32Ctz => self.encoder.byte(0x68),
                    I32Popcnt => self.encoder.byte(0x69),

                    I64Eqz => self.encoder.byte(0x50),
                    I64Clz => self.encoder.byte(0x79),
                    I64Ctz => self.encoder.byte(0x7a),
                    I64Popcnt => self.encoder.byte(0x7b),

                    F32Abs => self.encoder.byte(0x8b),
                    F32Neg => self.encoder.byte(0x8c),
                    F32Ceil => self.encoder.byte(0x8d),
                    F32Floor => self.encoder.byte(0x8e),
                    F32Trunc => self.encoder.byte(0x8f),
                    F32Nearest => self.encoder.byte(0x90),
                    F32Sqrt => self.encoder.byte(0x91),

                    F64Abs => self.encoder.byte(0x99),
                    F64Neg => self.encoder.byte(0x9a),
                    F64Ceil => self.encoder.byte(0x9b),
                    F64Floor => self.encoder.byte(0x9c),
                    F64Trunc => self.encoder.byte(0x9d),
                    F64Nearest => self.encoder.byte(0x9e),
                    F64Sqrt => self.encoder.byte(0x9f),

                    I32WrapI64 => self.encoder.byte(0xa7),
                    I32TruncSF32 => self.encoder.byte(0xa8),
                    I32TruncUF32 => self.encoder.byte(0xa9),
                    I32TruncSF64 => self.encoder.byte(0xaa),
                    I32TruncUF64 => self.encoder.byte(0xab),
                    I64ExtendSI32 => self.encoder.byte(0xac),
                    I64ExtendUI32 => self.encoder.byte(0xad),
                    I64TruncSF32 => self.encoder.byte(0xae),
                    I64TruncUF32 => self.encoder.byte(0xaf),
                    I64TruncSF64 => self.encoder.byte(0xb0),
                    I64TruncUF64 => self.encoder.byte(0xb1),

                    F32ConvertSI32 => self.encoder.byte(0xb2),
                    F32ConvertUI32 => self.encoder.byte(0xb3),
                    F32ConvertSI64 => self.encoder.byte(0xb4),
                    F32ConvertUI64 => self.encoder.byte(0xb5),
                    F32DemoteF64 => self.encoder.byte(0xb6),
                    F64ConvertSI32 => self.encoder.byte(0xb7),
                    F64ConvertUI32 => self.encoder.byte(0xb8),
                    F64ConvertSI64 => self.encoder.byte(0xb9),
                    F64ConvertUI64 => self.encoder.byte(0xba),
                    F64PromoteF32 => self.encoder.byte(0xbb),

                    I32ReinterpretF32 => self.encoder.byte(0xbc),
                    I64ReinterpretF64 => self.encoder.byte(0xbd),
                    F32ReinterpretI32 => self.encoder.byte(0xbe),
                    F64ReinterpretI64 => self.encoder.byte(0xbf),

                    I32Extend8S => self.encoder.byte(0xc0),
                    I32Extend16S => self.encoder.byte(0xc1),
                    I64Extend8S => self.encoder.byte(0xc2),
                    I64Extend16S => self.encoder.byte(0xc3),
                    I64Extend32S => self.encoder.byte(0xc4),

                    I8x16Splat => self.simd(0x0f),
                    I16x8Splat => self.simd(0x10),
                    I32x4Splat => self.simd(0x11),
                    I64x2Splat => self.simd(0x12),
                    F32x4Splat => self.simd(0x13),
                    F64x2Splat => self.simd(0x14),
                    I8x16ExtractLaneS { idx } => {
                        self.simd(0x15);
                        self.encoder.byte(idx);
                    }
                    I8x16ExtractLaneU { idx } => {
                        self.simd(0x16);
                        self.encoder.byte(idx);
                    }
                    I16x8ExtractLaneS { idx } => {
                        self.simd(0x18);
                        self.encoder.byte(idx);
                    }
                    I16x8ExtractLaneU { idx } => {
                        self.simd(0x19);
                        self.encoder.byte(idx);
                    }
                    I32x4ExtractLane { idx } => {
                        self.simd(0x1b);
                        self.encoder.byte(idx);
                    }
                    I64x2ExtractLane { idx } => {
                        self.simd(0x1d);
                        self.encoder.byte(idx);
                    }
                    F32x4ExtractLane { idx } => {
                        self.simd(0x1f);
                        self.encoder.byte(idx);
                    }
                    F64x2ExtractLane { idx } => {
                        self.simd(0x21);
                        self.encoder.byte(idx);
                    }

                    V128Not => self.simd(0x4d),

                    I8x16Abs => self.simd(0x60),
                    I8x16Neg => self.simd(0x61),
                    I8x16AnyTrue => self.simd(0x62),
                    I8x16AllTrue => self.simd(0x63),
                    I8x16Bitmask => self.simd(0x64),

                    I16x8Abs => self.simd(0x80),
                    I16x8Neg => self.simd(0x81),
                    I16x8AnyTrue => self.simd(0x82),
                    I16x8AllTrue => self.simd(0x83),
                    I16x8Bitmask => self.simd(0x84),
                    I16x8WidenLowI8x16S => self.simd(0x87),
                    I16x8WidenHighI8x16S => self.simd(0x88),
                    I16x8WidenLowI8x16U => self.simd(0x89),
                    I16x8WidenHighI8x16U => self.simd(0x8a),

                    I32x4Abs => self.simd(0xa0),
                    I32x4Neg => self.simd(0xa1),
                    I32x4AnyTrue => self.simd(0xa2),
                    I32x4AllTrue => self.simd(0xa3),
                    I32x4Bitmask => self.simd(0xa4),
                    I32x4WidenLowI16x8S => self.simd(0xa7),
                    I32x4WidenHighI16x8S => self.simd(0xa8),
                    I32x4WidenLowI16x8U => self.simd(0xa9),
                    I32x4WidenHighI16x8U => self.simd(0xaa),

                    I64x2Neg => self.simd(0xc1),

                    F32x4Abs => self.simd(0xe0),
                    F32x4Neg => self.simd(0xe1),
                    F32x4Sqrt => self.simd(0xe3),

                    F64x2Abs => self.simd(0xec),
                    F64x2Neg => self.simd(0xed),
                    F64x2Sqrt => self.simd(0xef),

                    I32x4TruncSatF32x4S => self.simd(0xf8),
                    I32x4TruncSatF32x4U => self.simd(0xf9),
                    F32x4ConvertI32x4S => self.simd(0xfa),
                    F32x4ConvertI32x4U => self.simd(0xfb),

                    I32TruncSSatF32 => self.encoder.raw(&[0xfc, 0x00]),
                    I32TruncUSatF32 => self.encoder.raw(&[0xfc, 0x01]),
                    I32TruncSSatF64 => self.encoder.raw(&[0xfc, 0x02]),
                    I32TruncUSatF64 => self.encoder.raw(&[0xfc, 0x03]),
                    I64TruncSSatF32 => self.encoder.raw(&[0xfc, 0x04]),
                    I64TruncUSatF32 => self.encoder.raw(&[0xfc, 0x05]),
                    I64TruncSSatF64 => self.encoder.raw(&[0xfc, 0x06]),
                    I64TruncUSatF64 => self.encoder.raw(&[0xfc, 0x07]),
                }
            }

            Select(e) => {
                match e.ty {
                    Some(ty) => {
                        self.encoder.byte(0x1c);
                        self.encoder.byte(0x01);
                        ty.emit(self.encoder);
                    }
                    None => {
                        self.encoder.byte(0x1b); // select
                    }
                }
            }

            Unreachable(_) => {
                self.encoder.byte(0x00); // unreachable
            }

            Br(e) => {
                let target = self.branch_target(e.block);
                self.encoder.byte(0x0c); // br
                self.encoder.u32(target);
            }

            BrIf(e) => {
                let target = self.branch_target(e.block);
                self.encoder.byte(0x0d); // br_if
                self.encoder.u32(target);
            }

            Call(e) => {
                let idx = self.indices.get_func_index(e.func);
                self.encoder.byte(0x10); // call
                self.encoder.u32(idx);
            }

            CallIndirect(e) => {
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
                let idx = self.local_indices[&e.local];
                self.encoder.byte(0x21); // local.set
                self.encoder.u32(idx);
            }

            LocalTee(e) => {
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
                let idx = self.indices.get_global_index(e.global);
                self.encoder.byte(0x24); // global.set
                self.encoder.u32(idx);
            }

            Load(e) => {
                use crate::ir::ExtendedLoad::*;
                use crate::ir::LoadKind::*;
                match e.kind {
                    I32 { atomic: false } => self.encoder.byte(0x28), // i32.load
                    I32 { atomic: true } => self.encoder.raw(&[0xfe, 0x10]), // i32.atomic.load
                    I64 { atomic: false } => self.encoder.byte(0x29), // i64.load
                    I64 { atomic: true } => self.encoder.raw(&[0xfe, 0x11]), // i64.atomic.load
                    F32 => self.encoder.byte(0x2a),                   // f32.load
                    F64 => self.encoder.byte(0x2b),                   // f64.load
                    V128 => self.simd(0x00),
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
                use crate::ir::StoreKind::*;
                match e.kind {
                    I32 { atomic: false } => self.encoder.byte(0x36), // i32.store
                    I32 { atomic: true } => self.encoder.raw(&[0xfe, 0x17]), // i32.atomic.store
                    I64 { atomic: false } => self.encoder.byte(0x37), // i64.store
                    I64 { atomic: true } => self.encoder.raw(&[0xfe, 0x18]), // i64.atomic.store
                    F32 => self.encoder.byte(0x38),                   // f32.store
                    F64 => self.encoder.byte(0x39),                   // f64.store
                    V128 => self.simd(0x0b),                          // v128.store
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
                use crate::ir::AtomicOp::*;
                use crate::ir::AtomicWidth::*;

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
                use crate::ir::AtomicWidth::*;

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
                self.encoder.byte(0xfe);
                self.encoder.byte(0x00);
                self.memarg(e.memory, &e.arg);
            }

            AtomicWait(e) => {
                self.encoder.byte(0xfe);
                self.encoder.byte(if e.sixty_four { 0x02 } else { 0x01 });
                self.memarg(e.memory, &e.arg);
            }

            AtomicFence(_e) => {
                self.encoder.byte(0xfe);
                self.encoder.byte(0x03);
                self.encoder.byte(0x00);
            }

            TableGet(e) => {
                self.encoder.byte(0x25);
                let idx = self.indices.get_table_index(e.table);
                self.encoder.u32(idx);
            }
            TableSet(e) => {
                self.encoder.byte(0x26);
                let idx = self.indices.get_table_index(e.table);
                self.encoder.u32(idx);
            }
            TableGrow(e) => {
                self.encoder.raw(&[0xfc, 0x0f]);
                let idx = self.indices.get_table_index(e.table);
                self.encoder.u32(idx);
            }
            TableSize(e) => {
                self.encoder.raw(&[0xfc, 0x10]);
                let idx = self.indices.get_table_index(e.table);
                self.encoder.u32(idx);
            }
            TableFill(e) => {
                self.encoder.raw(&[0xfc, 0x11]);
                let idx = self.indices.get_table_index(e.table);
                self.encoder.u32(idx);
            }
            RefNull(e) => {
                self.encoder.byte(0xd0);
                e.ty.emit(self.encoder);
            }
            RefIsNull(_e) => {
                self.encoder.byte(0xd1);
            }
            RefFunc(e) => {
                self.encoder.byte(0xd2);
                let idx = self.indices.get_func_index(e.func);
                self.encoder.u32(idx);
            }

            V128Bitselect(_) => {
                self.simd(0x52);
            }
            V128Shuffle(e) => {
                self.simd(0x0d);
                self.encoder.raw(&e.indices);
            }
            V128Swizzle(_) => {
                self.simd(0x0e);
            }
            LoadSimd(e) => {
                match e.kind {
                    LoadSimdKind::I16x8Load8x8S => self.simd(0x01),
                    LoadSimdKind::I16x8Load8x8U => self.simd(0x02),
                    LoadSimdKind::I32x4Load16x4S => self.simd(0x03),
                    LoadSimdKind::I32x4Load16x4U => self.simd(0x04),
                    LoadSimdKind::I64x2Load32x2S => self.simd(0x05),
                    LoadSimdKind::I64x2Load32x2U => self.simd(0x06),
                    LoadSimdKind::Splat8 => self.simd(0x07),
                    LoadSimdKind::Splat16 => self.simd(0x08),
                    LoadSimdKind::Splat32 => self.simd(0x09),
                    LoadSimdKind::Splat64 => self.simd(0x0a),
                }
                self.memarg(e.memory, &e.arg);
            }
            TableInit(e) => {
                self.encoder.raw(&[0xfc, 0x0c]);
                self.encoder.u32(self.indices.get_element_index(e.elem));
                self.encoder.u32(self.indices.get_table_index(e.table));
            }
            TableCopy(e) => {
                self.encoder.raw(&[0xfc, 0x0e]);
                self.encoder.u32(self.indices.get_table_index(e.dst));
                self.encoder.u32(self.indices.get_table_index(e.src));
            }
            ElemDrop(e) => {
                self.encoder.raw(&[0xfc, 0x0d]);
                self.encoder.u32(self.indices.get_element_index(e.elem));
            }
        }
    }
}

impl Emit<'_, '_> {
    fn branch_target(&self, block: InstrSeqId) -> u32 {
        self.blocks.iter().rev().position(|b| *b == block).expect(
            "attempt to branch to invalid block; bad transformation pass introduced bad branching?",
        ) as u32
    }

    fn block_type(&mut self, ty: InstrSeqType) {
        match ty {
            InstrSeqType::Simple(None) => self.encoder.byte(0x40),
            InstrSeqType::Simple(Some(ty)) => ty.emit(self.encoder),
            InstrSeqType::MultiValue(ty) => {
                let index = self.indices.get_type_index(ty);
                assert!(index < std::i32::MAX as u32);
                self.encoder.i32(index as i32);
            }
        }
    }

    fn memarg(&mut self, id: MemoryId, arg: &MemArg) {
        assert_eq!(self.indices.get_memory_index(id), 0);
        self.encoder.u32(arg.align.trailing_zeros());
        self.encoder.u32(arg.offset);
    }

    fn simd(&mut self, opcode: u32) {
        self.encoder.byte(0xfd);
        self.encoder.u32(opcode);
    }
}
