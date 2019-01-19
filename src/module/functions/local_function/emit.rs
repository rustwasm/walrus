use crate::emit::IdsToIndices;
use crate::ir::*;
use crate::module::functions::LocalFunction;
use parity_wasm::elements;

pub(crate) fn run(func: &LocalFunction, indices: &IdsToIndices) -> Vec<elements::Instruction> {
    let mut v = Emit {
        func,
        indices,
        id: func.entry_block().into(),
        blocks: vec![],
        instructions: vec![],
    };
    v.visit(func.entry_block());
    v.instructions
}

struct Emit<'a> {
    // The function we are visiting.
    func: &'a LocalFunction,

    // The id of the current expression.
    id: ExprId,

    // Needed so we can map locals to their indices.
    indices: &'a IdsToIndices,

    // Stack of blocks that we are currently emitting instructions for. A branch
    // is only valid if its target is one of these blocks.
    blocks: Vec<BlockId>,

    // The instruction sequence we are building up to emit.
    instructions: Vec<elements::Instruction>,
}

impl Emit<'_> {
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
            Const(e) => self.visit_const(e),
            Block(e) => self.visit_block(e),
            BrTable(e) => self.visit_br_table(e),
            IfElse(e) => self.visit_if_else(e),

            Drop(e) => {
                self.visit(e.expr);
                self.emit(elements::Instruction::Drop)
            }

            Return(e) => {
                for x in e.values.iter() {
                    self.visit(*x);
                }
                self.emit(elements::Instruction::Return);
            }

            MemorySize(e) => {
                let idx = self.indices.get_memory_index(e.memory);
                // TODO: should upstream a fix to parity-wasm to accept 32-bit
                // indices for memories.
                assert!(idx < 256);
                self.emit(elements::Instruction::CurrentMemory(idx as u8))
            }

            Binop(e) => {
                use BinaryOp::*;

                self.visit(e.lhs);
                self.visit(e.rhs);
                self.emit(match e.op {
                    I32Eq => elements::Instruction::I32Eq,
                    I32Ne => elements::Instruction::I32Ne,
                    I32LtS => elements::Instruction::I32LtS,
                    I32LtU => elements::Instruction::I32LtU,
                    I32GtS => elements::Instruction::I32GtS,
                    I32GtU => elements::Instruction::I32GtU,
                    I32LeS => elements::Instruction::I32LeS,
                    I32LeU => elements::Instruction::I32LeU,
                    I32GeS => elements::Instruction::I32GeS,
                    I32GeU => elements::Instruction::I32GeU,

                    I64Eq => elements::Instruction::I64Eq,
                    I64Ne => elements::Instruction::I64Ne,
                    I64LtS => elements::Instruction::I64LtS,
                    I64LtU => elements::Instruction::I64LtU,
                    I64GtS => elements::Instruction::I64GtS,
                    I64GtU => elements::Instruction::I64GtU,
                    I64LeS => elements::Instruction::I64LeS,
                    I64LeU => elements::Instruction::I64LeU,
                    I64GeS => elements::Instruction::I64GeS,
                    I64GeU => elements::Instruction::I64GeU,

                    F32Eq => elements::Instruction::F32Eq,
                    F32Ne => elements::Instruction::F32Ne,
                    F32Lt => elements::Instruction::F32Lt,
                    F32Gt => elements::Instruction::F32Gt,
                    F32Le => elements::Instruction::F32Le,
                    F32Ge => elements::Instruction::F32Ge,

                    F64Eq => elements::Instruction::F64Eq,
                    F64Ne => elements::Instruction::F64Ne,
                    F64Lt => elements::Instruction::F64Lt,
                    F64Gt => elements::Instruction::F64Gt,
                    F64Le => elements::Instruction::F64Le,
                    F64Ge => elements::Instruction::F64Ge,

                    I32Add => elements::Instruction::I32Add,
                    I32Sub => elements::Instruction::I32Sub,
                    I32Mul => elements::Instruction::I32Mul,
                    I32DivS => elements::Instruction::I32DivS,
                    I32DivU => elements::Instruction::I32DivU,
                    I32RemS => elements::Instruction::I32RemS,
                    I32RemU => elements::Instruction::I32RemU,
                    I32And => elements::Instruction::I32And,
                    I32Or => elements::Instruction::I32Or,
                    I32Xor => elements::Instruction::I32Xor,
                    I32Shl => elements::Instruction::I32Shl,
                    I32ShrS => elements::Instruction::I32ShrS,
                    I32ShrU => elements::Instruction::I32ShrU,
                    I32Rotl => elements::Instruction::I32Rotl,
                    I32Rotr => elements::Instruction::I32Rotr,

                    I64Add => elements::Instruction::I64Add,
                    I64Sub => elements::Instruction::I64Sub,
                    I64Mul => elements::Instruction::I64Mul,
                    I64DivS => elements::Instruction::I64DivS,
                    I64DivU => elements::Instruction::I64DivU,
                    I64RemS => elements::Instruction::I64RemS,
                    I64RemU => elements::Instruction::I64RemU,
                    I64And => elements::Instruction::I64And,
                    I64Or => elements::Instruction::I64Or,
                    I64Xor => elements::Instruction::I64Xor,
                    I64Shl => elements::Instruction::I64Shl,
                    I64ShrS => elements::Instruction::I64ShrS,
                    I64ShrU => elements::Instruction::I64ShrU,
                    I64Rotl => elements::Instruction::I64Rotl,
                    I64Rotr => elements::Instruction::I64Rotr,

                    F32Add => elements::Instruction::F32Add,
                    F32Sub => elements::Instruction::F32Sub,
                    F32Mul => elements::Instruction::F32Mul,
                    F32Div => elements::Instruction::F32Div,
                    F32Min => elements::Instruction::F32Min,
                    F32Max => elements::Instruction::F32Max,
                    F32Copysign => elements::Instruction::F32Copysign,

                    F64Add => elements::Instruction::F64Add,
                    F64Sub => elements::Instruction::F64Sub,
                    F64Mul => elements::Instruction::F64Mul,
                    F64Div => elements::Instruction::F64Div,
                    F64Min => elements::Instruction::F64Min,
                    F64Max => elements::Instruction::F64Max,
                    F64Copysign => elements::Instruction::F64Copysign,
                })
            }

            Unop(e) => {
                use UnaryOp::*;

                self.visit(e.expr);
                self.emit(match e.op {
                    I32Eqz => elements::Instruction::I32Eqz,
                    I32Clz => elements::Instruction::I32Clz,
                    I32Ctz => elements::Instruction::I32Ctz,
                    I32Popcnt => elements::Instruction::I32Popcnt,

                    I64Eqz => elements::Instruction::I64Eqz,
                    I64Clz => elements::Instruction::I64Clz,
                    I64Ctz => elements::Instruction::I64Ctz,
                    I64Popcnt => elements::Instruction::I64Popcnt,

                    F32Abs => elements::Instruction::F32Abs,
                    F32Neg => elements::Instruction::F32Neg,
                    F32Ceil => elements::Instruction::F32Ceil,
                    F32Floor => elements::Instruction::F32Floor,
                    F32Trunc => elements::Instruction::F32Trunc,
                    F32Nearest => elements::Instruction::F32Nearest,
                    F32Sqrt => elements::Instruction::F32Sqrt,

                    F64Abs => elements::Instruction::F64Abs,
                    F64Neg => elements::Instruction::F64Neg,
                    F64Ceil => elements::Instruction::F64Ceil,
                    F64Floor => elements::Instruction::F64Floor,
                    F64Trunc => elements::Instruction::F64Trunc,
                    F64Nearest => elements::Instruction::F64Nearest,
                    F64Sqrt => elements::Instruction::F64Sqrt,

                    I32WrapI64 => elements::Instruction::I32WrapI64,
                    I32TruncSF32 => elements::Instruction::I32TruncSF32,
                    I32TruncUF32 => elements::Instruction::I32TruncUF32,
                    I32TruncSF64 => elements::Instruction::I32TruncSF64,
                    I32TruncUF64 => elements::Instruction::I32TruncUF64,
                    I64ExtendSI32 => elements::Instruction::I64ExtendSI32,
                    I64ExtendUI32 => elements::Instruction::I64ExtendUI32,
                    I64TruncSF32 => elements::Instruction::I64TruncSF32,
                    I64TruncUF32 => elements::Instruction::I64TruncUF32,
                    I64TruncSF64 => elements::Instruction::I64TruncSF64,
                    I64TruncUF64 => elements::Instruction::I64TruncUF64,

                    F32ConvertSI32 => elements::Instruction::F32ConvertSI32,
                    F32ConvertUI32 => elements::Instruction::F32ConvertUI32,
                    F32ConvertSI64 => elements::Instruction::F32ConvertSI64,
                    F32ConvertUI64 => elements::Instruction::F32ConvertUI64,
                    F32DemoteF64 => elements::Instruction::F32DemoteF64,
                    F64ConvertSI32 => elements::Instruction::F64ConvertSI32,
                    F64ConvertUI32 => elements::Instruction::F64ConvertUI32,
                    F64ConvertSI64 => elements::Instruction::F64ConvertSI64,
                    F64ConvertUI64 => elements::Instruction::F64ConvertUI64,
                    F64PromoteF32 => elements::Instruction::F64PromoteF32,

                    I32ReinterpretF32 => elements::Instruction::I32ReinterpretF32,
                    I64ReinterpretF64 => elements::Instruction::I64ReinterpretF64,
                    F32ReinterpretI32 => elements::Instruction::F32ReinterpretI32,
                    F64ReinterpretI64 => elements::Instruction::F64ReinterpretI64,
                })
            }

            Select(e) => {
                self.visit(e.consequent);
                self.visit(e.alternative);
                self.visit(e.condition);
                self.emit(elements::Instruction::Select)
            }

            Unreachable(_) => {
                self.emit(elements::Instruction::Unreachable);
            }

            Br(e) => {
                for x in e.args.iter() {
                    self.visit(*x);
                }
                let target = self.branch_target(e.block);
                self.emit(elements::Instruction::Br(target))
            }

            BrIf(e) => {
                for x in e.args.iter() {
                    self.visit(*x);
                }
                self.visit(e.condition);
                let target = self.branch_target(e.block);
                self.emit(elements::Instruction::BrIf(target))
            }

            Call(e) => {
                for x in e.args.iter() {
                    self.visit(*x);
                }
                let idx = self.indices.get_func_index(e.func);
                self.emit(elements::Instruction::Call(idx))
            }

            CallIndirect(e) => {
                for x in e.args.iter() {
                    self.visit(*x);
                }
                self.visit(e.func);
                let idx = self.indices.get_type_index(e.ty);
                let table = self.indices.get_table_index(e.table);
                assert!(table < 256); // TODO: update parity-wasm to accept u32
                self.emit(elements::Instruction::CallIndirect(idx, table as u8))
            }

            LocalGet(e) => {
                let idx = self.indices.get_local_index(e.local);
                self.emit(elements::Instruction::GetLocal(idx))
            }

            LocalSet(e) => {
                self.visit(e.value);
                let idx = self.indices.get_local_index(e.local);
                self.emit(elements::Instruction::SetLocal(idx))
            }

            GlobalGet(e) => {
                let idx = self.indices.get_global_index(e.global);
                self.emit(elements::Instruction::GetGlobal(idx))
            }

            GlobalSet(e) => {
                self.visit(e.value);
                let idx = self.indices.get_global_index(e.global);
                self.emit(elements::Instruction::SetGlobal(idx))
            }
        }

        self.id = old;
    }

    fn emit(&mut self, i: elements::Instruction) {
        self.instructions.push(i);
    }

    fn branch_target(&self, block: BlockId) -> u32 {
        self.blocks.iter().rev().position(|b| *b == block).expect(
            "attempt to branch to invalid block; bad transformation pass introduced bad branching?",
        ) as u32
    }

    fn visit_block(&mut self, e: &Block) {
        self.blocks.push(Block::new_id(self.id));

        let block_ty = match e.results.len() {
            0 => elements::BlockType::NoResult,
            1 => elements::BlockType::Value(e.results[0].into()),
            _ => panic!(
                "multiple return values not supported yet; write a transformation to rewrite them"
            ),
        };

        match e.kind {
            BlockKind::Block => self.emit(elements::Instruction::Block(block_ty)),
            BlockKind::Loop => self.emit(elements::Instruction::Loop(block_ty)),
            BlockKind::FunctionEntry | BlockKind::IfElse => {}
        }

        for x in &e.exprs {
            self.visit(*x);
        }

        match e.kind {
            BlockKind::Block | BlockKind::Loop | BlockKind::FunctionEntry => {
                self.emit(elements::Instruction::End)
            }
            BlockKind::IfElse => {}
        }

        self.blocks.pop();
    }

    fn visit_const(&mut self, e: &Const) {
        self.emit(match e.value {
            Value::I32(i) => elements::Instruction::I32Const(i),
            Value::I64(i) => elements::Instruction::I64Const(i),
            Value::F32(i) => elements::Instruction::F32Const(i.to_bits()),
            Value::F64(i) => elements::Instruction::F64Const(i.to_bits()),
            Value::V128(i) => elements::Instruction::V128Const(Box::new([
                (i >> 0) as u8,
                (i >> 8) as u8,
                (i >> 16) as u8,
                (i >> 24) as u8,
                (i >> 32) as u8,
                (i >> 40) as u8,
                (i >> 48) as u8,
                (i >> 56) as u8,
                (i >> 64) as u8,
                (i >> 72) as u8,
                (i >> 80) as u8,
                (i >> 88) as u8,
                (i >> 96) as u8,
                (i >> 104) as u8,
                (i >> 112) as u8,
                (i >> 120) as u8,
            ])),
        })
    }

    fn visit_if_else(&mut self, e: &IfElse) {
        let block_ty = {
            let consequent = self.func.block(e.consequent);
            match consequent.results.len() {
                0 => elements::BlockType::NoResult,
                1 => elements::BlockType::Value(consequent.results[0].into()),
                _ => panic!(
                    "multiple return values not yet supported; write a transformation to \
                     rewrite them into single value returns"
                ),
            }
        };

        self.visit(e.condition);
        self.emit(elements::Instruction::If(block_ty));
        let _ = self.visit(e.consequent);
        self.emit(elements::Instruction::Else);
        let _ = self.visit(e.alternative);
        self.emit(elements::Instruction::End)
    }

    fn visit_br_table(&mut self, e: &BrTable) {
        for x in e.args.iter() {
            self.visit(*x);
        }
        self.visit(e.which);
        let table = e
            .blocks
            .iter()
            .map(|b| self.branch_target(*b))
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let default = self.branch_target(e.default);
        self.emit(elements::Instruction::BrTable(Box::new(
            elements::BrTableData { table, default },
        )))
    }
}
