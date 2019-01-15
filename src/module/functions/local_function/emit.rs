use crate::module::functions::LocalFunction;
use crate::ir::*;
use crate::module::emit::IdsToIndices;
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

            I32Add(e) => {
                self.visit(e.lhs);
                self.visit(e.rhs);
                self.emit(elements::Instruction::I32Add)
            }

            I32Sub(e) => {
                self.visit(e.lhs);
                self.visit(e.rhs);
                self.emit(elements::Instruction::I32Sub)
            }

            I32Mul(e) => {
                self.visit(e.lhs);
                self.visit(e.rhs);
                self.emit(elements::Instruction::I32Mul)
            }

            I32Eqz(e) => {
                self.visit(e.expr);
                self.emit(elements::Instruction::I32Eqz)
            }

            I32Popcnt(e) => {
                self.visit(e.expr);
                self.emit(elements::Instruction::I32Popcnt)
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
        self.blocks
            .iter()
            .rev()
            .skip(1)
            .position(|b| *b == block)
            .expect(
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
