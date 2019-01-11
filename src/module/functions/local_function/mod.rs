//! Functions defined locally within a wasm module.

mod context;
pub mod display;

use self::context::FunctionContext;
use super::{FunctionId};
use crate::ir::matcher::{I32ConstMatcher, Matcher};
use crate::dot::Dot;
use crate::error::{ErrorKind, Result};
use crate::ir::*;
use crate::module::Module;
use crate::module::emit::IdsToIndices;
use crate::ty::{TypeId, ValType};
use crate::validation_context::ValidationContext;
use failure::{Fail, ResultExt};
use id_arena::Arena;
use parity_wasm::elements::{self, Instruction};
use std::collections::{BTreeSet, HashSet};
use std::fmt;
use std::io::{self, Write};
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

        let mut ctx = FunctionContext::new(
            module,
            id,
            &mut func,
            &validation,
            operands,
            controls,
        );

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
        }

        impl SizeVisitor<'_> {
            fn visit<E>(&mut self, e: E) -> u64
            where
                E: Into<ExprId>,
            {
                self.func.exprs[e.into()].visit(self)
            }
        }

        impl Visitor for SizeVisitor<'_> {
            type Return = u64;

            fn visit_block(&mut self, e: &Block) -> u64 {
                1 + e.exprs.iter().map(|e| self.visit(*e)).sum::<u64>()
            }

            fn visit_call(&mut self, e: &Call) -> u64 {
                1 + e.args.iter().map(|e| self.visit(*e)).sum::<u64>()
            }

            fn visit_local_get(&mut self, _: &LocalGet) -> u64 {
                1
            }

            fn visit_local_set(&mut self, e: &LocalSet) -> u64 {
                1 + self.visit(e.value)
            }

            fn visit_i32_const(&mut self, _: &I32Const) -> u64 {
                1
            }

            fn visit_i32_add(&mut self, e: &I32Add) -> u64 {
                1 + self.visit(e.lhs) + self.visit(e.rhs)
            }

            fn visit_i32_sub(&mut self, e: &I32Sub) -> u64 {
                1 + self.visit(e.lhs) + self.visit(e.rhs)
            }

            fn visit_i32_mul(&mut self, e: &I32Mul) -> u64 {
                1 + self.visit(e.lhs) + self.visit(e.rhs)
            }

            fn visit_i32_eqz(&mut self, e: &I32Eqz) -> u64 {
                1 + self.visit(e.expr)
            }

            fn visit_i32_popcnt(&mut self, e: &I32Popcnt) -> u64 {
                1 + self.visit(e.expr)
            }

            fn visit_select(&mut self, e: &Select) -> u64 {
                1 + self.visit(e.condition) + self.visit(e.consequent) + self.visit(e.alternative)
            }

            fn visit_unreachable(&mut self, _: &Unreachable) -> u64 {
                1
            }

            fn visit_br(&mut self, e: &Br) -> u64 {
                1 + e.args.iter().map(|e| self.visit(*e)).sum::<u64>()
            }

            fn visit_br_if(&mut self, e: &BrIf) -> u64 {
                1 + e.args.iter().map(|e| self.visit(*e)).sum::<u64>()
            }

            fn visit_if_else(&mut self, e: &IfElse) -> u64 {
                1 + self.visit(e.condition) + self.visit(e.consequent) + self.visit(e.alternative)
            }

            fn visit_br_table(&mut self, e: &BrTable) -> u64 {
                1 + self.visit(e.which) + e.args.iter().map(|e| self.visit(*e)).sum::<u64>()
            }

            fn visit_drop(&mut self, e: &Drop) -> u64 {
                1 + self.visit(e.expr)
            }

            fn visit_return(&mut self, e: &Return) -> u64 {
                1 + e.values.iter().map(|e| self.visit(*e)).sum::<u64>()
            }

            fn visit_memory_size(&mut self, _: &MemorySize) -> u64 {
                1
            }
        }

        let v = &mut SizeVisitor { func: self };
        v.visit(self.entry_block())
    }

    /// Is this function's body a [constant
    /// expression](https://webassembly.github.io/spec/core/valid/instructions.html#constant-expressions)?
    pub fn is_const(&self) -> bool {
        let entry = match &self.exprs[self.entry_block().into()] {
            Expr::Block(b) => b,
            _ => unreachable!(),
        };
        let matcher = I32ConstMatcher::new();
        entry.exprs.iter().all(|e| {
            matcher.is_match(self, &self.exprs[*e])
        })
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

        impl Visitor for LocalsVisitor<'_> {
            type Return = ();

            fn visit_block(&mut self, e: &Block) {
                for x in e.exprs.iter() {
                    self.visit(*x);
                }
            }

            fn visit_call(&mut self, e: &Call) {
                for x in e.args.iter() {
                    self.visit(*x);
                }
            }

            fn visit_local_get(&mut self, e: &LocalGet) {
                self.insert_local(e.ty, e.local);
            }

            fn visit_local_set(&mut self, e: &LocalSet) {
                self.insert_local(e.ty, e.local);
            }

            fn visit_i32_add(&mut self, e: &I32Add) {
                self.visit(e.lhs);
                self.visit(e.rhs);
            }

            fn visit_i32_sub(&mut self, e: &I32Sub) {
                self.visit(e.lhs);
                self.visit(e.rhs);
            }

            fn visit_i32_mul(&mut self, e: &I32Mul) {
                self.visit(e.lhs);
                self.visit(e.rhs);
            }

            fn visit_i32_eqz(&mut self, e: &I32Eqz) {
                self.visit(e.expr);
            }

            fn visit_i32_popcnt(&mut self, e: &I32Popcnt) {
                self.visit(e.expr);
            }

            fn visit_select(&mut self, e: &Select) {
                self.visit(e.condition);
                self.visit(e.consequent);
                self.visit(e.alternative);
            }

            fn visit_br(&mut self, e: &Br) {
                for x in e.args.iter() {
                    self.visit(*x);
                }
            }

            fn visit_br_if(&mut self, e: &BrIf) {
                for x in e.args.iter() {
                    self.visit(*x);
                }
                self.visit(e.condition);
            }

            fn visit_if_else(&mut self, e: &IfElse) {
                self.visit(e.condition);
                self.visit(e.consequent);
                self.visit(e.alternative);
            }

            fn visit_br_table(&mut self, e: &BrTable) {
                self.visit(e.which);
                self.visit(e.default);
                for x in e.args.iter() {
                    self.visit(*x);
                }
            }

            fn visit_drop(&mut self, e: &Drop) {
                self.visit(e.expr)
            }

            fn visit_return(&mut self, e: &Return) {
                for x in e.values.iter() {
                    self.visit(*x);
                }
            }

            fn visit_i32_const(&mut self, _: &I32Const) {}
            fn visit_unreachable(&mut self, _: &Unreachable) {}
            fn visit_memory_size(&mut self, _: &MemorySize) {}
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
        struct EmitVisitor<'a> {
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

        impl EmitVisitor<'_> {
            fn visit<E>(&mut self, e: E) -> <Self as Visitor>::Return
            where
                E: Into<ExprId>,
            {
                let id = e.into();
                let old = self.id;
                self.id = id;
                let ret = self.func.exprs[id].visit(self);
                self.id = old;
                ret
            }

            fn emit(&mut self, i: elements::Instruction) -> <Self as Visitor>::Return {
                self.instructions.push(i);
                Ok(())
            }

            fn branch_target(&self, block: BlockId) -> u32 {
                println!("FITZGEN: looking for  = {:?}", block);
                println!("FITZGEN: blocks = {:?}", self.blocks);
                self.blocks
            .iter()
            .rev()
            .skip(1)
            .position(|b| *b == block)
            .expect(
            "attempt to branch to invalid block; bad transformation pass introduced bad branching?",
        ) as u32
            }
        }

        impl Visitor for EmitVisitor<'_> {
            // Returns whether code has become unreachable in this block or not. If
            // `Err`, then any following code (within the current block) is unreachable,
            // and we can skip emitting instructions until the end of the block. Using
            // `Result` allows us to use `?` for propagation and early exits.
            type Return = ::std::result::Result<(), ()>;

            fn visit_block(&mut self, e: &Block) -> Self::Return {
                self.blocks.push(Block::new_id(self.id));

                let block_ty = match e.results.len() {
                    0 => elements::BlockType::NoResult,
                    1 => elements::BlockType::Value(e.results[0].into()),
                    _ => panic!(
                "multiple return values not supported yet; write a transformation to rewrite them"
            ),
                };

                match e.kind {
                    BlockKind::Block => self.emit(elements::Instruction::Block(block_ty))?,
                    BlockKind::Loop => self.emit(elements::Instruction::Loop(block_ty))?,
                    BlockKind::FunctionEntry | BlockKind::IfElse => {}
                }

                for x in &e.exprs {
                    if self.visit(*x).is_err() {
                        break;
                    }
                }

                match e.kind {
                    BlockKind::Block | BlockKind::Loop | BlockKind::FunctionEntry => {
                        self.emit(elements::Instruction::End)?
                    }
                    BlockKind::IfElse => {}
                }

                self.blocks.pop();
                Ok(())
            }

            fn visit_call(&mut self, e: &Call) -> Self::Return {
                for x in e.args.iter() {
                    self.visit(*x)?;
                }
                let idx = self.indices.get_func_index(e.func);
                self.emit(elements::Instruction::Call(idx))
            }

            fn visit_local_get(&mut self, e: &LocalGet) -> Self::Return {
                let idx = self.indices.get_local_index(e.local);
                self.emit(elements::Instruction::GetLocal(idx))
            }

            fn visit_local_set(&mut self, e: &LocalSet) -> Self::Return {
                self.visit(e.value)?;
                let idx = self.indices.get_local_index(e.local);
                self.emit(elements::Instruction::SetLocal(idx))
            }

            fn visit_i32_const(&mut self, e: &I32Const) -> Self::Return {
                self.emit(elements::Instruction::I32Const(e.value))
            }

            fn visit_i32_add(&mut self, e: &I32Add) -> Self::Return {
                self.visit(e.lhs)?;
                self.visit(e.rhs)?;
                self.emit(elements::Instruction::I32Add)
            }

            fn visit_i32_sub(&mut self, e: &I32Sub) -> Self::Return {
                self.visit(e.lhs)?;
                self.visit(e.rhs)?;
                self.emit(elements::Instruction::I32Sub)
            }

            fn visit_i32_mul(&mut self, e: &I32Mul) -> Self::Return {
                self.visit(e.lhs)?;
                self.visit(e.rhs)?;
                self.emit(elements::Instruction::I32Mul)
            }

            fn visit_i32_eqz(&mut self, e: &I32Eqz) -> Self::Return {
                self.visit(e.expr)?;
                self.emit(elements::Instruction::I32Eqz)
            }

            fn visit_i32_popcnt(&mut self, e: &I32Popcnt) -> Self::Return {
                self.visit(e.expr)?;
                self.emit(elements::Instruction::I32Popcnt)
            }

            fn visit_select(&mut self, e: &Select) -> Self::Return {
                self.visit(e.consequent)?;
                self.visit(e.alternative)?;
                self.visit(e.condition)?;
                self.emit(elements::Instruction::Select)
            }

            fn visit_unreachable(&mut self, _: &Unreachable) -> Self::Return {
                self.emit(elements::Instruction::Unreachable)?;
                Err(())
            }

            fn visit_br(&mut self, e: &Br) -> Self::Return {
                for x in e.args.iter() {
                    self.visit(*x)?;
                }
                let target = self.branch_target(e.block);
                self.emit(elements::Instruction::Br(target))
            }

            fn visit_br_if(&mut self, e: &BrIf) -> Self::Return {
                for x in e.args.iter() {
                    self.visit(*x)?;
                }
                self.visit(e.condition)?;
                let target = self.branch_target(e.block);
                self.emit(elements::Instruction::BrIf(target))
            }

            fn visit_if_else(&mut self, e: &IfElse) -> Self::Return {
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

                self.visit(e.condition)?;
                self.emit(elements::Instruction::If(block_ty))?;
                let _ = self.visit(e.consequent);
                self.emit(elements::Instruction::Else)?;
                let _ = self.visit(e.alternative);
                self.emit(elements::Instruction::End)
            }

            fn visit_br_table(&mut self, e: &BrTable) -> Self::Return {
                for x in e.args.iter() {
                    self.visit(*x)?;
                }
                self.visit(e.which)?;
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

            fn visit_drop(&mut self, e: &Drop) -> Self::Return {
                self.visit(e.expr)?;
                self.emit(elements::Instruction::Drop)
            }

            fn visit_return(&mut self, e: &Return) -> Self::Return {
                for x in e.values.iter() {
                    self.visit(*x)?;
                }
                self.emit(elements::Instruction::Return)?;
                Err(())
            }

            fn visit_memory_size(&mut self, e: &MemorySize) -> Self::Return {
                self.emit(elements::Instruction::CurrentMemory(e.memory.index() as u8))
            }
        }

        let mut v = EmitVisitor {
            func: self,
            indices,
            id: self.entry_block().into(),
            blocks: vec![],
            instructions: vec![],
        };
        let _ = v.visit(self.entry_block());
        v.instructions
    }
}

impl fmt::Display for LocalFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::display::DisplayIr;
        self.display_ir(f, &(), 0)
    }
}

impl Dot for LocalFunction {
    fn dot(&self, out: &mut Write) -> io::Result<()> {
        struct DotVisitor<'a, 'b> {
            out: &'a mut Write,
            func: &'b LocalFunction,
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
                    BlockKind::IfElse => "if/else target",
                    BlockKind::FunctionEntry => "function entry",
                };
                self.node(format!("{}", kind))?;
                for (i, e) in e.exprs.iter().enumerate() {
                    self.edge(*e, format!("exprs[{}]", i))?;
                }
                if let BlockKind::Loop = e.kind {
                    self.edge(self.id, "loop")?;
                }
                Ok(())
            }

            fn visit_call(&mut self, e: &Call) -> io::Result<()> {
                self.node(format!("call {}", e.func.index()))?;
                for (i, arg) in e.args.iter().enumerate() {
                    self.edge(*arg, format!("arg[{}]", i))?;
                }
                Ok(())
            }

            fn visit_local_get(&mut self, e: &LocalGet) -> io::Result<()> {
                self.node(format!("local.get {}", e.local.index()))
            }

            fn visit_local_set(&mut self, e: &LocalSet) -> io::Result<()> {
                self.node(format!("local.set {}", e.local.index()))?;
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

            fn visit_memory_size(&mut self, m: &MemorySize) -> io::Result<()> {
                self.node(format!("memory.size {}", m.memory.index()))?;
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
            let local = ctx.module.locals.local_for_function_and_index(ctx.func_id, ty, *n);
            let expr = ctx.func.alloc(LocalGet { ty, local });
            ctx.push_operand(Some(ty), expr);
        }
        Instruction::SetLocal(n) => {
            let ty = ctx.validation.local(*n).context("invalid set_local")?;
            let (_, value) = ctx.pop_operand_expected(Some(ty))?;
            let local = ctx.module.locals.local_for_function_and_index(ctx.func_id, ty, *n);
            let expr = ctx.func.alloc(LocalSet { ty, local, value });
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
                None => failure::bail!("memory {} is out of bounds", mem),
            };
            let expr = ctx.func.alloc(MemorySize { memory });
            ctx.push_operand(Some(ValType::I32), expr);
        }

        op => failure::bail!("Have not implemented support for opcode yet: {:?}", op),
    }

    Ok(&insts[1..])
}
