//! TODO

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]

extern crate failure;
extern crate parity_wasm;

pub mod arena;
pub mod chunk_list;
pub mod error;
pub mod validation_context;

use self::arena::{Arena, Id};
use self::error::{ErrorKind, Result};
use self::validation_context::ValidationContext;
use failure::Fail;
use parity_wasm::elements::{self, Instruction};
use std::fmt;

/// TODO
#[derive(Debug)]
pub struct Function {
    exprs: Arena<Expr>,
    blocks: Arena<Block>,
    // TODO: provenance: ExprId -> offset in code section of the original
    // instruction
}

/// TODO
pub type ExprId = Id<Expr>;

/// TODO
pub type BlockId = Id<Block>;

/// TODO
#[derive(Debug)]
pub struct Block {
    exprs: Vec<ExprId>,
}

/// TODO
#[derive(Debug)]
pub enum Expr {
    /// TODO
    I32Const(i32),
    /// TODO
    I32Add(ExprId, ExprId),
    /// TODO
    Select(ExprId, ExprId),
    /// TODO
    Unreachable,
    /// TODO
    Phi,
    /// TODO
    BrIf(ExprId, BlockId),
    /// TODO
    BrTable(ExprId, Box<[BlockId]>, BlockId),
}

/// TODO
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ValType {
    /// TODO
    I32,
    /// TODO
    I64,
    /// TODO
    F32,
    /// TODO
    F64,
    /// TODO
    V128,
}

impl<'a> From<&'a elements::ValueType> for ValType {
    fn from(x: &'a elements::ValueType) -> ValType {
        match x {
            elements::ValueType::I32 => ValType::I32,
            elements::ValueType::I64 => ValType::I64,
            elements::ValueType::F32 => ValType::F32,
            elements::ValueType::F64 => ValType::F64,
            elements::ValueType::V128 => ValType::V128,
        }
    }
}

impl ValType {
    fn from_block_ty(block_ty: &elements::BlockType) -> Vec<ValType> {
        match block_ty {
            elements::BlockType::Value(ty) => vec![ty.into()],
            elements::BlockType::NoResult => vec![],
        }
    }
}

impl fmt::Display for ValType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ValType::I32 => "i32",
                ValType::I64 => "i64",
                ValType::F32 => "f32",
                ValType::F64 => "f64",
                ValType::V128 => "v128",
            }
        )
    }
}

struct ControlFrame {
    label_types: Vec<ValType>,
    end_types: Vec<ValType>,
    height: usize,
    // If `Some`, then this frame is unreachable, and the expression that makes
    // it unreachable is given.
    unreachable: Option<ExprId>,
    block: BlockId,
}

type OperandStack = Vec<(Option<ValType>, ExprId)>;
type ControlStack = Vec<ControlFrame>;

struct FunctionContext<'a> {
    func: &'a mut Function,
    validation: &'a ValidationContext<'a>,
    operands: OperandStack,
    controls: ControlStack,
}

impl<'a> FunctionContext<'a> {
    fn impl_push_operand(operands: &mut OperandStack, op: Option<ValType>, expr: ExprId) {
        operands.push((op, expr));
    }
    pub fn push_operand(&mut self, op: Option<ValType>, expr: ExprId) {
        Self::impl_push_operand(&mut self.operands, op, expr);
    }

    fn impl_pop_operand(
        operands: &mut OperandStack,
        controls: &ControlStack,
    ) -> Result<(Option<ValType>, ExprId)> {
        if operands.len() == controls.last().unwrap().height {
            if let Some(expr) = controls.last().unwrap().unreachable {
                return Ok((None, expr));
            }
        }
        if operands.len() == controls.last().unwrap().height {
            return Err(ErrorKind::InvalidWasm
                .context("popped operand past control frame height in non-unreachable code")
                .into());
        }
        Ok(operands.pop().unwrap())
    }
    pub fn pop_operand(&mut self) -> Result<(Option<ValType>, ExprId)> {
        Self::impl_pop_operand(&mut self.operands, &mut self.controls)
    }

    fn impl_pop_operand_expected(
        operands: &mut OperandStack,
        controls: &ControlStack,
        expected: Option<ValType>,
    ) -> Result<(Option<ValType>, ExprId)> {
        match (Self::impl_pop_operand(operands, controls)?, expected) {
            ((None, id), expected) => Ok((expected, id)),
            ((actual, id), None) => Ok((actual, id)),
            ((Some(actual), id), Some(expected)) => {
                if actual != expected {
                    Err(ErrorKind::InvalidWasm
                        .context(format!("expected type {}", expected))
                        .context(format!("found type {}", actual))
                        .into())
                } else {
                    Ok((Some(actual), id))
                }
            }
        }
    }
    pub fn pop_operand_expected(
        &mut self,
        expected: Option<ValType>,
    ) -> Result<(Option<ValType>, ExprId)> {
        Self::impl_pop_operand_expected(&mut self.operands, &mut self.controls, expected)
    }

    fn impl_push_operands(operands: &mut OperandStack, types: &[ValType], expr: ExprId) {
        for ty in types {
            Self::impl_push_operand(operands, Some(*ty), expr);
        }
    }
    pub fn push_operands(&mut self, types: &[ValType], expr: ExprId) {
        Self::impl_push_operands(&mut self.operands, types, expr)
    }

    fn impl_pop_operands(
        operands: &mut OperandStack,
        controls: &ControlStack,
        expected: &[ValType],
    ) -> Result<()> {
        for ty in expected.iter().cloned().rev() {
            Self::impl_pop_operand_expected(operands, controls, Some(ty))?;
        }
        Ok(())
    }
    pub fn pop_operands(&mut self, expected: &[ValType]) -> Result<()> {
        Self::impl_pop_operands(&mut self.operands, &self.controls, expected)
    }

    fn impl_push_control(
        func: &mut Function,
        controls: &mut ControlStack,
        operands: &OperandStack,
        label_types: Vec<ValType>,
        end_types: Vec<ValType>,
    ) {
        let block = func.blocks.alloc(Block { exprs: vec![] });
        let frame = ControlFrame {
            label_types,
            end_types,
            height: operands.len(),
            unreachable: None,
            block,
        };
        controls.push(frame);
    }
    pub fn push_control(&mut self, label_types: Vec<ValType>, end_types: Vec<ValType>) {
        Self::impl_push_control(
            self.func,
            &mut self.controls,
            &mut self.operands,
            label_types,
            end_types,
        )
    }

    fn impl_pop_control(
        controls: &mut ControlStack,
        operands: &mut OperandStack,
    ) -> Result<Vec<ValType>> {
        let frame = controls.last().ok_or_else(|| {
            ErrorKind::InvalidWasm.context("attempted to pop a frame from an empty control stack")
        })?;
        Self::impl_pop_operands(operands, controls, &frame.end_types)?;
        if operands.len() != frame.height {
            return Err(ErrorKind::InvalidWasm
                .context("incorrect number of operands on the stack at the end of a control frame")
                .into());
        }
        let frame = controls.pop().unwrap();
        Ok(frame.end_types)
    }
    pub fn pop_control(&mut self) -> Result<Vec<ValType>> {
        Self::impl_pop_control(&mut self.controls, &mut self.operands)
    }

    fn impl_unreachable(operands: &mut OperandStack, controls: &mut ControlStack, expr: ExprId) {
        let frame = controls.last_mut().unwrap();
        frame.unreachable = Some(expr);
        let height = frame.height;

        operands.truncate(height);
        for _ in operands.len()..height {
            operands.push((None, expr));
        }
    }
    pub fn unreachable(&mut self, expr: ExprId) {
        Self::impl_unreachable(&mut self.operands, &mut self.controls, expr)
    }
}

fn validate_opcode(ctx: &mut FunctionContext, opcode: &Instruction) -> Result<()> {
    match opcode {
        Instruction::I32Const(n) => {
            let expr = ctx.func.exprs.alloc(Expr::I32Const(*n));
            ctx.push_operand(Some(ValType::I32), expr);
        }
        Instruction::I32Add => {
            let (_, e1) = ctx.pop_operand_expected(Some(ValType::I32))?;
            let (_, e2) = ctx.pop_operand_expected(Some(ValType::I32))?;
            let expr = ctx.func.exprs.alloc(Expr::I32Add(e1, e2));
            ctx.push_operand(Some(ValType::I32), expr);
        }
        Instruction::Drop => {
            ctx.pop_operand()?;
        }
        Instruction::Select => {
            ctx.pop_operand_expected(Some(ValType::I32))?;
            let (t1, e1) = ctx.pop_operand()?;
            let (t2, e2) = ctx.pop_operand_expected(t1)?;
            let expr = ctx.func.exprs.alloc(Expr::Select(e1, e2));
            ctx.push_operand(t2, expr);
        }
        Instruction::Unreachable => {
            let expr = ctx.func.exprs.alloc(Expr::Unreachable);
            ctx.unreachable(expr);
        }
        Instruction::Block(block_ty) => {
            let t = ValType::from_block_ty(block_ty);
            ctx.push_control(t.clone(), t);
        }
        Instruction::Loop(block_ty) => {
            let t = ValType::from_block_ty(block_ty);
            ctx.push_control(vec![], t);
        }
        Instruction::If(block_ty) => {
            ctx.pop_operand_expected(Some(ValType::I32))?;
            let t = ValType::from_block_ty(block_ty);
            ctx.push_control(t.clone(), t);
        }
        Instruction::Else | Instruction::End => {
            let expr = ctx.func.exprs.alloc(Expr::Phi);
            let results = ctx.pop_control()?;
            ctx.push_operands(&results, expr);
        }
        Instruction::Br(n) => {
            let n = *n as usize;
            if ctx.controls.len() < n {
                return Err(ErrorKind::InvalidWasm
                    .context("attempt to branch to out-of-bounds block")
                    .into());
            }
            let expected = ctx.controls[ctx.controls.len() - n].label_types.clone();
            ctx.pop_operands(&expected)?;
        }
        Instruction::BrIf(n) => {
            let n = *n as usize;
            if ctx.controls.len() < n {
                return Err(ErrorKind::InvalidWasm
                    .context("attempt to branch to out-of-bounds block")
                    .into());
            }

            let block = ctx.controls[ctx.controls.len() - n].block;
            let (_, c) = ctx.pop_operand_expected(Some(ValType::I32))?;
            let expr = ctx.func.exprs.alloc(Expr::BrIf(c, block));

            let expected = ctx.controls[ctx.controls.len() - n].label_types.clone();
            ctx.pop_operands(&expected)?;
            ctx.push_operands(&expected, expr);
        }
        Instruction::BrTable(table) => {
            if ctx.controls.len() < table.default as usize {
                return Err(ErrorKind::InvalidWasm
                    .context(
                        "attempt to jump to an out-of-bounds block from the default table entry",
                    )
                    .into());
            }
            let default_block = ctx.controls[ctx.controls.len() - table.default as usize].block;

            let mut blocks = Vec::with_capacity(table.table.len());
            for n in table.table.iter() {
                let n = *n as usize;
                if ctx.controls.len() < n {
                    return Err(ErrorKind::InvalidWasm
                        .context("attempt to jump to an out-of-bounds block from a table entry")
                        .into());
                }
                if ctx.controls[ctx.controls.len() - n].label_types
                    != ctx.controls[ctx.controls.len() - table.default as usize].label_types
                {
                    return Err(ErrorKind::InvalidWasm
                        .context(
                            "attempt to jump to block non-matching label types from a table entry",
                        )
                        .into());
                }
                blocks.push(ctx.controls[ctx.controls.len() - n].block);
            }
            let blocks = blocks.into_boxed_slice();

            let (_, b) = ctx.pop_operand_expected(Some(ValType::I32))?;
            let expr = ctx
                .func
                .exprs
                .alloc(Expr::BrTable(b, blocks, default_block));

            let expected = ctx.controls[ctx.controls.len() - table.default as usize]
                .label_types
                .clone();
            ctx.pop_operands(&expected)?;
            ctx.unreachable(expr);
        }

        op => unimplemented!("Have not implemented support for opcode yet: {:?}", op),
    }

    Ok(())
}

impl Function {
    /// TODO
    pub fn new(
        validation: &ValidationContext,
        ty: &elements::FunctionType,
        body: &elements::FuncBody,
    ) -> Result<Function> {
        // TODO: context and locals and all that.
        let mut func = Function {
            blocks: Arena::new(),
            exprs: Arena::new(),
        };

        let mut ctx = FunctionContext {
            func: &mut func,
            validation,
            operands: OperandStack::new(),
            controls: ControlStack::new(),
        };

        let params: Vec<_> = ty.params().iter().map(ValType::from).collect();
        let result: Vec<_> = ty
            .return_type()
            .as_ref()
            .into_iter()
            .map(ValType::from)
            .collect();

        ctx.push_control(params, result);

        for op in body.code().elements() {
            validate_opcode(&mut ctx, op)?;
        }

        Ok(func)
    }
}
