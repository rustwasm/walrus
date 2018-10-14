//! TODO

use super::super::error::{ErrorKind, Result};
use super::super::validation_context::ValidationContext;
use super::Function;
use super::ValType;
use crate::ast::{Block, BlockId, ExprId};
use failure::Fail;

#[derive(Debug)]
pub struct ControlFrame {
    /// The type of the associated label (used to type-check branches).
    pub label_types: Vec<ValType>,

    /// The result type of the block (used to check its result).
    pub end_types: Vec<ValType>,

    /// The height of the operand stack at the start of the block (used to check
    /// that operands do not underflow the current block).
    pub height: usize,

    /// If `Some`, then this frame is unreachable (used to handle
    /// stack-polymorphic typing after branches), and the expression that makes
    /// it unreachable is given.
    pub unreachable: Option<ExprId>,

    /// The id of this control frame's block.
    pub block: BlockId,
}

/// The operand stack.
///
/// `None` is used for `Unknown` stack-polymophic values.
///
/// We also keep track of the expression that created the value at each stack
/// slot.
pub type OperandStack = Vec<(Option<ValType>, ExprId)>;

/// The control frame stack.
pub type ControlStack = Vec<ControlFrame>;

#[derive(Debug)]
pub struct FunctionContext<'a> {
    /// The function being validated/constructed.
    pub func: &'a mut Function,

    /// The context under which the function is being validated/constructed.
    pub validation: &'a ValidationContext<'a>,

    /// The operands stack.
    pub operands: &'a mut OperandStack,

    /// The control frames stack.
    pub controls: &'a mut ControlStack,
}

impl<'a> FunctionContext<'a> {
    /// Create a new function context.
    pub fn new(
        func: &'a mut Function,
        validation: &'a ValidationContext<'a>,
        operands: &'a mut OperandStack,
        controls: &'a mut ControlStack,
    ) -> FunctionContext<'a> {
        FunctionContext {
            func,
            validation,
            operands,
            controls,
        }
    }

    pub fn nested<'b>(&'b mut self, validation: &'b ValidationContext<'b>) -> FunctionContext<'b> {
        FunctionContext {
            func: self.func,
            validation,
            operands: self.operands,
            controls: self.controls,
        }
    }

    pub fn push_operand(&mut self, op: Option<ValType>, expr: ExprId) {
        impl_push_operand(&mut self.operands, op, expr);
    }

    pub fn pop_operand(&mut self) -> Result<(Option<ValType>, ExprId)> {
        impl_pop_operand(&mut self.operands, &mut self.controls)
    }

    pub fn pop_operand_expected(
        &mut self,
        expected: Option<ValType>,
    ) -> Result<(Option<ValType>, ExprId)> {
        impl_pop_operand_expected(&mut self.operands, &mut self.controls, expected)
    }

    pub fn push_operands(&mut self, types: &[ValType], exprs: &[ExprId]) {
        impl_push_operands(&mut self.operands, types, exprs)
    }

    pub fn pop_operands(&mut self, expected: &[ValType]) -> Result<Vec<ExprId>> {
        impl_pop_operands(&mut self.operands, &self.controls, expected)
    }

    pub fn push_control(
        &mut self,
        why: &'static str,
        label_types: Vec<ValType>,
        end_types: Vec<ValType>,
        continuation: BlockId,
    ) -> BlockId {
        if let Some(frame) = self.controls.last_mut() {
            frame.block = continuation;
        }
        impl_push_control(
            why,
            self.func,
            self.controls,
            self.operands,
            label_types,
            end_types,
        )
    }

    pub fn pop_control(&mut self) -> Result<(Vec<ValType>, Vec<ExprId>)> {
        let (results, _block, exprs) = impl_pop_control(&mut self.controls, &mut self.operands)?;

        Ok((results, exprs))
    }

    pub fn unreachable(&mut self, expr: ExprId) {
        impl_unreachable(&mut self.operands, &mut self.controls, expr)
    }

    pub fn control(&self, n: usize) -> &ControlFrame {
        let idx = self.controls.len() - n - 1;
        &self.controls[idx]
    }

    pub fn add_to_block(&mut self, block: BlockId, expr: ExprId) {
        self.func.blocks.get_mut(block).unwrap().exprs.push(expr);
    }

    pub fn add_to_frame_block(&mut self, control_frame: usize, expr: ExprId) {
        let block = self.control(control_frame).block;
        self.add_to_block(block, expr);
    }

    pub fn add_to_current_frame_block(&mut self, expr: ExprId) {
        self.add_to_frame_block(0, expr);
    }
}

fn impl_push_operand(operands: &mut OperandStack, op: Option<ValType>, expr: ExprId) {
    operands.push((op, expr));
}

fn impl_pop_operand(
    operands: &mut OperandStack,
    controls: &ControlStack,
) -> Result<(Option<ValType>, ExprId)> {
    if let Some(height) = controls.last().map(|f| f.height) {
        if operands.len() == height {
            if let Some(expr) = controls.last().unwrap().unreachable {
                return Ok((None, expr));
            }
        }
        if operands.len() == height {
            return Err(ErrorKind::InvalidWasm
                .context("popped operand past control frame height in non-unreachable code")
                .into());
        }
    }
    Ok(operands.pop().unwrap())
}

fn impl_pop_operand_expected(
    operands: &mut OperandStack,
    controls: &ControlStack,
    expected: Option<ValType>,
) -> Result<(Option<ValType>, ExprId)> {
    match (impl_pop_operand(operands, controls)?, expected) {
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

fn impl_push_operands(operands: &mut OperandStack, types: &[ValType], exprs: &[ExprId]) {
    for (ty, expr) in types.iter().zip(exprs.iter()) {
        impl_push_operand(operands, Some(*ty), *expr);
    }
}

fn impl_pop_operands(
    operands: &mut OperandStack,
    controls: &ControlStack,
    expected: &[ValType],
) -> Result<Vec<ExprId>> {
    let mut popped = vec![];
    for ty in expected.iter().cloned().rev() {
        let (_, e) = impl_pop_operand_expected(operands, controls, Some(ty))?;
        popped.push(e);
    }
    Ok(popped)
}

fn impl_push_control(
    why: &'static str,
    func: &mut Function,
    controls: &mut ControlStack,
    operands: &OperandStack,
    label_types: Vec<ValType>,
    end_types: Vec<ValType>,
) -> BlockId {
    let block = func.blocks.alloc(Block::new(
        why,
        label_types.clone().into_boxed_slice(),
    ));
    let frame = ControlFrame {
        label_types,
        end_types,
        height: operands.len(),
        unreachable: None,
        block,
    };
    controls.push(frame);
    block
}

fn impl_pop_control(
    controls: &mut ControlStack,
    operands: &mut OperandStack,
) -> Result<(Vec<ValType>, BlockId, Vec<ExprId>)> {
    let frame = controls.last().ok_or_else(|| {
        ErrorKind::InvalidWasm.context("attempted to pop a frame from an empty control stack")
    })?;
    let exprs = impl_pop_operands(operands, controls, &frame.end_types)?;
    if operands.len() != frame.height {
        return Err(ErrorKind::InvalidWasm
            .context(format!(
                "incorrect number of operands on the stack at the end of a control frame; \
                 found {}, expected {}",
                operands.len(),
                frame.height
            ))
            .into());
    }
    let frame = controls.pop().unwrap();
    Ok((frame.end_types, frame.block, exprs))
}

fn impl_unreachable(operands: &mut OperandStack, controls: &mut ControlStack, expr: ExprId) {
    let frame = controls.last_mut().unwrap();
    frame.unreachable = Some(expr);
    let height = frame.height;

    operands.truncate(height);
}
