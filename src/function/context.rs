//! TODO

use super::super::error::{ErrorKind, Result};
use super::super::validation_context::ValidationContext;
use super::super::{Block, BlockId, ExprId, ValType};
use super::Function;
use failure::Fail;

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

    /// If `Some`, then we parsed an `if` and its block, and are awaiting the
    /// consequent block to be parsed.
    pub if_else: Option<(ExprId, BlockId)>,
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

pub struct FunctionContext<'a> {
    /// The function being validated/constructed.
    pub func: &'a mut Function,

    /// The context under which the function is being validated/constructed.
    pub validation: &'a ValidationContext<'a>,

    /// The operands stack.
    pub operands: OperandStack,

    /// The control frames stack.
    pub controls: ControlStack,
}

impl<'a> FunctionContext<'a> {
    /// Create a new function context.
    pub fn new(
        func: &'a mut Function,
        validation: &'a ValidationContext<'a>,
    ) -> FunctionContext<'a> {
        FunctionContext {
            func,
            validation,
            operands: OperandStack::new(),
            controls: ControlStack::new(),
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

    pub fn push_operands(&mut self, types: &[ValType], expr: ExprId) {
        impl_push_operands(&mut self.operands, types, expr)
    }

    pub fn pop_operands(&mut self, expected: &[ValType]) -> Result<Vec<ExprId>> {
        impl_pop_operands(&mut self.operands, &self.controls, expected)
    }

    pub fn push_control(&mut self, label_types: Vec<ValType>, end_types: Vec<ValType>) -> BlockId {
        impl_push_control(
            self.func,
            &mut self.controls,
            &mut self.operands,
            label_types,
            end_types,
        )
    }

    pub fn pop_control(&mut self) -> Result<Vec<ValType>> {
        impl_pop_control(&mut self.controls, &mut self.operands)
    }

    pub fn unreachable(&mut self, expr: ExprId) {
        impl_unreachable(&mut self.operands, &mut self.controls, expr)
    }

    pub fn control(&self, n: usize) -> &ControlFrame {
        let idx = self.controls.len() - n - 1;
        &self.controls[idx]
    }

    pub fn control_mut(&mut self, n: usize) -> &mut ControlFrame {
        let idx = self.controls.len() - n - 1;
        &mut self.controls[idx]
    }
}

fn impl_push_operand(operands: &mut OperandStack, op: Option<ValType>, expr: ExprId) {
    operands.push((op, expr));
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

fn impl_push_operands(operands: &mut OperandStack, types: &[ValType], expr: ExprId) {
    for ty in types {
        impl_push_operand(operands, Some(*ty), expr);
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
    func: &mut Function,
    controls: &mut ControlStack,
    operands: &OperandStack,
    label_types: Vec<ValType>,
    end_types: Vec<ValType>,
) -> BlockId {
    let block = func.blocks.alloc(Block { exprs: vec![] });
    let frame = ControlFrame {
        label_types,
        end_types,
        height: operands.len(),
        unreachable: None,
        block,
        if_else: None,
    };
    controls.push(frame);
    block
}

fn impl_pop_control(
    controls: &mut ControlStack,
    operands: &mut OperandStack,
) -> Result<Vec<ValType>> {
    let frame = controls.last().ok_or_else(|| {
        ErrorKind::InvalidWasm.context("attempted to pop a frame from an empty control stack")
    })?;
    impl_pop_operands(operands, controls, &frame.end_types)?;
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
    Ok(frame.end_types)
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
