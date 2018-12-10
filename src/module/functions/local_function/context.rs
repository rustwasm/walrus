//! TODO

use crate::error::{ErrorKind, Result};
use crate::ir::{Block, BlockId, BlockKind, ExprId};
use crate::module::functions::{FunctionId, LocalFunction};
use crate::module::locals::ModuleLocals;
use crate::ty::ValType;
use crate::validation_context::ValidationContext;
use failure::Fail;

#[derive(Debug)]
pub struct ControlFrame {
    /// The type of the associated label (used to type-check branches).
    pub label_types: Box<[ValType]>,

    /// The result type of the block (used to check its result).
    pub end_types: Box<[ValType]>,

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
    /// The locals for this function's module.
    pub locals: &'a mut ModuleLocals,

    /// The arena id of `func`.
    pub func_id: FunctionId,

    /// The function being validated/constructed.
    pub func: &'a mut LocalFunction,

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
        locals: &'a mut ModuleLocals,
        func_id: FunctionId,
        func: &'a mut LocalFunction,
        validation: &'a ValidationContext<'a>,
        operands: &'a mut OperandStack,
        controls: &'a mut ControlStack,
    ) -> FunctionContext<'a> {
        FunctionContext {
            locals,
            func_id,
            func,
            validation,
            operands,
            controls,
        }
    }

    pub fn nested<'b>(&'b mut self, validation: &'b ValidationContext<'b>) -> FunctionContext<'b> {
        FunctionContext {
            locals: self.locals,
            func_id: self.func_id,
            func: self.func,
            validation,
            operands: self.operands,
            controls: self.controls,
        }
    }

    pub fn push_operand<E>(&mut self, op: Option<ValType>, expr: E)
    where
        E: Copy + Into<ExprId>,
    {
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

    pub fn push_operands<E>(&mut self, types: &[ValType], exprs: &[E])
    where
        E: Copy + Into<ExprId>,
    {
        impl_push_operands(&mut self.operands, types, exprs)
    }

    pub fn pop_operands(&mut self, expected: &[ValType]) -> Result<Vec<ExprId>> {
        impl_pop_operands(&mut self.operands, &self.controls, expected)
    }

    pub fn push_control(
        &mut self,
        why: &'static str,
        kind: BlockKind,
        label_types: Box<[ValType]>,
        end_types: Box<[ValType]>,
    ) -> BlockId {
        impl_push_control(
            why,
            kind,
            self.func,
            self.controls,
            self.operands,
            label_types,
            end_types,
        )
    }

    pub fn pop_control(&mut self) -> Result<(Box<[ValType]>, Vec<ExprId>)> {
        let block = self.control(0).block;
        let (results, _block, exprs) = impl_pop_control(&mut self.controls, &mut self.operands)?;
        self.func
            .block_mut(block)
            .exprs
            .extend(exprs.iter().cloned());
        Ok((results, exprs))
    }

    pub fn unreachable<E>(&mut self, expr: E)
    where
        E: Into<ExprId>,
    {
        impl_unreachable(&mut self.operands, &mut self.controls, expr)
    }

    pub fn control(&self, n: usize) -> &ControlFrame {
        let idx = self.controls.len() - n - 1;
        &self.controls[idx]
    }

    pub fn add_to_block<E>(&mut self, block: BlockId, expr: E)
    where
        E: Into<ExprId>,
    {
        let block = self.func.exprs[block.into()].unwrap_block_mut();
        block.exprs.push(expr.into());
    }

    pub fn add_to_frame_block<E>(&mut self, control_frame: usize, expr: E)
    where
        E: Into<ExprId>,
    {
        let block = self.control(control_frame).block;
        self.add_to_block(block, expr);
    }

    pub fn add_to_current_frame_block<E>(&mut self, expr: E)
    where
        E: Into<ExprId>,
    {
        self.add_to_frame_block(0, expr);
    }
}

fn impl_push_operand<E>(operands: &mut OperandStack, op: Option<ValType>, expr: E)
where
    E: Into<ExprId>,
{
    operands.push((op, expr.into()));
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

fn impl_push_operands<E>(operands: &mut OperandStack, types: &[ValType], exprs: &[E])
where
    E: Copy + Into<ExprId>,
{
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
    kind: BlockKind,
    func: &mut LocalFunction,
    controls: &mut ControlStack,
    operands: &OperandStack,
    label_types: Box<[ValType]>,
    end_types: Box<[ValType]>,
) -> BlockId {
    let block = func.alloc(Block::new(
        why,
        kind,
        label_types.clone(),
        end_types.clone(),
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
) -> Result<(Box<[ValType]>, BlockId, Vec<ExprId>)> {
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

fn impl_unreachable<E>(operands: &mut OperandStack, controls: &mut ControlStack, expr: E)
where
    E: Into<ExprId>,
{
    let frame = controls.last_mut().unwrap();
    frame.unreachable = Some(expr.into());
    let height = frame.height;

    operands.truncate(height);
}
