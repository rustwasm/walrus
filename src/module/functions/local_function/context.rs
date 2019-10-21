//! Context needed when validating instructions and constructing our `Instr` IR.

use crate::error::{ErrorKind, Result};
use crate::ir::{BlockKind, Instr, InstrLocId, InstrSeq, InstrSeqId, InstrSeqType};
use crate::module::functions::{FunctionId, LocalFunction};
use crate::module::Module;
use crate::parse::IndicesToIds;
use crate::ty::ValType;
use crate::{ModuleTypes, TypeId};
use anyhow::Context;

#[derive(Debug)]
pub(crate) struct ControlFrame {
    /// The parameter types of the block (checked before entering the block).
    pub start_types: Box<[ValType]>,

    /// The result type of the block (used to check its result).
    pub end_types: Box<[ValType]>,

    /// The height of the operand stack at the start of the block (used to check
    /// that operands do not underflow the current block).
    pub height: usize,

    /// If `true`, then this frame is unreachable. This is used to handle
    /// stack-polymorphic typing after unconditional branches.
    pub unreachable: bool,

    /// The id of this control frame's block.
    pub block: InstrSeqId,

    /// This control frame's kind of block, eg loop vs block vs if/else.
    pub kind: BlockKind,
}

impl ControlFrame {
    /// Get the expected types on the stack for branches to this block.
    pub fn label_types(&self) -> &[ValType] {
        if let BlockKind::Loop = self.kind {
            &self.start_types
        } else {
            &self.end_types
        }
    }
}

/// The operand stack.
///
/// `None` is used for `Unknown` stack-polymophic values.
///
/// We also keep track of the instruction that created the value at each stack
/// slot.
pub(crate) type OperandStack = Vec<Option<ValType>>;

/// The control frame stack.
pub(crate) type ControlStack = Vec<ControlFrame>;

#[derive(Debug)]
pub(crate) struct ValidationContext<'a> {
    /// The module that we're adding a function for.
    pub module: &'a Module,

    /// Mapping of indexes back to ids.
    pub indices: &'a IndicesToIds,

    /// The arena id of `func`.
    pub func_id: FunctionId,

    /// The function being validated/constructed.
    pub func: &'a mut LocalFunction,

    /// The operands stack.
    pub operands: &'a mut OperandStack,

    /// The control frames stack.
    pub controls: &'a mut ControlStack,

    /// If we're currently parsing an if/else instruction, where we're at
    pub if_else: Vec<IfElseState>,
}

#[derive(Debug)]
pub struct IfElseState {
    pub consequent: InstrSeqId,
    pub alternative: Option<InstrSeqId>,
}

impl<'a> ValidationContext<'a> {
    /// Create a new function context.
    pub fn new(
        module: &'a Module,
        indices: &'a IndicesToIds,
        func_id: FunctionId,
        func: &'a mut LocalFunction,
        operands: &'a mut OperandStack,
        controls: &'a mut ControlStack,
    ) -> ValidationContext<'a> {
        ValidationContext {
            module,
            indices,
            func_id,
            func,
            operands,
            controls,
            if_else: Vec::new(),
        }
    }

    pub fn push_operand(&mut self, op: Option<ValType>) {
        impl_push_operand(&mut self.operands, op);
    }

    pub fn pop_operand(&mut self) -> Result<Option<ValType>> {
        impl_pop_operand(&mut self.operands, &mut self.controls)
    }

    pub fn pop_operand_expected(&mut self, expected: Option<ValType>) -> Result<Option<ValType>> {
        impl_pop_operand_expected(&mut self.operands, &mut self.controls, expected)
    }

    pub fn push_operands(&mut self, types: &[ValType]) {
        impl_push_operands(&mut self.operands, types);
    }

    pub fn pop_operands(&mut self, expected: &[ValType]) -> Result<()> {
        impl_pop_operands(&mut self.operands, &self.controls, expected)
    }

    pub fn push_control(
        &mut self,
        kind: BlockKind,
        start_types: Box<[ValType]>,
        end_types: Box<[ValType]>,
    ) -> Result<InstrSeqId> {
        impl_push_control(
            &self.module.types,
            kind,
            self.func,
            self.controls,
            self.operands,
            start_types,
            end_types,
        )
    }

    pub fn push_control_with_ty(&mut self, kind: BlockKind, ty: TypeId) -> InstrSeqId {
        let (start_types, end_types) = self.module.types.params_results(ty);
        let start_types: Box<[_]> = start_types.into();
        let end_types: Box<[_]> = end_types.into();
        impl_push_control_with_ty(
            &self.module.types,
            kind,
            self.func,
            self.controls,
            self.operands,
            ty.into(),
            start_types,
            end_types,
        )
    }

    pub fn pop_control(&mut self) -> Result<(ControlFrame, InstrSeqId)> {
        let frame = impl_pop_control(&mut self.controls, &mut self.operands)?;
        let block = frame.block;
        Ok((frame, block))
    }

    pub fn unreachable(&mut self) {
        let frame = self.controls.last_mut().unwrap();
        frame.unreachable = true;
        let height = frame.height;
        self.operands.truncate(height);
    }

    pub fn control(&self, n: usize) -> Result<&ControlFrame> {
        if n >= self.controls.len() {
            anyhow::bail!("jump to nonexistent control block");
        }
        let idx = self.controls.len() - n - 1;
        Ok(&self.controls[idx])
    }

    pub fn alloc_instr_in_block(
        &mut self,
        block: InstrSeqId,
        instr: impl Into<Instr>,
        loc: InstrLocId,
    ) {
        self.func.block_mut(block).instrs.push((instr.into(), loc));
    }

    pub fn alloc_instr_in_control(
        &mut self,
        control: usize,
        instr: impl Into<Instr>,
        loc: InstrLocId,
    ) -> Result<()> {
        let frame = self.control(control)?;
        if frame.unreachable {
            return Ok(());
        }
        let block = frame.block;
        self.alloc_instr_in_block(block, instr, loc);
        Ok(())
    }

    pub fn alloc_instr(&mut self, instr: impl Into<Instr>, loc: InstrLocId) {
        self.alloc_instr_in_control(0, instr, loc).unwrap();
    }
}

fn impl_push_operand(operands: &mut OperandStack, op: Option<ValType>) {
    log::trace!("push operand: {:?}", op);
    operands.push(op);
}

fn impl_pop_operand(
    operands: &mut OperandStack,
    controls: &ControlStack,
) -> Result<Option<ValType>> {
    if let Some(f) = controls.last() {
        if operands.len() == f.height {
            if f.unreachable {
                log::trace!("pop operand: None");
                return Ok(None);
            }
            return Err(ErrorKind::InvalidWasm)
                .context("popped operand past control frame height in non-unreachable code");
        }
    }
    let op = operands.pop().unwrap();
    log::trace!("pop operand: {:?}", op);
    Ok(op)
}

fn impl_pop_operand_expected(
    operands: &mut OperandStack,
    controls: &ControlStack,
    expected: Option<ValType>,
) -> Result<Option<ValType>> {
    match (impl_pop_operand(operands, controls)?, expected) {
        (None, expected) => Ok(expected),
        (actual, None) => Ok(actual),
        (Some(actual), Some(expected)) => {
            if actual != expected {
                Err(ErrorKind::InvalidWasm)
                    .context(format!("expected type {}", expected))
                    .context(format!("found type {}", actual))
            } else {
                Ok(Some(actual))
            }
        }
    }
}

fn impl_push_operands(operands: &mut OperandStack, types: &[ValType]) {
    for ty in types {
        impl_push_operand(operands, Some(*ty));
    }
}

fn impl_pop_operands(
    operands: &mut OperandStack,
    controls: &ControlStack,
    expected: &[ValType],
) -> Result<()> {
    for ty in expected.iter().cloned().rev() {
        impl_pop_operand_expected(operands, controls, Some(ty))?;
    }
    Ok(())
}

fn impl_push_control(
    types: &ModuleTypes,
    kind: BlockKind,
    func: &mut LocalFunction,
    controls: &mut ControlStack,
    operands: &mut OperandStack,
    start_types: Box<[ValType]>,
    end_types: Box<[ValType]>,
) -> Result<InstrSeqId> {
    let ty = InstrSeqType::existing(types, &start_types, &end_types).ok_or_else(|| {
        anyhow::anyhow!(
            "attempted to push a control frame for an instruction \
             sequence with a type that does not exist"
        )
        .context(format!("type: {:?} -> {:?}", &start_types, &end_types))
    })?;

    Ok(impl_push_control_with_ty(
        types,
        kind,
        func,
        controls,
        operands,
        ty,
        start_types,
        end_types,
    ))
}

fn impl_push_control_with_ty(
    types: &ModuleTypes,
    kind: BlockKind,
    func: &mut LocalFunction,
    controls: &mut ControlStack,
    operands: &mut OperandStack,
    ty: InstrSeqType,
    start_types: Box<[ValType]>,
    end_types: Box<[ValType]>,
) -> InstrSeqId {
    if let InstrSeqType::MultiValue(ty) = ty {
        debug_assert_eq!(types.params(ty), &start_types[..]);
        debug_assert_eq!(types.results(ty), &end_types[..]);
    }

    let height = operands.len();
    impl_push_operands(operands, &start_types);

    let block = func.add_block(|id| InstrSeq::new(id, ty));

    controls.push(ControlFrame {
        start_types,
        end_types,
        height,
        unreachable: false,
        block,
        kind,
    });

    block
}

fn impl_pop_control(
    controls: &mut ControlStack,
    operands: &mut OperandStack,
) -> Result<ControlFrame> {
    let frame = controls
        .last()
        .ok_or_else(|| ErrorKind::InvalidWasm)
        .context("attempted to pop a frame from an empty control stack")?;
    impl_pop_operands(operands, controls, &frame.end_types)?;
    if operands.len() != frame.height {
        return Err(ErrorKind::InvalidWasm).context(format!(
            "incorrect number of operands on the stack at the end of a control frame; \
             found {}, expected {}",
            operands.len(),
            frame.height
        ));
    }
    let frame = controls.pop().unwrap();
    Ok(frame)
}
