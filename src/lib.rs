extern crate failure;
extern crate parity_wasm;

pub mod arena;
pub mod error;

use self::error::ErrorKind;

use self::arena::{Arena, Id};
use failure::Fail;
use parity_wasm::elements::{self, Instruction};
use std::fmt;

pub type Result<T> = ::std::result::Result<T, failure::Error>;

pub struct Function {
    exprs: Arena<Expr>,
    blocks: Arena<Block>,
}

pub type ExprId = Id<Expr>;
pub type BlockId = Id<Block>;

pub struct Block {
    exprs: Vec<ExprId>,
}

pub enum Expr {
    I32Const(i32),
    I32Add(ExprId, ExprId),
    Select(ExprId, ExprId),
    Unreachable,
    Phi,
    BrIf(ExprId, BlockId),
    BrTable(ExprId, Box<[BlockId]>, BlockId),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ValType {
    I32,
    I64,
    F32,
    F64,
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

pub struct ControlFrame {
    label_types: Vec<ValType>,
    end_types: Vec<ValType>,
    height: usize,
    // If `Some`, then this frame is unreachable, and the expression that makes
    // it unreachable is given.
    unreachable: Option<ExprId>,
    block: BlockId,
}

pub type OperandStack = Vec<(Option<ValType>, ExprId)>;
pub type ControlStack = Vec<ControlFrame>;

fn push_operand(operands: &mut OperandStack, op: Option<ValType>, expr: ExprId) {
    operands.push((op, expr));
}

fn pop_operand(
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

fn pop_operand_expected(
    operands: &mut OperandStack,
    controls: &ControlStack,
    expected: Option<ValType>,
) -> Result<(Option<ValType>, ExprId)> {
    match (pop_operand(operands, controls)?, expected) {
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

fn push_operands(operands: &mut OperandStack, types: &[ValType], expr: ExprId) {
    for ty in types {
        push_operand(operands, Some(*ty), expr);
    }
}

fn pop_operands(
    operands: &mut OperandStack,
    controls: &ControlStack,
    expected: &[ValType],
) -> Result<()> {
    for ty in expected.iter().cloned().rev() {
        pop_operand_expected(operands, controls, Some(ty))?;
    }
    Ok(())
}

fn push_control(
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

fn pop_control(controls: &mut ControlStack, operands: &mut OperandStack) -> Result<Vec<ValType>> {
    let frame = controls.last().ok_or_else(|| {
        ErrorKind::InvalidWasm.context("attempted to pop a frame from an empty control stack")
    })?;
    pop_operands(operands, controls, &frame.end_types)?;
    if operands.len() != frame.height {
        return Err(ErrorKind::InvalidWasm
            .context("incorrect number of operands on the stack at the end of a control frame")
            .into());
    }
    let frame = controls.pop().unwrap();
    Ok(frame.end_types)
}

fn unreachable(operands: &mut OperandStack, controls: &mut ControlStack, expr: ExprId) {
    let frame = controls.last_mut().unwrap();
    frame.unreachable = Some(expr);
    let height = frame.height;

    operands.truncate(height);
    for _ in operands.len()..height {
        operands.push((None, expr));
    }
}

fn validate_opcode(
    func: &mut Function,
    operands: &mut OperandStack,
    controls: &mut ControlStack,
    opcode: &Instruction,
) -> Result<()> {
    match opcode {
        Instruction::I32Const(n) => {
            let expr = func.exprs.alloc(Expr::I32Const(*n));
            push_operand(operands, Some(ValType::I32), expr);
        }
        Instruction::I32Add => {
            let (_, e1) = pop_operand_expected(operands, controls, Some(ValType::I32))?;
            let (_, e2) = pop_operand_expected(operands, controls, Some(ValType::I32))?;
            let expr = func.exprs.alloc(Expr::I32Add(e1, e2));
            push_operand(operands, Some(ValType::I32), expr);
        }
        Instruction::Drop => {
            pop_operand(operands, controls)?;
        }
        Instruction::Select => {
            pop_operand_expected(operands, controls, Some(ValType::I32))?;
            let (t1, e1) = pop_operand(operands, controls)?;
            let (t2, e2) = pop_operand_expected(operands, controls, t1)?;
            let expr = func.exprs.alloc(Expr::Select(e1, e2));
            push_operand(operands, t2, expr);
        }
        Instruction::Unreachable => {
            let expr = func.exprs.alloc(Expr::Unreachable);
            unreachable(operands, controls, expr);
        }
        Instruction::Block(block_ty) => {
            let t = ValType::from_block_ty(block_ty);
            push_control(func, controls, operands, t.clone(), t);
        }
        Instruction::Loop(block_ty) => {
            let t = ValType::from_block_ty(block_ty);
            push_control(func, controls, operands, vec![], t);
        }
        Instruction::If(block_ty) => {
            pop_operand_expected(operands, controls, Some(ValType::I32))?;
            let t = ValType::from_block_ty(block_ty);
            push_control(func, controls, operands, t.clone(), t);
        }
        Instruction::Else | Instruction::End => {
            let expr = func.exprs.alloc(Expr::Phi);
            let results = pop_control(controls, operands)?;
            push_operands(operands, &results, expr);
        }
        Instruction::Br(n) => {
            if controls.len() < *n as usize {
                return Err(ErrorKind::InvalidWasm
                    .context("attempt to branch to out-of-bounds block")
                    .into());
            }
            let expected = controls[controls.len() - *n as usize].label_types.clone();
            pop_operands(operands, controls, &expected)?;
        }
        Instruction::BrIf(n) => {
            let n = *n as usize;
            if controls.len() < n {
                return Err(ErrorKind::InvalidWasm
                    .context("attempt to branch to out-of-bounds block")
                    .into());
            }

            let block = controls[controls.len() - n].block;
            let (_, c) = pop_operand_expected(operands, controls, Some(ValType::I32))?;
            let expr = func.exprs.alloc(Expr::BrIf(c, block));

            let expected = controls[controls.len() - n].label_types.clone();
            pop_operands(operands, controls, &expected)?;
            push_operands(operands, &expected, expr);
        }
        Instruction::BrTable(table) => {
            if controls.len() < table.default as usize {
                return Err(ErrorKind::InvalidWasm
                    .context(
                        "attempt to jump to an out-of-bounds block from the default table entry",
                    )
                    .into());
            }
            let default_block = controls[controls.len() - table.default as usize].block;

            let mut blocks = Vec::with_capacity(table.table.len());
            for n in table.table.iter() {
                let n = *n as usize;
                if controls.len() < n {
                    return Err(ErrorKind::InvalidWasm
                        .context("attempt to jump to an out-of-bounds block from a table entry")
                        .into());
                }
                if controls[controls.len() - n].label_types
                    != controls[controls.len() - table.default as usize].label_types
                {
                    return Err(ErrorKind::InvalidWasm
                        .context(
                            "attempt to jump to block non-matching label types from a table entry",
                        )
                        .into());
                }
                blocks.push(controls[controls.len() - n].block);
            }
            let blocks = blocks.into_boxed_slice();

            let (_, b) = pop_operand_expected(operands, controls, Some(ValType::I32))?;
            let expr = func.exprs.alloc(Expr::BrTable(b, blocks, default_block));

            let expected = controls[controls.len() - table.default as usize]
                .label_types
                .clone();
            pop_operands(operands, controls, &expected)?;
            unreachable(operands, controls, expr);
        }

        op => unimplemented!("Have not implemented support for opcode yet: {:?}", op),
    }

    Ok(())
}

impl Function {
    pub fn new(ty: &elements::FunctionType, body: &elements::FuncBody) -> Result<Function> {
        // TODO: context and locals and all that.
        let mut func = Function {
            blocks: Arena::new(),
            exprs: Arena::new(),
        };

        let mut controls = ControlStack::new();
        let mut operands = OperandStack::new();

        let params: Vec<_> = ty.params().iter().map(ValType::from).collect();
        let result: Vec<_> = ty
            .return_type()
            .as_ref()
            .into_iter()
            .map(ValType::from)
            .collect();
        push_control(&mut func, &mut controls, &mut operands, params, result);

        for op in body.code().elements() {
            validate_opcode(&mut func, &mut operands, &mut controls, op)?;
        }
        Ok(func)
    }
}
