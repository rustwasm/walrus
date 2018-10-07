extern crate failure;
extern crate parity_wasm;

pub mod arena;
pub mod error;

use self::error::{Error, ErrorKind};

use self::arena::{Arena, Id};
use failure::Fail;
use parity_wasm::elements::{self, Instruction};
use std::fmt;

pub type Result<T> = ::std::result::Result<T, failure::Error>;

pub struct Function {
    exprs: Arena<Expression>,
    blocks: Arena<Block>,
}

pub struct Block {
    exprs: Vec<Id<Expression>>,
}

pub enum Expression {
    Unreachable,
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
    unreachable: bool,
}

pub type OperandStack = Vec<Option<ValType>>;
pub type ControlStack = Vec<ControlFrame>;

fn push_operand(operands: &mut OperandStack, op: Option<ValType>) {
    operands.push(op);
}

fn pop_operand(operands: &mut OperandStack, controls: &ControlStack) -> Result<Option<ValType>> {
    if operands.len() == controls.last().unwrap().height && controls.last().unwrap().unreachable {
        return Ok(None);
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
) -> Result<Option<ValType>> {
    match (pop_operand(operands, controls)?, expected) {
        (None, expected) => Ok(expected),
        (actual, None) => Ok(actual),
        (Some(actual), Some(expected)) => {
            if actual != expected {
                Err(ErrorKind::InvalidWasm
                    .context(format!("expected type {}", expected))
                    .context(format!("found type {}", actual))
                    .into())
            } else {
                Ok(Some(actual))
            }
        }
    }
}

fn push_operands(operands: &mut OperandStack, types: &[ValType]) {
    for ty in types {
        push_operand(operands, Some(*ty));
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
    controls: &mut ControlStack,
    operands: &OperandStack,
    label: Vec<ValType>,
    out: Vec<ValType>,
) {
    let frame = ControlFrame {
        label_types: label,
        end_types: out,
        height: operands.len(),
        unreachable: false,
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

fn unreachable(operands: &mut OperandStack, controls: &mut ControlStack) {
    let frame = controls.last_mut().unwrap();
    frame.unreachable = true;
    let height = frame.height;

    operands.truncate(height);
    for _ in operands.len()..height {
        operands.push(None);
    }
}

fn validate_opcode(
    operands: &mut OperandStack,
    controls: &mut ControlStack,
    opcode: &Instruction,
) -> Result<()> {
    match opcode {
        Instruction::I32Add => {
            pop_operand_expected(operands, controls, Some(ValType::I32))?;
            pop_operand_expected(operands, controls, Some(ValType::I32))?;
            push_operand(operands, Some(ValType::I32));
        }
        Instruction::Drop => {
            pop_operand(operands, controls)?;
        }
        Instruction::Select => {
            pop_operand_expected(operands, controls, Some(ValType::I32))?;
            let t1 = pop_operand(operands, controls)?;
            let t2 = pop_operand_expected(operands, controls, t1)?;
            push_operand(operands, t2);
        }
        Instruction::Unreachable => {
            unreachable(operands, controls);
        }
        Instruction::Block(block_ty) => {
            let t = ValType::from_block_ty(block_ty);
            push_control(controls, operands, t.clone(), t);
        }
        Instruction::Loop(block_ty) => {
            let t = ValType::from_block_ty(block_ty);
            push_control(controls, operands, vec![], t);
        }
        Instruction::If(block_ty) => {
            pop_operand_expected(operands, controls, Some(ValType::I32))?;
            let t = ValType::from_block_ty(block_ty);
            push_control(controls, operands, t.clone(), t);
        }
        Instruction::Else | Instruction::End => {
            let results = pop_control(controls, operands)?;
            push_operands(operands, &results);
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
            if controls.len() < *n as usize {
                return Err(ErrorKind::InvalidWasm
                    .context("attempt to branch to out-of-bounds block")
                    .into());
            }
            pop_operand_expected(operands, controls, Some(ValType::I32))?;
            let expected = controls[controls.len() - *n as usize].label_types.clone();
            pop_operands(operands, controls, &expected)?;
            push_operands(operands, &expected);
        }
        Instruction::BrTable(table) => {
            if controls.len() < table.default as usize {
                return Err(ErrorKind::InvalidWasm
                    .context(
                        "attempt to jump to an out-of-bounds block from the default table entry",
                    )
                    .into());
            }
            for n in table.table.iter().cloned() {
                if controls.len() < n as usize {
                    return Err(ErrorKind::InvalidWasm
                        .context("attempt to jump to an out-of-bounds block from a table entry")
                        .into());
                }
                if controls[controls.len() - n as usize].label_types
                    != controls[controls.len() - table.default as usize].label_types
                {
                    return Err(ErrorKind::InvalidWasm
                        .context(
                            "attempt to jump to block non-matching label types from a table entry",
                        )
                        .into());
                }
            }
            pop_operand_expected(operands, controls, Some(ValType::I32))?;
            let expected = controls[controls.len() - table.default as usize]
                .label_types
                .clone();
            pop_operands(operands, controls, &expected)?;
            unreachable(operands, controls);
        }

        _ => unimplemented!(),
    }

    Ok(())
}

impl Function {
    pub fn new(ty: elements::FunctionType, body: elements::FuncBody) -> Result<Function> {
        // TODO: context and locals and all that.
        let mut func = Function {
            blocks: Arena::new(),
            exprs: Arena::new(),
        };
        let mut controls = ControlStack::new();
        let mut operands = OperandStack::new();
        for op in body.code().elements() {
            validate_opcode(&mut operands, &mut controls, op)?;
        }
        Ok(func)
    }
}
