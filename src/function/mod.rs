//! TODO

mod context;

use self::context::FunctionContext;
use super::arena::Arena;
use super::dot::Dot;
use super::error::{ErrorKind, Result};
use super::validation_context::ValidationContext;
use super::ValType;
use crate::ast::{Block, Expr};
use failure::{Fail, ResultExt};
use parity_wasm::elements::{self, Instruction};
use std::io::{self, Write};

/// TODO
#[derive(Debug)]
pub struct Function {
    pub(crate) exprs: Arena<Expr>,
    pub(crate) blocks: Arena<Block>,
    // TODO: provenance: ExprId -> offset in code section of the original
    // instruction
}

impl Function {
    /// TODO
    pub fn new(
        validation: &ValidationContext,
        types: &elements::TypeSection,
        func: &elements::Func,
        body: &elements::FuncBody,
    ) -> Result<Function> {
        let validation = validation.for_function(func, body)?;

        let ty = func.type_ref();
        let ty = &types.types().get(ty as usize).ok_or_else(|| {
            ErrorKind::InvalidWasm
                .context("function's type is an out-of-bounds reference into the types section")
        })?;
        let ty = match ty {
            elements::Type::Function(f) => f,
        };

        let mut func = Function {
            blocks: Arena::new(),
            exprs: Arena::new(),
        };

        let mut ctx = FunctionContext::new(&mut func, &validation);

        let params: Vec<_> = ty.params().iter().map(ValType::from).collect();
        let result: Vec<_> = ty
            .return_type()
            .as_ref()
            .into_iter()
            .map(ValType::from)
            .collect();

        ctx.push_control("function entry", params, result);

        for op in body.code().elements() {
            validate_opcode(&mut ctx, op)?;
        }

        // Assert that every block ends in some sort of jump, branch, or return.
        if cfg!(debug_assertions) {
            for (_, block) in &func.blocks {
                if let Some(last) = block.exprs.last().cloned() {
                    assert!(
                        func.exprs.get(last).unwrap().is_jump(),
                        "every block should end in a jump"
                    );
                }
            }
        }

        Ok(func)
    }
}

impl Dot for Function {
    fn dot(&self, out: &mut Write) -> io::Result<()> {
        writeln!(out, "digraph {{")?;
        writeln!(out, "rankdir=LR;")?;
        for expr in &self.exprs {
            expr.dot(out)?;
        }
        for block in &self.blocks {
            block.dot(out)?;
        }
        writeln!(out, "}}")
    }
}

fn validate_opcode(ctx: &mut FunctionContext, opcode: &Instruction) -> Result<()> {
    match opcode {
        Instruction::GetLocal(n) => {
            let ty = ctx.validation.local(*n).context("invalid get_local")?;
            let expr = ctx.func.exprs.alloc(Expr::GetLocal { ty, local: *n });
            ctx.push_operand(Some(ty), expr);
        }
        Instruction::SetLocal(n) => {
            let ty = ctx.validation.local(*n).context("invalid set_local")?;
            let (_, value) = ctx.pop_operand_expected(Some(ty))?;
            let expr = ctx.func.exprs.alloc(Expr::SetLocal {
                ty,
                local: *n,
                value,
            });
            ctx.add_to_current_frame_block(expr);
        }
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
        Instruction::I32Mul => {
            let (_, e1) = ctx.pop_operand_expected(Some(ValType::I32))?;
            let (_, e2) = ctx.pop_operand_expected(Some(ValType::I32))?;
            let expr = ctx.func.exprs.alloc(Expr::I32Mul(e1, e2));
            ctx.push_operand(Some(ValType::I32), expr);
        }
        Instruction::I32Eqz => {
            let (_, e) = ctx.pop_operand_expected(Some(ValType::I32))?;
            let expr = ctx.func.exprs.alloc(Expr::I32Eqz(e));
            ctx.push_operand(Some(ValType::I32), expr);
        }
        Instruction::Drop => {
            let (_, e) = ctx.pop_operand()?;
            let expr = ctx.func.exprs.alloc(Expr::Drop(e));
            ctx.add_to_current_frame_block(expr);
        }
        Instruction::Select => {
            let (_, condition) = ctx.pop_operand_expected(Some(ValType::I32))?;
            let (t1, consequent) = ctx.pop_operand()?;
            let (t2, alternative) = ctx.pop_operand_expected(t1)?;
            let expr = ctx.func.exprs.alloc(Expr::Select {
                condition,
                consequent,
                alternative,
            });
            ctx.push_operand(t2, expr);
        }
        Instruction::Unreachable => {
            let expr = ctx.func.exprs.alloc(Expr::Unreachable);
            ctx.unreachable(expr);
            ctx.add_to_current_frame_block(expr);
        }
        Instruction::Block(block_ty) => {
            let t = ValType::from_block_ty(block_ty);
            let block = ctx.push_control("block", t.clone(), t.clone());
            let expr = ctx.func.exprs.alloc(Expr::Br {
                block,
                args: vec![].into_boxed_slice(),
            });
            ctx.add_to_frame_block(1, expr);

            let continuation = ctx.func.blocks.alloc(Block::new(
                "block continuation",
                t.clone().into_boxed_slice(),
                t.into_boxed_slice(),
            ));
            ctx.control_mut(1).continuation = Some(continuation);
        }
        Instruction::Loop(block_ty) => {
            let t = ValType::from_block_ty(block_ty);
            let block = ctx.push_control("loop", vec![], t.clone());
            let expr = ctx.func.exprs.alloc(Expr::Loop(block));
            ctx.add_to_frame_block(1, expr);
            let continuation = ctx.func.blocks.alloc(Block::new(
                "post-loop continuation",
                vec![].into_boxed_slice(),
                t.into_boxed_slice(),
            ));
            ctx.control_mut(1).continuation = Some(continuation);
        }
        Instruction::If(block_ty) => {
            let (_, condition) = ctx.pop_operand_expected(Some(ValType::I32))?;
            let ty = ValType::from_block_ty(block_ty);
            let consequent = ctx.push_control("consequent", ty.clone(), ty.clone());
            ctx.control_mut(1).if_else = Some((condition, consequent));
            let continuation = ctx.func.blocks.alloc(Block::new(
                "if/else continuation",
                ty.clone().into_boxed_slice(),
                ty.into_boxed_slice(),
            ));
            ctx.control_mut(1).continuation = Some(continuation);
        }
        Instruction::End => {
            let results = ctx.pop_control()?;
            let expr = ctx.func.exprs.alloc(Expr::Phi);
            ctx.push_operands(&results, expr);
        }
        Instruction::Else => {
            let entry_block = ctx.control(1).block;
            let results = ctx.pop_control()?;
            let (condition, consequent) = ctx
                .controls
                .last_mut()
                .unwrap()
                .if_else
                .take()
                .ok_or_else(|| {
                    ErrorKind::InvalidWasm.context("`else` that was not preceded by an `if`")
                })?;
            let alternative = ctx.push_control("alternative", results.clone(), results);
            let expr = ctx.func.exprs.alloc(Expr::IfElse {
                condition,
                consequent,
                alternative,
            });
            ctx.add_to_block(entry_block, expr);
        }
        Instruction::Br(n) => {
            let n = *n as usize;
            if ctx.controls.len() < n {
                return Err(ErrorKind::InvalidWasm
                    .context("attempt to branch to out-of-bounds block")
                    .into());
            }
            let expected = ctx.control(n).label_types.clone();
            let args = ctx.pop_operands(&expected)?.into_boxed_slice();
            let block = ctx.control(n).block;
            let expr = ctx.func.exprs.alloc(Expr::Br { block, args });
            ctx.add_to_current_frame_block(expr);
        }
        Instruction::BrIf(n) => {
            let n = *n as usize;
            if ctx.controls.len() < n {
                return Err(ErrorKind::InvalidWasm
                    .context("attempt to branch to out-of-bounds block")
                    .into());
            }

            let block = ctx.control(n).block;
            let (_, condition) = ctx.pop_operand_expected(Some(ValType::I32))?;
            let expected = ctx.control(n).label_types.clone();
            let args = ctx.pop_operands(&expected)?.into_boxed_slice();
            let expr = ctx.func.exprs.alloc(Expr::BrIf {
                condition,
                block,
                args,
            });
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
            let default = ctx.controls[ctx.controls.len() - table.default as usize].block;

            let mut blocks = Vec::with_capacity(table.table.len());
            for n in table.table.iter() {
                let n = *n as usize;
                if ctx.controls.len() < n {
                    return Err(ErrorKind::InvalidWasm
                        .context("attempt to jump to an out-of-bounds block from a table entry")
                        .into());
                }
                if ctx.control(n).label_types
                    != ctx.controls[ctx.controls.len() - table.default as usize].label_types
                {
                    return Err(ErrorKind::InvalidWasm
                        .context(
                            "attempt to jump to block non-matching label types from a table entry",
                        )
                        .into());
                }
                blocks.push(ctx.control(n).block);
            }
            let blocks = blocks.into_boxed_slice();

            let (_, which) = ctx.pop_operand_expected(Some(ValType::I32))?;

            let expected = ctx.controls[ctx.controls.len() - table.default as usize]
                .label_types
                .clone();

            let args = ctx.pop_operands(&expected)?.into_boxed_slice();
            let expr = ctx.func.exprs.alloc(Expr::BrTable {
                which,
                blocks,
                default,
                args,
            });

            ctx.unreachable(expr);
            ctx.add_to_current_frame_block(expr);
        }

        op => unimplemented!("Have not implemented support for opcode yet: {:?}", op),
    }

    Ok(())
}
