//! TODO

mod context;
pub mod display;

use self::context::FunctionContext;
use super::arena::Arena;
use super::dot::Dot;
use super::error::{ErrorKind, Result};
use super::validation_context::ValidationContext;
use super::ValType;
use crate::ir::{Block, BlockId, Expr, ExprId};
use failure::{Fail, ResultExt};
use parity_wasm::elements::{self, Instruction};
use std::fmt;
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

        let result: Vec<_> = ty
            .return_type()
            .as_ref()
            .into_iter()
            .map(ValType::from)
            .collect();

        let operands = &mut context::OperandStack::new();
        let controls = &mut context::ControlStack::new();

        let mut ctx = FunctionContext::new(&mut func, &validation, operands, controls);

        let func_exit = ctx.func.blocks.alloc(Block::new(
            "function exit",
            result.clone().into_boxed_slice(),
        ));
        ctx.push_control("function entry", vec![], result.clone(), func_exit);
        let values =
            validate_expression(&mut ctx, body.code().elements(), func_exit)?.into_boxed_slice();

        ctx.func.finish_block(func_exit, Expr::Return { values });

        if cfg!(debug_assertions) {
            assert_eq!(ctx.operands.len(), result.len());
            assert!(ctx.controls.is_empty());

            // Assert that every block ends in some sort of jump, branch, or
            // return.
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

    fn finish_block(&mut self, block: BlockId, expr: Expr) {
        assert!(expr.is_jump());
        let block = self.blocks.get_mut(block).unwrap();
        match block.exprs.last() {
            Some(e) => {
                if self.exprs.get(*e).unwrap().is_jump() {
                    return;
                }
            }
            _ => {}
        }
        let expr = self.exprs.alloc(expr);
        block.exprs.push(expr);
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::display::DisplayIr;
        self.display_ir(f, &())
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

macro_rules! const_ {
    ($ctx:ident, $op:ident, $ty:ident, $val:expr) => {
        let expr = $ctx.func.exprs.alloc(Expr::$op($val));
        $ctx.push_operand(Some(ValType::$ty), expr);
    };
}

macro_rules! binop {
    ($ctx:ident, $op:ident, $ty:ident) => {
        let (_, e1) = $ctx.pop_operand_expected(Some(ValType::$ty))?;
        let (_, e2) = $ctx.pop_operand_expected(Some(ValType::$ty))?;
        let expr = $ctx.func.exprs.alloc(Expr::$op(e2, e1));
        $ctx.push_operand(Some(ValType::$ty), expr);
    };
}

macro_rules! unop {
    ($ctx:ident, $op:ident, $ty:ident) => {
        let (_, e) = $ctx.pop_operand_expected(Some(ValType::$ty))?;
        let expr = $ctx.func.exprs.alloc(Expr::$op(e));
        $ctx.push_operand(Some(ValType::$ty), expr);
    };
}

macro_rules! testop {
    ($ctx:ident, $op:ident, $ty:ident) => {
        let (_, e) = $ctx.pop_operand_expected(Some(ValType::$ty))?;
        let expr = $ctx.func.exprs.alloc(Expr::$op(e));
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
                    .into())
            }
            Some(inst) if inst == &until => return Ok(&insts[1..]),
            Some(_) => {
                insts = validate_instruction(ctx, insts)?;
            }
        }
    }
}

fn validate_expression(
    ctx: &mut FunctionContext,
    expr: &[Instruction],
    continuation: BlockId,
) -> Result<Vec<ExprId>> {
    let rest = validate_instruction_sequence(ctx, expr, Instruction::End)?;
    let block = ctx.control(0).block;
    let exprs = validate_end(ctx)?;
    if rest.is_empty() {
        ctx.func.finish_block(
            block,
            Expr::Br {
                block: continuation,
                args: exprs.clone().into_boxed_slice(),
            },
        );
        Ok(exprs)
    } else {
        Err(ErrorKind::InvalidWasm
            .context("trailing instructions after final `end`")
            .into())
    }
}

fn validate_end(ctx: &mut FunctionContext) -> Result<Vec<ExprId>> {
    let (results, exprs) = ctx.pop_control()?;
    let phis: Vec<_> = results
        .iter()
        .map(|_| ctx.func.exprs.alloc(Expr::Phi))
        .collect();
    ctx.push_operands(&results, &phis);
    Ok(exprs)
}

fn validate_instruction<'a>(
    ctx: &mut FunctionContext,
    insts: &'a [Instruction],
) -> Result<&'a [Instruction]> {
    assert!(!insts.is_empty());
    match &insts[0] {
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
            let validation = ctx.validation.for_block(*block_ty);
            let mut ctx = ctx.nested(&validation);

            let entry_block = ctx.control(0).block;

            let params = ValType::from_block_ty(block_ty);
            let continuation = ctx.func.blocks.alloc(Block::new(
                "block continuation",
                params.clone().into_boxed_slice(),
            ));

            let block = ctx.push_control("block", params.clone(), params, continuation);

            ctx.func.finish_block(
                entry_block,
                Expr::Br {
                    block,
                    args: vec![].into_boxed_slice(),
                },
            );

            let rest = validate_instruction_sequence(&mut ctx, &insts[1..], Instruction::End)?;
            let values = validate_end(&mut ctx)?;

            ctx.func.finish_block(
                block,
                Expr::Br {
                    block: continuation,
                    args: values.into_boxed_slice(),
                },
            );

            return Ok(rest);
        }
        Instruction::Loop(block_ty) => {
            let validation = ctx.validation.for_loop();
            let mut ctx = ctx.nested(&validation);

            let entry = ctx.control(0).block;

            let t = ValType::from_block_ty(block_ty);
            let continuation = ctx.func.blocks.alloc(Block::new(
                "post-loop continuation block",
                t.clone().into_boxed_slice(),
            ));

            let block = ctx.push_control("loop", vec![], t, continuation);

            let expr = ctx.func.exprs.alloc(Expr::Br {
                block,
                args: vec![].into_boxed_slice(),
            });
            ctx.add_to_block(entry, expr);

            let rest = validate_instruction_sequence(&mut ctx, &insts[1..], Instruction::End)?;
            validate_end(&mut ctx)?;

            // The loop block branches back to itself.
            ctx.func.finish_block(
                block,
                Expr::Br {
                    block,
                    args: vec![].into_boxed_slice(),
                },
            );

            return Ok(rest);
        }
        Instruction::If(block_ty) => {
            let validation = ctx.validation.for_if_else(*block_ty);
            let mut ctx = ctx.nested(&validation);

            let (_, condition) = ctx.pop_operand_expected(Some(ValType::I32))?;

            let entry_block = ctx.control(0).block;

            let ty = ValType::from_block_ty(block_ty);
            let continuation = ctx.func.blocks.alloc(Block::new(
                "if/else continuation",
                ty.clone().into_boxed_slice(),
            ));
            let consequent = ctx.push_control("consequent", ty.clone(), ty, continuation);

            let rest = validate_instruction_sequence(&mut ctx, &insts[1..], Instruction::Else)?;
            let (results, values) = ctx.pop_control()?;

            ctx.func.finish_block(
                consequent,
                Expr::Br {
                    block: continuation,
                    args: values.into_boxed_slice(),
                },
            );

            let alternative =
                ctx.push_control("alternative", results.clone(), results, continuation);

            let expr = ctx.func.exprs.alloc(Expr::IfElse {
                condition,
                consequent,
                alternative,
            });

            ctx.add_to_block(entry_block, expr);

            let rest_rest = validate_instruction_sequence(&mut ctx, rest, Instruction::End)?;
            let values = validate_end(&mut ctx)?;

            ctx.func.finish_block(
                alternative,
                Expr::Br {
                    block: continuation,
                    args: values.into_boxed_slice(),
                },
            );

            return Ok(rest_rest);
        }
        Instruction::End => {
            unreachable!();
            // TODO: this call site should encounter an `End` only ever at the
            // return point, I think?
            // validate_end(ctx)?;
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
            let expr = ctx.func.exprs.alloc(Expr::Br {
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
            let expr = ctx.func.exprs.alloc(Expr::BrIf {
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
            let default = ctx.controls[ctx.controls.len() - table.default as usize].block;

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

    Ok(&insts[1..])
}
