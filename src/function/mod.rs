//! TODO

mod context;

use self::context::FunctionContext;
use super::arena::Arena;
use super::error::{ErrorKind, Result};
use super::validation_context::ValidationContext;
use super::{Block, Expr, ValType};
use failure::{Fail, ResultExt};
use parity_wasm::elements::{self, Instruction};

/// TODO
#[derive(Debug)]
pub struct Function {
    exprs: Arena<Expr>,
    blocks: Arena<Block>,
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

        ctx.push_control(params, result);

        for op in body.code().elements() {
            validate_opcode(&mut ctx, op)?;
        }

        Ok(func)
    }
}

fn validate_opcode(ctx: &mut FunctionContext, opcode: &Instruction) -> Result<()> {
    match opcode {
        Instruction::GetLocal(n) => {
            let t = ctx.validation.local(*n).context("invalid get_local")?;
            let expr = ctx.func.exprs.alloc(Expr::GetLocal(t, *n));
            ctx.push_operand(Some(t), expr);
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
