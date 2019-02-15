use crate::ir::*;
use crate::{FunctionId, LocalFunction, Module, TypeId, ValType};
use crate::{ModuleTypes, ModuleFunctions};
use id_arena::Arena;
use std::mem;
use std::ops::{Deref, DerefMut, Drop};

/// A helpful struct used for building instances of `LocalFunction`
#[derive(Default, Debug)]
pub struct FunctionBuilder {
    pub(crate) arena: Arena<Expr>,
}

impl FunctionBuilder {
    /// Creates a new blank builder ready to build a function.
    pub fn new() -> FunctionBuilder {
        FunctionBuilder::default()
    }

    pub(crate) fn alloc<T>(&mut self, val: T) -> T::Id
    where
        T: Ast,
    {
        let id = self.arena.alloc(val.into());
        T::new_id(id)
    }

    /// Create a `Block` node with the kind of `Block`.
    ///
    /// Note that instructions aren't passed here, but rather added to the
    /// returned `BlockBuilder` via the `expr` method.
    ///
    /// # Examples
    ///
    /// ```
    /// use walrus::FunctionBuilder;
    ///
    /// let mut builder = FunctionBuilder::new();
    /// let block = {
    ///     let mut block = builder.block(Box::new([]), Box::new([]));
    ///     let id = block.id();
    ///     let br = block.br(id, Box::new([]));
    ///     block.expr(br);
    ///     id
    /// }; // when `block` is `Drop`'d it'll fill in the node's expressions
    ///
    /// // ...
    /// ```
    pub fn block<'a>(
        &'a mut self,
        params: Box<[ValType]>,
        results: Box<[ValType]>,
    ) -> BlockBuilder<'a> {
        self.block_builder(Block {
            kind: BlockKind::Block,
            params,
            results,
            exprs: Vec::new(),
        })
    }

    /// Create a `Block` node with the kind of `Loop`
    pub fn loop_<'a>(&'a mut self, results: Box<[ValType]>) -> BlockBuilder<'a> {
        self.block_builder(Block {
            kind: BlockKind::Block,
            params: Vec::new().into_boxed_slice(),
            results,
            exprs: Vec::new(),
        })
    }

    /// Create a `Block` node with the kind of `IfElse`
    pub fn if_else_block<'a>(
        &'a mut self,
        params: Box<[ValType]>,
        results: Box<[ValType]>,
    ) -> BlockBuilder<'a> {
        self.block_builder(Block {
            kind: BlockKind::IfElse,
            params,
            results,
            exprs: Vec::new(),
        })
    }

    fn block_builder<'a>(&'a mut self, block: Block) -> BlockBuilder<'a> {
        let id = self.alloc(block);
        BlockBuilder {
            id,
            builder: self,
            exprs: Vec::new(),
        }
    }

    /// Creates an `i32.const` instruction for the specified value
    pub fn i32_const(&mut self, val: i32) -> ExprId {
        self.const_(Value::I32(val))
    }

    /// Creates an `i64.const` instruction for the specified value
    pub fn i64_const(&mut self, val: i64) -> ExprId {
        self.const_(Value::I64(val))
    }

    /// Creates an `f32.const` instruction for the specified value
    pub fn f32_const(&mut self, val: f32) -> ExprId {
        self.const_(Value::F32(val))
    }

    /// Creates an `f64.const` instruction for the specified value
    pub fn f64_const(&mut self, val: f64) -> ExprId {
        self.const_(Value::F64(val))
    }

    /// Finishes this builder, wrapping it all up and inserting it into the
    /// specified `Module`.
    pub fn finish(
        self,
        ty_id: TypeId,
        args: Vec<LocalId>,
        exprs: Vec<ExprId>,
        module: &mut Module,
    ) -> FunctionId {
        self.finish_parts(ty_id, args, exprs, &mut module.types, &mut module.funcs)
    }

    /// Finishes this builder, wrapping it all up and inserting it into the
    /// specified `Module`.
    pub fn finish_parts(
        mut self,
        ty_id: TypeId,
        args: Vec<LocalId>,
        exprs: Vec<ExprId>,
        types: &mut ModuleTypes,
        funcs: &mut ModuleFunctions,
    ) -> FunctionId {
        let ty = types.get(ty_id);
        let entry = self.alloc(Block {
            kind: BlockKind::FunctionEntry,
            params: ty.params().to_vec().into_boxed_slice(),
            results: ty.results().to_vec().into_boxed_slice(),
            exprs,
        });
        let func = LocalFunction::new(ty_id, args, self, entry);
        funcs.add_local(func)
    }
}

/// A builder returned by block-construction methods to build up block
/// expressions over time.
#[derive(Debug)]
pub struct BlockBuilder<'a> {
    id: BlockId,
    builder: &'a mut FunctionBuilder,
    exprs: Vec<ExprId>,
}

impl BlockBuilder<'_> {
    /// Pushes a new expression to be executed in the block we're building
    pub fn expr(&mut self, expr: ExprId) {
        self.exprs.push(expr);
    }

    /// Returns the id of the block that we're building
    pub fn id(&self) -> BlockId {
        self.id
    }
}

impl Deref for BlockBuilder<'_> {
    type Target = FunctionBuilder;

    fn deref(&self) -> &FunctionBuilder {
        &*self.builder
    }
}

impl DerefMut for BlockBuilder<'_> {
    fn deref_mut(&mut self) -> &mut FunctionBuilder {
        &mut *self.builder
    }
}

impl Drop for BlockBuilder<'_> {
    fn drop(&mut self) {
        let exprs = mem::replace(&mut self.exprs, Vec::new());
        let block = match &mut self.builder.arena[self.id.into()] {
            Expr::Block(b) => b,
            _ => unreachable!(),
        };
        block.exprs = exprs;
    }
}
