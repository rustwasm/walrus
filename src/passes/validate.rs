//! Validation of a wasm module
//!
//! Currently only does some basic sanity checks, but it's intended that
//! eventually this is a full typechecking pass!

use crate::ir::*;
use crate::ValType;
use crate::{Function, FunctionKind, InitExpr, Result};
use crate::{Global, GlobalKind, Memory, MemoryId, Module, Table, TableKind};
use failure::{bail, ResultExt};
use std::collections::HashSet;

#[cfg(feature = "parallel")]
use rayon::prelude::*;

/// Validate a wasm module, returning an error if it fails to validate.
pub fn run(module: &Module) -> Result<()> {
    log::debug!("validating module");

    if module.config.only_stable_features {
        if module.tables.iter().count() > 1 {
            bail!("multiple tables not allowed in the wasm spec yet");
        }
        if module.memories.iter().count() > 1 {
            bail!("multiple memories not allowed in the wasm spec yet");
        }
    }

    for memory in module.memories.iter() {
        validate_memory(memory)?;
    }
    for table in module.tables.iter() {
        validate_table(table)?;
    }
    for global in module.globals.iter() {
        validate_global(module, global)?;
    }
    validate_exports(module)?;

    // Validate the start function, if present, has the correct signature
    if let Some(start) = module.start {
        let ty = module.funcs.get(start).ty();
        let ty = module.types.get(ty);
        if ty.results().len() > 0 || ty.params().len() > 0 {
            bail!("start function must take no arguments and return nothing");
        }
    }

    // Validate each function in the module, collecting errors and returning
    // them all at once if there are any.
    let funcs = &module.funcs;
    let errs = maybe_parallel!(funcs.(iter | par_iter))
        .map(|function| {
            let mut errs = Vec::new();
            let local = match &function.kind {
                FunctionKind::Local(local) => local,
                _ => return Vec::new(),
            };
            let mut cx = Validate {
                errs: &mut errs,
                function,
                module,
            };
            dfs_in_order(&mut cx, local, local.entry_block());
            errs
        })
        .collect::<Vec<_>>();
    if errs.iter().all(|e| e.is_empty()) {
        return Ok(());
    }

    let mut msg = format!("errors validating module:\n");
    for error in errs.into_iter().flat_map(|v| v) {
        msg.push_str(&format!("  * {}\n", error));
        for cause in error.iter_causes() {
            msg.push_str(&format!("    * {}\n", cause));
        }
    }
    bail!("{}", msg)
}

fn validate_memory(m: &Memory) -> Result<()> {
    if m.shared && m.maximum.is_none() {
        bail!("shared memories must have a maximum size");
    }
    validate_limits(m.initial, m.maximum, u32::from(u16::max_value()) + 1)
        .context("when validating a memory")?;
    Ok(())
}

fn validate_table(t: &Table) -> Result<()> {
    validate_limits(t.initial, t.maximum, u32::max_value()).context("when validating a table")?;

    // Ensure that the table element type is `anyfunc`. This does
    // nothing, but if new wasm versions and future parity-wasm releases
    // get support for new table types, this may need to actually do
    // something.
    match t.kind {
        TableKind::Function(_) => {}
        TableKind::Anyref(_) => {}
    }
    Ok(())
}

fn validate_limits(initial: u32, maximum: Option<u32>, k: u32) -> Result<()> {
    match (initial, maximum) {
        (min, Some(max)) if max < min || max > k => {
            bail!("invalid limits: min = {}, max = {}; k = {}", min, max, k)
        }
        (min, _) => {
            if min <= k {
                Ok(())
            } else {
                bail!("invalid limits: min = {}, k = {}", min, k)
            }
        }
    }
}

fn validate_exports(module: &Module) -> Result<()> {
    // All exported names must be unique, so if there's any duplicate-named
    // exports then we generate an error
    let mut exports = HashSet::new();
    for export in module.exports.iter() {
        if !exports.insert(&export.name) {
            bail!("duplicate export of `{}`", export.name)
        }
    }
    Ok(())
}

fn validate_global(module: &Module, global: &Global) -> Result<()> {
    match global.kind {
        GlobalKind::Import(_) => return Ok(()),
        GlobalKind::Local(InitExpr::Value(value)) => {
            validate_value(value, global.ty).context("invalid type on global")?;
        }
        GlobalKind::Local(InitExpr::Global(other)) => {
            let other = module.globals.get(other);
            match other.kind {
                GlobalKind::Import(_) => {}
                GlobalKind::Local(_) => {
                    bail!("initializer for local global must be imported global");
                }
            }
            if other.ty != global.ty {
                bail!("locally defined global does not match type of import");
            }
        }
    }
    Ok(())
}

fn validate_value(value: Value, ty: ValType) -> Result<()> {
    match (value, ty) {
        (Value::I32(_), ValType::I32) => {}
        (Value::I64(_), ValType::I64) => {}
        (Value::F32(_), ValType::F32) => {}
        (Value::F64(_), ValType::F64) => {}
        (Value::V128(_), ValType::V128) => {}
        _ => bail!("mismatched types in value"),
    }
    Ok(())
}

struct Validate<'a> {
    errs: &'a mut Vec<failure::Error>,
    function: &'a Function,
    module: &'a Module,
}

impl Validate<'_> {
    fn memarg(&mut self, arg: &MemArg, width: u32) {
        // The alignment of a memory operation must be less than or equal to the
        // width of the memory operation, currently wasm doesn't allow
        // over-aligned memory ops.
        if arg.align > width {
            self.err("memory operation with alignment greater than natural size");
        }
    }

    fn require_shared(&mut self, m: MemoryId) {
        let mem = self.module.memories.get(m);
        if !mem.shared {
            self.err("atomic operations require a shared memory");
        }
    }

    fn require_atomic(&mut self, m: MemoryId, arg: &MemArg, width: u32) {
        self.require_shared(m);
        if arg.align != width {
            self.err("alignment for atomics must be same as natural width");
        }
    }

    fn err(&mut self, msg: &str) {
        let mut err = failure::format_err!("{}", msg);
        if let Some(name) = &self.function.name {
            err = err.context(format!("in function {}", name)).into();
        }
        self.errs.push(err);
    }
}

impl<'a> Visitor<'a> for Validate<'a> {
    fn visit_load(&mut self, e: &Load) {
        if e.kind.atomic() {
            self.require_atomic(e.memory, &e.arg, e.kind.width());
        } else {
            self.memarg(&e.arg, e.kind.width());
        }
    }

    fn visit_store(&mut self, e: &Store) {
        if e.kind.atomic() {
            self.require_atomic(e.memory, &e.arg, e.kind.width());
        } else {
            self.memarg(&e.arg, e.kind.width());
        }
    }

    fn visit_atomic_rmw(&mut self, e: &AtomicRmw) {
        self.require_atomic(e.memory, &e.arg, e.width.bytes());
    }

    fn visit_cmpxchg(&mut self, e: &Cmpxchg) {
        self.require_atomic(e.memory, &e.arg, e.width.bytes());
    }

    fn visit_atomic_notify(&mut self, e: &AtomicNotify) {
        self.require_shared(e.memory);
        // TODO: should alignment or things be validated?
    }

    fn visit_atomic_wait(&mut self, e: &AtomicWait) {
        let width = if e.sixty_four { 8 } else { 4 };
        self.require_atomic(e.memory, &e.arg, width);
    }
}
