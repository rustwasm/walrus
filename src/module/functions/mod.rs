//! Functions within a wasm module.

mod local_function;

use crate::dot::Dot;
use crate::emit::{Emit, EmitContext};
use crate::error::Result;
use crate::module::imports::ImportId;
use crate::module::Module;
use crate::parse::IndicesToIds;
use crate::ty::TypeId;
use crate::ty::ValType;
use failure::bail;
use id_arena::{Arena, Id};
use parity_wasm::elements;
use rayon::prelude::*;
use std::cmp;
use std::fmt;

pub use self::local_function::LocalFunction;

// have generated impls from the `#[walrus_expr]` macro
pub(crate) use self::local_function::display::DisplayExpr;
pub(crate) use self::local_function::DotExpr;

/// A function identifier.
pub type FunctionId = Id<Function>;

/// A wasm function.
///
/// Either defined locally or externally and then imported; see `FunctionKind`.
#[derive(Debug)]
pub struct Function {
    // NB: Not public so that it can't get out of sync with the arena that this
    // function lives within.
    id: FunctionId,

    /// The kind of function this is.
    pub kind: FunctionKind,

    /// An optional name associated with this function
    pub name: Option<String>,
}

impl Function {
    fn new_uninitialized(id: FunctionId, ty: TypeId) -> Function {
        Function {
            id,
            kind: FunctionKind::Uninitialized(ty),
            name: None,
        }
    }

    /// Get this function's identifier.
    pub fn id(&self) -> FunctionId {
        self.id
    }

    /// Get this function's type's identifier.
    pub fn ty(&self) -> TypeId {
        match &self.kind {
            FunctionKind::Local(l) => l.ty,
            FunctionKind::Import(i) => i.ty,
            FunctionKind::Uninitialized(t) => *t,
        }
    }
}

impl Dot for Function {
    fn dot(&self, out: &mut String) {
        match &self.kind {
            FunctionKind::Import(i) => i.dot(out),
            FunctionKind::Local(l) => l.dot(out),
            FunctionKind::Uninitialized(_) => unreachable!(),
        }
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            FunctionKind::Import(i) => fmt::Display::fmt(i, f),
            FunctionKind::Local(l) => fmt::Display::fmt(l, f),
            FunctionKind::Uninitialized(_) => unreachable!(),
        }
    }
}

/// The local- or external-specific bits of a function.
#[derive(Debug)]
pub enum FunctionKind {
    /// An externally defined, imported wasm function.
    Import(ImportedFunction),

    /// A locally defined wasm function.
    Local(LocalFunction),

    /// A locally defined wasm function that we haven't parsed yet (but have
    /// reserved its id and associated it with its original input wasm module
    /// index). This should only exist within
    /// `ModuleFunctions::add_local_functions`.
    Uninitialized(TypeId),
}

impl FunctionKind {
    /// Get the underlying `FunctionKind::Local` or panic if this is not a local
    /// function.
    pub fn unwrap_local(&self) -> &LocalFunction {
        match *self {
            FunctionKind::Local(ref l) => l,
            _ => panic!("not a local function"),
        }
    }
}

/// An externally defined, imported function.
#[derive(Debug)]
pub struct ImportedFunction {
    /// The import that brings this function into the module.
    pub import: ImportId,
    /// The type signature of this imported function.
    pub ty: TypeId,
}

impl Dot for ImportedFunction {
    fn dot(&self, out: &mut String) {
        out.push_str("digraph {{ imported_function; }}");
    }
}

impl fmt::Display for ImportedFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Imported function")
    }
}

/// The set of functions within a module.
#[derive(Debug, Default)]
pub struct ModuleFunctions {
    /// The arena containing this module's functions.
    arena: Arena<Function>,
}

impl ModuleFunctions {
    /// Construct a new, empty set of functions for a module.
    pub fn new() -> ModuleFunctions {
        Default::default()
    }

    /// Create a new externally defined, imported function.
    pub fn add_import(&mut self, ty: TypeId, import: ImportId) -> FunctionId {
        let id = self.arena.next_id();
        self.arena.alloc(Function {
            id,
            kind: FunctionKind::Import(ImportedFunction { import, ty }),
            name: None,
        })
    }

    /// Create a new internally defined function
    pub fn add_local(&mut self, func: LocalFunction) -> FunctionId {
        let id = self.arena.next_id();
        self.arena.alloc(Function {
            id,
            kind: FunctionKind::Local(func),
            name: None,
        })
    }

    /// Gets a reference to a function given its id
    pub fn get(&self, id: FunctionId) -> &Function {
        &self.arena[id]
    }

    /// Gets a reference to a function given its id
    pub fn get_mut(&mut self, id: FunctionId) -> &mut Function {
        &mut self.arena[id]
    }

    /// Get a shared reference to this module's functions.
    pub fn iter(&self) -> impl Iterator<Item = &Function> {
        self.arena.iter().map(|(_, f)| f)
    }

    /// Get a shared reference to this module's functions.
    pub fn par_iter(&self) -> impl ParallelIterator<Item = &Function> {
        self.arena.par_iter().map(|(_, f)| f)
    }
}

impl Module {
    /// Declare local functions after seeing the `function` section of a wasm
    /// executable.
    pub(crate) fn declare_local_functions(
        &mut self,
        section: wasmparser::FunctionSectionReader,
        ids: &mut IndicesToIds,
    ) -> Result<()> {
        for func in section {
            let ty = ids.get_type(func?)?;
            let id = self.funcs.arena.next_id();
            self.funcs.arena.alloc(Function::new_uninitialized(id, ty));
            ids.push_func(id);
        }

        Ok(())
    }

    /// Add the locally defined functions in the wasm module to this instance.
    pub(crate) fn parse_local_functions(
        &mut self,
        section: wasmparser::CodeSectionReader,
        function_section_count: u32,
        indices: &mut IndicesToIds,
    ) -> Result<()> {
        let amt = section.get_count();
        if amt != function_section_count {
            bail!("code and function sections must have same number of entries")
        }
        let num_imports = self.funcs.arena.len() - (amt as usize);

        // First up serially create corresponding `LocalId` instances for all
        // functions as well as extract the operators parser for each function.
        // This is pretty tough to parallelize, but we can look into it later if
        // necessary and it's a bottleneck!
        let mut bodies = Vec::with_capacity(amt as usize);
        for (i, body) in section.into_iter().enumerate() {
            let body = body?;
            let index = (num_imports + i) as u32;
            let id = indices.get_func(index)?;
            let ty = match self.funcs.arena[id].kind {
                FunctionKind::Uninitialized(ty) => ty,
                _ => unreachable!(),
            };

            // First up, implicitly add locals for all function arguments. We also
            // record these in the function itself for later processing.
            let mut args = Vec::new();
            for ty in self.types.get(ty).params() {
                let local_id = self.locals.add(*ty);
                indices.push_local(id, local_id);
                args.push(local_id);
            }

            // WebAssembly local indices are 32 bits, so it's a validation error to
            // have more than 2^32 locals. Sure enough there's a spec test for this!
            let mut total = 0u32;
            for local in body.get_locals_reader()? {
                let (count, _) = local?;
                total = match total.checked_add(count) {
                    Some(n) => n,
                    None => bail!("can't have more than 2^32 locals"),
                };
            }

            // Now that we know we have a reasonable amount of locals, put them in
            // our map.
            for local in body.get_locals_reader()? {
                let (count, ty) = local?;
                let ty = ValType::parse(&ty)?;
                for _ in 0..count {
                    let local_id = self.locals.add(ty);
                    indices.push_local(id, local_id);
                }
            }

            let body = body.get_operators_reader()?;
            bodies.push((id, body, args, ty));
        }

        // Wasm modules can often have a lot of functions and this operation can
        // take some time, so parse all function bodies in parallel.
        let results = bodies
            .into_par_iter()
            .map(|(id, body, args, ty)| {
                LocalFunction::parse(self, indices, id, ty, args, body).map(|local| Function {
                    id,
                    kind: FunctionKind::Local(local),
                    name: None,
                })
            })
            .collect::<Vec<_>>();

        // After all the function bodies are collected and finished push them
        // into our function arena.
        for func in results {
            let func = func?;
            let id = func.id;
            self.funcs.arena[id] = func;
        }

        Ok(())
    }
}

impl Emit for ModuleFunctions {
    fn emit(&self, cx: &mut EmitContext) {
        // Extract all local functions because imported ones were already
        // emitted as part of the import sectin. Find the size of each local
        // function. Sort imported functions in order so that we can get their
        // index in the function index space.
        let mut functions = Vec::new();
        for (id, f) in &self.arena {
            if !cx.used.funcs.contains(&id) {
                continue;
            }
            match &f.kind {
                FunctionKind::Local(l) => functions.push((id, l, l.size())),
                FunctionKind::Import(_) => {}
                FunctionKind::Uninitialized(_) => unreachable!(),
            }
        }

        // Sort local functions from largest to smallest; we will emit them in
        // this order. This helps load times, since wasm engines generally use
        // the function as their level of granularity for parallelism. We want
        // larger functions compiled before smaller ones because they will take
        // longer to compile.
        functions.sort_by_key(|(id, _, size)| (cmp::Reverse(*size), *id));

        let mut funcs = Vec::with_capacity(functions.len());
        let mut codes = Vec::with_capacity(functions.len());

        // Assign an index to all local defined functions before we start
        // translating them. While translating they may refer to future
        // functions, so we'll need to have an index for it by that point.
        for (id, _func, _size) in functions.iter() {
            cx.indices.push_func(*id);
        }

        for (_id, func, _size) in functions {
            debug_assert!(cx.used.types.contains(&func.ty));
            let ty_idx = cx.indices.get_type_index(func.ty);
            funcs.push(elements::Func::new(ty_idx));

            let locals = func.emit_locals(&cx.module.locals, cx.indices);
            let instructions = func.emit_instructions(cx.indices);
            let instructions = elements::Instructions::new(instructions);
            codes.push(elements::FuncBody::new(locals, instructions));
        }

        assert_eq!(funcs.len(), codes.len());
        if codes.is_empty() {
            return;
        }

        let funcs = elements::FunctionSection::with_entries(funcs);
        let funcs = elements::Section::Function(funcs);
        cx.dst.sections_mut().push(funcs);

        let codes = elements::CodeSection::with_bodies(codes);
        let codes = elements::Section::Code(codes);
        cx.dst.sections_mut().push(codes);
    }
}
