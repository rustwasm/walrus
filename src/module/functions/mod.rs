//! Functions within a wasm module.

mod local_function;

use crate::dot::Dot;
use crate::error::{ErrorKind, Result};
use crate::module::emit::{Emit, IdsToIndices};
use crate::module::imports::{ImportId, ImportKind};
use crate::module::locals::ModuleLocals;
use crate::module::Module;
use crate::passes::Used;
use crate::ty::TypeId;
use crate::validation_context::ValidationContext;
use failure::Fail;
use id_arena::{Arena, Id};
use parity_wasm::elements;
use std::collections::{BTreeMap, HashMap};
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
}

impl Function {
    fn new_uninitialized(id: FunctionId, ty: TypeId) -> Function {
        Function {
            id,
            kind: FunctionKind::Uninitialized(ty),
        }
    }

    /// Create a new externally defined, imported function.
    pub fn new_import(id: FunctionId, import: ImportId) -> Function {
        Function {
            id,
            kind: FunctionKind::Import(ImportedFunction { import }),
        }
    }

    /// Create a new function that is locally defined within the wasm module.
    pub fn parse_local(
        module: &mut Module,
        id: FunctionId,
        ty: TypeId,
        validation: &ValidationContext,
        body: &elements::FuncBody,
    ) -> Result<Function> {
        let local = LocalFunction::parse(module, id, ty, validation, body)?;
        Ok(Function {
            id,
            kind: FunctionKind::Local(local),
        })
    }

    /// Get this function's identifier.
    pub fn id(&self) -> FunctionId {
        self.id
    }

    /// Get this function's type's identifier.
    pub fn ty(&self, module: &Module) -> TypeId {
        match self.kind {
            FunctionKind::Local(ref l) => l.ty,
            FunctionKind::Import(ImportedFunction { import }) => {
                match module.imports[import].kind {
                    ImportKind::Function { ty } => ty,
                    _ => panic!(
                        "imported function referencing import that is importing something else"
                    ),
                }
            }
            FunctionKind::Uninitialized(_) => unreachable!(),
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
    pub arena: Arena<Function>,

    index_to_function_id: HashMap<u32, FunctionId>,
}

impl ModuleFunctions {
    /// Construct a new, empty set of functions for a module.
    pub fn new() -> ModuleFunctions {
        Default::default()
    }

    /// Get the function for the given index in the input wasm, if any exists.
    pub fn function_for_index(&self, index: u32) -> Option<FunctionId> {
        self.index_to_function_id.get(&index).cloned()
    }

    /// Associate the given function with the given index in the input wasm.
    fn add_function_for_index(&mut self, index: u32, id: FunctionId) {
        let old = self.index_to_function_id.insert(index, id);
        assert!(old.is_none());
    }
}

impl Module {
    /// Add the externally defined imported functions in the wasm module to this
    /// instance.
    pub fn register_imported_functions(&mut self) {
        assert_eq!(
            self.funcs.arena.len(),
            0,
            "should not already have any functions"
        );
        let mut idx = 0;
        for (imp_id, imp) in self.imports.iter() {
            if let ImportKind::Function { .. } = imp.kind {
                let id = self.funcs.arena.next_id();
                let id2 = self.funcs.arena.alloc(Function::new_import(id, imp_id));
                debug_assert_eq!(id, id2);

                self.funcs.add_function_for_index(idx, id);
                idx += 1;
            }
        }
    }

    /// Declare local functions after seeing the `function` section of a wasm
    /// executable.
    pub fn declare_local_functions(
        &mut self,
        func_section: &elements::FunctionSection,
    ) -> Result<()> {
        let import_count = self.funcs.arena.len() as u32;

        for (i, func) in func_section.entries().iter().enumerate() {
            let ty = self.types.type_for_index(func.type_ref()).ok_or_else(|| {
                ErrorKind::InvalidWasm
                    .context("function's type is an out-of-bounds reference into the types section")
            })?;
            let id = self.funcs.arena.next_id();
            let id2 = self.funcs.arena.alloc(Function::new_uninitialized(id, ty));
            debug_assert_eq!(id, id2);
            self.funcs
                .add_function_for_index(i as u32 + import_count, id);
        }

        Ok(())
    }

    /// Add the locally defined functions in the wasm module to this instance.
    pub fn parse_local_functions(
        &mut self,
        raw_module: &elements::Module,
        code_section: &elements::CodeSection,
    ) -> Result<()> {
        let num_imports = self.funcs.arena.len() - code_section.bodies().len();
        let validation = ValidationContext::for_module(raw_module)?;

        for (i, body) in code_section.bodies().iter().enumerate() {
            let index = (num_imports + i) as u32;
            let id = self
                .funcs
                .function_for_index(index)
                .ok_or_else(|| failure::format_err!("code and function section length mismatch"))?;
            let ty = match self.funcs.arena[id].kind {
                FunctionKind::Uninitialized(ty) => ty,
                _ => unreachable!(),
            };
            self.funcs.arena[id] = Function::parse_local(self, id, ty, &validation, body)?;
        }

        Ok(())
    }
}

impl Emit for ModuleFunctions {
    type Extra = ModuleLocals;

    fn emit(
        &self,
        locals: &ModuleLocals,
        used: &Used,
        module: &mut elements::Module,
        indices: &mut IdsToIndices,
    ) {
        // Partition used functions into two sets: imported and local
        // functions. Find the size of each local function. Sort imported
        // functions in order so that we can get their index in the function
        // index space.
        let mut sizes = HashMap::new();
        let mut imports = BTreeMap::new();
        for (id, f) in &self.arena {
            if !used.funcs.contains(&id) {
                continue;
            }
            match f.kind {
                FunctionKind::Local(ref l) => {
                    let old = sizes.insert(id, l.size());
                    assert!(old.is_none());
                }
                FunctionKind::Import(ref i) => {
                    let idx = indices.get_import_index(i.import);
                    let old = imports.insert(idx, id);
                    assert!(old.is_none());
                }
                FunctionKind::Uninitialized(_) => unreachable!(),
            }
        }

        // Associate each imported function with its index.
        let mut import_count = 0;
        for imp in imports.values() {
            indices.set_func_index(*imp, import_count);
            import_count += 1;
        }

        // Sort local functions from largest to smallest; we will emit them in
        // this order. This helps load times, since wasm engines generally use
        // the function as their level of granularity for parallelism. We want
        // larger functions compiled before smaller ones because they will take
        // longer to compile.
        let mut used_funcs: Vec<_> = self
            .arena
            .iter()
            .filter_map(|(id, f)| {
                if used.funcs.contains(&id) {
                    if let FunctionKind::Local(ref l) = f.kind {
                        return Some((id, l));
                    }
                }
                None
            })
            .collect();
        used_funcs.sort_by_key(|&(id, _f)| sizes[&id]);
        used_funcs.reverse();

        let mut funcs = Vec::with_capacity(used_funcs.len());
        let mut codes = Vec::with_capacity(used_funcs.len());

        for (id, func) in used_funcs {
            indices.set_func_index(id, funcs.len() as u32 + import_count);

            debug_assert!(used.types.contains(&func.ty));
            let ty_idx = indices.get_type_index(func.ty);
            funcs.push(elements::Func::new(ty_idx));

            let locals = func.emit_locals(locals, indices);
            let instructions = func.emit_instructions(indices);
            let instructions = elements::Instructions::new(instructions);
            codes.push(elements::FuncBody::new(locals, instructions));
        }

        assert_eq!(funcs.len(), codes.len());
        if codes.len() == 0 {
            return;
        }

        let funcs = elements::FunctionSection::with_entries(funcs);
        let funcs = elements::Section::Function(funcs);
        module.sections_mut().push(funcs);

        let codes = elements::CodeSection::with_bodies(codes);
        let codes = elements::Section::Code(codes);
        module.sections_mut().push(codes);
    }
}
