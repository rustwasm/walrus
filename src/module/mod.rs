//! A high-level API for manipulating wasm modules.

pub mod data;
pub mod elements;
pub mod exports;
pub mod functions;
pub mod globals;
pub mod imports;
pub mod locals;
pub mod memories;
mod parse;
pub mod tables;
pub mod types;

use crate::emit::{Emit, EmitContext, IdsToIndices};
use crate::error::Result;
use crate::module::data::ModuleData;
use crate::module::elements::ModuleElements;
use crate::module::exports::ModuleExports;
use crate::module::functions::{Function, FunctionId, ModuleFunctions};
use crate::module::globals::ModuleGlobals;
use crate::module::imports::ModuleImports;
use crate::module::locals::ModuleLocals;
use crate::module::memories::ModuleMemories;
use crate::module::tables::ModuleTables;
use crate::module::types::ModuleTypes;
use crate::passes;
use failure::ResultExt;
use parity_wasm::elements as parity;
use std::fs;
use std::path::Path;

/// A wasm module.
#[derive(Debug, Default)]
pub struct Module {
    pub(crate) imports: ModuleImports,
    pub(crate) tables: ModuleTables,
    pub(crate) types: ModuleTypes,
    pub(crate) funcs: ModuleFunctions,
    pub(crate) globals: ModuleGlobals,
    pub(crate) locals: ModuleLocals,
    pub(crate) exports: ModuleExports,
    pub(crate) memories: ModuleMemories,
    pub(crate) data: ModuleData,
    pub(crate) elements: ModuleElements,
    pub(crate) start: Option<FunctionId>,
}

impl Module {
    /// Construct a new module.
    pub fn from_file<P>(path: P) -> Result<Module>
    where
        P: AsRef<Path>,
    {
        Module::from_buffer(&fs::read(path)?)
    }

    /// Construct a new module.
    pub fn from_buffer(mut wasm: &[u8]) -> Result<Module> {
        use parity_wasm::elements::Deserialize;

        let module = parity::Module::deserialize(&mut wasm)?;
        if wasm.len() > 0 {
            failure::bail!("invalid wasm file");
        }

        let mut ret = Module::default();
        let mut indices = parse::IndicesToIds::default();

        for section in module.sections() {
            use parity_wasm::elements::Section;

            match section {
                Section::Data(s) => ret.parse_data(s, &mut indices)?,
                Section::Type(s) => ret.parse_types(s, &mut indices),
                Section::Import(s) => ret.parse_imports(s, &mut indices)?,
                Section::Table(s) => ret.parse_tables(s, &mut indices),
                Section::Memory(s) => ret.parse_memories(s, &mut indices),
                Section::Global(s) => ret.parse_globals(s, &mut indices)?,
                Section::Function(s) => ret.declare_local_functions(s, &mut indices)?,
                Section::Code(s) => ret.parse_local_functions(&module, s, &mut indices)?,
                Section::Export(s) => ret.parse_exports(s, &mut indices)?,
                Section::Element(s) => ret.parse_elements(s, &mut indices)?,
                Section::Start(idx) => ret.start = Some(indices.get_func(*idx)?),

                // TODO: handle these
                Section::Unparsed { .. } => {}
                Section::Custom(_) => {}
                Section::Name(_) => {}
                Section::Reloc(_) => {}
            }
        }

        Ok(ret)
    }

    /// Emit this module into a `.wasm` file at the given path.
    pub fn emit_wasm_file<P>(&self, path: P) -> Result<()>
    where
        P: AsRef<Path>,
    {
        let buffer = self.emit_wasm()?;
        fs::write(path, buffer).context("failed to write wasm module")?;
        Ok(())
    }

    /// Emit this module into an in-memory wasm buffer.
    pub fn emit_wasm(&self) -> Result<Vec<u8>> {
        let roots = self.exports.iter();
        let used = passes::Used::new(self, roots.map(|e| e.id()));

        let indices = &mut IdsToIndices::default();
        let mut module = parity::Module::new(Vec::new());

        let mut cx = EmitContext {
            module: self,
            indices,
            used: &used,
            dst: &mut module,
        };
        self.types.emit(&mut cx);
        self.imports.emit(&mut cx);
        self.tables.emit(&mut cx);
        self.memories.emit(&mut cx);
        self.globals.emit(&mut cx);
        self.funcs.emit(&mut cx);
        self.exports.emit(&mut cx);
        if let Some(start) = self.start {
            let idx = cx.indices.get_func_index(start);
            cx.dst.sections_mut().push(parity::Section::Start(idx));
        }
        self.elements.emit(&mut cx);
        self.data.emit(&mut cx);

        module.sections_mut().sort_by_key(|s| match s {
            parity::Section::Type(_) => 1,
            parity::Section::Import(_) => 2,
            parity::Section::Function(_) => 3,
            parity::Section::Table(_) => 4,
            parity::Section::Memory(_) => 5,
            parity::Section::Global(_) => 6,
            parity::Section::Export(_) => 7,
            parity::Section::Start(_) => 8,
            parity::Section::Element(_) => 9,
            parity::Section::Code(_) => 10,
            parity::Section::Data(_) => 11,

            parity::Section::Custom(_)
            | parity::Section::Unparsed { .. }
            | parity::Section::Reloc(_)
            | parity::Section::Name(_) => 12,
        });
        let buffer =
            parity::serialize(module).context("failed to serialize wasm module to file")?;
        Ok(buffer)
    }

    /// Returns an iterator over all functions in this module
    pub fn functions(&self) -> impl Iterator<Item = &Function> {
        self.funcs.iter()
    }
}
