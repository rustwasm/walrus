//! A high-level API for manipulating wasm modules.

pub mod elements;
pub(crate) mod emit;
pub mod exports;
pub mod functions;
pub mod globals;
pub mod imports;
pub mod locals;
pub mod memories;
pub mod tables;
pub mod types;

use self::elements::ModuleElements;
use self::emit::{Emit, IdsToIndices};
use self::exports::ModuleExports;
use self::functions::Function;
use self::functions::ModuleFunctions;
use self::globals::ModuleGlobals;
use self::imports::ModuleImports;
use self::locals::ModuleLocals;
use self::memories::ModuleMemories;
use self::tables::ModuleTables;
use self::types::ModuleTypes;
use crate::error::Result;
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
    // TODO: make this an internal type, not a parity-wasm type
    pub(crate) data: parity::DataSection,
    pub(crate) elements: ModuleElements,
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

        for section in module.sections() {
            use parity_wasm::elements::Section;

            match section {
                Section::Data(s) => ret.data = s.clone(),
                Section::Type(s) => ret.types = ModuleTypes::parse(s),
                Section::Import(s) => {
                    ret.imports = ModuleImports::parse(&ret.types, s)?;
                    ret.register_imported_functions();
                }
                Section::Table(s) => ret.tables = ModuleTables::parse(s),
                Section::Memory(s) => ret.memories = ModuleMemories::parse(s),
                Section::Global(s) => ret.globals = ModuleGlobals::parse(&module, s)?,
                Section::Function(s) => ret.declare_local_functions(s)?,
                Section::Code(s) => ret.parse_local_functions(&module, s)?,
                Section::Export(s) => ret.exports = ModuleExports::parse(&ret, s)?,
                Section::Element(s) => ret.elements = ModuleElements::parse(&mut ret, s)?,

                // TODO: handle these
                Section::Unparsed { .. } => {}
                Section::Custom(_) => {}
                Section::Start(_) => {}
                Section::Name(_) => {}
                Section::Reloc(_) => {}
            }
        }

        Ok(ret)
    }

    /// Get a shared reference to this module's functions.
    pub fn functions(&self) -> impl Iterator<Item = &Function> {
        self.funcs.arena.iter().map(|(_, f)| f)
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
        let roots = self.exports.arena.iter().map(|(id, _)| id);
        let used = passes::Used::new(self, roots);
        let data = parity::Section::Data(self.data.clone());

        let indices = &mut IdsToIndices::default();
        let mut module = parity::Module::new(vec![data]);

        self.types.emit(&(), &used, &mut module, indices);
        self.imports.emit(&(), &used, &mut module, indices);
        self.tables.emit(&(), &used, &mut module, indices);
        self.memories.emit(&(), &used, &mut module, indices);
        self.globals.emit(&(), &used, &mut module, indices);
        self.funcs.emit(&self.locals, &used, &mut module, indices);
        self.exports.emit(&(), &used, &mut module, indices);
        // TODO: start section
        self.elements
            .emit(&self.tables, &used, &mut module, indices);

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
}
