//! A high-level API for manipulating wasm modules.

pub mod data;
pub mod elements;
pub mod exports;
pub mod functions;
pub mod globals;
pub mod imports;
pub mod locals;
pub mod memories;
pub mod producers;
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
use crate::module::producers::ModuleProducers;
use crate::module::tables::ModuleTables;
use crate::module::types::ModuleTypes;
use crate::parse::IndicesToIds;
use crate::passes;
use failure::{bail, ResultExt};
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
    custom: Vec<CustomSection>,
    name: Option<String>,
    pub(crate) producers: ModuleProducers,
}

#[derive(Debug)]
struct CustomSection {
    name: String,
    value: Vec<u8>,
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

        let module = match parity::Module::deserialize(&mut wasm)?.parse_names() {
            Ok(m) => m,
            Err((_, m)) => m,
        };
        if wasm.len() > 0 {
            failure::bail!("invalid wasm file");
        }

        let mut ret = Module::default();
        let mut indices = IndicesToIds::default();

        for section in module.sections() {
            use parity_wasm::elements::NameSection;
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

                Section::Unparsed { .. } => bail!("failed to handle unparsed section"),
                Section::Custom(s) => {
                    // we attempted to parse the custom name sections above, so
                    // if it comes out here then it means the section may be
                    // malformed according to parity-wasm, so skip it.
                    if s.name() == "name" {
                        continue;
                    }
                    if s.name() == "producers" {
                        match ModuleProducers::parse(s.payload()) {
                            Ok(s) => ret.producers = s,
                            Err(e) => {
                                log::warn!("failed to parse producers section {}", e);
                            }
                        }
                        continue;
                    }
                    ret.custom.push(CustomSection {
                        name: s.name().to_string(),
                        value: s.payload().to_vec(),
                    });
                }
                Section::Name(NameSection::Module(s)) => {
                    ret.name = Some(s.name().to_string());
                }
                Section::Name(NameSection::Function(s)) => {
                    for (index, name) in s.names() {
                        let id = indices.get_func(index)?;
                        ret.funcs.get_mut(id).name = Some(name.clone());
                    }
                }
                Section::Name(NameSection::Local(s)) => {
                    for (func_index, names) in s.local_names() {
                        let func_id = indices.get_func(func_index)?;
                        for (local_index, name) in names {
                            let id = indices.get_local(func_id, local_index)?;
                            ret.locals.get_mut(id).name = Some(name.clone());
                        }
                    }
                }
                Section::Name(NameSection::Unparsed {
                    name_type,
                    name_payload: _,
                }) => {
                    bail!("unparsed name section of type {}", name_type);
                }
                Section::Reloc(_) => bail!("cannot handle the reloc section"),
            }
        }

        ret.producers
            .add_processed_by("walrus", env!("CARGO_PKG_VERSION"));

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

        emit_module_name_section(&mut cx);
        emit_function_name_section(&mut cx);
        emit_local_name_section(&mut cx);

        let producers = parity::CustomSection::new("producers".to_string(), self.producers.emit());
        module
            .sections_mut()
            .push(parity::Section::Custom(producers));

        for section in self.custom.iter() {
            let section = parity::CustomSection::new(section.name.clone(), section.value.clone());
            module.sections_mut().push(parity::Section::Custom(section));
        }

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

fn emit_module_name_section(cx: &mut EmitContext) {
    let name = match &cx.module.name {
        Some(name) => name,
        None => return,
    };
    let section = parity::ModuleNameSection::new(name.clone());
    let section = parity::NameSection::Module(section);
    cx.dst.sections_mut().push(parity::Section::Name(section));
}

fn emit_function_name_section(cx: &mut EmitContext) {
    let mut map = parity::NameMap::default();
    for id in cx.used.funcs.iter() {
        let name = match &cx.module.funcs.get(*id).name {
            Some(name) => name,
            None => continue,
        };
        map.insert(cx.indices.get_func_index(*id), name.clone());
    }
    if map.len() > 0 {
        let mut section = parity::FunctionNameSection::default();
        *section.names_mut() = map;
        let section = parity::NameSection::Function(section);
        cx.dst.sections_mut().push(parity::Section::Name(section));
    }
}

fn emit_local_name_section(cx: &mut EmitContext) {
    let mut map = parity::IndexMap::default();
    for id in cx.used.funcs.iter() {
        let mut locals = parity::NameMap::default();

        if let Some(set) = cx.used.locals.get(id) {
            for local_id in set {
                let name = match &cx.module.locals.get(*local_id).name {
                    Some(name) => name,
                    None => continue,
                };
                locals.insert(cx.indices.get_local_index(*local_id), name.clone());
            }
        }

        if locals.len() > 0 {
            map.insert(cx.indices.get_func_index(*id), locals);
        }
    }
    if map.len() > 0 {
        let mut section = parity::LocalNameSection::default();
        *section.local_names_mut() = map;
        let section = parity::NameSection::Local(section);
        cx.dst.sections_mut().push(parity::Section::Name(section));
    }
}
