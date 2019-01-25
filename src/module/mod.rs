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
    pub fn from_buffer(wasm: &[u8]) -> Result<Module> {
        let mut parser = wasmparser::ModuleReader::new(wasm)?;
        if parser.get_version() != 1 {
            bail!("only support version 1 of wasm");
        }

        let mut ret = Module::default();
        let mut indices = IndicesToIds::default();
        let mut function_section_size = None;

        while !parser.eof() {
            let section = parser.read()?;
            match section.code {
                wasmparser::SectionCode::Data => {
                    log::debug!("parsing data section");
                    let reader = section.get_data_section_reader()?;
                    ret.parse_data(reader, &mut indices)
                        .context("failed to parse data section")?;
                }
                wasmparser::SectionCode::Type => {
                    log::debug!("parsing type section");
                    let reader = section.get_type_section_reader()?;
                    ret.parse_types(reader, &mut indices)
                        .context("failed to parse type section")?;
                }
                wasmparser::SectionCode::Import => {
                    log::debug!("parsing import section");
                    let reader = section.get_import_section_reader()?;
                    ret.parse_imports(reader, &mut indices)
                        .context("failed to parse import section")?;
                }
                wasmparser::SectionCode::Table => {
                    log::debug!("parsing table section");
                    let reader = section.get_table_section_reader()?;
                    ret.parse_tables(reader, &mut indices)
                        .context("failed to parse table section")?;
                }
                wasmparser::SectionCode::Memory => {
                    log::debug!("parsing memory section");
                    let reader = section.get_memory_section_reader()?;
                    ret.parse_memories(reader, &mut indices)
                        .context("failed to parse memory section")?;
                }
                wasmparser::SectionCode::Global => {
                    log::debug!("parsing global section");
                    let reader = section.get_global_section_reader()?;
                    ret.parse_globals(reader, &mut indices)
                        .context("failed to parse global section")?;
                }
                wasmparser::SectionCode::Export => {
                    log::debug!("parsing export section");
                    let reader = section.get_export_section_reader()?;
                    ret.parse_exports(reader, &mut indices)
                        .context("failed to parse export section")?;
                }
                wasmparser::SectionCode::Element => {
                    log::debug!("parsing element section");
                    let reader = section.get_element_section_reader()?;
                    ret.parse_elements(reader, &mut indices)
                        .context("failed to parse element section")?;
                }
                wasmparser::SectionCode::Start => {
                    log::debug!("parsing start section");
                    let idx = section.get_start_section_content()?;
                    ret.start = Some(indices.get_func(idx)?);
                }
                wasmparser::SectionCode::Function => {
                    log::debug!("parsing function section");
                    let reader = section.get_function_section_reader()?;
                    function_section_size = Some(reader.get_count());
                    ret.declare_local_functions(reader, &mut indices)
                        .context("failed to parse function section")?;
                }
                wasmparser::SectionCode::Code => {
                    log::debug!("parsing code section");
                    let function_section_size = match function_section_size.take() {
                        Some(i) => i,
                        None => bail!("cannot have a code section without function section"),
                    };
                    let reader = section.get_code_section_reader()?;
                    ret.parse_local_functions(reader, function_section_size, &mut indices)
                        .context("failed to parse code section")?;
                }
                wasmparser::SectionCode::Custom { name, kind: _ } => {
                    log::debug!("parsing custom section `{}`", name);
                    let result = match name {
                        "producers" => {
                            let reader = section.get_binary_reader();
                            ret.parse_producers_section(reader)
                        }
                        "name" => section
                            .get_name_section_reader()
                            .map_err(failure::Error::from)
                            .and_then(|r| ret.parse_name_section(r, &indices)),
                        _ => {
                            let mut reader = section.get_binary_reader();
                            let len = reader.bytes_remaining();
                            let payload = reader.read_bytes(len)?;
                            ret.custom.push(CustomSection {
                                name: name.to_string(),
                                value: payload.to_vec(),
                            });
                            continue;
                        }
                    };
                    if let Err(e) = result {
                        log::warn!("failed to parse `{}` custom section {}", name, e);
                    }
                }
            }
        }

        if function_section_size.is_some() {
            bail!("cannot define a function section without a code section");
        }

        ret.producers
            .add_processed_by("walrus", env!("CARGO_PKG_VERSION"));

        // TODO: probably run this in a different location
        log::debug!("validating module");
        crate::passes::validate::run(&ret)?;

        log::debug!("parse complete");
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

    fn parse_name_section(
        &mut self,
        names: wasmparser::NameSectionReader,
        indices: &IndicesToIds,
    ) -> Result<()> {
        for name in names {
            match name? {
                wasmparser::Name::Module(m) => {
                    self.name = Some(m.get_name()?.to_string());
                }
                wasmparser::Name::Function(f) => {
                    let mut map = f.get_map()?;
                    for _ in 0..map.get_count() {
                        let naming = map.read()?;
                        let id = indices.get_func(naming.index)?;
                        self.funcs.get_mut(id).name = Some(naming.name.to_string());
                    }
                }
                wasmparser::Name::Local(l) => {
                    let mut reader = l.get_function_local_reader()?;
                    for _ in 0..reader.get_count() {
                        let name = reader.read()?;
                        let func_id = indices.get_func(name.func_index)?;
                        let mut map = name.get_map()?;
                        for _ in 0..map.get_count() {
                            let naming = map.read()?;
                            let id = indices.get_local(func_id, naming.index)?;
                            self.locals.get_mut(id).name = Some(naming.name.to_string());
                        }
                    }
                }
            }
        }
        Ok(())
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
