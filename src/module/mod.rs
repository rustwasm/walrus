//! A high-level API for manipulating wasm modules.

mod config;
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

use crate::emit::{Emit, EmitContext, IdsToIndices, Section};
use crate::encode::Encoder;
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
use std::fs;
use std::path::Path;

pub use self::config::ModuleConfig;

/// A wasm module.
#[derive(Debug, Default)]
#[allow(missing_docs)]
pub struct Module {
    pub imports: ModuleImports,
    pub tables: ModuleTables,
    pub types: ModuleTypes,
    pub funcs: ModuleFunctions,
    pub globals: ModuleGlobals,
    pub locals: ModuleLocals,
    pub exports: ModuleExports,
    pub memories: ModuleMemories,
    /// Registration of passive data segments, if any
    pub data: ModuleData,
    /// Registration of passive element segments, if any
    pub elements: ModuleElements,
    /// The `start` function, if any
    pub start: Option<FunctionId>,
    /// Representation of the eventual custom section, `producers`
    pub producers: ModuleProducers,
    custom: Vec<CustomSection>,
    /// The name of this module, used for debugging purposes in the `name`
    /// custom section.
    pub name: Option<String>,
    config: ModuleConfig,
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
        ModuleConfig::new().parse(wasm)
    }

    fn parse(wasm: &[u8], config: &ModuleConfig) -> Result<Module> {
        let mut parser = wasmparser::ModuleReader::new(wasm)?;
        if parser.get_version() != 1 {
            bail!("only support version 1 of wasm");
        }

        let mut ret = Module::default();
        ret.config = config.clone();
        let mut indices = IndicesToIds::default();
        let mut function_section_size = None;
        let mut data_count = None;

        while !parser.eof() {
            let section = parser.read()?;
            match section.code {
                wasmparser::SectionCode::Data => {
                    let reader = section.get_data_section_reader()?;
                    ret.parse_data(reader, &mut indices, data_count)
                        .context("failed to parse data section")?;
                }
                wasmparser::SectionCode::Type => {
                    let reader = section.get_type_section_reader()?;
                    ret.parse_types(reader, &mut indices)
                        .context("failed to parse type section")?;
                }
                wasmparser::SectionCode::Import => {
                    let reader = section.get_import_section_reader()?;
                    ret.parse_imports(reader, &mut indices)
                        .context("failed to parse import section")?;
                }
                wasmparser::SectionCode::Table => {
                    let reader = section.get_table_section_reader()?;
                    ret.parse_tables(reader, &mut indices)
                        .context("failed to parse table section")?;
                }
                wasmparser::SectionCode::Memory => {
                    let reader = section.get_memory_section_reader()?;
                    ret.parse_memories(reader, &mut indices)
                        .context("failed to parse memory section")?;
                }
                wasmparser::SectionCode::Global => {
                    let reader = section.get_global_section_reader()?;
                    ret.parse_globals(reader, &mut indices)
                        .context("failed to parse global section")?;
                }
                wasmparser::SectionCode::Export => {
                    let reader = section.get_export_section_reader()?;
                    ret.parse_exports(reader, &mut indices)
                        .context("failed to parse export section")?;
                }
                wasmparser::SectionCode::Element => {
                    let reader = section.get_element_section_reader()?;
                    ret.parse_elements(reader, &mut indices)
                        .context("failed to parse element section")?;
                }
                wasmparser::SectionCode::Start => {
                    let idx = section.get_start_section_content()?;
                    ret.start = Some(indices.get_func(idx)?);
                }
                wasmparser::SectionCode::Function => {
                    let reader = section.get_function_section_reader()?;
                    function_section_size = Some(reader.get_count());
                    ret.declare_local_functions(reader, &mut indices)
                        .context("failed to parse function section")?;
                }
                wasmparser::SectionCode::Code => {
                    let function_section_size = match function_section_size.take() {
                        Some(i) => i,
                        None => bail!("cannot have a code section without function section"),
                    };
                    let reader = section.get_code_section_reader()?;
                    ret.parse_local_functions(reader, function_section_size, &mut indices)
                        .context("failed to parse code section")?;
                }
                wasmparser::SectionCode::DataCount => {
                    let count = section.get_data_count_section_content()?;
                    data_count = Some(count);
                    ret.reserve_data(count, &mut indices);
                }
                wasmparser::SectionCode::Custom { name, kind: _ } => {
                    let result = match name {
                        "producers" => {
                            let reader = section.get_producers_section_reader()?;
                            ret.parse_producers_section(reader)
                        }
                        "name" => section
                            .get_name_section_reader()
                            .map_err(failure::Error::from)
                            .and_then(|r| ret.parse_name_section(r, &indices)),
                        _ => {
                            log::debug!("parsing custom section `{}`", name);
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
        log::debug!("start emit");
        let roots = self.exports.iter();
        let used = passes::Used::new(self, roots.map(|e| e.id()));

        let indices = &mut IdsToIndices::default();
        let mut wasm = Vec::new();
        wasm.extend(&[0x00, 0x61, 0x73, 0x6d]); // magic
        wasm.extend(&[0x01, 0x00, 0x00, 0x00]); // version

        let mut cx = EmitContext {
            module: self,
            indices,
            used: &used,
            encoder: Encoder::new(&mut wasm),
        };
        self.types.emit(&mut cx);
        self.imports.emit(&mut cx);
        self.funcs.emit_func_section(&mut cx);
        self.tables.emit(&mut cx);
        self.memories.emit(&mut cx);
        self.globals.emit(&mut cx);
        self.exports.emit(&mut cx);
        if let Some(start) = self.start {
            let idx = cx.indices.get_func_index(start);
            cx.start_section(Section::Start).encoder.u32(idx);
        }
        self.elements.emit(&mut cx);
        self.data.emit_data_count(&mut cx);
        self.funcs.emit(&mut cx);
        self.data.emit(&mut cx);

        emit_name_section(&mut cx);
        self.producers.emit(&mut cx);
        for section in self.custom.iter() {
            log::debug!("emitting custom section {}", section.name);
            cx.custom_section(&section.name).encoder.raw(&section.value);
        }

        log::debug!("emission finished");
        Ok(wasm)
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
        log::debug!("parse name section");
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
                            // Looks like tools like `wat2wasm` generate empty
                            // names for locals if they aren't specified, so
                            // just ignore empty names which would in theory
                            // make debugging a bit harder.
                            if self.config.generate_names && naming.name.is_empty() {
                                continue;
                            }
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

fn emit_name_section(cx: &mut EmitContext) {
    log::debug!("emit name section");
    let mut funcs = cx
        .module
        .funcs
        .iter_used(cx.used)
        .filter_map(|func| func.name.as_ref().map(|name| (func, name)))
        .map(|(func, name)| (cx.indices.get_func_index(func.id()), name))
        .collect::<Vec<_>>();
    funcs.sort_by_key(|p| p.0); // sort by index

    let mut locals = cx
        .module
        .funcs
        .iter_used(cx.used)
        .filter_map(|func| cx.used.locals.get(&func.id()).map(|l| (func, l)))
        .filter_map(|(func, locals)| {
            let local_names = locals
                .iter()
                .filter_map(|id| {
                    let name = cx.module.locals.get(*id).name.as_ref()?;
                    let index = cx.indices.locals.get(&func.id())?.get(id)?;
                    Some((*index, name))
                })
                .collect::<Vec<_>>();
            if local_names.len() == 0 {
                None
            } else {
                Some((cx.indices.get_func_index(func.id()), local_names))
            }
        })
        .collect::<Vec<_>>();
    locals.sort_by_key(|p| p.0); // sort by index

    if cx.module.name.is_none() && funcs.len() == 0 && locals.len() == 0 {
        return;
    }

    let mut cx = cx.custom_section("name");
    if let Some(name) = &cx.module.name {
        cx.subsection(0).encoder.str(name);
    }

    if funcs.len() > 0 {
        let mut cx = cx.subsection(1);
        cx.encoder.usize(funcs.len());
        for (index, name) in funcs {
            cx.encoder.u32(index);
            cx.encoder.str(name);
        }
    }

    if locals.len() > 0 {
        let mut cx = cx.subsection(2);
        cx.encoder.usize(locals.len());
        for (index, mut map) in locals {
            cx.encoder.u32(index);
            cx.encoder.usize(map.len());
            map.sort_by_key(|p| p.0); // sort by index
            for (index, name) in map {
                cx.encoder.u32(index);
                cx.encoder.str(name);
            }
        }
    }
}
