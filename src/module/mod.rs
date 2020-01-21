//! A high-level API for manipulating wasm modules.

mod config;
mod custom;
mod data;
mod elements;
mod exports;
mod functions;
mod globals;
mod imports;
mod locals;
mod memories;
mod producers;
mod tables;
mod types;

use crate::emit::{Emit, EmitContext, IdsToIndices, Section};
use crate::encode::Encoder;
use crate::error::Result;
pub use crate::ir::InstrLocId;
pub use crate::module::custom::{
    CustomSection, CustomSectionId, ModuleCustomSections, RawCustomSection, TypedCustomSectionId,
    UntypedCustomSectionId,
};
pub use crate::module::data::{ActiveData, ActiveDataLocation, Data, DataId, DataKind, ModuleData};
pub use crate::module::elements::{Element, ElementId, ModuleElements};
pub use crate::module::exports::{Export, ExportId, ExportItem, ModuleExports};
pub use crate::module::functions::{Function, FunctionId, ModuleFunctions};
pub use crate::module::functions::{FunctionKind, ImportedFunction, LocalFunction};
pub use crate::module::globals::{Global, GlobalId, GlobalKind, ModuleGlobals};
pub use crate::module::imports::{Import, ImportId, ImportKind, ModuleImports};
pub use crate::module::locals::ModuleLocals;
pub use crate::module::memories::{Memory, MemoryId, ModuleMemories};
pub use crate::module::producers::ModuleProducers;
pub use crate::module::tables::FunctionTable;
pub use crate::module::tables::{ModuleTables, Table, TableId, TableKind};
pub use crate::module::types::ModuleTypes;
use crate::parse::IndicesToIds;
use anyhow::{bail, Context};
use std::fs;
use std::mem;
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
    /// Custom sections found in this module.
    pub customs: ModuleCustomSections,
    /// The name of this module, used for debugging purposes in the `name`
    /// custom section.
    pub name: Option<String>,
    pub(crate) config: ModuleConfig,
}

/// Maps from an offset of an instruction in the input Wasm to its offset in the
/// output Wasm.
///
/// Note that an input offset may be mapped to multiple output offsets, and vice
/// versa, due to transformations like function inlinining or constant
/// propagation.
pub type CodeTransform = Vec<(InstrLocId, usize)>;

impl Module {
    /// Create a default, empty module that uses the given configuration.
    pub fn with_config(config: ModuleConfig) -> Self {
        Module {
            config,
            ..Default::default()
        }
    }

    /// Construct a new module from the given path with the default
    /// configuration.
    pub fn from_file<P>(path: P) -> Result<Module>
    where
        P: AsRef<Path>,
    {
        Module::from_buffer(&fs::read(path)?)
    }

    /// Construct a new module from the given path and configuration.
    pub fn from_file_with_config<P>(path: P, config: &ModuleConfig) -> Result<Module>
    where
        P: AsRef<Path>,
    {
        config.parse(&fs::read(path)?)
    }

    /// Construct a new module from the in-memory wasm buffer with the default
    /// configuration.
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
                    if ret.start.is_some() {
                        bail!("multiple start sections found");
                    }
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
                    let on_instr_loc = config.on_instr_loc.as_ref().map(|f| f.as_ref());
                    ret.parse_local_functions(
                        reader,
                        function_section_size,
                        &mut indices,
                        on_instr_loc,
                    )
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
                            .map_err(anyhow::Error::from)
                            .and_then(|r| ret.parse_name_section(r, &indices)),
                        _ => {
                            log::debug!("parsing custom section `{}`", name);
                            let mut reader = section.get_binary_reader();
                            let len = reader.bytes_remaining();
                            let payload = reader.read_bytes(len)?;
                            ret.customs.add(RawCustomSection {
                                name: name.to_string(),
                                data: payload.to_vec(),
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
        if !ret.config.skip_strict_validate {
            crate::passes::validate::run(&ret)?;
        }

        if let Some(ref on_parse) = config.on_parse {
            on_parse(&mut ret, &indices)?;
        }

        log::debug!("parse complete");
        Ok(ret)
    }

    /// Emit this module into a `.wasm` file at the given path.
    pub fn emit_wasm_file<P>(&mut self, path: P) -> Result<()>
    where
        P: AsRef<Path>,
    {
        let buffer = self.emit_wasm();
        fs::write(path, buffer).context("failed to write wasm module")?;
        Ok(())
    }

    /// Emit this module into an in-memory wasm buffer.
    pub fn emit_wasm(&mut self) -> Vec<u8> {
        log::debug!("start emit");

        let indices = &mut IdsToIndices::default();
        let mut wasm = Vec::new();
        wasm.extend(&[0x00, 0x61, 0x73, 0x6d]); // magic
        wasm.extend(&[0x01, 0x00, 0x00, 0x00]); // version

        let mut customs = mem::replace(&mut self.customs, ModuleCustomSections::default());

        let mut cx = EmitContext {
            module: self,
            indices,
            encoder: Encoder::new(&mut wasm),
            locals: Default::default(),
            code_transform: Vec::new(),
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

        if !self.config.skip_name_section {
            emit_name_section(&mut cx);
        }
        if !self.config.skip_producers_section {
            self.producers.emit(&mut cx);
        }

        let indices = mem::replace(cx.indices, Default::default());

        for (_id, section) in customs.iter_mut() {
            if !self.config.generate_dwarf && section.name().starts_with(".debug") {
                log::debug!("skipping DWARF custom section {}", section.name());
                continue;
            }

            log::debug!("emitting custom section {}", section.name());

            if self.config.preserve_code_transform {
                section.apply_code_transform(&cx.code_transform);
            }

            cx.custom_section(&section.name())
                .encoder
                .raw(&section.data(&indices));
        }

        log::debug!("emission finished");
        wasm
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
                            if self.config.generate_synthetic_names_for_anonymous_items
                                && naming.name.is_empty()
                            {
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
        .iter()
        .filter_map(|func| func.name.as_ref().map(|name| (func, name)))
        .map(|(func, name)| (cx.indices.get_func_index(func.id()), name))
        .collect::<Vec<_>>();
    funcs.sort_by_key(|p| p.0); // sort by index

    let mut locals = cx
        .module
        .funcs
        .iter()
        .filter_map(|func| cx.locals.get(&func.id()).map(|l| (func, l)))
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
