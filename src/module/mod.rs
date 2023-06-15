//! A high-level API for manipulating wasm modules.

mod config;
mod custom;
mod data;
mod debug;
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

use crate::emit::{Emit, EmitContext, IdsToIndices};
use crate::error::Result;
pub use crate::ir::InstrLocId;
pub use crate::module::custom::{
    CustomSection, CustomSectionId, ModuleCustomSections, RawCustomSection, TypedCustomSectionId,
    UntypedCustomSectionId,
};
pub use crate::module::data::{ActiveData, ActiveDataLocation, Data, DataId, DataKind, ModuleData};
pub use crate::module::debug::ModuleDebugData;
pub use crate::module::elements::ElementKind;
pub use crate::module::elements::{Element, ElementId, ModuleElements};
pub use crate::module::exports::{Export, ExportId, ExportItem, ModuleExports};
pub use crate::module::functions::{Function, FunctionId, ModuleFunctions};
pub use crate::module::functions::{FunctionKind, ImportedFunction, LocalFunction};
pub use crate::module::globals::{Global, GlobalId, GlobalKind, ModuleGlobals};
pub use crate::module::imports::{Import, ImportId, ImportKind, ModuleImports};
pub use crate::module::locals::ModuleLocals;
pub use crate::module::memories::{Memory, MemoryId, ModuleMemories};
pub use crate::module::producers::ModuleProducers;
pub use crate::module::tables::{ModuleTables, Table, TableId};
pub use crate::module::types::ModuleTypes;
use crate::parse::IndicesToIds;
use anyhow::{bail, Context};
use id_arena::Id;
use log::warn;
use std::fs;
use std::mem;
use std::path::Path;
use wasmparser::{Parser, Payload, Validator, WasmFeatures};

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
    /// Dwarf debug data.
    pub debug: ModuleDebugData,
    /// The name of this module, used for debugging purposes in the `name`
    /// custom section.
    pub name: Option<String>,
    pub(crate) config: ModuleConfig,
}

/// Code transformation records, which is used to transform DWARF debug entries.
#[derive(Debug, Default)]
pub struct CodeTransform {
    /// Maps from an offset of an instruction in the input Wasm to its offset in the
    /// output Wasm.
    ///
    /// Note that an input offset may be mapped to multiple output offsets, and vice
    /// versa, due to transformations like function inlinining or constant
    /// propagation.
    pub instruction_map: Vec<(InstrLocId, usize)>,

    /// Offset of code section from the front of Wasm binary
    pub code_section_start: usize,

    /// Emitted binary ranges of functions
    pub function_ranges: Vec<(Id<Function>, wasmparser::Range)>,
}

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
        let mut ret = Module::default();
        ret.config = config.clone();
        let mut indices = IndicesToIds::default();
        let mut validator = Validator::new();
        validator.wasm_features(WasmFeatures {
            reference_types: !config.only_stable_features,
            multi_value: true,
            bulk_memory: !config.only_stable_features,
            simd: !config.only_stable_features,
            threads: !config.only_stable_features,
            multi_memory: !config.only_stable_features,
            ..WasmFeatures::default()
        });

        let mut local_functions = Vec::new();
        let mut debug_sections = Vec::new();

        for payload in Parser::new(0).parse_all(wasm) {
            match payload? {
                Payload::Version { num, range } => {
                    validator.version(num, &range)?;
                }
                Payload::DataSection(s) => {
                    validator
                        .data_section(&s)
                        .context("failed to parse data section")?;
                    ret.parse_data(s, &mut indices)?;
                }
                Payload::TypeSection(s) => {
                    validator
                        .type_section(&s)
                        .context("failed to parse type section")?;
                    ret.parse_types(s, &mut indices)?;
                }
                Payload::ImportSection(s) => {
                    validator
                        .import_section(&s)
                        .context("failed to parse import section")?;
                    ret.parse_imports(s, &mut indices)?;
                }
                Payload::TableSection(s) => {
                    validator
                        .table_section(&s)
                        .context("failed to parse table section")?;
                    ret.parse_tables(s, &mut indices)?;
                }
                Payload::MemorySection(s) => {
                    validator
                        .memory_section(&s)
                        .context("failed to parse memory section")?;
                    ret.parse_memories(s, &mut indices)?;
                }
                Payload::GlobalSection(s) => {
                    validator
                        .global_section(&s)
                        .context("failed to parse global section")?;
                    ret.parse_globals(s, &mut indices)?;
                }
                Payload::ExportSection(s) => {
                    validator
                        .export_section(&s)
                        .context("failed to parse export section")?;
                    ret.parse_exports(s, &mut indices)?;
                }
                Payload::ElementSection(s) => {
                    validator
                        .element_section(&s)
                        .context("failed to parse element section")?;
                    ret.parse_elements(s, &mut indices)?;
                }
                Payload::StartSection { func, range, .. } => {
                    validator.start_section(func, &range)?;
                    ret.start = Some(indices.get_func(func)?);
                }
                Payload::FunctionSection(s) => {
                    validator
                        .function_section(&s)
                        .context("failed to parse function section")?;
                    ret.declare_local_functions(s, &mut indices)?;
                }
                Payload::DataCountSection { count, range } => {
                    validator.data_count_section(count, &range)?;
                    ret.reserve_data(count, &mut indices);
                }
                Payload::CodeSectionStart { count, range, .. } => {
                    validator.code_section_start(count, &range)?;
                    ret.funcs.code_section_offset = range.start;
                }
                Payload::CodeSectionEntry(body) => {
                    let validator = validator.code_section_entry()?;
                    local_functions.push((body, validator));
                }
                Payload::CustomSection {
                    name,
                    data,
                    data_offset,
                    range: _range,
                } => {
                    let result = match name {
                        "producers" => wasmparser::ProducersSectionReader::new(data, data_offset)
                            .map_err(anyhow::Error::from)
                            .and_then(|s| ret.parse_producers_section(s)),
                        "name" => wasmparser::NameSectionReader::new(data, data_offset)
                            .map_err(anyhow::Error::from)
                            .and_then(|r| ret.parse_name_section(r, &indices)),
                        _ => {
                            log::debug!("parsing custom section `{}`", name);
                            if name.starts_with(".debug") {
                                debug_sections.push(RawCustomSection {
                                    name: name.to_string(),
                                    data: data.to_vec(),
                                });
                            } else {
                                ret.customs.add(RawCustomSection {
                                    name: name.to_string(),
                                    data: data.to_vec(),
                                });
                            }
                            continue;
                        }
                    };
                    if let Err(e) = result {
                        log::warn!("failed to parse `{}` custom section {}", name, e);
                    }
                }
                Payload::UnknownSection { id, range, .. } => {
                    validator.unknown_section(id, &range)?;
                    unreachable!()
                }

                Payload::End => validator.end()?,

                // the module linking proposal is not implemented yet.
                Payload::AliasSection(s) => {
                    validator.alias_section(&s)?;
                    bail!("not supported yet");
                }
                Payload::InstanceSection(s) => {
                    validator.instance_section(&s)?;
                    bail!("not supported yet");
                }
                Payload::ModuleSectionEntry {
                    parser: _,
                    range: _,
                } => {
                    validator.module_section_entry();
                    bail!("not supported yet");
                }
                Payload::ModuleSectionStart {
                    count,
                    range,
                    size: _,
                } => {
                    validator.module_section_start(count, &range)?;
                    bail!("not supported yet");
                }
                // exception handling is not implemented yet.
                Payload::TagSection(s) => {
                    validator.tag_section(&s)?;
                    bail!("not supported yet");
                }
            }
        }

        ret.parse_local_functions(
            local_functions,
            &mut indices,
            config.on_instr_loc.as_ref().map(|f| f.as_ref()),
        )
        .context("failed to parse code section")?;

        ret.parse_debug_sections(debug_sections)
            .context("failed to parse debug data section")?;

        ret.producers
            .add_processed_by("walrus", env!("CARGO_PKG_VERSION"));

        if let Some(on_parse) = &config.on_parse {
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

        let mut customs = mem::replace(&mut self.customs, ModuleCustomSections::default());

        let mut cx = EmitContext {
            module: self,
            indices,
            wasm_module: wasm_encoder::Module::new(),
            locals: Default::default(),
            code_transform: Default::default(),
        };
        self.types.emit(&mut cx);
        self.imports.emit(&mut cx);
        self.funcs.emit_func_section(&mut cx);
        self.tables.emit(&mut cx);
        self.memories.emit(&mut cx);
        // TODO: tag section
        self.globals.emit(&mut cx);
        self.exports.emit(&mut cx);
        if let Some(start) = self.start {
            let idx = cx.indices.get_func_index(start);
            cx.wasm_module.section(&wasm_encoder::StartSection {
                function_index: idx,
            });
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

        if self.config.generate_dwarf {
            self.debug.emit(&mut cx);
        } else {
            log::debug!("skipping DWARF custom section");
        }

        let indices = mem::replace(cx.indices, Default::default());

        for (_id, section) in customs.iter_mut() {
            if section.name().starts_with(".debug") {
                continue;
            }

            log::debug!("emitting custom section {}", section.name());

            if self.config.preserve_code_transform {
                section.apply_code_transform(&cx.code_transform);
            }

            cx.wasm_module.section(&wasm_encoder::CustomSection {
                name: section.name().into(),
                data: section.data(&indices).into(),
            });
        }

        let out = cx.wasm_module.finish();
        log::debug!("emission finished");

        // let mut validator = Validator::new();
        // if let Err(err) = validator.validate_all(&out) {
        //     eprintln!("{:?}", err);
        //     panic!("Unable to validate serialized output");
        // }

        out
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
                        match indices.get_func(naming.index) {
                            Ok(id) => self.funcs.get_mut(id).name = Some(naming.name.to_string()),
                            // If some tool fails to GC function names properly,
                            // it doesn't really hurt anything to ignore the
                            // broken references and keep going.
                            Err(e) => warn!("in name section: {}", e),
                        }
                    }
                }
                wasmparser::Name::Type(t) => {
                    let mut map = t.get_map()?;
                    for _ in 0..map.get_count() {
                        let naming = map.read()?;
                        match indices.get_type(naming.index) {
                            Ok(id) => self.types.get_mut(id).name = Some(naming.name.to_string()),
                            Err(e) => warn!("in name section: {}", e),
                        }
                    }
                }
                wasmparser::Name::Memory(m) => {
                    let mut map = m.get_map()?;
                    for _ in 0..map.get_count() {
                        let naming = map.read()?;
                        match indices.get_memory(naming.index) {
                            Ok(id) => {
                                self.memories.get_mut(id).name = Some(naming.name.to_string())
                            }
                            Err(e) => warn!("in name section: {}", e),
                        }
                    }
                }
                wasmparser::Name::Table(t) => {
                    let mut map = t.get_map()?;
                    for _ in 0..map.get_count() {
                        let naming = map.read()?;
                        match indices.get_table(naming.index) {
                            Ok(id) => self.tables.get_mut(id).name = Some(naming.name.to_string()),
                            Err(e) => warn!("in name section: {}", e),
                        }
                    }
                }
                wasmparser::Name::Data(d) => {
                    let mut map = d.get_map()?;
                    for _ in 0..map.get_count() {
                        let naming = map.read()?;
                        match indices.get_data(naming.index) {
                            Ok(id) => self.data.get_mut(id).name = Some(naming.name.to_string()),
                            Err(e) => warn!("in name section: {}", e),
                        }
                    }
                }
                wasmparser::Name::Element(e) => {
                    let mut map = e.get_map()?;
                    for _ in 0..map.get_count() {
                        let naming = map.read()?;
                        match indices.get_element(naming.index) {
                            Ok(id) => {
                                self.elements.get_mut(id).name = Some(naming.name.to_string())
                            }
                            Err(e) => warn!("in name section: {}", e),
                        }
                    }
                }
                wasmparser::Name::Global(e) => {
                    let mut map = e.get_map()?;
                    for _ in 0..map.get_count() {
                        let naming = map.read()?;
                        match indices.get_global(naming.index) {
                            Ok(id) => self.globals.get_mut(id).name = Some(naming.name.to_string()),
                            Err(e) => warn!("in name section: {}", e),
                        }
                    }
                }
                wasmparser::Name::Local(l) => {
                    let mut reader = l.get_indirect_map()?;
                    for _ in 0..reader.get_indirect_count() {
                        let name = reader.read()?;
                        let func_id = indices.get_func(name.indirect_index)?;
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
                            match indices.get_local(func_id, naming.index) {
                                Ok(id) => {
                                    self.locals.get_mut(id).name = Some(naming.name.to_string())
                                }
                                // It looks like emscripten leaves broken
                                // function references in the locals subsection
                                // sometimes.
                                Err(e) => warn!("in name section: {}", e),
                            }
                        }
                    }
                }
                wasmparser::Name::Unknown { ty, .. } => warn!("unknown name subsection {}", ty),
                wasmparser::Name::Label(_) => warn!("labels name subsection ignored"),
            }
        }
        Ok(())
    }
}

fn emit_name_section(cx: &mut EmitContext) {
    log::debug!("emit name section");

    let mut wasm_name_section = wasm_encoder::NameSection::new();

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

    if let Some(name) = &cx.module.name {
        wasm_name_section.module(name);
    }

    if funcs.len() > 0 {
        let mut name_map = wasm_encoder::NameMap::new();
        for (index, name) in funcs {
            name_map.append(index, name);
        }
        wasm_name_section.functions(&name_map);
    }

    if locals.len() > 0 {
        let mut indirect_name_map = wasm_encoder::IndirectNameMap::new();
        for (index, mut map) in locals {
            let mut name_map = wasm_encoder::NameMap::new();
            map.sort_by_key(|p| p.0); // sort by index
            for (index, name) in map {
                name_map.append(index, name);
            }
            indirect_name_map.append(index, &name_map);
        }
        wasm_name_section.locals(&indirect_name_map);
    }

    cx.wasm_module.section(&wasm_name_section);
}
