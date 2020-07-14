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
use anyhow::Context;
use std::fs;
use std::mem;
use std::path::Path;
use wasmparser::{Parser, Payload, Validator};

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
        let mut ret = Module::default();
        ret.config = config.clone();
        let mut indices = IndicesToIds::default();
        let mut validator = Validator::new();
        validator.wasm_multi_value(true);
        if !config.only_stable_features {
            validator
                .wasm_reference_types(true)
                .wasm_multi_value(true)
                .wasm_bulk_memory(true)
                .wasm_simd(true)
                .wasm_threads(true);
        }

        let mut local_functions = Vec::new();

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
                }
                Payload::CodeSectionEntry(body) => {
                    let (validator, ops) = validator.code_section_entry(&body)?;
                    local_functions.push((body, validator, ops));
                }
                Payload::CustomSection {
                    name,
                    data,
                    data_offset,
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
                            ret.customs.add(RawCustomSection {
                                name: name.to_string(),
                                data: data.to_vec(),
                            });
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

                // the module linking proposal is not implemented yet
                Payload::AliasSection(s) => {
                    validator.alias_section(&s)?;
                    unreachable!()
                }
                Payload::InstanceSection(s) => {
                    validator.instance_section(&s)?;
                    unreachable!()
                }
                Payload::ModuleSection(s) => {
                    validator.module_section(&s)?;
                    unreachable!()
                }
                Payload::ModuleCodeSectionStart { count, range, .. } => {
                    validator.module_code_section_start(count, &range)?;
                    unreachable!()
                }
                Payload::ModuleCodeSectionEntry { .. } => unreachable!(),
            }
        }

        ret.parse_local_functions(
            local_functions,
            &mut indices,
            config.on_instr_loc.as_ref().map(|f| f.as_ref()),
        )
        .context("failed to parse code section")?;

        ret.producers
            .add_processed_by("walrus", env!("CARGO_PKG_VERSION"));

        // TODO: probably run this in a different location
        if !ret.config.skip_strict_validate {
            crate::passes::validate::run(&ret)?;
        }

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
