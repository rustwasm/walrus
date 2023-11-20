mod dwarf;
mod units;

use crate::emit::{Emit, EmitContext};
use crate::{CustomSection, Function, InstrLocId, Module, ModuleFunctions, RawCustomSection};
use gimli::*;
use id_arena::Id;
use std::cmp::Ordering;

use self::dwarf::{AddressSearchPreference, ConvertContext, DEAD_CODE};
use self::units::DebuggingInformationCursor;

/// The DWARF debug section in input WebAssembly binary.
#[derive(Debug, Default)]
pub struct ModuleDebugData {
    /// DWARF debug data
    pub dwarf: read::Dwarf<Vec<u8>>,
}

/// Specify roles of origial address.

#[derive(Debug, PartialEq)]
enum CodeAddress {
    /// The address is instruction within a function.
    InstrInFunction { instr_id: InstrLocId },
    /// The address is one byte before the instruction.
    InstrEdge { instr_id: InstrLocId },
    /// The address is within a function, but does not match any instruction.
    OffsetInFunction { id: Id<Function>, offset: usize },
    /// The address is boundary of functions. Equals to OffsetInFunction with offset(section size).
    FunctionEdge { id: Id<Function> },
    /// The address is unknown.
    Unknown,
}

/// Converts original code address to CodeAddress
struct CodeAddressConverter {
    /// Function range based convert table
    address_convert_table: Vec<(wasmparser::Range, Id<Function>)>,
    /// Instrument based convert table
    instrument_address_convert_table: Vec<(usize, InstrLocId)>,
}

impl CodeAddressConverter {
    fn from_emit_context(funcs: &ModuleFunctions) -> Self {
        let mut address_convert_table = funcs
            .iter_local()
            .filter_map(|(func_id, func)| func.original_range.map(|range| (range, func_id)))
            .collect::<Vec<_>>();

        let mut instrument_address_convert_table = funcs
            .iter_local()
            .flat_map(|(_, func)| &func.instruction_mapping)
            .copied()
            .collect::<Vec<_>>();

        address_convert_table.sort_by_key(|i| i.0.start);
        instrument_address_convert_table.sort_by_key(|i| i.0);

        Self {
            address_convert_table,
            instrument_address_convert_table,
        }
    }

    fn find_address(
        &self,
        address: usize,
        search_preference: AddressSearchPreference,
    ) -> CodeAddress {
        match self
            .instrument_address_convert_table
            .binary_search_by_key(&address, |i| i.0)
        {
            Ok(id) => {
                return CodeAddress::InstrInFunction {
                    instr_id: self.instrument_address_convert_table[id].1,
                }
            }
            Err(id) => {
                if id < self.instrument_address_convert_table.len()
                    && self.instrument_address_convert_table[id].0 - 1 == address
                {
                    return CodeAddress::InstrEdge {
                        instr_id: self.instrument_address_convert_table[id].1,
                    };
                }
            }
        };

        // If the address is not mapped to any instruction, falling back to function-range-based comparison.
        let inclusive_range_comparor = |range: &(wasmparser::Range, Id<Function>)| {
            // range.start < address <= range.end
            if range.0.end < address {
                Ordering::Less
            } else if address <= range.0.start {
                Ordering::Greater
            } else {
                Ordering::Equal
            }
        };
        let exclusive_range_comparor = |range: &(wasmparser::Range, Id<Function>)| {
            // normal comparison: range.start <= address < range.end
            if range.0.end <= address {
                Ordering::Less
            } else if address < range.0.start {
                Ordering::Greater
            } else {
                Ordering::Equal
            }
        };
        let range_comparor: &dyn Fn(_) -> Ordering = match search_preference {
            AddressSearchPreference::InclusiveFunctionEnd => &inclusive_range_comparor,
            AddressSearchPreference::ExclusiveFunctionEnd => &exclusive_range_comparor,
        };

        match self.address_convert_table.binary_search_by(range_comparor) {
            Ok(i) => {
                let entry = &self.address_convert_table[i];
                let code_offset_from_function_start = address - entry.0.start;

                if address == entry.0.end {
                    CodeAddress::FunctionEdge { id: entry.1 }
                } else {
                    CodeAddress::OffsetInFunction {
                        id: entry.1,
                        offset: code_offset_from_function_start,
                    }
                }
            }
            Err(_) => CodeAddress::Unknown,
        }
    }
}

impl Module {
    pub(crate) fn parse_debug_sections(
        &mut self,
        mut debug_sections: Vec<RawCustomSection>,
    ) -> Result<()> {
        let load_section = |id: gimli::SectionId| -> Result<Vec<u8>> {
            Ok(
                match debug_sections
                    .iter_mut()
                    .find(|section| section.name() == id.name())
                {
                    Some(section) => std::mem::take(&mut section.data),
                    None => Vec::new(),
                },
            )
        };

        self.debug.dwarf = read::Dwarf::load(load_section)?;

        Ok(())
    }
}

impl Emit for ModuleDebugData {
    fn emit(&self, cx: &mut EmitContext) {
        let address_converter = CodeAddressConverter::from_emit_context(&cx.module.funcs);

        let code_transform = &cx.code_transform;
        let instruction_map = &code_transform.instruction_map;

        let convert_address = |address, search_preference| -> Option<write::Address> {
            let address = address as usize;
            let address = match address_converter.find_address(address, search_preference) {
                CodeAddress::InstrInFunction { instr_id } => {
                    match instruction_map.binary_search_by_key(&instr_id, |i| i.0) {
                        Ok(id) => Some(instruction_map[id].1),
                        Err(_) => None,
                    }
                }
                CodeAddress::InstrEdge { instr_id } => {
                    match instruction_map.binary_search_by_key(&instr_id, |i| i.0) {
                        Ok(id) => Some(instruction_map[id].1 - 1),
                        Err(_) => None,
                    }
                }
                CodeAddress::OffsetInFunction { id, offset } => {
                    match code_transform
                        .function_ranges
                        .binary_search_by_key(&id, |i| i.0)
                    {
                        Ok(id) => Some(code_transform.function_ranges[id].1.start + offset),
                        Err(_) => None,
                    }
                }
                CodeAddress::FunctionEdge { id } => {
                    match code_transform
                        .function_ranges
                        .binary_search_by_key(&id, |i| i.0)
                    {
                        Ok(id) => Some(code_transform.function_ranges[id].1.end),
                        Err(_) => None,
                    }
                }
                CodeAddress::Unknown => None,
            };

            address
                .map(|x| (x - cx.code_transform.code_section_start) as u64)
                .map(write::Address::Constant)
        };

        let from_dwarf = cx
            .module
            .debug
            .dwarf
            .borrow(|sections| EndianSlice::new(sections.as_ref(), LittleEndian));

        let mut dwarf = write::Dwarf::from(&from_dwarf, &|address| {
            if address == 0 || address == DEAD_CODE {
                return Some(write::Address::Constant(address));
            }
            convert_address(address, AddressSearchPreference::InclusiveFunctionEnd)
                .or(Some(write::Address::Constant(DEAD_CODE)))
        })
        .expect("cannot convert to writable dwarf");

        let mut from_units = from_dwarf.units();
        let mut unit_entries = Vec::new();

        while let Some(from_unit) = from_units.next().expect("") {
            unit_entries.push(from_unit);
        }

        let mut convert_context = ConvertContext::new(
            &from_dwarf,
            &convert_address,
            &mut dwarf.line_strings,
            &mut dwarf.strings,
        );

        for index in 0..dwarf.units.count() {
            let id = dwarf.units.id(index);

            let unit = dwarf.units.get_mut(id);
            let from_unit: Unit<EndianSlice<'_, LittleEndian>, usize> =
                from_dwarf.unit(unit_entries[index]).expect("readable unit");

            // perform high pc transformation of DWARF .debug_info
            {
                let mut from_entries = from_unit.entries();
                let mut entries = DebuggingInformationCursor::new(unit);

                convert_context.convert_high_pc(&mut from_entries, &mut entries);
            }

            // perform line program transformation
            if let Some(program) = convert_context.convert_unit_line_program(from_unit) {
                unit.line_program = program;
            }
        }

        let mut sections = write::Sections::new(write::EndianVec::new(gimli::LittleEndian));
        dwarf.write(&mut sections).expect("write failed");
        sections
            .for_each(
                |id: SectionId, data: &write::EndianVec<LittleEndian>| -> Result<()> {
                    cx.wasm_module.section(&wasm_encoder::CustomSection {
                        name: id.name().into(),
                        data: data.slice().into(),
                    });
                    Ok(())
                },
            )
            .expect("never");
    }
}

#[test]
fn dwarf_address_converter() {
    let mut module = crate::Module::default();

    let mut func1 = crate::LocalFunction::new(
        Vec::new(),
        crate::FunctionBuilder::new(&mut module.types, &[], &[]),
    );

    func1.original_range = Some(wasmparser::Range { start: 20, end: 30 });

    let id1 = module.funcs.add_local(func1);

    let mut func2 = crate::LocalFunction::new(
        Vec::new(),
        crate::FunctionBuilder::new(&mut module.types, &[], &[]),
    );

    func2.original_range = Some(wasmparser::Range { start: 30, end: 50 });

    let id2 = module.funcs.add_local(func2);

    let address_converter = CodeAddressConverter::from_emit_context(&module.funcs);

    assert_eq!(
        address_converter.find_address(10, AddressSearchPreference::InclusiveFunctionEnd),
        CodeAddress::Unknown
    );
    assert_eq!(
        address_converter.find_address(20, AddressSearchPreference::ExclusiveFunctionEnd),
        CodeAddress::OffsetInFunction { id: id1, offset: 0 }
    );
    assert_eq!(
        address_converter.find_address(20, AddressSearchPreference::InclusiveFunctionEnd),
        CodeAddress::Unknown
    );
    assert_eq!(
        address_converter.find_address(25, AddressSearchPreference::ExclusiveFunctionEnd),
        CodeAddress::OffsetInFunction { id: id1, offset: 5 }
    );
    assert_eq!(
        address_converter.find_address(29, AddressSearchPreference::ExclusiveFunctionEnd),
        CodeAddress::OffsetInFunction { id: id1, offset: 9 }
    );
    assert_eq!(
        address_converter.find_address(29, AddressSearchPreference::InclusiveFunctionEnd),
        CodeAddress::OffsetInFunction { id: id1, offset: 9 }
    );
    assert_eq!(
        address_converter.find_address(30, AddressSearchPreference::InclusiveFunctionEnd),
        CodeAddress::FunctionEdge { id: id1 }
    );
    assert_eq!(
        address_converter.find_address(30, AddressSearchPreference::ExclusiveFunctionEnd),
        CodeAddress::OffsetInFunction { id: id2, offset: 0 }
    );
    assert_eq!(
        address_converter.find_address(50, AddressSearchPreference::InclusiveFunctionEnd),
        CodeAddress::FunctionEdge { id: id2 }
    );
    assert_eq!(
        address_converter.find_address(50, AddressSearchPreference::ExclusiveFunctionEnd),
        CodeAddress::Unknown
    );
}
