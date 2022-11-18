mod dwarf;

use crate::emit::{Emit, EmitContext};
use crate::{CustomSection, Function, InstrLocId, Module, ModuleFunctions, RawCustomSection};
use gimli::*;
use id_arena::Id;
use std::cell::RefCell;
use std::cmp::Ordering;

use self::dwarf::ConvertContext;

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
    /// The address is within a function, but does not match any instruction.
    OffsetInFunction { id: Id<Function>, offset: usize },
    /// The address is boundary of functions. Equals to OffsetInFunction with offset 0.
    FunctionEdge {
        previous_id: Option<Id<Function>>,
        current_id: Id<Function>,
    },
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
            .map(|(_, func)| &func.instruction_mapping)
            .flatten()
            .copied()
            .collect::<Vec<_>>();

        address_convert_table.sort_by_key(|i| i.0.start);
        instrument_address_convert_table.sort_by_key(|i| i.0);

        Self {
            address_convert_table,
            instrument_address_convert_table,
        }
    }

    fn find_address(&self, address: usize) -> CodeAddress {
        if let Ok(id) = self
            .instrument_address_convert_table
            .binary_search_by_key(&address, |i| i.0)
        {
            return CodeAddress::InstrInFunction {
                instr_id: self.instrument_address_convert_table[id].1,
            };
        }

        let range_comparor = |range: &(wasmparser::Range, _)| {
            if range.0.end <= address {
                Ordering::Less
            } else if address < range.0.start {
                Ordering::Greater
            } else {
                Ordering::Equal
            }
        };

        match self.address_convert_table.binary_search_by(range_comparor) {
            Ok(i) => {
                let entry = &self.address_convert_table[i];
                let code_offset_from_function_start = address - entry.0.start;

                if code_offset_from_function_start == 0 {
                    let previous_id = if i > 0 {
                        Some(self.address_convert_table[i - 1].1)
                    } else {
                        None
                    };

                    CodeAddress::FunctionEdge {
                        previous_id,
                        current_id: entry.1,
                    }
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
        let refer_previous_function_at_function_edge = RefCell::from(false);

        let convert_address = |address| -> Option<write::Address> {
            let address = address as usize;
            let address = match address_converter.find_address(address) {
                CodeAddress::InstrInFunction { instr_id } => {
                    match instruction_map.binary_search_by_key(&instr_id, |i| i.0) {
                        Ok(id) => Some(
                            (instruction_map[id].1 - cx.code_transform.code_section_start) as u64,
                        ),
                        Err(_) => None,
                    }
                }
                CodeAddress::OffsetInFunction { id, offset } => {
                    match code_transform
                        .function_ranges
                        .binary_search_by_key(&id, |i| i.0)
                    {
                        Ok(id) => {
                            Some((code_transform.function_ranges[id].1.start + offset) as u64)
                        }
                        Err(_) => None,
                    }
                }
                CodeAddress::FunctionEdge {
                    previous_id,
                    current_id,
                } => {
                    let id = if *refer_previous_function_at_function_edge.borrow() {
                        if let Some(id) = previous_id {
                            id
                        } else {
                            return None;
                        }
                    } else {
                        current_id
                    };
                    match code_transform
                        .function_ranges
                        .binary_search_by_key(&id, |i| i.0)
                    {
                        Ok(id) => Some((code_transform.function_ranges[id].1.start) as u64),
                        Err(_) => None,
                    }
                }
                CodeAddress::Unknown => None,
            };

            address.or(Some(0)).map(write::Address::Constant)
        };

        let from_dwarf = cx
            .module
            .debug
            .dwarf
            .borrow(|sections| EndianSlice::new(sections.as_ref(), LittleEndian));

        let mut dwarf = write::Dwarf::from(&from_dwarf, &convert_address)
            .expect("cannot convert to writable dwarf");

        let mut from_units = from_dwarf.units();
        let mut unit_entries = Vec::new();

        while let Some(from_unit) = from_units.next().expect("") {
            unit_entries.push(from_unit);
        }

        refer_previous_function_at_function_edge.replace(true);

        let mut convert_context = ConvertContext::new(&from_dwarf, &convert_address);

        for index in 0..dwarf.units.count() {
            let id = dwarf.units.id(index);
            let unit = dwarf.units.get_mut(id);

            if let Some(program) =
                convert_context.convert_attributes(from_dwarf.unit(unit_entries[index]).expect(""))
            {
                unit.line_program = program;
            }
        }

        let mut sections = write::Sections::new(write::EndianVec::new(gimli::LittleEndian));
        dwarf.write(&mut sections).expect("write failed");
        sections
            .for_each(
                |id: SectionId, data: &write::EndianVec<LittleEndian>| -> Result<()> {
                    cx.custom_section(id.name()).encoder.raw(data.slice());
                    Ok(())
                },
            )
            .expect("never");
    }
}

#[test]
fn dwarf_address_converter() {
    let mut module = crate::Module::default();

    let mut func = crate::LocalFunction::new(
        Vec::new(),
        crate::FunctionBuilder::new(&mut module.types, &[], &[]),
    );

    func.original_range = Some(wasmparser::Range { start: 20, end: 30 });

    let id = module.funcs.add_local(func);

    let address_converter = CodeAddressConverter::from_emit_context(&module.funcs);

    assert_eq!(address_converter.find_address(10), CodeAddress::Unknown);
    assert_eq!(
        address_converter.find_address(20),
        CodeAddress::FunctionEdge {
            previous_id: None,
            current_id: id
        }
    );
    assert_eq!(
        address_converter.find_address(25),
        CodeAddress::OffsetInFunction { id, offset: 5 }
    );
    assert_eq!(address_converter.find_address(30), CodeAddress::Unknown);
}
