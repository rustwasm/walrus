mod dwarf;

use crate::emit::{Emit, EmitContext};
use crate::{CustomSection, FunctionKind, Module, RawCustomSection};
use gimli::*;
use std::cmp::Ordering;

use self::dwarf::ConvertContext;

/// The set of de-duplicated types within a module.
#[derive(Debug, Default)]
pub struct ModuleDebugData {
    /// DWARF debug data
    pub dwarf: read::Dwarf<Vec<u8>>,
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
                    Some(section) => std::mem::replace(&mut section.data, Vec::new()),
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
        let mut address_convert_table = cx
            .code_transform
            .function_ranges
            .iter()
            .filter_map(
                |(func_id, func_range)| match cx.module.funcs.get(*func_id).kind {
                    FunctionKind::Local(ref func) => {
                        func.original_range.map(|range| (range, func_range))
                    }
                    _ => None,
                },
            )
            .map(|(original_range, new_range)| {
                (
                    original_range,
                    (new_range.start - cx.code_transform.code_section_start - original_range.start)
                        as isize,
                )
            })
            .collect::<Vec<_>>();

        let mut instrument_address_convert_table = cx
            .code_transform
            .function_ranges
            .iter()
            .filter_map(|(func_id, _)| match cx.module.funcs.get(*func_id).kind {
                FunctionKind::Local(ref func) => Some(&func.instruction_mapping),
                _ => None,
            })
            .flatten()
            .map(|x| x.clone())
            .collect::<Vec<_>>();

        address_convert_table.sort_by_key(|i| i.0.start);
        instrument_address_convert_table.sort_by_key(|i| i.0);

        let code_transform = &cx.code_transform;
        let convert_range_address = |address: u64| -> u64 {
            let address = address as usize;
            let comparor = |range: &(wasmparser::Range, isize)| {
                if range.0.end < address {
                    Ordering::Less
                } else if address < range.0.start {
                    Ordering::Greater
                } else {
                    Ordering::Equal
                }
            };
            match address_convert_table.binary_search_by(comparor) {
                Ok(i) => (address as i64 + address_convert_table[i].1 as i64) as u64,
                Err(_) => 0,
            }
        };

        let instruction_map = &code_transform.instruction_map;
        let convert_instrument_address = |address: u64| -> Option<u64> {
            let address = address as usize;

            let instr_id =
                match instrument_address_convert_table.binary_search_by_key(&address, |i| i.0) {
                    Ok(id) => instrument_address_convert_table[id].1,
                    Err(_) => {
                        return None;
                    }
                };

            match instruction_map.binary_search_by_key(&instr_id, |i| i.0) {
                Ok(id) => {
                    Some((instruction_map[id].1 - cx.code_transform.code_section_start) as u64)
                }
                Err(_) => None,
            }
        };

        let convert_address = |address| -> Option<write::Address> {
            convert_instrument_address(address)
                .or(Some(convert_range_address(address)))
                .map(|x| write::Address::Constant(x))
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
                    cx.custom_section(&id.name()).encoder.raw(data.slice());
                    Ok(())
                },
            )
            .expect("never");
    }
}
