mod dwarf;

use crate::emit::{Emit, EmitContext};
use crate::{CustomSection, FunctionKind, Module, RawCustomSection};
use gimli::*;
use std::cmp::Ordering;

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

impl ModuleDebugData {
    /// c
    fn convert_attributes<R: Reader<Offset = usize>>(
        from_unit: read::Unit<R>,
        dwarf: &read::Dwarf<R>,
        convert_address: &dyn Fn(u64) -> Option<write::Address>,
    ) -> Option<write::LineProgram> {
        match from_unit.line_program {
            Some(ref from_program) => {
                let from_program = from_program.clone();
                let line_program = ModuleDebugData::convert_line_program(
                    from_program,
                    dwarf,
                    convert_address,
                )
                .expect("cannot convert line program");
                Some(line_program)
            }
            None => None,
        }
    }

    fn convert_line_program_header<R: Reader<Offset = usize>>(
        from_program: &read::IncompleteLineProgram<R>,
        dwarf: &read::Dwarf<R>,
        line_strings: &mut write::LineStringTable,
        strings: &mut write::StringTable,
        dirs: &mut Vec<write::DirectoryId>,
        files: &mut Vec<write::FileId>,
    ) -> write::ConvertResult<write::LineProgram> {
        let from_header = from_program.header();
        let encoding = from_header.encoding();

        let comp_dir = match from_header.directory(0) {
            Some(comp_dir) => {
                ModuleDebugData::convert_line_string(comp_dir, dwarf, line_strings, strings)?
            }
            None => write::LineString::new(&[][..], encoding, line_strings),
        };

        let (comp_name, comp_file_info) = match from_header.file(0) {
            Some(comp_file) => {
                if comp_file.directory_index() != 0 {
                    return Err(write::ConvertError::InvalidDirectoryIndex);
                }
                (
                    ModuleDebugData::convert_line_string(
                        comp_file.path_name(),
                        dwarf,
                        line_strings,
                        strings,
                    )?,
                    Some(write::FileInfo {
                        timestamp: comp_file.timestamp(),
                        size: comp_file.size(),
                        md5: *comp_file.md5(),
                    }),
                )
            }
            None => (
                write::LineString::new(&[][..], encoding, line_strings),
                None,
            ),
        };

        if from_header.line_base() > 0 {
            return Err(write::ConvertError::InvalidLineBase);
        }
        let mut program = write::LineProgram::new(
            encoding,
            from_header.line_encoding(),
            comp_dir,
            comp_name,
            comp_file_info,
        );

        let file_skip;
        if from_header.version() <= 4 {
            // The first directory is implicit.
            dirs.push(program.default_directory());
            // A file index of 0 is invalid for version <= 4, but putting
            // something there makes the indexing easier.
            file_skip = 0;
        } else {
            // We don't add the first file to `files`, but still allow
            // it to be referenced from converted instructions.
            file_skip = 1;
        }

        for from_dir in from_header.include_directories() {
            let from_dir = ModuleDebugData::convert_line_string(
                from_dir.clone(),
                dwarf,
                line_strings,
                strings,
            )?;
            dirs.push(program.add_directory(from_dir));
        }

        for from_file in from_header.file_names().iter().skip(file_skip) {
            let from_name = ModuleDebugData::convert_line_string(
                from_file.path_name(),
                dwarf,
                line_strings,
                strings,
            )?;
            let from_dir = from_file.directory_index();
            if from_dir >= dirs.len() as u64 {
                return Err(write::ConvertError::InvalidDirectoryIndex);
            }
            let from_dir = dirs[from_dir as usize];
            let from_info = Some(write::FileInfo {
                timestamp: from_file.timestamp(),
                size: from_file.size(),
                md5: *from_file.md5(),
            });
            files.push(program.add_file(from_name, from_dir, from_info));
        }

        Ok(program)
    }

    /// hei
    fn convert_line_program<R: Reader<Offset = usize>>(
        mut from_program: read::IncompleteLineProgram<R>,
        dwarf: &read::Dwarf<R>,
        convert_address: &dyn Fn(u64) -> Option<write::Address>,
    ) -> write::ConvertResult<write::LineProgram> {
        // Create mappings in case the source has duplicate files or directories.
        let mut line_strings = write::LineStringTable::default();
        let mut strings = write::StringTable::default();
        let mut dirs = Vec::new();
        let mut files = Vec::new();

        let mut program = ModuleDebugData::convert_line_program_header(
            &from_program,
            dwarf,
            &mut line_strings,
            &mut strings,
            &mut dirs,
            &mut files,
        )
        .expect("");

        // We can't use the `from_program.rows()` because that wouldn't let
        // us preserve address relocations.
        let mut from_row = read::LineRow::new(from_program.header());
        let mut instructions = from_program.header().instructions();
        let mut address = None;
        let mut from_base_address = 0;
        let mut base_address = 0;
        let mut previous_from_raw_address = 0;
        let mut previous_raw_address = 0;
        let mut non_existent_symbol = false;
        while let Some(instruction) = instructions.next_instruction(from_program.header())? {
            match instruction {
                read::LineInstruction::SetAddress(val) => {
                    if program.in_sequence() {
                        return Err(write::ConvertError::UnsupportedLineInstruction);
                    }
                    match convert_address(val) {
                        Some(converted) => {
                            address = Some(converted);
                            from_base_address = val;

                            if let write::Address::Constant(x) = converted {
                                base_address = x;
                            }
                            non_existent_symbol = false;
                        }
                        None => {
                            non_existent_symbol = true;
                        }
                    }
                    previous_from_raw_address = 0;
                    previous_raw_address = 0;
                    from_row.execute(read::LineInstruction::SetAddress(0), &mut from_program);
                }
                read::LineInstruction::DefineFile(_) => {
                    return Err(write::ConvertError::UnsupportedLineInstruction);
                }
                _ => {
                    if from_row.execute(instruction, &mut from_program) {
                        if !non_existent_symbol {
                            if !program.in_sequence() {
                                program.begin_sequence(address);
                                address = None;
                            }
                            let row_address = if let Some(address) =
                                convert_address(from_row.address() + from_base_address).map(|x| {
                                    match x {
                                        write::Address::Constant(x) => x,
                                        _ => 0
                                    }
                                })
                            {
                                address - base_address
                            } else {
                                previous_raw_address + from_row.address()
                                    - previous_from_raw_address
                            };
                            previous_from_raw_address = from_row.address();
                            previous_raw_address = row_address;

                            if from_row.end_sequence() {
                                program.end_sequence(row_address);
                            } else {
                                program.row().address_offset = row_address;
                                program.row().op_index = from_row.op_index();
                                program.row().file = {
                                    let file = from_row.file_index();
                                    if file > files.len() as u64 {
                                        return Err(write::ConvertError::InvalidFileIndex);
                                    }
                                    if file == 0 && program.version() <= 4 {
                                        return Err(write::ConvertError::InvalidFileIndex);
                                    }
                                    files[(file - 1) as usize]
                                };
                                program.row().line = match from_row.line() {
                                    Some(line) => line.get(),
                                    None => 0,
                                };
                                program.row().column = match from_row.column() {
                                    read::ColumnType::LeftEdge => 0,
                                    read::ColumnType::Column(val) => val.get(),
                                };
                                program.row().discriminator = from_row.discriminator();
                                program.row().is_statement = from_row.is_stmt();
                                program.row().basic_block = from_row.basic_block();
                                program.row().prologue_end = from_row.prologue_end();
                                program.row().epilogue_begin = from_row.epilogue_begin();
                                program.row().isa = from_row.isa();
                                program.generate_row();
                            }
                        }
                        from_row.reset(from_program.header());
                    }
                }
            };
        }
        Ok(program)
    }

    fn convert_line_string<R: Reader<Offset = usize>>(
        from_attr: read::AttributeValue<R>,
        dwarf: &read::Dwarf<R>,
        line_strings: &mut write::LineStringTable,
        strings: &mut write::StringTable,
    ) -> write::ConvertResult<write::LineString> {
        Ok(match from_attr {
            read::AttributeValue::String(r) => write::LineString::String(r.to_slice()?.to_vec()),
            read::AttributeValue::DebugStrRef(offset) => {
                let r = dwarf.debug_str.get_str(offset)?;
                let id = strings.add(r.to_slice()?);
                write::LineString::StringRef(id)
            }
            read::AttributeValue::DebugLineStrRef(offset) => {
                let r = dwarf.debug_line_str.get_str(offset)?;
                let id = line_strings.add(r.to_slice()?);
                write::LineString::LineStringRef(id)
            }
            _ => return Err(write::ConvertError::UnsupportedLineStringForm),
        })
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
                    (new_range.start - original_range.start) as isize,
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
                Ok(id) => Some(instruction_map[id].1 as u64),
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

        for index in 0..dwarf.units.count() {
            let id = dwarf.units.id(index);
            let unit = dwarf.units.get_mut(id);

            if let Some(program) = ModuleDebugData::convert_attributes(
                from_dwarf.unit(unit_entries[index]).expect(""),
                &from_dwarf,
                &convert_address
            ) {
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
