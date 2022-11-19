//! transform entries in WebAssembly DWARF sections

/// `write` provides only address-based entry conversions,
/// does not provide entry-wise conversions.
/// We want to convert addresses of instructions, here will re-implement.
use gimli::*;

/// DWARF convertion context
pub(crate) struct ConvertContext<'a, R: Reader<Offset = usize>> {
    /// Source DWARF debug data
    pub debug_str: &'a read::DebugStr<R>,
    pub debug_line_str: &'a read::DebugLineStr<R>,

    /// Address conversion function
    pub convert_address: &'a dyn Fn(u64) -> Option<write::Address>,

    pub line_strings: write::LineStringTable,
    pub strings: write::StringTable,
    pub dirs: Vec<write::DirectoryId>,
    pub files: Vec<write::FileId>,
}

impl<'a, R> ConvertContext<'a, R>
where
    R: Reader<Offset = usize>,
{
    pub(crate) fn new(
        dwarf: &'a read::Dwarf<R>,
        convert_address: &'a dyn Fn(u64) -> Option<write::Address>,
    ) -> Self {
        ConvertContext {
            debug_str: &dwarf.debug_str,
            debug_line_str: &dwarf.debug_line_str,
            convert_address,
            line_strings: Default::default(),
            strings: Default::default(),
            dirs: Default::default(),
            files: Default::default(),
        }
    }

    pub(crate) fn convert_attributes(
        &mut self,
        from_unit: read::Unit<R>,
    ) -> Option<write::LineProgram> {
        match from_unit.line_program {
            Some(ref from_program) => {
                let from_program = from_program.clone();
                let line_program = self
                    .convert_line_program(from_program)
                    .expect("cannot convert line program");
                Some(line_program)
            }
            None => None,
        }
    }

    /// Perform conversion in DWARF line program header.
    /// Almostly cloned from https://github.com/gimli-rs/gimli/blob/master/src/write/line.rs#L985
    fn convert_line_program_header(
        &mut self,
        from_program: &read::IncompleteLineProgram<R>,
    ) -> write::ConvertResult<write::LineProgram> {
        let from_header = from_program.header();
        let encoding = from_header.encoding();

        let comp_dir = match from_header.directory(0) {
            Some(comp_dir) => self.convert_line_string(comp_dir)?,
            None => write::LineString::new(&[][..], encoding, &mut self.line_strings),
        };

        let (comp_name, comp_file_info) = match from_header.file(0) {
            Some(comp_file) => {
                if comp_file.directory_index() != 0 {
                    return Err(write::ConvertError::InvalidDirectoryIndex);
                }
                (
                    self.convert_line_string(comp_file.path_name())?,
                    Some(write::FileInfo {
                        timestamp: comp_file.timestamp(),
                        size: comp_file.size(),
                        md5: *comp_file.md5(),
                    }),
                )
            }
            None => (
                write::LineString::new(&[][..], encoding, &mut self.line_strings),
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

        let file_skip = if from_header.version() <= 4 {
            // The first directory is implicit.
            self.dirs.push(program.default_directory());
            // A file index of 0 is invalid for version <= 4, but putting
            // something there makes the indexing easier.
            0
        } else {
            // We don't add the first file to `files`, but still allow
            // it to be referenced from converted instructions.
            1
        };

        for from_dir in from_header.include_directories() {
            let from_dir = self.convert_line_string(from_dir.clone())?;
            self.dirs.push(program.add_directory(from_dir));
        }

        for from_file in from_header.file_names().iter().skip(file_skip) {
            let from_name = self.convert_line_string(from_file.path_name())?;
            let from_dir = from_file.directory_index();
            if from_dir >= self.dirs.len() as u64 {
                return Err(write::ConvertError::InvalidDirectoryIndex);
            }
            let from_dir = self.dirs[from_dir as usize];
            let from_info = Some(write::FileInfo {
                timestamp: from_file.timestamp(),
                size: from_file.size(),
                md5: *from_file.md5(),
            });
            self.files
                .push(program.add_file(from_name, from_dir, from_info));
        }

        Ok(program)
    }

    /// Perform address conversion in DWARF line program entries.
    /// Almostly cloned from https://github.com/gimli-rs/gimli/blob/master/src/write/line.rs#L1066
    fn convert_line_program(
        &mut self,
        mut from_program: read::IncompleteLineProgram<R>,
    ) -> write::ConvertResult<write::LineProgram> {
        // Create mappings in case the source has duplicate files or directories.
        let mut program = self.convert_line_program_header(&from_program).expect("");

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
                    match (self.convert_address)(val) {
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
                            let row_address =
                                if let Some(address) =
                                    (self.convert_address)(from_row.address() + from_base_address)
                                        .map(|x| match x {
                                            write::Address::Constant(x) => x,
                                            _ => 0,
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
                                    if file > self.files.len() as u64 {
                                        return Err(write::ConvertError::InvalidFileIndex);
                                    }
                                    if file == 0 && program.version() <= 4 {
                                        return Err(write::ConvertError::InvalidFileIndex);
                                    }
                                    self.files[(file - 1) as usize]
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

    /// Almostly cloned from https://github.com/gimli-rs/gimli/blob/master/src/write/line.rs#L1131
    fn convert_line_string(
        &mut self,
        from_attr: read::AttributeValue<R>,
    ) -> write::ConvertResult<write::LineString> {
        Ok(match from_attr {
            read::AttributeValue::String(r) => write::LineString::String(r.to_slice()?.to_vec()),
            read::AttributeValue::DebugStrRef(offset) => {
                let r = self.debug_str.get_str(offset)?;
                let id = self.strings.add(r.to_slice()?);
                write::LineString::StringRef(id)
            }
            read::AttributeValue::DebugLineStrRef(offset) => {
                let r = self.debug_line_str.get_str(offset)?;
                let id = self.line_strings.add(r.to_slice()?);
                write::LineString::LineStringRef(id)
            }
            _ => return Err(write::ConvertError::UnsupportedLineStringForm),
        })
    }
}

#[cfg(test)]
mod tests {
    use gimli::*;
    use std::cell::RefCell;

    fn make_test_debug_line<'a>(
        debug_line: &'a mut write::DebugLine<write::EndianVec<LittleEndian>>,
        make_line_row: &'a dyn Fn(&mut write::LineProgram) -> (),
    ) -> IncompleteLineProgram<EndianSlice<'a, LittleEndian>> {
        let encoding = Encoding {
            format: Format::Dwarf32,
            version: 4,
            address_size: 4,
        };
        let dir1 = &b"dir1"[..];
        let file1 = &b"file1"[..];
        let comp_dir = write::LineString::String(dir1.to_vec());
        let comp_file = write::LineString::String(file1.to_vec());

        let mut program = write::LineProgram::new(
            encoding,
            LineEncoding {
                minimum_instruction_length: 1,
                maximum_operations_per_instruction: 8,
                line_base: 0,
                line_range: 10,
                default_is_stmt: true,
            },
            comp_dir.clone(),
            comp_file.clone(),
            None,
        );

        {
            program.row().file = program.add_file(comp_file, program.default_directory(), None);
            make_line_row(&mut program);
        }

        {
            let debug_line_str_offsets = write::DebugLineStrOffsets::none();
            let debug_str_offsets = write::DebugStrOffsets::none();
            program
                .write(
                    debug_line,
                    encoding,
                    &debug_line_str_offsets,
                    &debug_str_offsets,
                )
                .unwrap();
        }

        let debug_line = read::DebugLine::new(debug_line.slice(), LittleEndian);
        let incomplete_debug_line = debug_line
            .program(DebugLineOffset(0), 4, None, None)
            .unwrap();

        incomplete_debug_line
    }

    #[test]
    fn convert_context() {
        let called_address_to_be_converted: RefCell<Vec<u64>> = RefCell::new(Vec::new());

        let mut debug_line = write::DebugLine::from(write::EndianVec::new(LittleEndian));
        let mut converted_debug_line = write::DebugLine::from(write::EndianVec::new(LittleEndian));

        {
            let make_line_row = |program: &mut write::LineProgram| {
                program.begin_sequence(Some(write::Address::Constant(0x1000)));
                program.generate_row();
                let address_offset = program.row().address_offset + 1u64;
                program.end_sequence(address_offset);
            };
            let incomplete_debug_line = make_test_debug_line(&mut debug_line, &make_line_row);

            let convert_address = |address| -> Option<write::Address> {
                called_address_to_be_converted.borrow_mut().push(address);
                Some(write::Address::Constant(address + 0x10))
            };

            let empty_dwarf = Dwarf::load(|_| -> Result<EndianSlice<LittleEndian>> {
                Ok(EndianSlice::new(&[], LittleEndian))
            })
            .unwrap();

            let mut convert_context =
                crate::module::debug::ConvertContext::new(&empty_dwarf, &convert_address);
            let converted_program = convert_context
                .convert_line_program(incomplete_debug_line)
                .unwrap();

            converted_program
                .write(
                    &mut converted_debug_line,
                    Encoding {
                        format: Format::Dwarf32,
                        version: 4,
                        address_size: 4,
                    },
                    &write::DebugLineStrOffsets::none(),
                    &write::DebugStrOffsets::none(),
                )
                .unwrap();
        }

        {
            let called_address_to_be_converted = called_address_to_be_converted.borrow();
            assert_eq!(called_address_to_be_converted.len(), 3);
            assert_eq!(called_address_to_be_converted[0], 0x1000); // begin sequence
            assert_eq!(called_address_to_be_converted[1], 0x1000); // first line row
            assert_eq!(called_address_to_be_converted[2], 0x1001); // end sequence
        }

        {
            let read_debug_line = read::DebugLine::new(converted_debug_line.slice(), LittleEndian);
            let read_program = read_debug_line
                .program(DebugLineOffset(0), 4, None, None)
                .unwrap();

            let mut rows = read_program.rows();
            let row = rows.next_row().unwrap().unwrap().1;
            assert_eq!(row.address(), 0x1010);
        }
    }
}
