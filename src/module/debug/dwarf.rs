use gimli::*;

/// d
struct ConvertContext<'a, R: Reader<Offset = usize>> {
    /// b
    dwarf: &'a read::Dwarf<R>,

    /// c
    convert_address: &'a dyn Fn(u64) -> Option<write::Address>,
}
