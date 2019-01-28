pub const MAX_U32_LENGTH: usize = 5;

#[derive(Debug)]
pub struct Encoder<'a> {
    dst: &'a mut Vec<u8>,
}

impl<'data> Encoder<'data> {
    pub fn new(dst: &'data mut Vec<u8>) -> Encoder<'data> {
        Encoder { dst }
    }

    pub fn byte(&mut self, byte: u8) {
        self.dst.push(byte);
    }

    pub fn bytes(&mut self, bytes: &[u8]) {
        self.usize(bytes.len());
        self.raw(bytes);
    }

    pub fn str(&mut self, data: &str) {
        self.bytes(data.as_bytes())
    }

    pub fn usize(&mut self, amt: usize) {
        assert!(amt <= u32::max_value() as usize);
        self.u32(amt as u32)
    }

    pub fn u32(&mut self, amt: u32) {
        leb128::write::unsigned(&mut self.dst, amt.into()).unwrap();
    }

    pub fn i32(&mut self, val: i32) {
        leb128::write::signed(&mut self.dst, val.into()).unwrap();
    }

    pub fn i64(&mut self, val: i64) {
        leb128::write::signed(&mut self.dst, val).unwrap();
    }

    pub fn f32(&mut self, val: f32) {
        let bits = val.to_bits();
        for i in 0..4 {
            self.byte((bits >> (i * 8)) as u8);
        }
    }

    pub fn f64(&mut self, val: f64) {
        let bits = val.to_bits();
        for i in 0..8 {
            self.byte((bits >> (i * 8)) as u8);
        }
    }

    pub fn raw(&mut self, raw: &[u8]) {
        self.dst.extend_from_slice(raw);
    }

    /// Reserves `bytes` bytes of space, returning the position at which the
    /// reservation starts
    pub fn reserve(&mut self, bytes: usize) -> usize {
        let start = self.dst.len();
        for _ in 0..bytes {
            self.byte(0);
        }
        return start;
    }

    /// Reserves space to write a uleb128 `u32`, returning the postition at
    /// hwich it can be written.
    pub fn reserve_u32(&mut self) -> usize {
        self.reserve(MAX_U32_LENGTH)
    }

    pub fn pos(&self) -> usize {
        self.dst.len()
    }

    // TODO: don't write this code here, use upstream once
    // gimli-rs/leb128#6 is implemented
    pub fn u32_at(&mut self, pos: usize, mut amt: u32) {
        for i in 0..MAX_U32_LENGTH {
            let flag = if i == MAX_U32_LENGTH - 1 { 0 } else { 0x80 };
            self.dst[pos + i] = (amt as u8) & 0x7f | flag;
            amt >>= 7;
        }
    }
}
