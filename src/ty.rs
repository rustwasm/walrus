//! WebAssembly function and value types.

use crate::emit::{Emit, EmitContext};
use crate::encode::Encoder;
use crate::error::Result;
use crate::tombstone_arena::Tombstone;
use anyhow::bail;
use id_arena::Id;
use std::cmp::Ordering;
use std::fmt;
use std::hash;

/// An identifier for types.
pub type TypeId = Id<Type>;

/// A function type.
#[derive(Debug, Clone)]
pub struct Type {
    id: TypeId,
    params: Box<[ValType]>,
    results: Box<[ValType]>,

    // Whether or not this type is for a multi-value function entry block, and
    // therefore is for internal use only and shouldn't be emitted when we
    // serialize the Type section.
    is_for_function_entry: bool,

    /// An optional name for debugging.
    ///
    /// This is not really used by anything currently, but a theoretical WAT to
    /// walrus parser could keep track of the original name in the WAT.
    pub name: Option<String>,
}

impl PartialEq for Type {
    #[inline]
    fn eq(&self, rhs: &Type) -> bool {
        // NB: do not compare id or name.
        self.params == rhs.params
            && self.results == rhs.results
            && self.is_for_function_entry == rhs.is_for_function_entry
    }
}

impl Eq for Type {}

impl PartialOrd for Type {
    fn partial_cmp(&self, rhs: &Type) -> Option<Ordering> {
        Some(self.cmp(rhs))
    }
}

impl Ord for Type {
    fn cmp(&self, rhs: &Type) -> Ordering {
        self.params()
            .cmp(rhs.params())
            .then_with(|| self.results().cmp(rhs.results()))
    }
}

impl hash::Hash for Type {
    #[inline]
    fn hash<H: hash::Hasher>(&self, h: &mut H) {
        // Do not hash id or name.
        self.params.hash(h);
        self.results.hash(h);
        self.is_for_function_entry.hash(h);
    }
}

impl Tombstone for Type {
    fn on_delete(&mut self) {
        self.params = Box::new([]);
        self.results = Box::new([]);
    }
}

impl Type {
    /// Construct a new function type.
    #[inline]
    pub(crate) fn new(id: TypeId, params: Box<[ValType]>, results: Box<[ValType]>) -> Type {
        Type {
            id,
            params,
            results,
            is_for_function_entry: false,
            name: None,
        }
    }

    /// Construct a new type for function entry blocks.
    #[inline]
    pub(crate) fn for_function_entry(id: TypeId, results: Box<[ValType]>) -> Type {
        let params = vec![].into();
        Type {
            id,
            params,
            results,
            is_for_function_entry: true,
            name: None,
        }
    }

    /// Get the id of this type.
    #[inline]
    pub fn id(&self) -> TypeId {
        self.id
    }

    /// Get the parameters to this function type.
    #[inline]
    pub fn params(&self) -> &[ValType] {
        &*self.params
    }

    /// Get the results of this function type.
    #[inline]
    pub fn results(&self) -> &[ValType] {
        &*self.results
    }

    pub(crate) fn is_for_function_entry(&self) -> bool {
        self.is_for_function_entry
    }
}

impl Emit for Type {
    fn emit(&self, cx: &mut EmitContext) {
        assert!(!self.is_for_function_entry());
        cx.encoder.byte(0x60);
        cx.list(self.params.iter());
        cx.list(self.results.iter());
    }
}

/// A value type.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ValType {
    /// 32-bit integer.
    I32,
    /// 64-bit integer.
    I64,
    /// 32-bit float.
    F32,
    /// 64-bit float.
    F64,
    /// 128-bit vector.
    V128,
    /// The `externref` opaque value type
    Externref,
    /// The `funcref` value type, representing a callable function
    Funcref,
}

impl ValType {
    pub(crate) fn from_wasmparser_type(ty: wasmparser::Type) -> Result<Box<[ValType]>> {
        let v = match ty {
            wasmparser::Type::EmptyBlockType => Vec::new(),
            _ => vec![ValType::parse(&ty)?],
        };
        Ok(v.into_boxed_slice())
    }

    pub(crate) fn parse(input: &wasmparser::Type) -> Result<ValType> {
        match input {
            wasmparser::Type::I32 => Ok(ValType::I32),
            wasmparser::Type::I64 => Ok(ValType::I64),
            wasmparser::Type::F32 => Ok(ValType::F32),
            wasmparser::Type::F64 => Ok(ValType::F64),
            wasmparser::Type::V128 => Ok(ValType::V128),
            wasmparser::Type::ExternRef => Ok(ValType::Externref),
            wasmparser::Type::FuncRef => Ok(ValType::Funcref),
            _ => bail!("not a value type"),
        }
    }

    pub(crate) fn emit(&self, encoder: &mut Encoder) {
        match self {
            ValType::I32 => encoder.byte(0x7f),
            ValType::I64 => encoder.byte(0x7e),
            ValType::F32 => encoder.byte(0x7d),
            ValType::F64 => encoder.byte(0x7c),
            ValType::V128 => encoder.byte(0x7b),
            ValType::Funcref => encoder.byte(0x70),
            ValType::Externref => encoder.byte(0x6f),
        }
    }
}

impl fmt::Display for ValType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ValType::I32 => "i32",
                ValType::I64 => "i64",
                ValType::F32 => "f32",
                ValType::F64 => "f64",
                ValType::V128 => "v128",
                ValType::Externref => "externref",
                ValType::Funcref => "funcref",
            }
        )
    }
}

impl Emit for ValType {
    fn emit(&self, cx: &mut EmitContext) {
        self.emit(&mut cx.encoder);
    }
}
