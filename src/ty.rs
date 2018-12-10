//! TODO

use id_arena::Id;
use parity_wasm::elements;
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
}

impl PartialEq for Type {
    #[inline]
    fn eq(&self, rhs: &Type) -> bool {
        // Do not compare id.
        self.params == rhs.params && self.results == rhs.results
    }
}

impl Eq for Type {}

impl hash::Hash for Type {
    #[inline]
    fn hash<H: hash::Hasher>(&self, h: &mut H) {
        // Do not hash id.
        self.params.hash(h);
        self.results.hash(h)
    }
}

impl Type {
    /// Construct a new function type.
    #[inline]
    pub fn new(id: TypeId, params: Box<[ValType]>, results: Box<[ValType]>) -> Type {
        Type {
            id,
            params,
            results,
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
}

/// TODO
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ValType {
    /// TODO
    I32,
    /// TODO
    I64,
    /// TODO
    F32,
    /// TODO
    F64,
    /// TODO
    V128,
}

impl<'a> From<&'a elements::ValueType> for ValType {
    fn from(x: &'a elements::ValueType) -> ValType {
        match x {
            elements::ValueType::I32 => ValType::I32,
            elements::ValueType::I64 => ValType::I64,
            elements::ValueType::F32 => ValType::F32,
            elements::ValueType::F64 => ValType::F64,
            elements::ValueType::V128 => ValType::V128,
        }
    }
}

impl From<ValType> for elements::ValueType {
    fn from(x: ValType) -> elements::ValueType {
        match x {
            ValType::I32 => elements::ValueType::I32,
            ValType::I64 => elements::ValueType::I64,
            ValType::F32 => elements::ValueType::F32,
            ValType::F64 => elements::ValueType::F64,
            ValType::V128 => elements::ValueType::V128,
        }
    }
}

impl ValType {
    /// Construct a vector of `ValType`s from a parity-wasm `BlockType`.
    pub fn from_block_ty(block_ty: &elements::BlockType) -> Box<[ValType]> {
        let v = match block_ty {
            elements::BlockType::Value(ty) => vec![ty.into()],
            elements::BlockType::NoResult => vec![],
        };
        v.into_boxed_slice()
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
            }
        )
    }
}
