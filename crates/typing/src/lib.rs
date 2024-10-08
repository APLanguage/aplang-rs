use std::num::NonZeroUsize;

use slotmap::new_key_type;
use strum_macros::Display;

use aplang_parser::parsers::number::LiteralWidth;

new_key_type! { pub struct TypeId; }

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum OperationResult {
    If {
        condition: TypeId,
        then: TypeId,
        other: Option<TypeId>,
    },
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum Type {
    PrimitiveType(PrimitiveType),
    Array {
        ty: TypeId,
        size: Option<NonZeroUsize>,
    },
    Ref(TypeId),
    Error,
}

#[derive(Clone, Debug, PartialEq, Copy, Hash, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum FloatWidth {
    _32 = 32,
    _64 = 64,
}

impl TryFrom<u8> for FloatWidth {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        FloatWidth::try_from(Into::<u64>::into(value))
    }
}

impl TryFrom<u64> for FloatWidth {
    type Error = ();

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        Ok(match value {
            32 => FloatWidth::_32,
            64 => FloatWidth::_64,
            _ => return Err(()),
        })
    }
}

#[derive(Debug, PartialEq, Clone, Copy, Eq, Hash, Display)]
pub enum PrimitiveType {
    String,
    Integer(bool, LiteralWidth),
    Float(FloatWidth),
    Boolean,
    Unit,
    Nothing,
}
