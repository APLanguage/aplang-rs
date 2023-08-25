pub mod typedast;

use std::num::NonZeroUsize;

use lasso::Spur;
use slotmap::new_key_type;

use crate::{
    project::{DependencyId, StructId, FileId},
    source::DeclarationPath, parsing::Spanned,
};

new_key_type! { pub struct TypeId; }

#[derive(Debug, PartialEq, Clone)]
pub enum OperationResult {
    If {
        condition: TypeId,
        then: TypeId,
        other: Option<TypeId>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    PrimitiveType(PrimitiveType),
    Data(DependencyId, StructId),
    Array {
        ty: TypeId,
        size: Option<NonZeroUsize>,
    },
    Function {
        parameters: Box<[TypeId]>,
        retty: Option<TypeId>,
    },
    Trait(DeclarationPath),
    Union(Box<[TypeId]>),
    Intersection(Box<[TypeId]>),
    InternalUnion(Box<[TypeId]>),
    Ref(TypeId),
    OperationResult(OperationResult),
    Unknown(FileId, Spanned<Spur>),
    Unit,
    Nothing,
}

#[derive(Clone, Debug, PartialEq, Copy)]
#[repr(u8)]
pub enum IntegerWidth {
    _8 = 8,
    _16 = 16,
    _32 = 32,
    _64 = 64,
}

impl TryFrom<u8> for IntegerWidth {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        IntegerWidth::try_from(Into::<u64>::into(value))
    }
}

impl TryFrom<u64> for IntegerWidth {
    type Error = ();

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        Ok(match value {
            8 => IntegerWidth::_8,
            16 => IntegerWidth::_16,
            32 => IntegerWidth::_32,
            64 => IntegerWidth::_64,
            _ => return Err(()),
        })
    }
}

#[derive(Clone, Debug, PartialEq, Copy)]
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

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum PrimitiveType {
    String,
    Integer(bool, IntegerWidth),
    Float(FloatWidth),
}
