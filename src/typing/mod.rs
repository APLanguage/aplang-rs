pub mod typedast;

use std::num::NonZeroUsize;

use crate::source::DeclarationPath;

pub type TypeId = usize;

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
    Data(DeclarationPath),
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
    Unknown,
    Unit,
    Nothing,
}

pub enum PrimitiveType {
    String(bool),
}
