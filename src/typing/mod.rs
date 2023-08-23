pub mod typedast;

use std::num::NonZeroUsize;

use slotmap::new_key_type;

use crate::{source::DeclarationPath, project::{DependencyId, StructId}};

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
    Unknown,
    Unit,
    Nothing,
}

pub enum PrimitiveType {
    String(bool),
}
