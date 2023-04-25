pub mod typedast;

use std::{fmt::Debug, num::NonZeroUsize};

use itertools::Itertools;

use crate::parsing::ast::{file::File};

pub type TypeId = usize;

#[derive(Debug)]
pub struct NodePath(Box<[usize]>);

impl PartialEq for NodePath {
    fn eq(&self, other: &Self) -> bool {
        self.0.len() == other.0.len() && self.0.iter().zip_eq(other.0.iter()).all(|(&a, &b)| a == b)
    }
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Data(NodePath),
    Array {
        ty: TypeId,
        size: Option<NonZeroUsize>,
    },
    Function {
        parameters: Box<[TypeId]>,
        retty: Option<TypeId>,
    },
    Trait(NodePath),
    Union(Box<[TypeId]>),
    Intersection(Box<[TypeId]>),
    InternalUnion(Box<[TypeId]>),
    Unknown,
    Unit,
    Nothing
}

pub struct FileNamespace {
    imports: Vec<Type>,
    ty_cache: Vec<Type>,
    ast: File
}
