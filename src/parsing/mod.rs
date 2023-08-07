use chumsky::span::SimpleSpan;

use crate::typing::{NodePath, TypeId};

use self::utilities::SourceId;

pub mod ast;
pub mod parsers;
pub mod tokenizer;
pub mod utilities;

#[derive(Debug, PartialEq)]
pub struct Infoed<T> {
    pub inner: T,
    pub info: Option<Info>,
    pub loc: Location,
}

#[derive(Debug, PartialEq)]
pub struct Location {
    pub span: SimpleSpan,
    pub source_id: Option<SourceId>,
}

#[derive(Debug, PartialEq)]
pub enum DeclarationRef {
    LocalVariable(usize),
    GlobalVariable(NodePath),
    Function(NodePath),
}

#[derive(Debug, PartialEq)]
pub enum Info {
    Type(Option<TypeId>),
    DeclarationRef(Option<DeclarationRef>),
}
