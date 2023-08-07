use chumsky::span::SimpleSpan;

use crate::{typing::TypeId, source::DeclarationPath};

pub mod ast;
pub mod parsers;
pub mod tokenizer;
pub mod utilities;

#[derive(Debug, PartialEq)]
pub struct Infoed<T: Infoable> {
    pub inner: T,
    pub info: Option<T::Info>,
    pub span: SimpleSpan
}

pub trait Infoable {
    type Info;
}

#[derive(Debug, PartialEq)]
pub enum DeclarationRef {
    LocalVariable(usize),
    GlobalVariable(DeclarationPath),
    Function(DeclarationPath),
}

#[derive(Debug, PartialEq)]
pub enum Info {
    Type(Option<TypeId>),
    DeclarationRef(Option<DeclarationRef>),
}
