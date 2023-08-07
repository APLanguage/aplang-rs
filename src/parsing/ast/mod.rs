use crate::typing::TypeId;

use super::{utilities::Spanned, tokenizer::Identifier, Infoable};

pub mod expressions;
pub mod statements;
pub mod declarations;

#[derive(Debug)]
pub enum ParsedType {
    Data(Spanned<Identifier>),
    Array(Box<ParsedType>),
}

impl Infoable for ParsedType {
    type Info = TypeId;
}