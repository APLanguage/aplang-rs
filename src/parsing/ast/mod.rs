use super::{utilities::Spanned, tokenizer::Identifier};

pub mod expressions;
pub mod statements;
pub mod declarations;
pub mod file;

#[derive(Debug)]
pub enum ParsedType {
    Data(Spanned<Identifier>),
    Array(Box<ParsedType>),
}