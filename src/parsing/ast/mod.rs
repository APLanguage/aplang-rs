use lasso::Spur;

use super::{utilities::Spanned, Infoable};

pub mod expressions;
pub mod statements;
pub mod declarations;

#[derive(Debug)]
pub enum ParsedType {
    Data(Spanned<Spur>),
    Array(Box<ParsedType>),
}

impl Infoable for ParsedType {
    type Info = ();
}