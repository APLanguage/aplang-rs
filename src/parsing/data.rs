use chumsky::{
    prelude::Simple,
    primitive::just,
    text::{ident, keyword, TextParser},
    Parser,
};
#[derive(Debug)]
pub enum ParsedType {
    Data(String),
}
#[derive(Debug)]
pub struct Data {
    name: String,
    fields: Vec<(String, ParsedType)>,
}

fn field_parser() -> impl Parser<char, (String, ParsedType), Error = Simple<char>> {
    ident().labelled("data-field-name")
        .then_ignore(just(":").padded())
        .then(type_parser().labelled("data-field-type"))
        .labelled("data-field")
}

pub fn data_parser() -> impl Parser<char, Data, Error = Simple<char>> {
    keyword("data")
        .ignore_then(ident().padded().labelled("data-identifier"))
        .then(
            field_parser().padded()
                .separated_by(just(","))
                .allow_trailing()
                .delimited_by(just("{"), just("}"))
                .labelled("data-fields"),
        )
        .map(|(name, fields)| Data { name, fields })
        .labelled("data")
}

pub fn type_parser() -> impl Parser<char, ParsedType, Error = Simple<char>> {
    ident().map(ParsedType::Data).padded()
}
