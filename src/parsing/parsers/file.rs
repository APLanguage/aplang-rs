use chumsky::{primitive::choice, IterParser, Parser};

use crate::parsing::{
    ast::{declarations::Declaration, file::File},
    parsers::declarations::{function_parser, struct_parser, variable_parser},
    TokenInput, TokenParser,
};

pub fn file_parser<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, File> {
    choice((
        struct_parser().map(Declaration::Struct),
        variable_parser(),
        function_parser(),
    ))
    .paddedln()
    .repeated()
    .collect::<Vec<_>>()
    .map(|v| File::new(v.into_boxed_slice()))
}
