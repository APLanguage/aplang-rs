use chumsky::{primitive::choice, Parser, IterParser};

use super::{
    data::struct_parser,
    declaration::{function_parser, variable_parser, Declaration},
    TokenInput, TokenParser
};

pub fn file_parser<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, Vec<Declaration>> {
    choice((
        struct_parser().map(Declaration::Struct),
        variable_parser(),
        function_parser(),
    ))
    .paddedln()
    .repeated()
    .collect()
}
