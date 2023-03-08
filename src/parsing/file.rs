use chumsky::{primitive::choice, Parser, IterParser};

use super::{
    data::struct_parser,
    declaration::{function_parser, variable_parser, Declaration}, tokenizer::newline,
    TokenInput, TokenParser
};

pub fn file_parser<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, Vec<Declaration>> {
    choice((
        struct_parser().map(Declaration::Struct),
        variable_parser(),
        function_parser(),
    ))
    .padded_by(newline().repeated())
    .repeated()
    .collect()
}
