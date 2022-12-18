use chumsky::{prelude::Simple, primitive::choice, Parser};

use super::{
    data::data_parser,
    declaration::{function_parser, variable_parser, Declaration},
};

pub fn file_parser() -> impl Parser<char, Vec<Declaration>, Error = Simple<char>> {
    choice((
        data_parser().map(Declaration::Data),
        variable_parser(),
        function_parser(),
    ))
    .repeated()
}
