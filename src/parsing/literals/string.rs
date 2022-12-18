use chumsky::{
    prelude::Simple,
    primitive::{filter, just, one_of},
    Parser,
};

pub fn string_parser() -> impl Parser<char, String, Error = Simple<char>>  + Clone{
    just('"')
        .ignore_then(
            filter(|&c| c != '"')
                .repeated()
                .collect::<String>()
                .map(|s| s.replace("\r\n", "\n")),
        )
        .then_ignore(one_of("\n\""))
}
