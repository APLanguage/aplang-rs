use chumsky::{
    extra::Full,
    input::{SliceInput, ValueInput},
    prelude::Cheap,
    primitive::{any, just},
    span::SimpleSpan,
    ConfigIterParser, IterParser, Parser,
};

pub type CharParserExtra<'a> = Full<Cheap<>, (), ()>;
pub trait CharInput<'a> = ValueInput<'a, Token = char, Span = SimpleSpan> + SliceInput<'a>;

#[derive(Debug, PartialEq, Clone)]
pub enum StringLiteral {
    Raw(String),
}

// TODO: uplift parsing to parser, for interpolation
pub fn string_parser<'a, I: CharInput<'a>>(
) -> impl Parser<'a, I, StringLiteral, CharParserExtra<'a>> + Clone {
    let end = just('"').then(just('#').repeated().configure(|cfg, n| cfg.exactly(*n)));
    return just('r')
        .ignore_then(just('#').repeated().count())
        .then_ignore(just('"'))
        .then_with_ctx(
            any()
                .and_is(end.not())
                .repeated()
                .collect::<String>()
                .map(StringLiteral::Raw)
                .then_ignore(end),
        );
}
