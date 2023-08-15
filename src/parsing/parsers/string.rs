use chumsky::{
    extra::Full,
    input::{SliceInput, ValueInput},
    prelude::Cheap,
    primitive::{any, just},
    span::SimpleSpan,
    ConfigIterParser, IterParser, Parser,
};
use lasso::Rodeo;

use crate::parsing::ast::expressions::StringLiteral;

pub type CharParserExtra<'a> = Full<Cheap, (), ()>;
pub trait CharInput<'a> = ValueInput<'a, Token = char, Span = SimpleSpan> + SliceInput<'a>;

#[derive(Debug, PartialEq, Clone)]
pub enum StringLiteralType {
    Raw,
}

pub enum UnlassoedStringLiteral {
    Raw(String),
}

impl UnlassoedStringLiteral {
    pub fn into_lassoed(self, rodeo: &mut Rodeo) -> StringLiteral {
        match self {
            UnlassoedStringLiteral::Raw(s) => StringLiteral::Raw(rodeo.get_or_intern(s)),
        }
    }
}

pub fn string_literal_outline_parser<'a, I: CharInput<'a>>(
) -> impl Parser<'a, I, StringLiteralType, CharParserExtra<'a>> + Clone {
    let end = just('"').then(just('#').repeated().configure(|cfg, n| cfg.exactly(*n)));
    return just('r')
        .ignore_then(just('#').repeated().count())
        .then_ignore(just('"'))
        .then_with_ctx(
            any()
                .and_is(end.not())
                .repeated()
                .to(StringLiteralType::Raw)
                .then_ignore(end),
        );
}

pub fn string_parser<'a>(
    literal_type: StringLiteralType,
) -> impl Parser<'a, &'a str, UnlassoedStringLiteral, Full<Cheap, (), ()>> + Clone {
    return match literal_type {
        StringLiteralType::Raw => {
            let end = just('"').then(just('#').repeated().configure(|cfg, n| cfg.exactly(*n)));
            just('r')
                .ignore_then(just('#').repeated().count())
                .then_ignore(just('"'))
                .then_with_ctx(
                    any()
                        .and_is(end.not())
                        .repeated()
                        .slice()
                        .map(|s: &'a str| {
                            UnlassoedStringLiteral::Raw(s.to_owned().replace("\r\n", "\n"))
                        })
                        .then_ignore(end),
                )
        }
    };
}
