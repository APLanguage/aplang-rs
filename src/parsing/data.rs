use chumsky::{
    primitive::{just, choice},
    Parser, recursive::recursive, IterParser,
};

use super::{
    utilities::Spanned,
    tokenizer::{ident, Token,  keyword, Identifier, newline},
    TokenInput, TokenParser
};

#[derive(Debug)]
pub enum ParsedType {
    Data(Spanned<Identifier>),
    Array(Box<ParsedType>)
}

#[derive(Debug)]
pub struct Struct {
    pub name: Spanned<Identifier>,
    pub fields: Box<[(Spanned<Identifier>, Spanned<ParsedType>)]>,
}

fn field_parser<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, (Spanned<Identifier>, Spanned<ParsedType>)>
{
    ident()
        .map_with_span(Spanned)
        .then_ignore(just(Token::Colon).padded_by(newline().repeated()))
        .then(
            type_parser()
                .map_with_span(Spanned)
                .padded_by(newline().repeated())
                /* .labelled("data-field-type") */,
        ).boxed()
        /* .labelled("data-field") */
}

pub fn struct_parser<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, Struct> {
    keyword(Identifier::Struct)
        .ignore_then(
            ident()
                .map_with_span(Spanned)
                .padded_by(newline().repeated())
                /* .labelled("data-identifier") */,
        )
        .then(
            field_parser()
                .padded_by(newline().repeated())
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>().map(Vec::into_boxed_slice)
                .delimited_by(just(Token::BraceOpen), just(Token::BraceClosed)).boxed()
                /* .labelled("data-fields") */,
        )
        .map(|(name, fields)| Struct { name, fields })
        .boxed()
        /* .labelled("data") */
}

pub fn type_parser<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, ParsedType> + Clone {
    recursive(|p| choice((
        ident().map_with_span(Spanned).map(ParsedType::Data),
        p.delimited_by(just(Token::BracketOpen), just(Token::BracketClosed)).map(|t| ParsedType::Array(Box::new(t)))
    )))
}
