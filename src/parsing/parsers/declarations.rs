use crate::parsing::{
    ast::{
        declarations::{Declaration, Function, Struct, Variable},
        ParsedType,
    },
    parsers::{expressions::expression_parser, CollectBoxedSliceExt, TokenInput, TokenParser},
    tokenizer::{ident, keyword, Identifier, Token},
    utilities::Spanned,
    Infoed,
};
use chumsky::{
    primitive::{choice, group, just},
    recursive::recursive,
    span::SimpleSpan,
};

use super::{statements::statement_parser, TokenParserExt};

fn use_parser<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, Box<[&'a str]>> {
    keyword(Identifier::Use).ignore_then(
        ident()
            .map_with_state(|_, s: SimpleSpan, state| state.slice(s.into()))
            .separated_by(just(Token::ColonColon))
            .collect_boxed_slice(),
    )
}

fn field_parser<'a, I: TokenInput<'a>>(
) -> impl TokenParser<'a, I, (Spanned<Identifier>, Infoed<ParsedType>)> {
    group((
        ident()
            .map_with_span(Spanned)
            .then_ignore(just(Token::Colon).paddedln()),
        type_parser()
            .infoed()
            .paddedln()
            .labelled("data-field-type")
            .boxed(),
    ))
    .labelled("data-field")
    .boxed()
}

pub fn struct_parser<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, Struct> {
    keyword(Identifier::Struct)
        .ignore_then(
            ident()
                .map_with_span(Spanned)
                .paddedln()
                .labelled("data-identifier"),
        )
        .then(
            field_parser()
                .paddedln()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect_boxed_slice()
                .delimited_by(just(Token::BraceOpen), just(Token::BraceClosed))
                .labelled("data-fields")
                .boxed(),
        )
        .map(|(name, fields)| Struct { name, fields })
        .labelled("data")
        .boxed()
}

pub fn type_parser<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, ParsedType> + Clone {
    recursive(|p| {
        choice((
            ident().map_with_span(Spanned).map(ParsedType::Data),
            p.delimited_by(just(Token::BracketOpen), just(Token::BracketClosed))
                .map(|t| ParsedType::Array(Box::new(t))),
        ))
    })
    .boxed()
}

pub fn variable_parser<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, Declaration> + Clone {
    just(Token::Identifier(Identifier::Var))
        .ignore_then(group((
            ident().map_with_span(Spanned).paddedln(),
            just(Token::Colon)
                .paddedln()
                .ignore_then(type_parser().infoed().paddedln()),
            just(Token::Equal)
                .paddedln()
                .ignore_then(expression_parser().infoed())
                .boxed(),
        )))
        .map(|(name, ty, expression)| {
            Declaration::Variable(Variable {
                name,
                ty,
                expression,
            })
        })
        .paddedln()
        .labelled("var")
        .boxed()
}

fn parameter_parser<'a, I: TokenInput<'a>>(
) -> impl TokenParser<'a, I, (Spanned<Identifier>, Infoed<ParsedType>)> {
    ident()
        .map_with_span(Spanned)
        .labelled("fn-param-name")
        .then_ignore(just(Token::Colon).paddedln())
        .then(type_parser().infoed().labelled("fn-param-type"))
        .labelled("fn-param")
        .boxed()
}

pub fn function_parser<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, Declaration> {
    keyword(Identifier::Fn)
        .ignore_then(group((
            ident()
                .map_with_span(Spanned)
                .paddedln()
                .labelled("fn-name"),
            parameter_parser()
                .separated_by(just(Token::Comma))
                .collect_boxed_slice()
                .delimited_by(just(Token::ParenOpen), just(Token::ParenClosed))
                .paddedln()
                .labelled("fn-params")
                .boxed(),
            just(Token::ArrowRight)
                .paddedln()
                .labelled("fn-arrow")
                .ignore_then(type_parser().infoed().labelled("fn-type"))
                .or_not()
                .boxed(),
            statement_parser()
                .paddedln()
                .repeated()
                .collect_boxed_slice()
                .delimited_by(just(Token::BraceOpen), just(Token::BraceClosed))
                .labelled("fn-stmts")
                .boxed(),
        )))
        .map(|(name, parameters, ty, statements)| {
            Declaration::Function(Function {
                name,
                parameters,
                ty,
                statements,
            })
        })
        .paddedln()
        .labelled("function")
        .boxed()
}
