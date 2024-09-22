use crate::parsing::{
    ast::{
        declarations::{Field, Function, Parameter, Struct, Variable},
        ParsedType,
    },
    parsers::{expressions::expression_parser, TokenInput, TokenParser},
    tokenizer::{ident, keyword, Identifier, Token},
};
use chumsky::{
    primitive::{choice, group, just},
    recursive::recursive,
    Parser,
};

use super::{statements::statement_parser, CollectBoxedSliceExt, TokenParserExt};

fn field_parser<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, Field> {
    group((
        choice((
            keyword(Identifier::Var).to(true),
            keyword(Identifier::Val).to(false),
        ))
        .spanned(),
        ident().spur().spanned(),
        just(Token::Colon).paddedln(),
        type_parser()
            .spanned()
            .paddedln()
            .labelled("data-field-type")
            .boxed(),
    ))
    .labelled("data-field")
    .map(|(reassignable, name, _, ty)| Field {
        reassignable,
        name,
        ty,
    })
    .boxed()
}

pub fn struct_parser<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, Struct> {
    keyword(Identifier::Struct)
        .ignore_then(
            ident()
                .spur()
                .spanned()
                .paddedln()
                .labelled("data-identifier")
                .boxed(),
        )
        .then(
            field_parser()
                .paddedln()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect_boxed_slice()
                .boxed()
                .delimited_by(just(Token::BraceOpen), just(Token::BraceClosed))
                .labelled("data-fields")
                .or_not()
                .map(|f| f.unwrap_or_else(|| Box::new([])))
                .boxed(),
        )
        .map(|(name, fields)| Struct { name, fields })
        .labelled("data")
        .boxed()
}

pub fn type_parser<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, ParsedType> {
    recursive(|p| {
        choice((
            ident().spur().spanned().map(ParsedType::Data),
            p.delimited_by(just(Token::BracketOpen), just(Token::BracketClosed))
                .map(|t| ParsedType::Array(Box::new(t))),
        ))
    })
    .boxed()
}

pub fn variable_parser<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, Variable> {
    group((
        choice((
            keyword(Identifier::Var).to(true),
            keyword(Identifier::Val).to(false),
        ))
        .spanned()
        .labelled("var-modifier")
        .boxed(),
        ident()
            .spur()
            .spanned()
            .paddedln()
            .labelled("var-name")
            .boxed(),
        just(Token::Colon)
            .paddedln()
            .ignore_then(type_parser().spanned().paddedln().labelled("var-type"))
            .or_not()
            .labelled("var-colon-type")
            .boxed(),
        just(Token::Equal).paddedln(),
        expression_parser()
            .spanned()
            .boxed()
            .labelled("var-expression"),
    ))
    .map(|(reassignable, name, ty, _, expression)| Variable {
        reassignable,
        name,
        ty,
        expression,
    })
    .paddedln()
    .labelled("var")
    .boxed()
}

fn parameter_parser<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, Parameter> {
    group((
        choice((
            keyword(Identifier::Var).to(true),
            keyword(Identifier::Val).to(false),
        ))
        .spanned()
        .or_not()
        .labelled("fn-param-reassignable"),
        ident().spur().spanned().labelled("fn-param-name"),
        just(Token::Colon).paddedln().ignored(),
        type_parser()
            .spanned()
            .labelled("fn-param-type")
            .labelled("fn-param")
            .boxed(),
    ))
    .map(|(reassignable, name, _, ty)| Parameter {
        reassignable,
        name,
        ty,
    })
}

pub fn function_parser<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, Function> {
    keyword(Identifier::Fn)
        .ignore_then(group((
            ident().spur().spanned().paddedln().labelled("fn-name"),
            parameter_parser()
                .paddedln()
                .separated_by(just(Token::Comma))
                .collect_boxed_slice()
                .delimited_by(just(Token::ParenOpen), just(Token::ParenClosed))
                .paddedln()
                .labelled("fn-params")
                .boxed(),
            just(Token::ArrowRight)
                .paddedln()
                .labelled("fn-arrow")
                .ignore_then(type_parser().spanned().labelled("fn-type"))
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
        .map(|(name, parameters, ty, statements)| Function {
            name,
            parameters,
            ty,
            statements,
        })
        .paddedln()
        .labelled("function")
        .boxed()
}
