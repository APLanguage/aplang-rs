use crate::parsing::{
    parsers::expressions::expression_parser,
    ast::declarations::{Struct, ParsedType, Declaration, Variable, Function},
    tokenizer::{ident, keyword, Identifier, Token},
    utilities::Spanned,
    TokenInput, TokenParser,
};
use chumsky::{primitive::{just, choice}, IterParser, Parser, recursive::recursive};

use super::statements::statement_parser;

fn field_parser<'a, I: TokenInput<'a>>(
) -> impl TokenParser<'a, I, (Spanned<Identifier>, Spanned<ParsedType>)> {
    ident()
        .map_with_span(Spanned)
        .then_ignore(just(Token::Colon).paddedln())
        .then(
            type_parser()
                .map_with_span(Spanned)
                .paddedln()
                .labelled("data-field-type"),
        )
        .boxed()
        .labelled("data-field")
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
                .collect::<Vec<_>>()
                .map(Vec::into_boxed_slice)
                .delimited_by(just(Token::BraceOpen), just(Token::BraceClosed))
                .boxed()
                .labelled("data-fields"),
        )
        .map(|(name, fields)| Struct { name, fields })
        .boxed()
        .labelled("data")
}

pub fn type_parser<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, ParsedType> + Clone {
    recursive(|p| {
        choice((
            ident().map_with_span(Spanned).map(ParsedType::Data),
            p.delimited_by(just(Token::BracketOpen), just(Token::BracketClosed))
                .map(|t| ParsedType::Array(Box::new(t))),
        ))
    })
}


pub fn variable_parser<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, Declaration> + Clone {
    just(Token::Identifier(Identifier::Var))
        .ignore_then(
            ident()
                .map_with_span(Spanned)
                .paddedln(),
        )
        .then_ignore(just(Token::Colon).paddedln())
        .then(
            type_parser()
                .map_with_span(Spanned)
                .paddedln(),
        )
        .then_ignore(just(Token::Equal).paddedln())
        .then(expression_parser().map_with_span(Spanned))
        .map(|((name, r#type), expression)| {
            Declaration::Variable(Variable {
                name,
                r#type,
                expression,
            })
        })
        .paddedln()
        .boxed()
        .labelled("var")
}

fn parameter_parser<'a, I: TokenInput<'a>>(
) -> impl TokenParser<'a, I, (Spanned<Identifier>, Spanned<ParsedType>)> {
    ident()
        .map_with_span(Spanned)
        .labelled("fn-param-name")
        .then_ignore(just(Token::Colon).paddedln())
        .then(
            type_parser()
                .map_with_span(Spanned)
                .labelled("fn-param-type"),
        )
        .labelled("fn-param")
}

pub fn function_parser<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, Declaration> {
    keyword(Identifier::Fn)
        .ignore_then(
            ident()
                .map_with_span(Spanned)
                .paddedln()
                .labelled("fn-name"),
        )
        .then(
            parameter_parser()
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .map(Vec::into_boxed_slice)
                .delimited_by(just(Token::ParenOpen), just(Token::ParenClosed))
                .paddedln()
                .boxed()
                .labelled("fn-params"),
        )
        .then(
            just(Token::ArrowRight)
                .paddedln()
                .labelled("fn-arrow")
                .ignore_then(type_parser().map_with_span(Spanned).labelled("fn-type"))
                .or_not(),
        )
        .then(
            statement_parser()
                .paddedln()
                .repeated()
                .collect::<Vec<_>>()
                .map(Vec::into_boxed_slice)
                .delimited_by(just(Token::BraceOpen), just(Token::BraceClosed))
                .boxed()
                .labelled("fn-stmts"),
        )
        .map(|(((name, parameters), r#type), statements)| {
            Declaration::Function(Function {
                name,
                parameters,
                r#type,
                statements,
            })
        })
        .paddedln()
        .boxed()
        .labelled("function")
}
