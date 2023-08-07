use std::convert::identity;

use chumsky::{
    primitive::{choice, just},
    recursive::recursive,
    IterParser, Parser,
};
use either::Either;
use itertools::Itertools;

use crate::parsing::{
    ast::declarations::{Declaration, UseDeclaration, UsePath, UsePathEnd},
    parsers::{
        declarations::{function_parser, struct_parser, variable_parser},
        CollectBoxedSliceExt, TokenInput, TokenParser, TokenParserExt,
    },
    tokenizer::{ident, keyword, newline, Identifier, Token},
};
pub type File = (Box<[UseDeclaration]>, Box<[Declaration]>);

pub fn file_parser<'a, I: TokenInput<'a>>(
) -> impl TokenParser<'a, I, File> + Clone {
    choice((
        struct_parser().map(Declaration::Struct).map(Either::Right),
        variable_parser()
            .map(Declaration::Variable)
            .map(Either::Right),
        function_parser()
            .map(Declaration::Function)
            .map(Either::Right),
        use_parser().map(Either::Left),
    ))
    .boxed()
    .paddedln()
    .repeated()
    .collect::<Vec<_>>()
    .map(|v: Vec<_>| {
        let (uses, declarations): (Vec<_>, Vec<_>) = v.into_iter().partition_map(identity);
        (uses.into_boxed_slice(), declarations.into_boxed_slice())
    })
    .boxed()
}

pub fn use_parser<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, UseDeclaration> {
    keyword(Identifier::Use).ignore_then(
        ident()
            .span()
            .separated_by(just(Token::Slash))
            .at_least(1)
            .collect::<Vec<_>>()
            .labelled("use-scope")
            .boxed()
            .delimited_by(just(Token::ParenOpen), just(Token::ParenClosed))
            .labelled("use-scope-delimiters")
            .boxed()
            .then({
                let path_p = recursive(|p| {
                    ident()
                        .span()
                        .separated_by(just(Token::ColonColon))
                        .at_least(1)
                        .collect_boxed_slice()
                        .labelled("use-path")
                        .boxed()
                        .then(
                            just(Token::ColonColon)
                                .ignore_then(choice((
                                    p.separated_by(just(Token::Comma).paddedln())
                                        .collect_boxed_slice()
                                        .delimited_by(
                                            just(Token::BraceOpen).then(newline().repeated()),
                                            newline().repeated().then(just(Token::BraceClosed)),
                                        )
                                        .map(UsePathEnd::Split)
                                        .boxed()
                                        .labelled("use-split"),
                                    just(Token::Asterisk).span().map(UsePathEnd::Star),
                                )))
                                .or_not()
                                .map(|e| e.unwrap_or(UsePathEnd::Single))
                                .boxed()
                                .labelled("use-path-corona"),
                        )
                        .map(|(paths, end)| UsePath(paths, end))
                });
                choice((
                    path_p
                        .clone()
                        .separated_by(just(Token::Comma).paddedln())
                        .collect_boxed_slice()
                        .boxed()
                        .delimited_by(
                            just(Token::BraceOpen).then(newline().repeated()),
                            newline().repeated().then(just(Token::BraceClosed)),
                        )
                        .map(|paths| UsePath(Box::new([]), UsePathEnd::Split(paths)))
                        .boxed(),
                    path_p,
                ))
            })
            .map(|(start, use_path)| UseDeclaration(start.into_boxed_slice(), use_path)),
    )
}
