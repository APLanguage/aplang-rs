use chumsky::{
    primitive::{choice, just}, Parser,
};

use crate::parsing::{
    ast::{
        declarations::{Declaration, UseDeclaration, UsePath},
        file::File,
    },
    parsers::{
        declarations::{function_parser, struct_parser, variable_parser},
        CollectBoxedSliceExt, TokenInput, TokenParser, TokenParserExt,
    },
    tokenizer::{ident, keyword, Identifier, Token},
};

pub fn file_parser<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, File> {
    choice((
        struct_parser().map(Declaration::Struct),
        variable_parser(),
        function_parser(),
        use_parser().map(Declaration::Use),
    ))
    .boxed()
    .paddedln()
    .repeated()
    .collect_boxed_slice()
    .map(|declarations| File { declarations })
    .boxed()
}

fn use_parser<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, UseDeclaration> {
    keyword(Identifier::Use).ignore_then(
        choice((
            ident().to(UsePath::Part).infoed(),
            just(Token::Asterisk).to(UsePath::Star).infoed(),
        ))
        .separated_by(just(Token::ColonColon))
        .collect_boxed_slice()
        .map(UseDeclaration),
    )
}
