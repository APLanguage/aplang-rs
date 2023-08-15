use chumsky::{prelude::just, primitive::choice, recursive::recursive, Parser};

use crate::parsing::{
    ast::{
        declarations::Declaration,
        statements::{ControlFlow, Statement},
    },
    parsers::{expressions::expression_parser, CollectBoxedSliceExt, TokenInput, TokenParser},
    tokenizer::{keyword, Identifier, Token},
    utilities::Spanned,
};

use super::{declarations::variable_parser, TokenParserExt};

pub fn if_parser<'a, I, SP>(stmt_parser: SP) -> impl TokenParser<'a, I, ControlFlow> + Clone
where
    I: TokenInput<'a>,
    SP: TokenParser<'a, I, Statement> + Clone + 'a, {
    keyword(Identifier::If)
        .ignore_then(
            expression_parser().delimited_by(just(Token::ParenOpen), just(Token::ParenClosed)),
        )
        .then(stmt_parser.clone().paddedln())
        .then(
            keyword(Identifier::Else)
                .paddedln()
                .ignore_then(stmt_parser)
                .or_not()
                .boxed(),
        )
        .map(|((condition, then), other)| ControlFlow::If {
            condition,
            then: Box::new(then),
            other: other.map(Box::new),
        })
        .labelled("if")
}

pub fn while_parser<'a, I, SP>(stmt_parser: SP) -> impl TokenParser<'a, I, ControlFlow> + Clone
where
    I: TokenInput<'a>,
    SP: TokenParser<'a, I, Statement> + Clone + 'a, {
    keyword(Identifier::While)
        .ignore_then(
            expression_parser()
                .paddedln()
                .delimited_by(just(Token::ParenOpen), just(Token::ParenClosed)),
        )
        .then(choice((
            stmt_parser
                .clone()
                .paddedln()
                .repeated()
                .exactly(1)
                .collect_boxed_slice(),
            stmt_parser
                .paddedln()
                .repeated()
                .collect_boxed_slice()
                .delimited_by(just(Token::BraceOpen), just(Token::BraceClosed))
                .labelled("fn-stmts")
                .boxed(),
        )))
        .map(|(condition, statements)| ControlFlow::While {
            condition,
            statements,
        })
        .labelled("while")
        .boxed()
}

pub fn control_flow_parser<'a, I, SP>(
    stmt_parser: SP,
) -> impl TokenParser<'a, I, ControlFlow> + Clone
where
    I: TokenInput<'a>,
    SP: TokenParser<'a, I, Statement> + Clone + 'a, {
    choice((
        if_parser(stmt_parser.clone()),
        while_parser(stmt_parser),
        keyword(Identifier::Break)
            .map(|_| ControlFlow::Break)
            .labelled("break"),
        return_parser(),
    ))
    .labelled("control-flow")
}

fn return_parser<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, ControlFlow> + Clone {
    keyword(Identifier::Return)
        .ignore_then(
            expression_parser()
                .map_with_span(Spanned)
                .paddedln()
                .or_not()
                .labelled("return-expr")
                .boxed(),
        )
        .map(ControlFlow::Return)
        .labelled("return")
        .boxed()
}

pub fn statement_parser<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, Statement> + Clone {
    recursive(|p| {
        choice((
            variable_parser()
                .map(Declaration::Variable)
                .map(Statement::Declaration),
            control_flow_parser(p).map(Statement::ControlFlow),
            expression_parser().map(Statement::Expression),
        ))
        .then_ignore(choice((
            just(Token::NewLine).repeated().ignored(),
            just(Token::Semicolon).repeated().ignored(),
            choice((just(Token::BraceClosed), just(Token::ParenClosed)))
                .rewind()
                .ignored(),
        )))
        .or(just(Token::Semicolon).map(|_| Statement::None))
    })
    .labelled("statement")
    .boxed()
}
