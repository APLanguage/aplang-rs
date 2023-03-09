use chumsky::{prelude::just, primitive::choice, recursive::recursive, Parser};

use crate::parsing::{
    ast::statements::{ControlFlow, Statement},
    parsers::expressions::expression_parser,
    tokenizer::{keyword, Identifier, Token},
    utilities::Spanned,
    TokenInput, TokenParser,
};

use super::declarations::variable_parser;

pub fn if_parser<'a, I: TokenInput<'a>>(
    stmt_parser: impl TokenParser<'a, I, Statement> + Clone,
) -> impl TokenParser<'a, I, ControlFlow> + Clone {
    keyword(Identifier::If)
        .ignore_then(
            expression_parser().delimited_by(just(Token::ParenOpen), just(Token::ParenClosed)),
        )
        .then(stmt_parser.clone().paddedln())
        .then(
            keyword(Identifier::Else)
                .paddedln()
                .ignore_then(stmt_parser)
                .or_not(),
        )
        .map(|((condition, then), other)| ControlFlow::If {
            condition,
            then: Box::new(then),
            other: other.map(Box::new),
        })
        .labelled("if")
}

pub fn while_parser<'a, I: TokenInput<'a>>(
    stmt_parser: impl TokenParser<'a, I, Statement> + Clone,
) -> impl TokenParser<'a, I, ControlFlow> + Clone {
    keyword(Identifier::While)
        .ignore_then(
            expression_parser().delimited_by(just(Token::ParenOpen), just(Token::ParenClosed)),
        )
        .then(stmt_parser.paddedln())
        .map(|(condition, statement)| ControlFlow::While {
            condition,
            statement: Box::new(statement),
        })
        .labelled("while")
}

pub fn control_flow_parser<'a, I: TokenInput<'a>>(
    stmt_parser: impl TokenParser<'a, I, Statement> + Clone,
) -> impl TokenParser<'a, I, ControlFlow> + Clone {
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
                .labelled("return-expr"),
        )
        .map(ControlFlow::Return)
        .labelled("return")
}

pub fn statement_parser<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, Statement> + Clone {
    recursive(|p| {
        choice((
            variable_parser().map(Statement::Declaration),
            control_flow_parser(p).map(Statement::ControlFlow),
            expression_parser().map(Statement::Expression),
            just(Token::Semicolon).map(|_| Statement::None),
        ))
        .paddedln()
    })
    .boxed()
    .labelled("statement")
}
