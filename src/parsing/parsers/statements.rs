use chumsky::{prelude::just, primitive::choice, recursive::recursive, Parser};

use crate::parsing::{
    ast::statements::{ControlFlow, Statement},
    parsers::{expressions::expression_parser, TokenInput, TokenParser},
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
    SP: TokenParser<'a, I, Statement> + Clone, {
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
            variable_parser().map(Statement::Declaration),
            control_flow_parser(p).map(Statement::ControlFlow),
            expression_parser().map(Statement::Expression),
            just(Token::Semicolon).map(|_| Statement::None),
        ))
        .paddedln()
    })
    .labelled("statement")
    .boxed()
}
