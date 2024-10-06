use chumsky::{prelude::just, primitive::choice, recursive::recursive, Parser};

use crate::parsing::{
    ast::{
        declarations::Declaration,
        statements::{ControlFlow, Statement},
    },
    parsers::{expressions::expression_parser, CollectBoxedSliceExt, TokenInput, TokenParser},
    tokenizer::{keyword, Identifier, Token},
    Spanned,
};

use super::{declarations::variable_parser, TokenParserExt};

pub fn if_parser<'a, I, SP>(stmt_parser: SP) -> impl TokenParser<'a, I, ControlFlow>
where
    I: TokenInput<'a>,
    SP: TokenParser<'a, I, Spanned<Statement>> + Clone + 'a,
{
    keyword(Identifier::If)
        .ignore_then(
            expression_parser()
                .spanned()
                .paddedln()
                .delimited_by(just(Token::ParenOpen), just(Token::ParenClosed)),
        )
        .then(choice((
            stmt_parser
                .clone()
                .paddedln()
                .repeated()
                .collect_boxed_slice()
                .delimited_by(just(Token::BraceOpen), just(Token::BraceClosed))
                .labelled("fn-stmts")
                .boxed(),
            stmt_parser
                .clone()
                .paddedln()
                .repeated()
                .exactly(1)
                .collect_boxed_slice(),
        )))
        .boxed()
        .then(
            keyword(Identifier::Else)
                .paddedln()
                .ignore_then(choice((
                    stmt_parser
                        .clone()
                        .paddedln()
                        .repeated()
                        .collect_boxed_slice()
                        .delimited_by(just(Token::BraceOpen), just(Token::BraceClosed))
                        .labelled("fn-stmts")
                        .boxed(),
                    stmt_parser
                        .paddedln()
                        .repeated()
                        .exactly(1)
                        .collect_boxed_slice(),
                )))
                .or_not()
                .boxed(),
        )
        .map(|((condition, then), other)| ControlFlow::If {
            condition,
            then,
            other,
        })
        .labelled("if")
        .boxed()
}

pub fn while_parser<'a, I, SP>(stmt_parser: SP) -> impl TokenParser<'a, I, ControlFlow>
where
    I: TokenInput<'a>,
    SP: TokenParser<'a, I, Spanned<Statement>> + Clone + 'a,
{
    keyword(Identifier::While)
        .ignore_then(
            expression_parser()
                .spanned()
                .paddedln()
                .delimited_by(just(Token::ParenOpen), just(Token::ParenClosed)),
        )
        .then(choice((
            stmt_parser
                .clone()
                .paddedln()
                .repeated()
                .collect_boxed_slice()
                .delimited_by(just(Token::BraceOpen), just(Token::BraceClosed))
                .labelled("fn-stmts")
                .boxed(),
            stmt_parser
                .paddedln()
                .repeated()
                .exactly(1)
                .collect_boxed_slice(),
        )))
        .map(|(condition, statements)| ControlFlow::While {
            condition,
            statements,
        })
        .labelled("while")
        .boxed()
}

pub fn control_flow_parser<'a, I, SP>(stmt_parser: SP) -> impl TokenParser<'a, I, ControlFlow>
where
    I: TokenInput<'a>,
    SP: TokenParser<'a, I, Spanned<Statement>> + Clone + 'a,
{
    choice((
        if_parser(stmt_parser.clone()),
        while_parser(stmt_parser),
        keyword(Identifier::Break)
            .map(|_| ControlFlow::Break)
            .labelled("break")
            .boxed(),
        return_parser(),
    ))
    .labelled("control-flow")
}

fn return_parser<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, ControlFlow> {
    keyword(Identifier::Return)
        .ignore_then(
            expression_parser()
                .spanned()
                .paddedln()
                .or_not()
                .labelled("return-expr")
                .boxed(),
        )
        .map(ControlFlow::Return)
        .labelled("return")
        .boxed()
}

pub fn statement_parser<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, Spanned<Statement>> {
    recursive::<_, Spanned<Statement>, _, _, _>(|p| {
        choice((
            variable_parser()
                .map(Declaration::Variable)
                .map(Statement::Declaration),
            control_flow_parser(p).map(Statement::ControlFlow),
            expression_parser().map(Statement::Expression),
        ))
        .spanned()
        .then_ignore(choice((
            just(Token::NewLine).repeated().ignored(),
            just(Token::Semicolon).repeated().ignored(),
            choice((just(Token::BraceClosed), just(Token::ParenClosed)))
                .rewind()
                .ignored(),
        )))
        .boxed()
        .or(just(Token::Semicolon).map(|_| Statement::None).spanned())
    })
    .labelled("statement")
}
