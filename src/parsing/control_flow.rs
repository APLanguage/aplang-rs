use chumsky::{
    prelude::Simple,
    primitive::{choice, just},
    text::{keyword, TextParser},
    Parser,
};

use super::{
    expression::{expression_parser, Expression},
    statement::Statement,
};

#[derive(Debug)]
pub enum ControlFlow {
    If {
        condition: Expression,
        then: Box<Statement>,
        other: Option<Box<Statement>>,
    },
    While {
        condition: Expression,
        statement: Box<Statement>,
    },
}

pub fn if_parser(
    stmt_parser: &(impl Parser<char, Statement, Error = Simple<char>> + Clone),
) -> impl Parser<char, ControlFlow, Error = Simple<char>> {
    just("if")
        .ignore_then(expression_parser().delimited_by(just("("), just(")")))
        .then(stmt_parser.clone().padded())
        .then(
            keyword("else")
                .padded()
                .ignore_then(stmt_parser.clone())
                .or_not(),
        )
        .map(|((condition, then), other)| ControlFlow::If {
            condition,
            then: Box::new(then),
            other: other.map(Box::new),
        }).labelled("if")
}

pub fn while_parser(
    stmt_parser: &(impl Parser<char, Statement, Error = Simple<char>> + Clone),
) -> impl Parser<char, ControlFlow, Error = Simple<char>> {
    just("while")
        .ignore_then(expression_parser().delimited_by(just("("), just(")")))
        .then(stmt_parser.clone().padded())
        .map(|(condition, statement)| ControlFlow::While {
            condition,
            statement: Box::new(statement),
        }).labelled("while")
}

pub fn control_flow_parser(
    stmt_parser: &(impl Parser<char, Statement, Error = Simple<char>> + Clone),
) -> impl Parser<char, ControlFlow, Error = Simple<char>> {
    choice((if_parser(stmt_parser), while_parser(stmt_parser))).labelled("control-flow")
}