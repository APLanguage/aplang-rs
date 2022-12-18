use chumsky::{
    prelude::Simple,
    primitive::{choice, filter, just},
    recursive::recursive,
    text::{keyword, newline, TextParser},
    Parser,
};

use super::{
    control_flow::{control_flow_parser, ControlFlow},
    declaration::{variable_parser, Declaration},
    expression::{atom_parser, expression_parser, Expression},
};

#[derive(Debug)]
pub enum Statement {
    ControlFlow(ControlFlow),
    Declaration(Declaration),
    Expression(Expression),
    None,
}

pub fn statement_parser() -> impl Parser<char, Statement, Error = Simple<char>> {
    recursive(|p| {
        choice((
            variable_parser().map(Statement::Declaration),
            control_flow_parser(&p).map(Statement::ControlFlow),
            expression_parser().map(Statement::Expression),
            just(";").map(|_| Statement::None),
        ))
        .padded()
    })
    .labelled("statement")
}
