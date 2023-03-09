use chumsky::{
    primitive::{choice, just},
    recursive::recursive,
    Parser,
};

use super::{
    control_flow::{control_flow_parser, ControlFlow},
    declaration::{variable_parser, Declaration},
    expression::{expression_parser, Expression}, tokenizer::{Token, newline,},
    TokenInput, TokenParser
};

#[derive(Debug)]
pub enum Statement {
    ControlFlow(ControlFlow),
    Declaration(Declaration),
    Expression(Expression),
    None,
}

pub fn statement_parser<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, Statement> + Clone {
    recursive(|p| {
        choice((
            variable_parser().map(Statement::Declaration),
            control_flow_parser(p).map(Statement::ControlFlow),
            expression_parser().map(Statement::Expression),
            just(Token::Semicolon).map(|_| Statement::None),
        )).padded_by(newline().repeated())
    }).boxed()
    // .labelled("statement")
}
