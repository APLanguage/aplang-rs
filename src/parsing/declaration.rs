use super::{
    data::ParsedType,
    data::{type_parser, Data},
    expression::{expression_parser, Expression},
    statement::{statement_parser, Statement},
};
use chumsky::{
    prelude::Simple,
    primitive::just,
    text::{ident, keyword, TextParser},
    Parser,
};

#[derive(Debug)]
pub enum Declaration {
    Variable {
        name: String,
        r#type: ParsedType,
        expression: Expression,
    },
    Function {
        name: String,
        parameters: Vec<(String, ParsedType)>,
        r#type: Option<ParsedType>,
        statements: Vec<Statement>,
    },
    Data(Data),
}

pub fn variable_parser() -> impl Parser<char, Declaration, Error = Simple<char>> {
    just("var")
        .ignore_then(ident().padded())
        .then_ignore(keyword(":").padded())
        .then(type_parser().padded())
        .then_ignore(keyword("=").padded())
        .then(expression_parser())
        .map(|((name, r#type), expression)| Declaration::Variable {
            name,
            r#type,
            expression,
        })
        .padded().labelled("var")
}

fn parameter_parser() -> impl Parser<char, (String, ParsedType), Error = Simple<char>> {
    ident()
        .labelled("fn-param-name")
        .then_ignore(just(":").padded())
        .then(type_parser().labelled("fn-param-type"))
        .labelled("fn-param")
}

pub fn function_parser() -> impl Parser<char, Declaration, Error = Simple<char>> {
    just("fn")
        .ignore_then(ident().padded().labelled("fn-name"))
        .then(
            parameter_parser()
                .separated_by(just(","))
                .delimited_by(just("("), just(")")).padded()
                .labelled("fn-params"),
        )
        .then(
            just("->").padded()
                .labelled("fn-arrow")
                .ignore_then(type_parser().labelled("fn-type"))
                .or_not(),
        )
        .then(
            statement_parser()
                .padded()
                .repeated()
                .delimited_by(just("{"), just("}"))
                .labelled("fn-stmts"),
        )
        .map(
            |(((name, parameters), r#type), statements)| Declaration::Function {
                name,
                parameters,
                r#type,
                statements,
            },
        )
        .padded().labelled("function")
}