use chumsky::{
    prelude::Simple,
    primitive::{choice, just, one_of},
    recursive::recursive,
    text::{ident, keyword, TextParser},
    Parser,
};
use num_derive::{FromPrimitive, ToPrimitive};

use super::literals::{
    number::{complex_number_parser, NumberLiteralResult},
    string::string_parser,
};
use logos::Logos;

#[derive(Debug)]
pub enum Call {
    Identifier(String),
    Call {
        identifier: String,
        parameters: Vec<Expression>,
    },
}

#[derive(Debug)]
pub enum Expression {
    If {
        condition: Box<Expression>,
        then: Box<Expression>,
        other: Box<Expression>,
    },
    Number(NumberLiteralResult),
    StringLiteral(String),
    CallChain {
        expression: Box<Expression>,
        calls: Vec<Call>,
    },
    Call(Call),
    Operation {
        base: Box<Expression>,
        continuation: Vec<(Operator, Expression)>,
    },
}

#[derive(Logos, Debug, PartialEq, ToPrimitive, FromPrimitive)]
pub enum Operator {
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("/")]
    Slash,
    #[token("*")]
    Asterisk,
    #[token("**")]
    AsteriskAsterisk,

    #[error]
    Error,
}

fn call<EP>(expr_parser: EP) -> impl Parser<char, Call, Error = Simple<char>> + Clone
where
    EP: Parser<char, Expression, Error = Simple<char>> + Clone,
{
    ident()
        .then(
            expr_parser
                .padded()
                .separated_by(just(","))
                .allow_trailing()
                .at_least(0)
                .delimited_by(just("("), just(")"))
                .or_not(),
        )
        .map(|(identifier, params)| match params {
            Some(parameters) => Call::Call {
                identifier,
                parameters,
            },
            None => Call::Identifier(identifier),
        })
        .labelled("call")
}

fn if_expr_parser<EP>(expr_parser: EP) -> impl Parser<char, Expression, Error = Simple<char>>
where
    EP: Parser<char, Expression, Error = Simple<char>> + Clone,
{
    just("if")
        .ignore_then(expr_parser.clone().delimited_by(keyword("("), keyword(")")))
        .then(expr_parser.clone().padded())
        .then(keyword("else").padded().ignore_then(expr_parser))
        .map(|((condition, then), other)| Expression::If {
            condition: Box::new(condition),
            then: Box::new(then),
            other: Box::new(other),
        })
}

pub fn atom_parser<P>(
    expr_parser: P,
) -> impl Parser<char, Expression, Error = Simple<char>> + Clone
where
    P: Parser<char, Expression, Error = Simple<char>> + Clone,
 {
    choice((
        string_parser().map(Expression::StringLiteral),
        complex_number_parser().map(Expression::Number),
        call(expr_parser.clone()).map(Expression::Call),
    ))
    .then(just(".").ignore_then(call(expr_parser)).repeated())
    .map(|(expression, calls)| {
        if calls.is_empty() {
            expression
        } else {
            Expression::CallChain {
                expression: Box::new(expression),
                calls,
            }
        }
    })
    .labelled("atom")
}

pub fn expression_parser() -> impl Parser<char, Expression, Error = Simple<char>> + Clone {
    recursive(|p| {
        choice((
            if_expr_parser(p.clone()),
            p.clone().delimited_by(just("("), just(")")),
            math_parser("+-", math_parser("*/", atom_parser(p.clone()), p.clone()), p).labelled("expression"),
        ))
    })
}

pub fn math_parser<NP, P>(
    ops: &'static str,
    next_parser: NP,
    expr_parser: P,
) -> impl Parser<char, Expression, Error = Simple<char>> + Clone
where
    NP: Parser<char, Expression, Error = Simple<char>> + Clone,
    P: Parser<char, Expression, Error = Simple<char>> + Clone,
{
    atom_parser(expr_parser)
        .padded()
        .then(
            one_of(ops)
                .repeated()
                .at_least(1)
                .collect::<String>()
                .map(|o| Operator::lexer(&o).next().unwrap_or(Operator::Error))
                .padded()
                .then(next_parser)
                .repeated()
                .or_not(),
        )
        .map(|(atom, chain)| match chain {
            Some(chain) => {
                if chain.is_empty() {
                    atom
                } else {
                    Expression::Operation {
                        base: Box::new(atom),
                        continuation: chain,
                    }
                }
            }
            None => atom,
        })
        .padded()
        .labelled("math")
}
