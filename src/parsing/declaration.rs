use super::{
    data::ParsedType,
    data::{type_parser, Struct},
    expression::{expression_parser, Expression},
    statement::{statement_parser, Statement},
    tokenizer::{ident, keyword, Identifier, Token},
    utilities::Spanned,
    TokenInput, TokenParser,
};
use chumsky::{primitive::just, IterParser, Parser};

#[derive(Debug)]
pub struct Function {
    pub name: Spanned<Identifier>,
    pub parameters: Box<[(Spanned<Identifier>, Spanned<ParsedType>)]>,
    pub r#type: Option<Spanned<ParsedType>>,
    pub statements: Box<[Statement]>,
}

#[derive(Debug)]
pub struct Variable {
    pub name: Spanned<Identifier>,
    pub r#type: Spanned<ParsedType>,
    pub expression: Spanned<Expression>,
}

#[derive(Debug)]
pub enum Declaration {
    Variable(Variable),
    Function(Function),
    Struct(Struct),
}

pub fn variable_parser<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, Declaration> + Clone {
    just(Token::Identifier(Identifier::Var))
        .ignore_then(
            ident()
                .map_with_span(Spanned)
                .paddedln(),
        )
        .then_ignore(just(Token::Colon).paddedln())
        .then(
            type_parser()
                .map_with_span(Spanned)
                .paddedln(),
        )
        .then_ignore(just(Token::Equal).paddedln())
        .then(expression_parser().map_with_span(Spanned))
        .map(|((name, r#type), expression)| {
            Declaration::Variable(Variable {
                name,
                r#type,
                expression,
            })
        })
        .paddedln()
        .boxed()
        .labelled("var")
}

fn parameter_parser<'a, I: TokenInput<'a>>(
) -> impl TokenParser<'a, I, (Spanned<Identifier>, Spanned<ParsedType>)> {
    ident()
        .map_with_span(Spanned)
        .labelled("fn-param-name")
        .then_ignore(just(Token::Colon).paddedln())
        .then(
            type_parser()
                .map_with_span(Spanned)
                .labelled("fn-param-type"),
        )
        .labelled("fn-param")
}

pub fn function_parser<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, Declaration> {
    keyword(Identifier::Fn)
        .ignore_then(
            ident()
                .map_with_span(Spanned)
                .paddedln()
                .labelled("fn-name"),
        )
        .then(
            parameter_parser()
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .map(Vec::into_boxed_slice)
                .delimited_by(just(Token::ParenOpen), just(Token::ParenClosed))
                .paddedln()
                .boxed()
                .labelled("fn-params"),
        )
        .then(
            just(Token::ArrowRight)
                .paddedln()
                .labelled("fn-arrow")
                .ignore_then(type_parser().map_with_span(Spanned).labelled("fn-type"))
                .or_not(),
        )
        .then(
            statement_parser()
                .paddedln()
                .repeated()
                .collect::<Vec<_>>()
                .map(Vec::into_boxed_slice)
                .delimited_by(just(Token::BraceOpen), just(Token::BraceClosed))
                .boxed()
                .labelled("fn-stmts"),
        )
        .map(|(((name, parameters), r#type), statements)| {
            Declaration::Function(Function {
                name,
                parameters,
                r#type,
                statements,
            })
        })
        .paddedln()
        .boxed()
        .labelled("function")
}
