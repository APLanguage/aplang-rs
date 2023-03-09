use crate::parsing::{
    literals::number::NumberLiteralResult,
    tokenizer::{Operation, Identifier},
    utilities::Spanned,
};

#[derive(Debug, PartialEq)]
pub enum Call {
    Identifier(Spanned<Identifier>),
    Call {
        identifier: Spanned<Identifier>,
        parameters: Box<[Spanned<Expression>]>,
    },
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    If {
        condition: Box<Spanned<Expression>>,
        then: Box<Spanned<Expression>>,
        other: Box<Spanned<Expression>>,
    },
    Number(NumberLiteralResult),
    StringLiteral(String),
    CallChain {
        expression: Box<Spanned<Expression>>,
        calls: Box<[Spanned<Call>]>,
    },
    Call(Call),
    Operation {
        base: Box<Spanned<Expression>>,
        continuation: Box<[(Spanned<Operation>, Spanned<Expression>)]>,
    },
    Assignement {
        call: Spanned<Call>,
        op: Spanned<Operation>,
        expression: Box<Spanned<Expression>>,
    },
    Unary {
        ops: Box<[Spanned<Operation>]>,
        expression: Box<Expression>,
    },
}