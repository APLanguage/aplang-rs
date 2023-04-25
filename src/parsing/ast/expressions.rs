use crate::{
    parsing::{
        literals::number::NumberLiteralResult,
        tokenizer::{Identifier, Operation},
        utilities::Spanned,
        Infoed,
    },
    typing::NodePath,
};

#[derive(Debug, PartialEq)]
pub enum CallKind {
    Identifier(Spanned<Identifier>),
    Call {
        identifier: Spanned<Identifier>,
        parameters: Box<[Infoed<Expression>]>,
    },
}

#[derive(Debug, PartialEq)]
pub struct Call {
    pub kind: CallKind,
    pub declaration: Option<NodePath>,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    If {
        condition: Box<Infoed<Expression>>,
        then: Box<Infoed<Expression>>,
        other: Box<Infoed<Expression>>,
    },
    Number(NumberLiteralResult),
    StringLiteral(String),
    CallChain {
        expression: Box<Infoed<Expression>>,
        calls: Box<[Infoed<Call>]>,
    },
    Call(Call),
    Operation {
        base: Box<Infoed<Expression>>,
        continuation: Box<[(Spanned<Operation>, Infoed<Expression>)]>,
    },
    Assignement {
        call: Infoed<Call>,
        op: Spanned<Operation>,
        expression: Box<Infoed<Expression>>,
    },
    Unary {
        ops: Box<[Spanned<Operation>]>,
        expression: Box<Infoed<Expression>>,
    },
}
