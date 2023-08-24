use lasso::Spur;

use crate::{
    parsing::{
        parsers::number::NumberLiteralResult,
        tokenizer::Operation,
        Spanned,
        Infoable, Infoed,
    },
    source::DeclarationPath, typing::TypeId,
};

#[derive(Debug, PartialEq)]
pub enum CallKind {
    Identifier(Spanned<Spur>),
    Call {
        identifier: Spanned<Spur>,
        parameters: Box<[Infoed<Expression>]>,
    },
}

#[derive(Debug, PartialEq)]
pub enum StringLiteral {
    Raw(Spur)
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    If {
        condition: Box<Infoed<Expression>>,
        then: Box<Infoed<Expression>>,
        other: Box<Infoed<Expression>>,
    },
    Number(NumberLiteralResult),
    StringLiteral(StringLiteral),
    CallChain {
        expression: Box<Infoed<Expression>>,
        calls: Box<[Infoed<CallKind>]>,
    },
    Call(CallKind),
    Operation {
        base: Box<Infoed<Expression>>,
        continuation: Box<[(Spanned<Operation>, Infoed<Expression>)]>,
    },
    Assignement {
        call: Infoed<CallKind>,
        op: Spanned<Operation>,
        expression: Box<Infoed<Expression>>,
    },
    Unary {
        ops: Box<[Spanned<Operation>]>,
        expression: Box<Infoed<Expression>>,
    },
}

impl Infoable for Expression {
    type Info = TypeId;
}

impl Infoable for CallKind {
    type Info = DeclarationPath;
}
