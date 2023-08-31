use lasso::Spur;

use crate::parsing::{
    parsers::number::NumberLiteralResult,
    tokenizer::{Operation, OperationGroup},
    Spanned,
};

#[derive(Debug, PartialEq)]
pub enum CallKind {
    Identifier(Spanned<Spur>),
    Call {
        identifier: Spanned<Spur>,
        parameters: Box<[Spanned<Expression>]>,
    },
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum StringLiteral {
    Raw(Spur),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    If {
        condition: Box<Spanned<Expression>>,
        then: Box<Spanned<Expression>>,
        other: Box<Spanned<Expression>>,
    },
    Number(NumberLiteralResult),
    StringLiteral(StringLiteral),
    CallChain {
        expression: Box<Spanned<Expression>>,
        calls: Box<[Spanned<CallKind>]>,
    },
    Call(CallKind),
    OperationChain {
        base: Box<Spanned<Expression>>,
        continuation: Box<[(Spanned<Operation>, Spanned<Expression>)]>,
        group: OperationGroup,
    },
    Binary {
        lhs: Box<Spanned<Expression>>,
        op: Spanned<Operation>,
        rhs: Box<Spanned<Expression>>,
        group: OperationGroup,
    },
    Assignement {
        call: Spanned<CallKind>,
        op: Spanned<Operation>,
        expression: Box<Spanned<Expression>>,
    },
    Unary {
        ops: Box<[Spanned<Operation>]>,
        expression: Box<Spanned<Expression>>,
    },
}
