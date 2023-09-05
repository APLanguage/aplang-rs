use strum_macros::IntoStaticStr;

use crate::parsing::{
    ast::expressions::Expression,
    Spanned,
};

use super::declarations::Declaration;

#[derive(Debug, IntoStaticStr)]
pub enum ControlFlow {
    If {
        condition: Spanned<Expression>,
        then: Box<[Spanned<Statement>]>,
        other: Option<Box<[Spanned<Statement>]>>,
    },
    While {
        condition: Spanned<Expression>,
        statements: Box<[Spanned<Statement>]>,
    },
    Return(Option<Spanned<Expression>>),
    Break,
}

#[derive(Debug, IntoStaticStr)]
pub enum Statement {
    ControlFlow(ControlFlow),
    Declaration(Declaration),
    Expression(Expression),
    None,
}