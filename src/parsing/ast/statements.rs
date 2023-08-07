use crate::parsing::{
    ast::expressions::Expression,
    utilities::Spanned,
};

use super::declarations::Declaration;

#[derive(Debug)]
pub enum ControlFlow {
    If {
        condition: Expression,
        then: Box<Statement>,
        other: Option<Box<Statement>>,
    },
    While {
        condition: Expression,
        statements: Box<[Statement]>,
    },
    Return(Option<Spanned<Expression>>),
    Break,
}

#[derive(Debug)]
pub enum Statement {
    ControlFlow(ControlFlow),
    Declaration(Declaration),
    Expression(Expression),
    None,
}