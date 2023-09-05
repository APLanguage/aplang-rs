use either::Either;
use lasso::Spur;
use slotmap::{new_key_type, SlotMap};

use crate::{
    parsing::{
        ast::expressions::StringLiteral,
        parsers::number::NumberLiteralResult,
        tokenizer::{Operation, OperationGroup},
        Infoable, Infoed, Spanned,
    },
    project::{DependencyId, FunctionId, StructId, VariableId},
    typing::TypeId,
};

new_key_type! { pub struct LocalVarId; }

#[derive(Debug)]
pub struct Function {
    pub locals: SlotMap<LocalVarId, Variable>,
    pub statements: Box<[Statement]>,
}

#[derive(Debug, PartialEq)]
pub enum VariableType {
    Local(LocalVarId),
    Parameter(usize),
    Outside(DependencyId, VariableId),
}

#[derive(Debug, PartialEq)]
pub enum CallKind {
    StructField(DependencyId, StructId, usize),
    Variable(VariableType),
    Function {
        dependency_id: DependencyId,
        f_or_s_id: Either<FunctionId, StructId>,
        parameters: Box<[Infoed<Expression>]>,
    },
    Unresolved,
}
#[derive(Debug, PartialEq)]
pub enum AssignableTarget {
    Var(TypeId, VariableType),
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
        calls: Box<[Spanned<CallKind>]>,
    },
    Call(Spanned<CallKind>),
    OperationChain {
        base: Box<Infoed<Expression>>,
        continuation: Box<[(Spanned<Operation>, Infoed<Expression>)]>,
        group: OperationGroup,
    },
    Binary {
        lhs: Box<Infoed<Expression>>,
        op: Spanned<Operation>,
        rhs: Box<Infoed<Expression>>,
        group: OperationGroup,
    },
    Assignement {
        call: Spanned<Result<AssignableTarget, ()>>,
        op: Spanned<Operation>,
        expression: Box<Infoed<Expression>>,
    },
    Unary {
        op: Spanned<Operation>,
        expression: Box<Infoed<Expression>>,
    },
}

#[derive(Debug)]
pub enum ControlFlow {
    If {
        condition: Infoed<Expression>,
        then: Box<[Spanned<Statement>]>,
        other: Option<Box<[Spanned<Statement>]>>,
    },
    While {
        condition: Infoed<Expression>,
        statements: Box<[Spanned<Statement>]>,
    },
    Return(Option<Infoed<Expression>>),
    Break,
}

#[derive(Debug)]
pub struct Variable {
    pub reassignable: Spanned<bool>,
    pub name: Spanned<Spur>,
    pub ty: TypeId,
}

#[derive(Debug)]
pub enum Statement {
    ControlFlow(ControlFlow),
    VariableDeclaration {
        var_id: LocalVarId,
        expr: Infoed<Expression>,
    },
    Expression(Infoed<Expression>),
    None,
}

impl Infoable for Expression {
    type Info = TypeId;
}
