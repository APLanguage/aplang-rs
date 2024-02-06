use either::Either;
use lasso::Spur;
use slotmap::{new_key_type, SlotMap};

use crate::{
    parsing::{
        self,
        ast::expressions::StringLiteral,
        parsers::{self, number::NumberLiteralError},
        tokenizer::{Operation, OperationGroup},
        Infoable, Infoed, Spanned,
    },
    project::{DependencyId, FunctionId, ProjectLink, StructId, StructLink, VariableId},
    typing::{FloatWidth, IntegerWidth, TypeId},
};

new_key_type! { pub struct LocalVarId; }

#[derive(Debug)]
pub struct Function {
    pub locals: SlotMap<LocalVarId, Variable>,
    pub statements: Box<[Statement]>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum VariableType {
    Local(LocalVarId),
    Parameter(usize),
    Outside(DependencyId, VariableId),
}

#[derive(Debug, PartialEq)]
pub enum CallKind {
    StructField(StructLink, usize),
    Variable(VariableType),
    Function {
        project_link: ProjectLink,
        f_or_s_id: Either<FunctionId, StructId>,
        parameters: Box<[Infoed<Expression>]>,
    },
    Unresolved,
}
#[derive(Debug, PartialEq)]
pub enum AssignableTarget {
    Var(TypeId, VariableType),
    StructField(TypeId, StructLink, usize),
    Unnassignable,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    If {
        condition: Box<Infoed<Expression>>,
        then: Box<Infoed<Expression>>,
        other: Box<Infoed<Expression>>,
    },
    Number(Result<NumberLiteral, NumberLiteralError>),
    StringLiteral(StringLiteral),
    Bool(bool),
    CallChain {
        expression: Box<Infoed<Expression>>,
        calls: Box<[Infoed<CallKind>]>,
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

impl Infoable for CallKind {
    type Info = TypeId;
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum NumberLiteral {
    Float(f64, FloatWidth),
    Integer(bool, i128, IntegerWidth),
}

impl From<parsing::parsers::number::NumberLiteral> for NumberLiteral {
    fn from(value: parsing::parsers::number::NumberLiteral) -> Self {
        match value {
            parsers::number::NumberLiteral::Unsigned(n, w) => {
                NumberLiteral::Integer(false, n as i128, w.into())
            }
            parsers::number::NumberLiteral::Signed(n, w) => {
                NumberLiteral::Integer(true, n as i128, w.into())
            }
            parsers::number::NumberLiteral::Float(n, w) => NumberLiteral::Float(n, w.into()),
            parsers::number::NumberLiteral::Inferred(_) => {
                panic!("Cannot transform inferred number")
            }
        }
    }
}
