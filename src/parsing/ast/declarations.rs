

use crate::parsing::{
    ast::{expressions::Expression, statements::Statement},
    tokenizer::Identifier,
    utilities::Spanned, Infoed,
};

use super::ParsedType;

type NameTypeTuple = (Spanned<Identifier>, Infoed<ParsedType>);

#[derive(Debug, Clone)]
pub enum UsePath {
    Part,
    Multiple(Box<[UsePath]>),
    Star,
}

#[derive(Debug)]
pub struct UseDeclaration(pub Box<[Infoed<UsePath>]>);

#[derive(Debug)]
pub struct Struct {
    pub name: Spanned<Identifier>,
    pub fields: Box<[NameTypeTuple]>,
}

#[derive(Debug)]
pub struct Function {
    pub name: Spanned<Identifier>,
    pub parameters: Box<[NameTypeTuple]>,
    pub ty: Option<Infoed<ParsedType>>,
    pub statements: Box<[Statement]>,
}

#[derive(Debug)]
pub struct Variable {
    pub name: Spanned<Identifier>,
    pub ty: Infoed<ParsedType>,
    pub expression: Infoed<Expression>,
}

#[derive(Debug)]
pub enum Declaration {
    Variable(Variable),
    Function(Function),
    Struct(Struct),
    Use(UseDeclaration),
}
