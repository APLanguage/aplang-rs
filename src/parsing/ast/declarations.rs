use crate::parsing::{
    ast::{statements::Statement, expressions::Expression},
    tokenizer::Identifier,
    utilities::Spanned,
};

#[derive(Debug)]
pub enum ParsedType {
    Data(Spanned<Identifier>),
    Array(Box<ParsedType>),
}

#[derive(Debug)]
pub struct Struct {
    pub name: Spanned<Identifier>,
    pub fields: Box<[(Spanned<Identifier>, Spanned<ParsedType>)]>,
}

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