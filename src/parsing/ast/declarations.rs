use std::iter::empty;

use crate::parsing::{
    ast::{expressions::Expression, statements::Statement},
    utilities::Spanned,
    Infoed,
};

use super::ParsedType;
use chumsky::span::SimpleSpan;
use itertools::Either;
use lasso::Spur;
use traversal::DftLongestPaths;

type NameTypeTuple = (Spanned<Spur>, Infoed<ParsedType>);

#[derive(Debug, Clone, PartialEq)]
pub enum UsePathEnd {
    Split(Box<[UsePath]>),
    Star(SimpleSpan),
    Single,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UsePath(pub Box<[SimpleSpan]>, pub UsePathEnd);

#[derive(Debug, Clone, PartialEq)]
pub enum UsePathStart {
    Root,
    Std,
    Lib,
}

#[derive(Debug)]
pub struct UseDeclaration(pub Box<[SimpleSpan]>, pub UsePath);

impl UseDeclaration {
    pub fn flatten_tree(&self) -> impl Iterator<Item = (Box<[SimpleSpan]>, bool)> + '_ {
        use UsePathEnd::*;
        DftLongestPaths::new(&self.1, |node: &UsePath| match &node.1 {
            Star(_) | Single => Either::Left(empty()),
            Split(branches) => Either::Right(branches.iter()),
        })
        .map(|path| {
            let star = path.last().map(|p| matches!(p.1, Star(_))).unwrap_or(false);
            (
                path.iter()
                    .flat_map(|p| p.0.clone().to_vec())
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                star,
            )
        })
    }
}

#[derive(Debug)]
pub struct Struct {
    pub name: Spanned<Spur>,
    pub fields: Box<[NameTypeTuple]>,
}

#[derive(Debug)]
pub struct Function {
    pub name: Spanned<Spur>,
    pub parameters: Box<[NameTypeTuple]>,
    pub ty: Option<Infoed<ParsedType>>,
    pub statements: Box<[Statement]>,
}

#[derive(Debug)]
pub struct Variable {
    pub name: Spanned<Spur>,
    pub ty: Infoed<ParsedType>,
    pub expression: Infoed<Expression>,
}

#[derive(Debug)]
pub enum Declaration {
    Variable(Variable),
    Function(Function),
    Struct(Struct),
}
