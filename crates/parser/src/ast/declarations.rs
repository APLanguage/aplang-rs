use std::iter::empty;

use crate::{
    ast::{expressions::Expression, statements::Statement},
    Spanned,
};

use super::ParsedType;
use chumsky::span::SimpleSpan;
use itertools::Either;
use lasso::Spur;
use strum_macros::IntoStaticStr;
use traversal::DftLongestPaths;

#[derive(Debug, Clone, PartialEq)]
pub enum UsePathEnd {
    Split(Box<[UsePath]>),
    Star(SimpleSpan),
    Single(Option<Spanned<Spur>>),
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
pub struct UseDeclaration {
    pub scope: Box<[SimpleSpan]>,
    pub path: UsePath,
}

pub struct FlatUseDeclaration {
    pub path: Box<[SimpleSpan]>,
    pub star: bool,
    pub single_alias: Option<Spanned<Spur>>,
}

impl UseDeclaration {
    pub fn flatten_tree(&self) -> impl Iterator<Item = FlatUseDeclaration> + '_ {
        use UsePathEnd::*;
        DftLongestPaths::new(&self.path, |node: &UsePath| match &node.1 {
            Star(_) | Single(_) => Either::Left(empty()),
            Split(branches) => Either::Right(branches.iter()),
        })
        .map(|path| {
            let star = path.last().map(|p| matches!(p.1, Star(_))).unwrap_or(false);
            let single_alias = match path.last() {
                Some(UsePath(_, Single(alias))) => *alias,
                _ => None,
            };
            FlatUseDeclaration {
                path: path
                    .iter()
                    .flat_map(|p| p.0.clone().to_vec())
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                star,
                single_alias,
            }
        })
    }
}

#[derive(Debug)]
pub struct Field {
    pub reassignable: Spanned<bool>,
    pub name: Spanned<Spur>,
    pub ty: Spanned<ParsedType>,
}

#[derive(Debug)]
pub struct Parameter {
    pub reassignable: Option<Spanned<bool>>,
    pub name: Spanned<Spur>,
    pub ty: Spanned<ParsedType>,
}

#[derive(Debug)]
pub struct Function {
    pub name: Spanned<Spur>,
    pub parameters: Box<[Parameter]>,
    pub ty: Option<Spanned<ParsedType>>,
    pub statements: Box<[Spanned<Statement>]>,
}

#[derive(Debug)]
pub struct Variable {
    pub reassignable: Spanned<bool>,
    pub name: Spanned<Spur>,
    pub ty: Option<Spanned<ParsedType>>,
    pub expression: Spanned<Expression>,
}

#[derive(Debug, IntoStaticStr)]
pub enum Declaration {
    Variable(Variable),
    Function(Function),
}
