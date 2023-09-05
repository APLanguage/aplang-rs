use chumsky::span::SimpleSpan;

use crate::{source::DeclarationPath, typing::TypeId};

pub mod ast;
pub mod parsers;
pub mod tokenizer;

#[derive(Debug, PartialEq)]
pub struct Infoed<T: Infoable> {
    pub inner: T,
    pub info: T::Info,
    pub span: SimpleSpan,
}

impl<T: Infoable> Infoed<T> {
    pub fn into_spanned_inner(self) -> Spanned<T> {
        self.into()
    }

    pub fn into_spanned_info(self) -> Spanned<T::Info> {
        let Infoed {
            inner: _,
            info,
            span,
        } = self;
        Spanned(info, span)
    }

    pub fn as_spanned_inner(&self) -> Spanned<&T> {
        let Infoed {
            inner,
            info: _,
            span,
        } = self;
        Spanned(inner, *span)
    }

    pub fn as_spanned_info(&self) -> Spanned<&T::Info> {
        let Infoed {
            inner: _,
            info,
            span,
        } = self;
        Spanned(info, *span)
    }

    pub fn as_spanned_cloned_info(&self) -> Spanned<T::Info>
    where T::Info: ToOwned<Owned = T::Info> {
        let Infoed {
            inner: _,
            info,
            span,
        } = self;
        Spanned(info.to_owned(), *span)
    }
}

pub trait Infoable {
    type Info;
}

#[derive(Debug, PartialEq)]
pub enum DeclarationRef {
    LocalVariable(usize),
    GlobalVariable(DeclarationPath),
    Function(DeclarationPath),
}

#[derive(Debug, PartialEq)]
pub enum Info {
    Type(Option<TypeId>),
    DeclarationRef(Option<DeclarationRef>),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Spanned<T>(pub T, pub SimpleSpan);

impl<T> Spanned<T> {
    pub fn map_new<R, F>(&self, mapping: F) -> Spanned<R>
    where F: Fn(&T) -> R {
        Spanned(mapping(&self.0), self.1)
    }

    pub fn map_to<R>(&self, to: R) -> Spanned<R> {
        Spanned(to, self.1)
    }

    pub fn map_move<R, F>(self, mapping: F) -> Spanned<R>
    where F: Fn(T) -> R {
        Spanned(mapping(self.0), self.1)
    }

    pub fn map_into<R>(self) -> Spanned<R>
    where T: Into<R> {
        Spanned(self.0.into(), self.1)
    }
}

impl<T> Spanned<&T> {
    pub fn clone_inner(&self) -> Spanned<T>
    where T: ToOwned<Owned = T> {
        Spanned(self.0.to_owned(), self.1)
    }
}

impl<T: Infoable> From<Infoed<T>> for Spanned<T> {
    fn from(val: Infoed<T>) -> Self {
        Spanned(val.inner, val.span)
    }
}
