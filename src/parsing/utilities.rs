use core::ops::Fn;

use chumsky::span::SimpleSpan;

pub type SourceId = usize;

#[derive(Debug, PartialEq)]
pub struct Spanned<T>(pub T, pub SimpleSpan);

impl<T> Spanned<T> {
    pub fn map_new<R, F>(&self, mapping: F) -> Spanned<R>
    where F: Fn(&T) -> R {
        Spanned(mapping(&self.0), self.1)
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