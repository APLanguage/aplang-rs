use std::ops::Range;

#[derive(Debug, PartialEq)]
pub struct Spanned<T>(pub T, pub Range<usize>);
