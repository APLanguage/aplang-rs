pub mod utilities;
pub mod tokenizer;
pub mod parsers;
pub mod literals;
pub mod ast;

use std::ops::Range;

use chumsky::{
    extra::Full,
    input::{SliceInput, SpannedInput, Stream, ValueInput},
    prelude::Rich,
    span::SimpleSpan,
    Parser,
};

use self::tokenizer::{newline, Token};

pub struct ParserState<'a> {
    input: &'a str,
}

impl<'a> ParserState<'a> {
    pub fn new(input: &'a str) -> Self {
        ParserState { input }
    }

    pub fn slice(&self, range: Range<usize>) -> &'a str {
        self.input.slice(range)
    }
}

pub type SpannedTokenInput<'a, It> = SpannedInput<Token, SimpleSpan<usize>, Stream<It>>;
pub trait TokenInput<'a> = ValueInput<'a, Token = Token, Span = SimpleSpan>;
pub trait TokenParser<'a, I: TokenInput<'a>, O>:
    Parser<'a, I, O, Full<Rich<'a, Token>, ParserState<'a>, ()>> + Clone {
    fn paddedln(self) -> impl TokenParser<'a, I, O>;
}

impl<'a, I, O, T> TokenParser<'a, I, O> for T
where
    I: TokenInput<'a>,
    T: Parser<'a, I, O, Full<Rich<'a, Token>, ParserState<'a>, ()>> + Clone,
{
    fn paddedln(self) -> impl TokenParser<'a, I, O> {
        self.padded_by(newline().repeated())
    }
}
