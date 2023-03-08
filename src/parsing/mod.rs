use std::ops::Range;

use chumsky::{
    extra::Full,
    input::{SpannedInput, Stream, ValueInput, SliceInput},
    prelude::Rich,
    span::SimpleSpan,
    Parser,
};

use self::tokenizer::Token;

pub mod control_flow;
pub mod data;
pub mod declaration;
pub mod expression;
pub mod file;
pub mod literals;
pub mod statement;
pub mod tokenizer;
pub mod utilities;

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
pub trait TokenParser<'a, I: TokenInput<'a>, O> =
    Parser<'a, I, O, Full<Rich<'a, Token>, ParserState<'a>, ()>>;
