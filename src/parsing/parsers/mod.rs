use std::{fmt::Debug, ops::Range};

use chumsky::{
    extra::Full,
    input::{SliceInput, ValueInput},
    prelude::Rich,
    span::SimpleSpan,
    IterParser, Parser,
};

use super::{
    tokenizer::{newline, Token},
    utilities::SourceId,
    Infoed,
};

pub mod declarations;
pub mod expressions;
pub mod file;
pub mod statements;
pub mod number;
pub mod string;

pub struct ParserState<'a> {
    source: &'a str,
    source_id: Option<SourceId>,
}
impl<'a> ParserState<'a> {
    pub fn new(input: &'a str) -> Self {
        ParserState {
            source: input,
            source_id: None,
        }
    }

    pub fn source_id(&self) -> Option<usize> {
        self.source_id
    }

    pub fn slice(&self, span: Range<usize>) -> &'a str {
        self.source.slice(span)
    }
}

pub type TokenParserExtra<'a> = Full<Rich<'a, Token>, ParserState<'a>, ()>;
pub trait TokenInput<'a> = ValueInput<'a, Token = Token, Span = SimpleSpan>;
pub trait TokenParser<'a, I: TokenInput<'a>, O> = Parser<'a, I, O, TokenParserExtra<'a>> + Clone;

pub trait CollectBoxedSliceExt<'a, I, O>:
    IterParser<'a, I, O, TokenParserExtra<'a>> + Clone
where I: TokenInput<'a> {
    fn collect_boxed_slice(self) -> impl TokenParser<'a, I, Box<[O]>> + Clone;
}

impl<'a, I, O, T> CollectBoxedSliceExt<'a, I, O> for T
where
    I: TokenInput<'a>,
    T: IterParser<'a, I, O, TokenParserExtra<'a>> + Clone,
{
    fn collect_boxed_slice(self) -> impl TokenParser<'a, I, Box<[O]>> + Clone {
        self.collect::<Vec<O>>()
            .map::<Box<[O]>, _>(|v| v.into_boxed_slice())
    }
}

pub trait TokenParserExt<'a, I, O>: TokenParser<'a, I, O> + Clone
where
    I: TokenInput<'a>,
    O: Debug, {
    fn infoed(self) -> impl TokenParser<'a, I, Infoed<O>> + Clone {
        self.map_with_state(|t, span, state| Infoed {
            inner: t,
            info: None,
            loc: super::Location {
                span,
                source_id: state.source_id,
            },
        })
    }

    fn paddedln(self) -> impl TokenParser<'a, I, O> + Clone {
        self.padded_by(newline().repeated())
    }
}

impl<'a, I, O, T> TokenParserExt<'a, I, O> for T
where
    I: TokenInput<'a>,
    T: TokenParser<'a, I, O> + Clone,
    O: Debug,
{
}
