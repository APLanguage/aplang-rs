use std::{fmt::Debug, ops::Range};

use chumsky::{
    container::OrderedSeq, extra::Full, input::ValueInput, prelude::Rich, primitive::just,
    span::SimpleSpan, IterParser, Parser,
};
use lasso::{Rodeo, Spur};

use aplang_source::SourceFile;

use super::{
    tokenizer::{newline, Token},
    Infoable, Infoed, Spanned,
};

pub mod declarations;
pub mod expressions;
pub mod file;
pub mod number;
pub mod statements;
pub mod string;

pub struct ParserState<'a> {
    source: &'a dyn SourceFile,
    rodeo: &'a mut Rodeo,
}
impl<'a> ParserState<'a> {
    pub fn new<S: SourceFile>(rodeo: &'a mut Rodeo, input: &'a S) -> Self {
        ParserState {
            source: input,
            rodeo,
        }
    }

    pub fn slice(&self, span: Range<usize>) -> Option<&'a str> {
        self.source.read_range(span)
    }

    pub fn intern(&mut self, span: Range<usize>) -> Option<Spur> {
        self.slice(span).map(|s| self.rodeo.get_or_intern(s))
    }
}

pub type TokenParserExtra<'a> = Full<Rich<'a, Token>, ParserState<'a>, ()>;
pub trait TokenInput<'a>: ValueInput<'a, Token = Token, Span = SimpleSpan> {}
impl<'a, T> TokenInput<'a> for T where T: ValueInput<'a, Token = Token, Span = SimpleSpan> {}
pub trait TokenParser<'a, I: TokenInput<'a>, O>:
    Parser<'a, I, O, TokenParserExtra<'a>> + Clone {
}

impl<'a, I: TokenInput<'a>, O, T> TokenParser<'a, I, O> for T where T: Parser<'a, I, O, TokenParserExtra<'a>> + Clone {}

pub trait CollectBoxedSliceExt<'a, I, O>:
    IterParser<'a, I, O, TokenParserExtra<'a>> + Clone
where I: TokenInput<'a> {
    fn collect_boxed_slice(self) -> impl TokenParser<'a, I, Box<[O]>> {
        self.collect::<Vec<O>>()
            .map::<Box<[O]>, _>(|v| v.into_boxed_slice())
    }
}

impl<'a, I, O, T> CollectBoxedSliceExt<'a, I, O> for T
where
    I: TokenInput<'a>,
    T: IterParser<'a, I, O, TokenParserExtra<'a>> + Clone,
{
}

pub trait TokenParserExt<'a, I, O>: TokenParser<'a, I, O> + Clone
where
    I: TokenInput<'a>,
    O: Debug, {
    fn infoed<F, IA>(self, func: F) -> impl TokenParser<'a, I, Infoed<O>>
    where
        O: Infoable<Info = IA> + Debug + Clone,
        F: (Fn(O, SimpleSpan, &mut ParserState<'a>) -> (O, IA)) + Clone, {
        self.map_with_state(move |t, span, _state| {
            let (t, info) = func(t, span, _state);
            Infoed {
                inner: t,
                info,
                span,
            }
        })
    }

    fn paddedln(self) -> impl TokenParser<'a, I, O> {
        self.padded_by(newline().repeated())
    }

    #[inline]
    fn map_with_state<R, F: (Fn(O, SimpleSpan, &mut ParserState<'a>) -> R) + Clone>(
        self,
        f: F,
    ) -> impl TokenParser<'a, I, R> {
        self.map_with(move |t, e| f(t, e.span(), e.state()))
    }

    #[inline(always)]
    fn span(self) -> impl TokenParser<'a, I, SimpleSpan> {
        self.to_span()
    }

    fn spur(self) -> impl TokenParser<'a, I, Spur> {
        self.span()
            .map_with_state(|_, span, state| state.intern(span.into()).unwrap())
    }

    fn src(self) -> impl TokenParser<'a, I, &'a str> {
        self.span()
            .map_with_state(|_, span, state| state.slice(span.into()).unwrap())
    }

    fn spanned(self) -> impl TokenParser<'a, I, Spanned<O>> {
        self.map_with(|t, e| Spanned(t, e.span()))
    }

    fn map_into<R: From<O>>(self) -> impl TokenParser<'a, I, R> {
        self.map(Into::into)
    }
}

impl<'a, I, O, T> TokenParserExt<'a, I, O> for T
where
    I: TokenInput<'a>,
    T: TokenParser<'a, I, O> + Clone,
    O: Debug,
{
}

pub fn just_span<'a, T, I>(seq: T) -> impl TokenParser<'a, I, SimpleSpan>
where
    I: TokenInput<'a>,
    T: OrderedSeq<'a, I::Token> + Clone, {
    just(seq).to_span()
}
