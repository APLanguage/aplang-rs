use super::parsers::{
    string::{string_literal_outline_parser, StringLiteralType},
    TokenInput, TokenParser,
};
use chumsky::{input::Stream, prelude::*};
use logos::{Lexer, Logos, Source};
use strum_macros::{Display, IntoStaticStr};

#[derive(Logos, Debug, PartialEq, Eq, Clone, Hash)]
pub enum Identifier {
    #[token("fn")]
    Fn,
    #[token("for")]
    For,
    #[token("while")]
    While,
    #[token("return")]
    Return,
    #[token("break")]
    Break,
    #[token("struct")]
    Struct,
    #[token("var")]
    Var,
    #[token("val")]
    Val,
    #[token("if")]
    If,
    #[token("else")]
    Else,

    #[token("use")]
    Use,
    #[regex("as")]
    As,

    #[token("true")]
    True,
    #[regex("false")]
    False,

    #[regex(".+")]
    Custom,
}

fn string_parser_callback(lex: &mut Lexer<Token>) -> Option<StringLiteralType> {
    let end = lex.source().len();
    let start = lex.span().start;
    let len = lex.span().end - lex.span().start;
    let (literal, span) = string_literal_outline_parser()
        .map_with_span(|t, s| (t, s))
        .lazy()
        .parse(lex.source().slice(start..end).unwrap())
        .into_output()?;
    lex.bump(span.end - len);
    Some(literal)
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\f\r]+")]
pub enum Token {
    #[token("->")]
    ArrowRight,

    #[token("+")]
    Plus,
    #[token("-")]
    Minus,

    #[token("/")]
    Slash,
    #[token("//")]
    SlashSlash,
    #[token("*")]
    Asterisk,
    #[token("**")]
    AsteriskAsterisk,
    #[token("%")]
    Percent,

    #[token("&")]
    Ampersand,
    #[token("|")]
    Bar,
    #[token("^")]
    Circumflex,

    // #[token(">>")]
    // GreaterGreater,
    #[token("<<")]
    LessLess,
    // #[token(">>>")]
    // GreaterGreaterGreater,

    #[token("&&")]
    AmpersandAmpersand,
    #[token("||")]
    BarBar,

    #[token(">")]
    Greater,
    #[token(">=")]
    GreaterEqual,
    #[token("<")]
    Less,
    #[token("<=")]
    LessEqual,

    #[token("==")]
    EqualEqual,
    #[token("!=")]
    BangEqual,

    #[token("!")]
    Bang,
    #[token("~")]
    Tilde,

    #[token("=")]
    Equal,

    #[token("+=")]
    PlusEqual,
    #[token("-=")]
    MinusEqual,

    #[token("/=")]
    SlashEqual,
    #[token("//=")]
    SlashSlashEqual,
    #[token("*=")]
    AsteriskEqual,
    #[token("**=")]
    AsteriskAsteriskEqual,
    #[token("%=")]
    PercentEqual,

    #[token("&=")]
    AmpersandEqual,
    #[token("|=")]
    BarEqual,
    #[token("^=")]
    CircumflexEqual,

    #[token(">>=")]
    GreaterGreaterEqual,
    #[token("<<=")]
    LessLessEqual,
    #[token(">>>=")]
    GreaterGreaterGreaterEqual,

    #[token("(")]
    ParenOpen,
    #[token(")")]
    ParenClosed,

    #[token("{")]
    BraceOpen,
    #[token("}")]
    BraceClosed,

    #[token("[")]
    BracketOpen,
    #[token("]")]
    BracketClosed,

    #[token(".")]
    Dot,
    #[token(":")]
    Colon,
    #[token("::")]
    ColonColon,
    #[token(";")]
    Semicolon,
    #[token(",")]
    Comma,

    #[regex(r"[_a-zA-Z]\w*", |lex| Identifier::lexer(lex.slice()).next().unwrap())]
    Identifier(Identifier),

    #[regex(r"\d[_\w]*(\.\d[_\w]*)?")]
    Number,

    #[regex("r#*\"", string_parser_callback)]
    String(StringLiteralType),

    #[token("\n")]
    NewLine,

    Error,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, Display, IntoStaticStr)]
pub enum Operation {
    Addition,
    Substraction,

    Division,
    DivisionInteger,
    Multiplication,
    Power,

    And,
    Or,

    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Equal,
    NotEqual,

    Remainder,

    AndBitwise,
    OrBitwise,
    XOR,

    ShiftRight,
    ShiftLeft,
    ShiftRightUnsigned,

    Not,
    NotBitwise,

    Unknown,
}
#[derive(PartialEq, Eq, Debug, Clone, Copy, Display, IntoStaticStr)]
pub enum OperationGroup {
    Logic,
    Equality,
    Comparison,
    Term,
    Factor,
    Bitops,
}

impl From<Token> for Operation {
    fn from(value: Token) -> Self {
        use Operation::*;
        use Token::*;
        match value {
            Plus => Addition,
            Minus => Substraction,
            Slash => Division,
            SlashSlash => DivisionInteger,
            Asterisk => Multiplication,
            AsteriskAsterisk => Power,
            Percent => Remainder,
            Ampersand => AndBitwise,
            Bar => OrBitwise,
            Circumflex => XOR,
            // GreaterGreater => ShiftRight,
            LessLess => ShiftLeft,
            // GreaterGreaterGreater => ShiftRightUnsigned,
            AmpersandAmpersand => And,
            BarBar => Or,
            Token::Greater => Operation::Greater,
            Token::GreaterEqual => Operation::GreaterEqual,
            Token::Less => Operation::Less,
            Token::LessEqual => Operation::LessEqual,
            EqualEqual => Operation::Equal,
            BangEqual => NotEqual,
            PlusEqual => Addition,
            MinusEqual => Substraction,
            SlashEqual => Division,
            SlashSlashEqual => DivisionInteger,
            AsteriskEqual => Multiplication,
            AsteriskAsteriskEqual => Power,
            PercentEqual => Remainder,
            AmpersandEqual => AndBitwise,
            BarEqual => OrBitwise,
            CircumflexEqual => XOR,
            GreaterGreaterEqual => ShiftRight,
            LessLessEqual => ShiftLeft,
            GreaterGreaterGreaterEqual => ShiftRightUnsigned,
            Bang => Not,
            Tilde => NotBitwise,
            _ => Unknown,
        }
    }
}

pub fn ident<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, Identifier> + Clone {
    select! { Token::Identifier(id) => id }
}

#[inline]
pub fn keyword<'a, I: TokenInput<'a>>(id: Identifier) -> impl TokenParser<'a, I, ()> + Clone {
    just(Token::Identifier(id)).ignored()
}

#[inline]
pub fn newline<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, ()> + Clone {
    any().filter(|t| matches!(t, Token::NewLine)).ignored()
}

pub fn tokenize(input: &str) -> impl TokenInput<'_> {
    Stream::from_iter(
        Token::lexer(input)
            .spanned()
            .map(|(tok, span)| (tok.unwrap_or(Token::Error), span.into())),
    )
    .spanned((input.len()..input.len()).into())
}
